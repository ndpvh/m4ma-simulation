#' Create an initial condition
#' 
#' @export 
initial_condition <- function(model,
                              N, 
                              goal_stack, 
                              walk_duration, 
                              sit_duration,
                              group_size = matrix(c(1, 1), nrow = 2),
                              ...) {
    
    # Create a small utility function that takes in the list of agents that 
    # already exist, the criterion for the while-loop, the duration of the 
    # goals, and the criterion for which goals to add. 
    #
    # Introduced because the procedure of adding "walk" and "sit" goals is the 
    # same.
    inx <- function(agent_list, 
                    duration, 
                    kind, 
                    number) {
        
        # Get the goals of interest
        idx <- which(grepl(kind, sapply(goal_stack, predped::id), fixed = TRUE))

        # Keep track of how many agents were added in this round. This will allow 
        # us to correct how long the people should be waiting if a sitting goal 
        # is assigned.
        n <- 1

        # Add as many agents as the criterion demands. For each, choose a random 
        # goal
        while(length(agent_list) < number) {
            # Sample a random number of people to simulate. Handy for groups
            agent_number <- ifelse(
                nrow(group_size) > 1, 
                sample(group_size[,1], 1, prob = group_size[,2]), 
                group_size[1, 1]
            )

            # Choose a random goal from the goal stack
            idy <- ifelse(length(idx) > 1, sample(idx, 1), idx)
            goal <- goal_stack[[idy]]
    
            # Delete this goal from the potential goals to be assigned
            idx <- idx[idx != idy]
            
            # Adjust the counter of the goal
            predped::counter(goal) <- duration(n)
            n <- n + 1
    
            # Add the agent and return
            agent_list <- append(
                agent_list, 
                add_group(
                    model, 
                    goal, 
                    group_number = length(agent_list), 
                    agent_number = agent_number,
                    ...
                )
            )
        }

        return(agent_list)
    }

    # Execute the function for both walking and sitting goals
    agents <- inx(
        list(), 
        walk_duration, 
        "walk",
        N[1]
    )

    agents <- inx(
        agents, 
        sit_duration, 
        "sit", 
        sum(N)
    )

    return(agents)
}

#' Create an agent
#' 
#' @export
add_agent <- function(model, 
                      goal,
                      group_number = 1,
                      standing_start = 0.1, 
                      individual_differences = FALSE) {

    # Extract which of the objects in the background corresponds to the goal
    # that was drawn
    obj <- predped::objects(model@setting)
    idx <- sapply(obj, \(x) grepl(predped::id(x), predped::id(goal), fixed = TRUE))

    obj <- obj[idx][[1]]

    # Draw a random agent from the parameter distributions
    params <- predped::parameters(model)
    idx <- sample(
        model@archetypes,
        1,
        prob = model@weights,
        replace = TRUE
    )
    idy <- params[["params_archetypes"]]$name == idx

    color <- params[["params_archetypes"]]$color[idy]
    params <- predped::generate_parameters(
        1,
        mean = params[["params_archetypes"]][idy, ],
        Sigma = params[["params_sigma"]][[idx]],
        bounds = params[["params_bounds"]],
        archetype = idx,
        individual_differences = individual_differences
    )

    # Get the agent's position relative to the goal. Computed by extracting the 
    # orientation of the tablet and then adjusting this orientation so that the 
    # agent is standing in front of it.
    if(inherits(obj, "rectangle")) {
        orientation <- predped::orientation(obj) - pi/2
        
        alpha <- orientation + pi
        R <- matrix(
            c(cos(alpha), sin(alpha), -sin(alpha), cos(alpha)), 
            nrow = 2,
            ncol = 2
        )
        center <- predped::position(goal) + R %*% c(params[["radius"]] + 2e-1, 0)
    } else {
        # Create alternative positions at which the agent can be located
        alpha <- seq(0, 2 * pi, pi/8)
        rep_goal <- matrix(
            predped::position(goal),
            nrow = length(alpha), 
            ncol = 2, 
            byrow = TRUE
        )

        center <- rep_goal + (params[["radius"]] + 2e-1) * cbind(cos(alpha), sin(alpha))

        # For each potential position, check whether the agent would intersect 
        # the object in question. Use the nodes_on_circumference function for 
        # the object, and then check whether any of its nodes lies within the 
        # agent at a given position.
        obj_nodes <- predped::nodes_on_circumference(obj)
        dummy <- predped::circle(
            center = c(0, 0), 
            radius = params[["radius"]]
        )

        idx <- sapply(
            1:nrow(center), 
            function(i) {
                predped::center(dummy) <- center[i,]
                return(
                    !any(
                        predped::in_object(dummy, obj_nodes)
                    )
                )
            }
        )

        # Choose one of the left-over positions as the starting position
        idx <- ifelse(sum(idx) > 1, sample(which(idx), 1), which(idx))
        center <- center[idx,]

        # Also identify the orientation the agent has to look in to look 
        # straight at the goal
        xy <- predped::position(goal)[] - center
        orientation <- atan2(xy[2], xy[1])
    }

    # With this defined, we can create the agent itself
    return(
        predped::agent(
            center = as.numeric(center), 
            radius = params[["radius"]],
            orientation = orientation * 180 / pi,
            speed = standing_start * params[["preferred_speed"]], 
            current_goal = goal, 
            goals = list(), 
            parameters = params,
            group = group_number,
            color = color,
            status = "completing goal"
        )
    )
}


#' Add a group of agents to the simulation
#'
#' This function is the same as the one defined in \code{predped}, but with the 
#' exception that it uses the \code{add_agent} function defined in this same 
#' file instead.
#'
#' @param object Object of the \code{\link[predped]{predped-class}}.
#' @param agent_number Numeric denoting the number of agents to add. Defaults
#' to \code{1}.
#' @param standing_start Numeric denoting the factor of their preferred speed
#' that agents move when they just came from standing still. Defaults to
#' \code{0.1}.
#' @param individual_differences Logical denoting whether to use the standard
#' deviations in the parameter list to create some variation in the parameters.
#' Defaults to \code{FALSE}.
#'
#' @return List of instances of the \code{\link[predped]{agent-class}}.
#'
#' @export
add_group <- function(model,
                      goal,
                      agent_number = 1,
                      group_number = 1,
                      standing_start = 0.1,
                      individual_differences = FALSE) {

    agents <- list()
    obj <- predped::objects(model@setting)

    # Generate a single agent who will serve as the basis for all other
    # agents (imposed so that all agents in a group share the same goals and
    # enter the space at the same location).
    agents[[1]] <- add_agent(
        model,
        goal,
        group_number = group_number,
        standing_start = standing_start,
        individual_differences = individual_differences
    )

    # If only one agent needed, we will return it immediately
    if(agent_number == 1) {
        return(agents)
    }

    # Otherwise, we should loop over all other agents and change their
    # parameter values (and color) so that each agent is unique.
    #
    # First, draw the parameters for each of the new agents
    model_parameters <- predped::parameters(model)
    idx <- sample(
        model@archetypes,
        agent_number - 1,
        prob = model@weights,
        replace = TRUE
    )

    # Create a whole scale of new positions that other agents might take
    #
    # Based on a small calculation, I found out that for a given radius R that 
    # is shared among all circles, we can put 6 of these circles besides each 
    # other. Adding some padding and using the maximal radius 0.3, we then get 
    # to the positions created here.
    alpha <- 0 + 0:5 * pi / 3
    start <- matrix(
        predped::position(agents[[1]]), 
        nrow = length(alpha), 
        ncol = 2,
        byrow = TRUE
    )

    positions <- start + (2 * 0.3 + 5e-2) * cbind(cos(alpha), sin(alpha))

    # Check whether any of these positions implies that the agent will intersect
    # with something or someone else. If so, delete these ids
    obj_nodes <- do.call(
        "rbind", 
        lapply(obj, predped::nodes_on_circumference)
    )
    dummy <- predped::circle(
        center = c(0, 0), 
        radius = agents[[1]]@radius
    )

    idy <- sapply(
        1:nrow(positions), 
        function(i) {
            predped::center(dummy) <- positions[i,]
            return(
                !any(
                    predped::in_object(dummy, obj_nodes)
                )
            )
        }
    )
    positions <- positions[idy,]

    # Loop over these agents
    for(i in 2:agent_number) {
        # Check whether there is any space left
        if(length(positions) == 0) {
            warning(
                paste0(
                    "No space left for group members. ",
                    "Returning group of ", 
                    length(agents), 
                    " instead."
                )
            )

            return(agents)
        }

        # Create a temporary agent as a copy of the first simulated agent
        tmp_agent <- agents[[1]]

        # Change this temporary agent's characterstics based on simulated
        # parameters
        tmp <- model_parameters[["params_archetypes"]]
        mean_params <- tmp[tmp$name == idx[i - 1], ]
        params <- predped::generate_parameters(
            1,
            mean = mean_params,
            Sigma = model_parameters[["params_sigma"]][[idx[i - 1]]],
            bounds = model_parameters[["params_bounds"]],
            archetype = idx[i - 1],
            individual_differences = individual_differences
        )

        predped::id(tmp_agent) <- paste(sample(letters, 5, replace = TRUE), collapse = "")
        predped::radius(tmp_agent) <- params$radius
        predped::color(tmp_agent) <- mean_params$color
        predped::speed(tmp_agent) <- standing_start * params[["preferred_speed"]]

        # Position should differ from the main agent. Draw one of the generated
        # positions for this
        idy <- sample(1:nrow(positions), 1)

        predped::position(tmp_agent) <- positions[idy,]
        positions <- positions[-idy,]

        # Add the agent to the list
        agents[[i]] <- tmp_agent
    }

    return(agents)
}