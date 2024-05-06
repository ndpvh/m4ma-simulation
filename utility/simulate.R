# Load predped
library(predped)

# Load the setting and goals within this script
source(file.path("utility", "setting.R"))
source(file.path("utility", "goals.R"))

#' Create a function that will simulate a given experimental procedure with 
#' the following parameters.
#'
#' @param n Integer denoting the number of agents to put in the simulation.
#' @param n_sitting Integer denoting how many of the participants should sit at
#' any given time.
#' @param iterations Integer denoting the number of iterations to run.
#' @param walking_goal_duration Integer or function that determines the duration 
#' of each goal assigned to a tablet in the square. Defaults to a function that 
#' draws `x` numbers from a normal distribution with mean 10 and standard 
#' deviation 2.
#' @param sitting_goal_duration Integer denoting the duration of a goal in which
#' the agent has to wait for a while. Defaults to `5 * 60 / time_step`, or 5 min
#' of rest.
#' @param walking_goals List containing the walking goals.
#' @param sitting_goals List containing the sitting goals.
#' @param time_step Numeric denoting the time step at each iteration in seconds.
#' Defaults to `0.5` or 500msec.
#' @param plot_live Logical denoting whether to plot each iteration while the 
#' simulation is going on. Defaults to `FALSE`.
#' @param plot_time Numeric denoting the amount of time (in seconds) to wait 
#' between iterations, i.e., the time between updating the plot. Defaults to 
#' `0.2`.
#' 
#' @return Trace, or list of different states at the different iterations.
simulate_experiment <- function(n,
                                n_sitting,
                                iterations, 
                                predped_model,
                                walking_goal_duration = \(x) rnorm(x, 10, 2),
                                sitting_goal_duration = 5 * 60 / time_step,
                                walking_goals = walk, 
                                sitting_goals = sit,
                                time_step = 0.5,
                                plot_live = FALSE, 
                                plot_time = 0.2) {

    if(n > length(walking_goals) + length(sitting_goals)) {
        stop(paste0(n, 
                    " agents specified, but only ", 
                    length(walking_goals) + length(sitting_goals),
                    " goals available."))
    }

    if(n_sitting > length(sitting_goals)) {
        stop(paste0(n_sitting, 
                    " sitting agents specified, but only ",
                    length(sitting_goals), 
                    " sitting goals available."))
    }

    # Precompute the edges for later use in the simulation
    edges <- precompute_edges(predped_model@setting)

    # Create the initial condition 
    agents <- initial_condition(n, 
                                n_sitting,
                                predped_model,
                                walking_goals = walk,
                                sitting_goals = sit,
                                walking_goal_duration = walking_goal_duration,
                                sitting_goal_duration = sitting_goal_duration,
                                time_step = time_step)

    # Get all ids of the goals. Will be useful later
    walking_ids <- sapply(walk, \(x) id(x))
    sitting_ids <- sapply(sit, \(x) id(x))

    # Create a mapping of the goal ids and the actual goal specifications. This
    # will help in dynamically assigning goals to an agent.
    walking <- list()
    for(i in seq_along(walk)) {
        walking[[id(walk[[i]])]] <- walk[[i]]
    }

    sitting <- list()
    for(i in seq_along(sit)) {
        sitting[[id(sit[[i]])]] <- sit[[i]]
        counter(sitting[[id(sit[[i]])]]) <- sitting_goal_duration # Done to ensure that everyone sits down an equal amount of time
    }

    # With this done, now also make a list mapping which agent is currently doing
    # which goal. This will get updated dynamically and allows us to see which 
    # goals are free to do and which are not.
    walking_occupied <- list()
    sitting_occupied <- list()
    for(i in seq_along(agents)) {
        tmp <- agents[[i]]

        if(id(current_goal(tmp)) %in% walking_ids) {
            walking_occupied[[id(tmp)]] <- id(current_goal(tmp))
        } else {
            sitting_occupied[[id(tmp)]] <- id(current_goal(tmp))
        }
    }    

    # Bookkeeping that needs to be done right. Done with the following helper 
    # variables:
    #   - Counter for how many people are sitting down and how many are walking 
    #     around
    #   - Counter defining how long people have sat down already. Will help in 
    #     defining a cycle and reset every cycle
    #   - Variable defining the number of iterations within each cycle
    counter <- c("sitting" = n_sitting, 
                 "walking" = n - n_sitting)

    sitting_counter <- sitting_counter_copy <- c()
    for(i in seq_along(agents)) {
        idx <- id(agents[[i]])
        sitting_counter[idx] <- sitting_counter_copy[idx] <- sitting_goal_duration
    }

    cycle_length <- ceiling(sitting_goal_duration * n / n_sitting)

    # Create the trace list and simulate data for each iteration
    state <- list(setting = setting, agents = agents)
    trace <- list(state)
    for(i in seq_len(iterations)) {
        cat(paste0("\rIteration ", i))

        # Update the state of the agents
        state <- update_state(state, 
                              predped_model@setting,
                              time_step = time_step,
                              precomputed_edges = edges,
                              report = FALSE)

        # Check whether an agent has just finished their goal. If so, they need
        # to be assigned another goal from the list
        for(j in seq_along(state$agents)) {
            current_agent <- state$agents[[j]]

            # Define which sitting and walking goals are still available
            walking_avail <- walking_ids[!(walking_ids %in% walking_occupied)]
            sitting_avail <- sitting_ids[!(sitting_ids %in% sitting_occupied)]

            # Define the counter that is running for this agent, if relevant
            if(id(current_agent) %in% names(sitting_occupied)) {
                sitting_counter[id(current_agent)] <- sitting_counter[id(current_agent)] - 1
            }

            # Check whether the agent has just completed their goal. In this case
            # predped gives them a predefined goal to go to the exit, named 
            # "goal exit"
            if(id(current_goal(current_agent)) == "goal exit") {
                # First check whether there are still enough agents doing a 
                # sitting goal. If this is not the case, and this agent is one 
                # that still needs to sit down, then assign them a sitting goal.
                #
                # Important to note here that we only consider those agents who
                # have to sit for the maximal amount, as this is what is assumed
                # by the cycle structure of the experiment.
                if((counter["sitting"] != n_sitting) & 
                   (sitting_counter[id(current_agent)] == max(sitting_counter))) {
                    # Sample a random goal to give to the agent
                    goal_name <- sample(sitting_avail, 1)
                    current_goal(current_agent) <- sitting[[goal_name]]
                    
                    # Update the walking and sitting lists
                    walking_occupied <- walking_occupied[names(walking_occupied) != id(current_agent)]
                    sitting_occupied[id(current_agent)] <- goal_name

                    counter["sitting"] <- counter["sitting"] + 1
                    counter["walking"] <- counter["walking"] - 1

                    # Let the agent replan
                    status(current_agent) <- "replan"

                # If there is no space for an additional sitting agent, then we
                # need to give them a walking goal. Importantly, there is a 
                # difference in bookkeeping for those who originally sat down
                # and those that were just walking around
                } else {
                    # Check whether there is actually something available, if 
                    # not, put the agent in a waiting state
                    if(length(walking_avail) == 0) {
                        status(current_agent) <- "wait"
                    } else {
                        # Sample a random goal to give the agent
                        goal_name <- sample(walking_avail, 1)
                        current_goal(current_agent) <- walking[[goal_name]]

                        # Update the counter of the goal to reflect differences
                        # in how quickly they update them
                        counter(current_goal(current_agent)) <- walking_goal_duration(1)

                        # Update the walking and/or sitting lists
                        if(id(current_agent) %in% names(sitting_occupied)) {
                            sitting_occupied <- sitting_occupied[names(sitting_occupied) != id(current_agent)]
                            walking_occupied[id(current_agent)] <- goal_name

                            counter["sitting"] <- counter["sitting"] - 1
                            counter["walking"] <- counter["walking"] + 1
                        } else {
                            walking_occupied[id(current_agent)] <- goal_name
                        }

                        # Let the agent replan
                        status(current_agent) <- "replan"
                    }
                }

                # Put the agent back in the agent-list
                state$agents[[j]] <- current_agent
            }
        }

        # Check whether we already went full cycle. If so, then we need to reset
        # the counters
        if(iterations %% cycle_length == 0) {
            sitting_counter <- sitting_counter_copy
        }

        # Save the new state in the trace
        trace[[i + 1]] <- state

        # If you want to plot the result immediately, do so
        if(plot_live) {
            print(plot(list(state), 
                       trace = TRUE,
                       print_progress = FALSE,
                       iterations = i)[[1]])
            Sys.sleep(plot_time)
        }
    }

    cat("\n")

    return(trace)
}

# Function that will precompute the edges along which agents can walk around.
precompute_edges <- function(setting) {
    print("Precomputing edges")

    # Create mock edges
    edges <- create_edges(c(0, 0), 
                          c(0, 0), 
                          setting,
                          space_between = 0.35)

    # Delete all references to agents and goals from these edges. 
    edges$edges <- edges$edges[!(edges$edges$from %in% c("agent", "goal")),]
    edges$edges <- edges$edges[!(edges$edges$to %in% c("agent", "goal")),]
    edges$nodes <- edges$nodes[!(edges$nodes$node_ID %in% c("agent", "goal")),]

    return(edges)
}

# Create a function that will create the initial conditions
initial_condition <- function(n, 
                              n_sitting,
                              predped_model,
                              walking_goal_duration = \(x) rnorm(x, 10, 2),
                              sitting_goal_duration = 5 * 60 / time_step,
                              walking_goals = walk, 
                              sitting_goals = sit,
                              time_step = 0.5) {

    # Create agents and put them in an agent list. First do this for the agents
    # that will have to walk around and then for those who will sit down.
    agents <- create_agents(n - n_sitting, 
                            predped_model, 
                            goals = walking_goals, 
                            goal_duration = walking_goal_duration,
                            time_step = time_step)
    sitting_agents <-  create_agents(n_sitting, 
                                     predped_model, 
                                     goals = sitting_goals, 
                                     goal_duration = \(x) sitting_goal_duration,
                                     time_step = time_step)

    # For the sitting agents, we need to subtract a given number of their counter
    # so that not everyone leaves at the same point. 
    for(i in seq_along(sitting_agents)) {
        num <- counter(current_goal(sitting_agents[[i]]))
        counter(current_goal(sitting_agents[[i]])) <- round(num - (i - 1) * (num / n_sitting))
    }
    
    # Add the sitting agents to the rest
    agents <- append(agents, sitting_agents)
    
    return(agents)
}

# Helper function to create an agent based on the variables in create_initial_condition.
create_agents <- function(n, 
                          predped_model, 
                          goals = list(), 
                          goal_duration = \(x) 5,
                          time_step = 0.5,
                          individual_differences = TRUE) {

    # Sample the goals to assign to the agents
    idx <- sample(seq_along(goals), n, replace = FALSE)

    # Create agents and put them in an agent list.
    obj <- predped_model@setting@objects
    agents <- list()
    for(i in seq_along(idx)) {
        # Sample a random set of parameters from the `predped` class. Furthermore
        # adjust the speed of the agent depending on the time_step
        idz <- sample(1:nrow(predped_model@parameters), 
                      1, 
                      prob = predped_model@weights)
        params <- predped_model@parameters[idz,]

        # Save the color and generate individual differences if needed
        color <- params$color
        params <- draw_parameters(1, 
                                  params, 
                                  archetype = params$name,
                                  individual_differences = individual_differences)

        # Adjust the preferred speed to the time_step taken
        params$preferred_speed <- params$preferred_speed * time_step

        # Adjust the duration of the current goal
        agent_goal <- goals[[idx[i]]]
        agent_goal@counter <- goal_duration(1)

        # Adjust the path the agent will have to walk towards the goal. Agents
        # will always start out at their goal, so this just contains the goal
        # position
        path(agent_goal) <- position(agent_goal) |>
            matrix(ncol = 2)

        # Determine where the agent has to stand and at what orientation. Let the
        # agent look at the goal.
        ids <- sapply(obj, id)
        goal_obj <- obj[sapply(ids, \(x) grepl(x, paste0(id(agent_goal)), fixed = TRUE))][[1]]
        coord <- center(goal_obj) - position(agent_goal)

        angle <- atan2(coord[2], coord[1])
        pos <- position(agent_goal) + (params$radius + 5e-2) * c(cos(angle + pi), sin(angle + pi))

        # Check whether there is an intersection. If so, use the more conservative
        # `perpendicular_orientation`
        if(intersects(circle(center = pos, radius = params$radius), goal_obj)) {
            angle <- perpendicular_orientation(goal_obj, position(agent_goal)) * pi / 180
            pos <- position(agent_goal) + (params$radius + 5e-2) * c(cos(angle + pi), sin(angle + pi))
        }

        # Create the agent and give them the goal of interest. This is still a 
        # dummy: Orientation and position will still need to be created.
        tmp <- agent(center = pos, 
                     orientation = angle * 180 / pi,
                     speed = 0.1,
                     radius = params$radius,
                     parameters = params,
                     current_goal = agent_goal,
                     color = color)

        agents <- append(agents, tmp)
    }

    return(agents)
}
