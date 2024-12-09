#' Create a function that will simulate a given experimental procedure with 
#' the following parameters.
#'
#' @param model Object of the \code{\link[predped]{predped-class}} specifying 
#' the details of the simulation.
#' @param N Vector of integers denoting the number of participants that should 
#' walk around (first number) and the number of participants that should sit 
#' down (second number). The sample size is limited by the number of tablets in 
#' the environment (by default 8 "walk" and 2 "sit" tablets). Defaults to 6 
#' walking agents and 2 sitting ones.
#' @param iterations Integer denoting the number of iterations to run. Defaults 
#' to \code{1800}.
#' @param cycle_duration Integer denoting how long a single cycle of the 
#' experiment takes. During each cycle, every participant should sit down a 
#' specified amount of time and walk for the rest of the cycle. Defaults to 
#' \code{1800}, or 900sec if \code{time_step = 0.5}.
#' @param walk_duration Integer or function that determines the duration 
#' of each goal assigned to a "walk" tablet in the square. Defaults to a normal
#' distribution with mean 10 and standard deviation 2.
#' @param fx Function that takes in and returns an object of the
#' \code{\link[predped]{state-class}}. This will be executed at the beginning of 
#' each iteration and allows users some flexibility in their simulations. For 
#' example useful when simulating evacuations (giving everyone "goal exit") or 
#' trying to guide behavior in any other way. Defaults to "\(x) x", meaning the 
#' state remains unaltered.
#' @param time_step Numeric denoting the number of seconds each discrete step in
#' time should mimic. Defaults to \code{0.5}, or half a second.
#' @param ... Additional arguments that control the simulation (see 
#' \code{\link[predped]{simulate,predped-method}})
#' 
#' @return Trace, or list of different states at the different iterations.
simulate <- function(model,
                     iterations = 1800, 
                     N = c(6, 2),
                     cycle_duration = 1800,
                     walk_duration = \(x) abs(rnorm(1, 10, 2)),
                     fx = \(x) x,
                     time_step = 0.5,
                     max_agents = NULL,
                     group_size = matrix(c(1, 1), nrow = 1),
                     individual_differences = FALSE,
                     standing_start = 0.1,
                     ...) {

    # Create the goal stack to be used in the experiment. Also do a check of 
    # whether there are enough goals for all the participants that they want
    goals <- goal_stack(model@setting)

    if(sum(N) > length(goals)) {
        stop('Not enough goals defined. Please add more "tablet" or "sofa" objects, or reduce the sample size.')
    }

    if(N[2] > sum(grepl("sofa", sapply(goals, predped::id), fixed = TRUE))) {
        stop('Not enough "sit" goals defined. Please add more "sofa" objects, or reduce the sample size.')
    }

    # Add the user-defined `fx` function to the `assign_tablet` function. This 
    # allows users to still change the simulation in the way they want
    gx <- function(state) {
        state <- assign_tablet(state)
        return(fx(state))
    }

    # Define the sit_duration function. Is dependent on the number of people in 
    # the room and the time one cycle takes.
    sit_duration <- \(x) x * cycle_duration / sum(N)

    # Create an initial condition to be used as a starting position in the 
    # experiment.
    inx <- initial_condition(
        model,
        N,
        goals, 
        walk_duration, 
        sit_duration,
        group_size = group_size,
        individual_differences = individual_differences,
        standing_start = standing_start
    )

    # Create an initial state from which to start. This state should include the
    # bookkeeping variables that I use and adjust in the `assign_tablet` function
    history <- rep(sit_duration(1), sum(N))
    names(history) <- sapply(inx, predped::id)

    previous_goal <- sapply(inx, \(x) predped::id(predped::current_goal(x)))
    names(previous_goal) <- sapply(inx, predped::id)

    inx <- predped::state(
        iteration = 0,
        setting = model@setting, 
        agents = inx, 
        potential_agents = list(),
        variables = list(
            "history" = history,
            "goal_stack" = goals, 
            "assign_tablet" = TRUE,
            "sit_counter" = sit_duration, 
            "walk_counter" = walk_duration,
            "cycle_duration" = cycle_duration,
            "N" = N,
            "previous_goal" = previous_goal
        )
    )

    # Now, we can run the simulation and return the result
    return(
        predped::simulate(
            model,
            iterations = iterations,
            max_agents = 0,             # Enforced to make sure noone enters the room
            fx = gx,
            standing_start = standing_start,
            time_step = time_step,
            initial_condition = inx,
            ...
        )
    )
}