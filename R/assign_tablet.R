#' Assign a tablet to a participant
#' 
#' Draws a quasi-random tablet from the list of free tablets for each agent that
#' currently does not have a goal. When a "sit" tablet is free, the function 
#' will check whether its the agents turn to sit down. If not, then we assign a 
#' "walk" tablet. If it is the agents' turn, then we assign the agent a "sit" 
#' tablet instead. If no "sit" tablets are free, then the agent is automatically 
#' assigned a "walk" tablet.
#' 
#' Basically a function that does the bookkeeping of the experiment.
#' 
#' @param state Object of the \code{\link[predped]{state-class}} containing the 
#' current state.
#' 
#' @export 
assign_tablet <- function(state) {
    # Extract the needed variables:
    #   - Agents in the simulation
    #   - Goal stack for the experiment
    #   - Whether we should assign a new goal in the first place
    #   - How long each agent has sitten down
    #   - A function that defines the counter for sitting down
    #   - A function that defines the counter for walking around
    agents <- predped::agents(state)

    goal_stack <- predped::variables(state)$goal_stack
    assign_tablet <- predped::variables(state)$assign_tablet

    history <- predped::variables(state)$history
    cycle_duration <- predped::variables(state)$cycle_duration
    N <- predped::variables(state)$N

    sit_counter <- predped::variables(state)$sit_counter
    walk_counter <- predped::variables(state)$walk_counter

    # If no tablets should be assigned, skip this function and return the 
    # original state
    if(!assign_tablet) {
        return(state)
    }

    # Check whether the cycle has been completed. If so, reset the history of 
    # the participants and start over again.
    if(state@iteration %% cycle_duration == 0) {
        history <- rep(sit_counter(N[2]), each = length(agents))
        names(history) <- sapply(agents, predped::id)
    }

    # Get the id's of the current goals of the agents and of the goal stack. The 
    # mapping between these two will tell us which goals are free.
    id_goals <- sapply(
        agents, 
        \(x) predped::current_goal(x)@id
    )
    id_stack <- sapply(
        goal_stack, 
        predped::id
    )

    # Based on these id's, find out which goals are currently free and which ones
    # aren't
    idx <- !(id_stack %in% id_goals)
    free_tablets <- id_stack[idx]

    # Find out whether one of the free tablets is a sitting tablet
    sit <- any(grepl("sit", free_tablets, fixed = TRUE))

    # Loop over the agents and do the bookkeeping
    for(i in seq_along(agents)) {
        # Check whether the goal of this agent is "goal exit". If not, then 
        # this agent can continue doing what they do.
        # 
        # Importantly, some bookkeeping should be done here for the agents with 
        # "sit" goals. We need to update the history of these agents to show 
        # that they are currently sitting down. This will allow us to define 
        # which agents already sat down and which ones didn't.
        #
        # This might deviate somewhat from the experiment, where a fixed order 
        # of walking and sitting can be used. Given that the fixed order is not 
        # always consistent, and given that it doesn't really matter all that 
        # much, I keep the implementation as is.
        if(id_goals[i] != "goal exit") {
            # Adjust counter on the agent's history if they are currently sitting 
            # down
            if(grepl("sit", current_goal(agents[[i]])@id, fixed = TRUE)) {
                ids <- predped::id(agents[[i]])
                history[ida] <- history[ida] - 1
            }

            next
        }

        # First check, is a "sit" tablet available?
        if(sit) {
            # Now check whether the person sat down for the required time. If not
            # we will assign them a "sit" tablet
            if(history[predped::id(agents[[i]])] == max(history)) {
                # Assign the "sit" tablet
                idx <- which(grepl("sit", free_tablets, fixed = TRUE))[1]
                idy <- free_tablets[idx] == id_stack

                current_goal(agents[[i]]) <- goal_stack[idy]
                current_goal(agents[[i]])@counter <- sit_counter(1)

                # Delete this tablet from the free tablets
                free_tablets <- free_tablets[-idx]

                # Continue to the next participant
                next
            }
        }

        # If no "sit" tablet is assigned, a "walk" tablet will be assigned to 
        # the agent
        idx <- which(grepl("walk", free_tablets, fixed = TRUE))
        idx <- ifelse(length(idx) > 1, sample(idx, 1), idx)
        idy <- free_tablets[idx] == id_stack

        current_goal(agents[[i]]) <- goal_stack[idy]
        current_goal(agents[[i]])@counter <- walk_counter(1)

        # Delete this tablet from the free tablets
        free_tablets <- free_tablets[-idx]        
    }

    # Update the state
    state$agents <- agents
    variables(state)$history <- history

    return(state)
}