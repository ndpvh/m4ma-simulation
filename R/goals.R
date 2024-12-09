#' Create the goal stack
#' 
#' @param room Object of the \code{\link[predped]{background-class}} containing 
#' the experimental room. This function assumes that all objects with "tablet" in
#' the name are to be walking goals, while those with "sofa" in the name are 
#' to be the sitting goals.
#' 
#' @export 
goal_stack <- function(room) {
    # Extract the objects within this room and their respective id's
    tablets <- predped::objects(room)
    ids <- sapply(tablets, predped::id)

    # First focus on the walking tablets: Loop over the tablets, create a goal
    # for each
    idx <- sapply(
        ids, 
        \(x) grepl("tablet", x, fixed = TRUE)
    )
    walk <- tablets[idx]

    walk <- lapply(
        walk, 
        \(x) predped::add_goal(
            x,
            room,
            middle_edge = TRUE,
            id = paste0("walk: ", predped::id(x)),
            counter = 5
        )
    )

    # Now do the same for the sitting goals
    idx <- sapply(
        ids, 
        \(x) grepl("sofa", x, fixed = TRUE)
    )
    sit <- tablets[idx]

    sit <- lapply(
        sit, 
        \(x) predped::add_goal(
            x,
            room,
            middle_edge = TRUE,
            id = paste0("sit: ", predped::id(x)),
            counter = 5
        )
    )
    
    # Put them together in the list and return
    return(append(walk, sit))
}
