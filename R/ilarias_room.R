#' Define the setting of the experiment
#'
#' @export
ilarias_room <- function() {
    
    ############################################################################
    # PRELIMINARIES TABLETS
    
    dx <- 8/3
    dy <- 3

    x <- c(8, 8, 8, 8, 6, 4, 2, 0, 0, 0, 0)
    y <- c(6, 4, 2, 0, 0, 0, 0, 0, 2, 4, 6)

    centers <- cbind(x, y) + 1
    
    sofa <- function(center, size) {
        dx <- c(-1/2, -3/2, -3/2, -1/2, -1/2, 1/2, 1/2, 3/2, 3/2, 1/2, 1/2, -1/2)
        dy <- c(-1/2, -1/2, 1/2, 1/2, 3/2, 3/2, 1/2, 1/2, -1/2, -1/2, -3/2, -3/2)

        dx <- dx * size
        dy <- dy * size
    
        return(
            cbind(
                center[1] + dx,
                center[2] + dy
            )
        )
    }
    
    
    
    ############################################################################
    # PUTTING IT ALL TOGETHER
    
    # Define the room itself
    room <- predped::background(
        # Size approximately defined by anchors
        shape = predped::rectangle(
            center = c(5, 6),
            size = c(10, 12)
        ),

        objects = list(
            # Tablets: Used when walking around
            predped::rectangle(
                id = "tablet 1",
                center = centers[1,],
                size = c(0.2, 0.05),
                orientation = pi/2,
                forbidden = c(1, 3, 4)
            ),
            predped::rectangle(
                id = "tablet 2",
                center = centers[2,],
                size = c(0.2, 0.05),
                orientation = pi/2,
                forbidden = c(1, 3, 4)
            ),
            predped::rectangle(
                id = "tablet 3",
                center = centers[3,],
                size = c(0.2, 0.05),
                orientation = pi/2,
                forbidden = c(1, 3, 4)
            ),
            predped::rectangle(
                id = "tablet 4",
                center = centers[4,],
                size = c(0.2, 0.05),
                orientation = pi/4,
                forbidden = c(1, 3, 4)
            ),
            predped::rectangle(
                id = "tablet 5",
                center = centers[5,],
                size = c(0.2, 0.05),
                orientation = 0,
                forbidden = c(1, 3, 4)
            ),
            predped::rectangle(
                id = "tablet 6",
                center = centers[6,],
                size = c(0.2, 0.05),
                orientation = 0,
                forbidden = c(1, 3, 4)
            ),
            predped::rectangle(
                id = "tablet 7",
                center = centers[7,],
                size = c(0.2, 0.05),
                orientation = 0,
                forbidden = c(1, 3, 4)
            ),
            predped::rectangle(
                id = "tablet 8",
                center = centers[8,],
                size = c(0.2, 0.05),
                orientation = -pi/4,
                forbidden = c(1, 3, 4)
            ),
            predped::rectangle(
                id = "tablet 9",
                center = centers[9,],
                size = c(0.2, 0.05),
                orientation = -pi/2,
                forbidden = c(1, 3, 4)
            ),
            predped::rectangle(
                id = "tablet 10",
                center = centers[10,],
                size = c(0.2, 0.05),
                orientation = -pi/2,
                forbidden = c(1, 3, 4)
            ),
            predped::rectangle(
                id = "tablet 11",
                center = centers[11,],
                size = c(0.2, 0.05),
                orientation = -pi/2,
                forbidden = c(1, 3, 4)
            ),
            # Object in the middle of the room
            predped::circle(
                id = "table 1",
                center = c(5, centers[2, 2]),
                radius = 0.75/2,
                forbidden = matrix(
                c(0, 2 * pi),
                nrow = 1
                )
            ),
            # Sofa's used for resting
            predped::polygon(
                id = "sofa 1",
                points = sofa(
                center = c(10/3, 10),
                size = 0.5
                ),
                forbidden = c(2:8, 10:12)
            ),
            predped::polygon(
                id = "sofa 2",
                points = sofa(
                center = c(2 * 10/3, 10),
                size = 0.5
                ),
                forbidden = c(2:8, 10:12)
            )
        ),
        # Exit and entrance. Note that this won't be used during the simulation
        entrance = c(5, 12),
        # Add some boundaries participants cannot cross between the tablets.
        # This will make sure that agents stay within the experimental
        # room
        limited_access = list(
            # Between the tablets
            predped::segment(
                from = centers[2,] + c(0.025, 0),
                to = centers[1,] + c(0.025, 0)                
            ),
            predped::segment(
                from = centers[3,],
                to = centers[2,] + c(0.025, 0)                
            ),
            predped::segment(                
                from = centers[4,] - c(0, 0.025),
                to = centers[3,]
            ),
            predped::segment(
                from = centers[5,] - c(0, 0.025),
                to = centers[4,] - c(0, 0.025)                
            ),
            predped::segment(                
                from = centers[6,],
                to = centers[5,] - c(0, 0.025)
            ),
            predped::segment(
                from = centers[7,] - c(0.025, 0),
                to = centers[6,]                
            ),
            predped::segment(
                from = centers[8,] - c(0.025, 0),
                to = centers[7,] - c(0.025, 0)                
            ),
            predped::segment(
                from = centers[9,] + c(0, 0.025),
                to = centers[8,] + c(0, 0.025)                
            ),
            predped::segment(
                from = centers[10,] - c(0, 0.025),
                to = centers[9,] - c(0, 0.025)                
            ),
            predped::segment(
                from = centers[11,] - c(0, 0.025),
                to = centers[10,] - c(0, 0.025)                
            ),
            # From waiting room to tablets
            predped::segment(
                from = centers[1,],
                to = c(centers[1,1], 12)
            ),
            predped::segment(
                from = c(centers[11,1], 12),
                to = centers[11,]
            ),
            # New limited access segments
            predped::segment(
                from = centers[1,],
                to = c(5, 5)
            ),
            predped::segment(
                from = centers[2,],
                to = c(5, 5)
            ),
            predped::segment(
                from = centers[3,],
                to = c(5, 5)
            ),
            predped::segment(
                from = centers[4,],
                to = c(5, 5)
            ),
            predped::segment(
                from = centers[5,],
                to = c(5, 5)
            ),
            predped::segment(
                from = centers[6,],
                to = c(5, 5)
            ),
            predped::segment(
                from = centers[7,],
                to = c(5, 5)
            ),
            predped::segment(
                from = centers[8,],
                to = c(5, 5)
            ),
            predped::segment(
                from = centers[9,],
                to = c(5, 5)
            ),
            predped::segment(
                from = centers[10,],
                to = c(5, 5)
            ),
            predped::segment(
                from = centers[11,],
                to = c(5, 5)
            ),
            predped::segment(
                from = c(5, 10),
                to = c(5, 5)
            )
        )
        )
    
    return(room)
}
