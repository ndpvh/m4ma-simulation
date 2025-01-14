#' Define the setting of the experiment
#' 
#' @export 
dans_room <- function() {
  
    ############################################################################
    # PRELIMINARIES TABLETS
    
    # Distance between achors in x and y direction
    dx <- 8/3
    dy <- 3
    
    # Create all locations for the tablets through a logical function, 
    # connecting the distances in the x- and y-direction with each other.
    # Importantly, this is relative to the tablet standing in the left-bottom 
    # corner (tablet 6).
    x <- c(3, 3, 3, 3, 3, 2.5, 2, 1.5, 1, 0.5, 0, 0, 0, 0, 0) * dx
    y <- c(2, 1.5, 1, 0.5, 0, 0, 0, 0, 0, 0, 0, 0.5, 1, 1.5, 2) * dy
    
    # Add a starting position to each of these deviations. Here, I'll assume 
    # that tablet 6 (to which the deviations are relative) is standing at 
    # coordinate c(1, 1)
    centers <- cbind(x, y) + 1




    ############################################################################
    # PRELIMINARIES SOFAS

    # Create a function that will help us create the coordinates of the 
    # sofa based on its location (center) and the size of the rectangles 
    # which it is made of.
    #
    # A similar reasoning with dx and dy is used here
    sofa <- function(center, size) {
        # Define the multiplicative deviation of a corner to the center in the 
        # x- and y-directions. I start at the bottom-left corner of the inner 
        # square and then move clockwise. 
        dx <- c(-1/2, -3/2, -3/2, -1/2, -1/2, 1/2, 1/2, 3/2, 3/2, 1/2, 1/2, -1/2)
        dy <- c(-1/2, -1/2, 1/2, 1/2, 3/2, 3/2, 1/2, 1/2, -1/2, -1/2, -3/2, -3/2)

        # These are relative distances compared to the size of the square, so 
        # you have to multply them still
        dx <- dx * size 
        dy <- dy * size

        # Add the center coordinate to it and bind them together
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
            center = c(5, 7), 
            size = c(10, 14)
        ),
        # Objects in the room, both those that are used while walking around, 
        # the object in the middle of the room, and the sofa's on which 
        # participants who aren't doing anything are resting.
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
                orientation = pi/2,
                forbidden = c(1, 3, 4)
            ),
            predped::rectangle(
                id = "tablet 5",
                center = centers[5,],
                size = c(0.2, 0.05),
                orientation = pi/4,
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
                orientation = 0,
                forbidden = c(1, 3, 4)
            ),
            predped::rectangle(
                id = "tablet 9",
                center = centers[9,],
                size = c(0.2, 0.05),
                orientation = 0,
                forbidden = c(1, 3, 4)
            ),
            predped::rectangle(
                id = "tablet 10",
                center = centers[10,],
                size = c(0.2, 0.05),
                orientation = 0,
                forbidden = c(1, 3, 4)
            ),
            predped::rectangle(
                id = "tablet 11",
                center = centers[11,],
                size = c(0.2, 0.05),
                orientation = -pi/4,
                forbidden = c(1, 3, 4)
            ),
            predped::rectangle(
                id = "tablet 12",
                center = centers[12,],
                size = c(0.2, 0.05),
                orientation = -pi/2,
                forbidden = c(1, 3, 4)
            ),
            predped::rectangle(
                id = "tablet 13",
                center = centers[13,],
                size = c(0.2, 0.05),
                orientation = -pi/2,
                forbidden = c(1, 3, 4)
            ),
            predped::rectangle(
                id = "tablet 14",
                center = centers[14,],
                size = c(0.2, 0.05),
                orientation = -pi/2,
                forbidden = c(1, 3, 4)
            ),
            predped::rectangle(
                id = "tablet 15",
                center = centers[15,],
                size = c(0.2, 0.05),
                orientation = -pi/2,
                forbidden = c(1, 3, 4)
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
        entrance = c(5, 14),
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
                from = centers[3,] + c(0.025, 0),
                to = centers[2,] + c(0.025, 0)                
            ),
            predped::segment(                
                from = centers[4,] + c(0.025, 0),
                to = centers[3,] + c(0.025, 0)
            ),
            predped::segment(
                from = centers[5,],
                to = centers[4,] + c(0.025, 0)                
            ),
            predped::segment(                
                from = centers[6,] - c(0, 0.025),
                to = centers[5,]
            ),
            predped::segment(
                from = centers[7,] - c(0, 0.025),
                to = centers[6,] - c(0, 0.025)                
            ),
            predped::segment(
                from = centers[8,] - c(0, 0.025),
                to = centers[7,] - c(0, 0.025)               
            ),
            predped::segment(
                from = centers[9,] - c(0, 0.025),
                to = centers[8,] - c(0, 0.025)                
            ),
            predped::segment(
                from = centers[10,] - c(0, 0.025),
                to = centers[9,] - c(0, 0.025)                
            ),
            predped::segment(
                from = centers[11,],
                to = centers[10,] - c(0, 0.025)                
            ),
            predped::segment(
                from = centers[12,] - c(0.025, 0),
                to = centers[11,]                
            ),
            predped::segment(
                from = centers[13,] - c(0.025, 0),
                to = centers[12,] - c(0.025, 0)                
            ),
            predped::segment(
                from = centers[14,] - c(0.025, 0),
                to = centers[13,] - c(0.025, 0)                
            ),
            predped::segment(
                from = centers[15,] - c(0.025, 0),
                to = centers[14,] - c(0.025, 0)                
            ),
            # From waiting room to tablets
            predped::segment(
                from = centers[1,],
                to = c(centers[1,1], 12)
            ),
            predped::segment(
                from = c(centers[15,1], 12),
                to = centers[15,]
            ),
            
            ### [Dan] Exit segments 
            predped::segment(
                from = c(4.65, 10),
                to = c(centers[15, 1], 10)
            ),
            predped::segment(
                from = c(centers[1, 1], 10),
                to = c(5.35, 10)
            )
        )
    )
        
    return(room)
}


####################################3

