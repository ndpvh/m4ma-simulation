# Generate all goal_stacks that the agents will draw from. Make a distinction
# between goals that they have to perform while walking (i.e., short-lived, 
# with the tablets) and those that they have to perform while sitting (i.e., 
# longer duration).
#
# Goals are always placed at a location that can easily be reached from within 
# the walking grid.
#
# Important to note that the id's of the goals should contain the name of the 
# object to which they are assigned in their name. This will allow us to place 
# agents at the correct tablet in the initial conditions
walk <- list(goal(id = "tablet 2.",
                  position = c(14.948, 1.272)),
             goal(id = "tablet 1.",
                  position = c(14.890, 3.596)),
             goal(id = "tablet 12.",
                  position = c(14.838, 6.517)),
             goal(id = "tablet 11.",
                  position = c(14.779, 8.964)),
             
             goal(id = "tablet 5.",
                  position = c(5.170, 1.372)),
             goal(id = "tablet 6.",
                  position = c(5.194, 3.576)),
             goal(id = "tablet 7.",
                  position = c(5.076, 6.494)),
             goal(id = "tablet 8.",
                  position = c(4.943, 9.012)),
             
             goal(id = "tablet 9.",
                  position = c(8.351, 9.031)),
             goal(id = "tablet 10.",
                  position = c(11.630, 8.890)),
             
             goal(id = "tablet 4.",
                  position = c(8.231, 1.426)),
             goal(id = "tablet 3.",
                  position = c(11.716, 1.464)),
             
             goal(id = "table. 1",
                  position = c(9.173, 5.179)),
             goal(id = "table. 2",
                  position = c(10.993, 5.179)),
             goal(id = "table. 3",
                  position = c(10.083, 4.969)),
             goal(id = "table. 4",
                  position = c(10.083, 5.389)))

sit <- list(goal(id = "bench 1. 1",
                 position = c(0.61, 8.00)),
            goal(id = "bench 1. 2",
                 position = c(0.61, 7.00)),
                      
            goal(id = "bench 4. 1",
                 position = c(0.61, 5.00)),
            goal(id = "bench 4. 2",
                 position = c(0.61, 4.00)),
                      
            goal(id = "bench 2. 1",
                 position = c(1.61, 8.00)),
            goal(id = "bench 2. 2",
                 position = c(1.61, 7.00)),

            goal(id = "bench 5. 1",
                 position = c(1.61, 5.00)),
            goal(id = "bench 5. 2",
                 position = c(1.61, 4.00)),
                      
            goal(id = "bench 3. 1",
                 position = c(2.61, 8.00)),
            goal(id = "bench 3. 2",
                 position = c(2.61, 7.00)),

            goal(id = "bench 6. 1",
                 position = c(2.61, 5.00)),
            goal(id = "bench 6. 2",
                 position = c(2.61, 4.00)))
