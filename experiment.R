# Loads utility functions to be used for the simulation.
source(file.path("utility", "simulate.R"))

# Visualize the setting and where the goals are to get a grasp of where and how
# the agents will walk around
coords <- do.call("rbind",
                  lapply(append(sit, walk),
                         \(x) position(x)))
plot(setting, 
     fill = "gray",
     color = "black") +
    ggplot2::geom_point(ggplot2::aes(x = coords[,1], 
                                     y = coords[,2]),
                        color = "cornflowerblue",
                        size = 2)

# Define the predped model that you want to simulate. Consists of the setting 
# and agent specifications
model <- predped(id = "experiment",
                 setting = setting, 
                 archetypes = c("BaselineEuropean", 
                                "BigRushingDutch"),
                 weights = c(0.75, 0.25))

# Do the simulation: 1000 iterations, 500 sec / 8.33 min, takes 4.65 min
set.seed(1)
trace <- simulate_experiment(21,                  # Number of agents
                             6,                   # Number of agents seated
                             1000,                # Number of iterations
                             model,               # Model
                             plot_live = FALSE)    # Whether to plot results immediately

# Create a gif
points <- shape(setting)@points
poly_size <- c(max(points[,1] - min(points[,1])),
               max(points[,2] - min(points[,2])))

plt <- plot(trace, trace = TRUE, linewidth = 2, size = 4)

# Change sizes of the labels on the plot
plt <- lapply(plt, 
              \(x) x + 
                   ggplot2::theme(legend.position = "none",
                                  plot.title = ggplot2::element_text(size = 5 * max(poly_size),
                                                                     hjust = 0.5),
                                  axis.text = ggplot2::element_text(size = 2.5 * max(poly_size))))
gifski::save_gif(lapply(plt, \(x) print(x)),                                     
                 file.path("results", "simulation.gif"),
                 delay = 1/10, 
                 width = poly_size[1] * 200, 
                 height = poly_size[2] * 200)
