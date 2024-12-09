devtools::load_all()

# Load the experimental room and create a model
setting <- experimental_room()
predped::plot(setting)

model <- predped::predped(
    setting = setting, 
    archetypes = c(
        "BaselineEuropean",
        "BigRushingDutch"
    ), 
    weights = c(0.5, 0.5)
)

# Run the simulation
set.seed(5) # Sword - Sufferer
trace <- simulate(
    model, 
    N = c(6, 2),
    iterations = 100,
    cycle_duration = 1800,    , 
    group_size = matrix(c(1, 1), nrow = 1)
)

# Plot and save output
plt <- predped::plot(
    trace, 
    dark_mode = TRUE
)
gifski::save_gif(
    lapply(plt, \(x) print(x)),                                     
    file.path("results", "simulation.gif"),
    delay = 1/10
)
