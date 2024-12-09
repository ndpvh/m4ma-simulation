################################################################################
# parameters.R                                                                 #
#                                                                              #
# Script that you can use to play around with some parameters and their        #
# distributions.                                                               #
################################################################################

devtools::load_all()

#-------------------------------------------------------------------------------
# Reading and writing

# Get the parameters from the defaults for predped.
params <- predped::params_from_csv

# Predped expects you to provide a named list containing "params_archetypes"
# (dataframe of mean values for the parameters), "params_sigma" (a matrix 
# containing standard deviations on the diagonal and correlations on the 
# off-diagonal for each of the parameters), and "params_bounds" (a matrix 
# containing the lower and upper bounds for each parameter). 
#
# How to play around with the distributions is for later. For now, let's focus 
# on setting their values and saving them for later use. Let's start with the 
# "params_archetypes" and create a new archetype called the FastWalker with a 
# mean preferred_speed of 1.8 m/sec. We do this by:
#   - Selecting a baseline parameter set to use
#   - Changing its name to FastWalker
#   - Changing its preferred speed to 1.8
my_archetype <- params[["params_archetypes"]]
my_archetype <- my_archetype[my_archetype$name == "BaselineEuropean", ]

my_archetype$name <- "FastWalker"
my_archetype$preferred_speed <- 1.8

# We will now add this archetype to the original dataframe and see whether 
# we did everything right.
params[["params_archetypes"]] <- rbind(
    params[["params_archetypes"]], 
    my_archetype
)
View(params[["params_archetypes"]])

# We changed the mean parameters of this archetype, but did not consider creating
# its standard deviation-correlation matrix yet. For this, we can also use a 
# baseline matrix from the other archetypes already. 
#
# Note that the row- and colunn names denote the utility parameters.
sigma <- params[["params_sigma"]][["BaselineEuropean"]]
print(rownames(sigma))
print(colnames(sigma))
View(sigma)

# Let's change the standard deviation for the preferred speed to a smaller value
# and save it in the parameters list
sigma["preferred_speed", "preferred_speed"] <- 0.05
View(sigma)

params[["params_sigma"]][["FastWalker"]] <- sigma

# Now to save this parameter-list, we can just use the saveRDS function. To read
# it in and use it again, you can load it in the simulation through defining 
# the file.path to the file.
saveRDS(
    params, 
    file.path("results", "parameters.Rds")
)



#-------------------------------------------------------------------------------
# Parameter distributions

# Extract the mean parameters under the code "params_archetypes". Let us 
# furthermore select a single archetypes from this dataframe for future use.
mu <- params[["params_archetypes"]]
View(mu)

baseline <- mu[mu$name == "BaselineEuropean",]

# Define the covariance matrix you want to use. Here, I just use the default 
# diagonal matrix I created for predped, but can be more complex. 
#
# If you wish to make it more complex, then it is important to consider structure.
# On the diagonal, standard deviations for the parameters are contained, while 
# correlations between the parameters are contained in the off-diagonal elements.
# By default, the code then transforms this matrix into a proper covariance 
# matrix.
#
# It is important to note that the row- and column-names of this matrix should 
# both contain reference to the parameter of choice. I already provided a line 
# of code for this.
#
# If, for some reason, you wish to provide an actual covariance matrix instead, 
# you can set `transform_covariance` in `plot_distribution` to `FALSE` (instead
# of the default `TRUE`).
sigma <- diag(c(0.15, 0.1, 0.05, 0.1, 0.01, 0.1, 0.1, 0, 0, 
                0.15, 0, 0.15, 0, 0.15, 0, 0, 0.15, 0, 0.15, 0, 
                0, 0.15, 0, 0, 0.15, 0.15, 0, 0, 0, 0, 0))
rownames(sigma) <- colnames(sigma) <- predped:::utility_parameters(baseline)
View(sigma)

# Plot the distribution of 1000 draws of the parameter from the distribution 
# defined by mu and sigma. The transformation of the parameters happens in the 
# way you asked. Specifically:
#   - The means get transformed to a probit 0 - 1 scale through their bounds
#   - We transform the means to the normal distribution with `qnorm`
#   - We draw several new parameters from the normal with the transformed means
#     and the covariance matrix defined by transforming sigma
#   - Once parameters are drawn, we transform back to probit with `pnorm` and 
#     then to the original range of the parameters
#
# For more information on this function, consult the documentation of predped.
predped::plot_distribution(
    1000, 
    mean = baseline,
    Sigma = sigma,
    individual_differences = TRUE
)

# One downside of `plot_distribution` is that it only creates plots, but does 
# not actually output the parameters themselves. If you would wish to get the 
# parameters themselves, you can use `draw_parameters` instead. This has the 
# same arguments as `plot_distribution`.
predped::draw_parameters(
    1000, 
    mean = baseline, 
    Sigma = sigma, 
    individual_differences = TRUE
)
