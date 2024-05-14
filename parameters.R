# Loads utility functions to be used for the simulation.
source(file.path("utility", "simulate.R"))

# Vectors that define the parameters by name. Used in the functions defined here.
# Not exported because not needed anywhere else.
nest_parameters <- c("central", 
                     "non_central", 
                     "acceleration", 
                     "constant_speed", 
                     "deceleration")

utility_parameters <- colnames(params_archetypes)
utility_parameters <- utility_parameters[!(utility_parameters %in% c("name", "color"))]

# Define the means of the parameters. In this example, I just make use of a 
# specific archetype, but can be anything you like
mu <- params_archetypes[params_archetypes$name == "BaselineEuropean", ]
View(mu)

# Define the covariance matrix you want to use. Here, I just use the default 
# diagonal matrix I created for predped, but again can be more complex. 
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
                0, 0.15, 0, 0, 0, 0, 0, 0))
rownames(sigma) <- colnames(sigma) <- utility_parameters
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
# As a small introduction to the parameters that are at play here, a quick copy-
# paste from the documentation
# 
# @param n Integer denoting the number of parameters to draw. Defaults to `1`.
# @param mean A named list containing the mean for each of the parameters for 
# a given agent.
# @param Sigma Either a covariance matrix that defines the individual differences
# on each of the parameters (when `transform_covariance == FALSE`), or a matrix
# containing standard deviations for each of the parameters on its diagonal and
# correlations between the parameters on its off-diagonal (when 
# `transform_covariance == TRUE`). Default covariance matrices exist for each
# of the archetypes in `params_archetypes` and thus changes with the value of 
# `archetype`.
# @param archetype String denoting the archetype to be used for the covariance
# matrix. Ignored if `Sigma` is provided. Defaults to `BaselineEuropean`.
# @param individual_differences Logical denoting whether to use the standard 
# deviations in the parameter list to create some variation in the parameters.
# Defaults to `TRUE`.
# @param transform_covariance Logical denoting whether to transform `Sigma` to 
# a proper covariance matrix or not. Defaults to `TRUE`. 
#
# I think that for you, the arguments `n`, `mean`, and `Sigma` are most interesting.
plot_distribution(1000, 
                  mean = mu,
                  Sigma = sigma,
                  individual_differences = TRUE)

# One downside of `plot_distribution` is that it only creates plots, but does 
# not actually output the parameters themselves. If you would wish to get the 
# parameters themselves, you can use `draw_parameters` instead. This has the 
# same arguments as `plot_distribution`.
draw_parameters(1000, 
                mean = mu, 
                Sigma = sigma, 
                individual_differences = TRUE)
