###############################################################################
# This script finds the Maximum Likelihood Estimators for distribution given  #
#    your data. Although it's set to the Weibull (which is what I'm most      #
#    often asked about), a variety of distributions are possible              #
###############################################################################

# Call the MASS library
library(MASS)

# Run this code to load the estimator function
mleEstimator <- function (Data, Distribution){
  Estimator <- fitdistr(Data, Distribution)
  return(Estimator)
}

###############################################################################

# Plug in your data
yourData <- c( 0.94, 1.26, 1.44, 1.49, 1.63, 1.80, 2.00, 2.00, 2.56,
               2.58, 3.24, 3.39, 3.53, 3.77, 4.36, 4.41, 4.60, 4.67,
               5.39, 6.25, 7.02, 7.89, 7.97, 8.00, 8.28, 8.83, 8.91,
               8.96, 9.92, 11.36, 12.15, 14.40, 16.00, 18.61, 18.75, 19.05,
               21.00, 21.41, 23.27, 24.71, 25.00, 28.75, 30.23, 35.45, 36.35
)

# Choose the distribution you want: Weibull, Normal, Poisson, etc....
desiredDistribution <- 'Weibull'

# To use, just run the code below:
mleEstimator(yourData, desiredDistribution)



