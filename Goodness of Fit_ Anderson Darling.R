#Anderson-Darling Goodness of Fit test
#Script Created by: Patrick Vo
#Performs the Anderson-Darling GOF Test, which is more tail-sensitive than K-S/von-Mises
#   and is generally preferred

#Caution: Is distribution-dependent. 
#Caution: P-value tables listed below are for Normal, Weibull, Exponential



#This example calculates Weibull Goodness of Fit for Weibull in the form 
#         e ^ ( - e ^ (Data - Location)/Scale)
#This is known as the extreme value distribution, and is the result of the
#   transformation Y = -log(X) where X is Weibull

#Load the MASS Library
library(MASS)

#Load data into a vector
yourData <- c(10, 12, 13, 16, 37, 42, 43, 45, 55, 63,
             66, 82, 99, 100, 100, 101, 107, 117, 122, 135,
             138, 140, 142, 149, 150, 151, 154, 165, 170, 183,
             194, 214, 218, 219, 224, 229, 232, 247, 268, 268,
             298, 299, 325, 332, 379, 400, 434, 464, 499, 537)

n<- length(yourData)




################################################################################33
#Distribution Dependent: Weibull/EVD

#   Define y as the transformed data -log(ratData)
#   Sort y 
y <- -log(yourData)
y<- sort(y)


#Find MLE parameters of the data
Estimator <- fitdistr(yourData, 'Weibull')
Estimator

#Define an index(i), and EVD parameters a = -log(WeibullScale) and b = 1/(WeibullShape)

#   Isolate the MLE scale and shape
shape <- Estimator$estimate[1]
scale <- Estimator$estimate[2]


#Define the parameters for the EVD using the estimated shape
a <- -log(scale)
b <- 1/shape


#   Define z as the Weibull CDF for each value y
z <- exp(-exp(-(y-a)/b))
#################################################################################



#Define an index for summation and the two terms in the Anderson-Darling 
i <- seq(1,n, 1)
andersonDarlingTerm1 <- (2*i-1) * log(z)
andersonDarlingTerm2 <- (2*n+1 - 2*i)*log(1-z)

#Sum the terms
sumAD1 <- sum(andersonDarlingTerm1)
sumAD2 <- sum(andersonDarlingTerm2)

andersonDarling <- -n -(1/n) * (sumAD1 + sumAD2)

#Modify the andersonDarling for the Weibull
andersonDarlingModified <- andersonDarling *(1+0.2/sqrt(n))

andersonDarling
andersonDarlingModified


