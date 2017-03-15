# In this example, calculates the fit of a poisson distribution

# Caution: Script is distribution-dependent

#Load the MASS Library
library(MASS)

#Unpack Data to make your sample data vector
FreqData <- c(121, 110, 38, 7, 3, 1)
yourData <-0
for (index in 1:length(FreqData)){
  yourData <- c(yourData, rep(index-1, FreqData[index]))
}


#Omit the first value, which was used to initialize the data
yourData <- yourData[2:length(yourData)]



#Use the MASS library to fit a poisson model and return MLE Estimator
Estimator <- fitdistr(yourData, 'Poisson')
Estimator
#Note that the MLE should equal mean of the sample for a poisson
mean(yourData)



#Find an expected probability vector for the poisson model
expectedProportion <- dpois (0:5, lambda = Estimator$estimate)

#Verify that 20% of the expected are >5, none are <1
expectedProportion * sum(FreqData)

# The 4 and 5 are small--too small. Use the poisson CDF to 
#        find the probability of 3 or higher
expectedProportion[4] <- 1-ppois(2, lambda = Estimator$estimate)
expectedProportion <- expectedProportion[1:4]

#Check to make sure answers are good
expectedProportion * sum(FreqData)

#Also adjust the vector of data
FreqData [4] <- sum(FreqData[4:6])
FreqData <- FreqData[1:4]



#Call the chi-squared test
GOF <- chisq.test(FreqData, p=expectedProportion)

#Note that we're not done yet. The chisq.test() function automatically uses
#   df = length(freqData) - 1 to calculate p-value of the Chi-Square.
#   However, if you estimate parameters, you have to use 
#     df = length(yourData) - 1 - (# of estimated parameters)
1 - pchisq(GOF$statistic, df = (length(FreqData) - 1 - 1))

