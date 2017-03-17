# Bootstrap Estimation
# Script created by: Patrick Vo
# Date: 3/17/2017
# Purpose: Define Bootstrap Estimation function that estimates mean, median, standard deviation

#Input your data
yourData <- rnorm (25, 0, 1)


#Define the bootstrap sample function. Draw 10000 samples by default
bootStrapSampler<- function(yourData,numberofSamples = 10000){
numberofSamples = 10000

#Store the length of your data, initialize a matrix
n = length(yourData)
d= matrix(data = 0, nrow =numberofSamples, ncol = n) #An array that will hold all the samples

#For every row in d, find mean, median, sd
bootStrapMean <- numeric(numberofSamples)
bootStrapMedian <- numeric(numberofSamples)
bootStrapSD <- numeric(numberofSamples)

#For loop populates every row of your matrix with a separate sample, then calculates the values 
#   for each sample
  for(i in 1:numberofSamples) {
    d[i, ]= sample(yourData, size = n, replace=TRUE)
    bootStrapMean[i] <- mean(d[i, ])
    bootStrapMedian[i] <- median(d[i, ])
    bootStrapSD[i] <- sd(d[i, ])
  }

#The estimated parameters will be the mean value of the parameters of all your samples
estimatedMean <- mean(bootStrapMean)
estimatedMedian<-mean(bootStrapMedian)
estimatedSD<- mean(bootStrapSD)
Estimations <- cbind(estimatedMean, estimatedMedian, estimatedSD)

#The standard error will be the standard deviation of all your samples  
estMeanError <- sd(bootStrapMean)
estMedianError<-sd(bootStrapMedian)
estSDError <- sd(bootStrapSD)
Errors <- cbind(estMeanError, estMedianError, estSDError)

#Put all these things in an array and print out
bootStrapEstimators <- rbind(Estimations, Errors)
print (bootStrapEstimators)
print ('Top number is estimate. Bottom is standard error')
}


bootStrapSampler(yourData)









