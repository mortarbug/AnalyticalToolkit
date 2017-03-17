# Bootstrap Estimation
# Script created by: Patrick Vo
# Date: 3/17/2017
# Purpose: Define Bootstrap Estimation function that estimates mean, 
#             median, standard deviation, and median absolute deviation

#Input your data
yourData <- c(0.6, 0.7, 1.1, 1.3, 1.8, 2.0, 2.3, 2.7, 2,9, 3.1,
              3.9, 4.3, 4.4, 4.9, 5.2, 5.4, 6.1, 6.8, 7.1, 8.0,
              9.4, 10.3, 12.9, 15.9, 16.0, 22.0, 22.2, 22.5, 23.0, 23.1,
              23.9, 26.5, 26.7, 28.4, 28.5, 32.2, 40.2, 42.5, 47.2, 48.3,
              55.8, 57.0, 57.2, 64.9, 67.6, 71.3, 79.5, 114.5, 128.6, 293.5)


yourData<- log(yourData)
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
bootStrapMAD <- numeric(numberofSamples)

#For loop populates every row of your matrix with a separate sample, then calculates the values 
#   for each sample
  for(i in 1:numberofSamples) {
    d[i, ]= sample(yourData, size = n, replace=TRUE)
    bootStrapMean[i] <- mean(d[i, ])
    bootStrapMedian[i] <- median(d[i, ])
    bootStrapSD[i] <- sd(d[i, ])
    bootStrapMAD[i] <- median(abs(d[i,] - median(d[i,])))
  }

#The estimated parameters will be the mean value of the parameters of all your samples
estimatedMean <- mean(bootStrapMean)
estimatedMedian<-mean(bootStrapMedian)
estimatedSD<- mean(bootStrapSD)
estimatedMAD <- mean(bootStrapMAD)
Estimations <- cbind(estimatedMean, estimatedMedian, estimatedSD, estimatedMAD)

#The standard error will be the standard deviation of all your samples  
estMeanError <- sd(bootStrapMean)
estMedianError<-sd(bootStrapMedian)
estSDError <- sd(bootStrapSD)
estMADError <- sd(bootStrapMAD)
Errors <- cbind(estMeanError, estMedianError, estSDError, estMADError)

#Put all these things in an array and print out
bootStrapEstimators <- rbind(Estimations, Errors)
print (bootStrapEstimators)
print ('Top number is estimate. Bottom is standard error')
}


bootStrapSampler(yourData)









