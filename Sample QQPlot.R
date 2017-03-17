# Quantile-quantile Plotting for Distributions and Samples
# Created by Patrick Vo
# Date: 3/17/2017
# Purpose: To demonstrate a sample quantile-quantile plot 
#     instead of using the qqplot function

x  =  seq(0.0001,.9999,length=5000) # a vector of values to be plugged into the quantile function
y1  =  qt(x,2)      #The quantiles that you're comparing
y2  =  qnorm(x,0,1) #The quantiles of the reference distribution


par(mfrow=c(1,1))
matplot(y2,y1,
        main="Quantile-quantile against Reference Distribution",
        ylab="Q(u) ",
        xlab="Q(u) for Reference Distribution",
        type="l",ylim=c(-10,10),
        xlim=c(-3.5,3.5),lab=c(9,8,7),cex=.5)
lines(y2,y2) #Plots the quantile reference against itself, returning a straight line


###########################################################################
#Sample normal reference distribution plot

yourData  <- c(0.6, 0.7, 1.1, 1.3, 1.8, 2.0, 2.3, 2.7, 2,9, 3.1,
               3.9, 4.3, 4.4, 4.9, 5.2, 5.4, 6.1, 6.8, 7.1, 8.0,
               9.4, 10.3, 12.9, 15.9, 16.0, 22.0, 22.2, 22.5, 23.0, 23.1,
               23.9, 26.5, 26.7, 28.4, 28.5, 32.2, 40.2, 42.5, 47.2, 48.3,
               55.8, 57.0, 57.2, 64.9, 67.6, 71.3, 79.5, 114.5, 128.6, 293.5)
yourData <- log(yourData) #yourData (Log was necessary, as this problem dealt with lognormality)

yourData <- sort(yourData)     #Sort the data

n = length(yourData)

#Find the quantiles of the sample. Notice we use Yi = Quantile( (i-0.5) / n )
#   for sorted data of length n
i = 1:n         
ui = (i-.5)/n

#Find the quantiles of the reference distribution
Qreference <- qnorm(ui, mean = 3, sd = 1.5)

plot(Qreference, yourData,
     main="Sample vs Reference plot",
     xlab="Reference Quantile",
     ylab="Sample Quantile  Q(u)")
#Draw a reference line by plotting the Q reference against itself.
#In a regular q-q plot, you'd want to do a linear model instead
lines (Qreference, Qreference)
