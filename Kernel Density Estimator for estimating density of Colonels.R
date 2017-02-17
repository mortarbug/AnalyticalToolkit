###############################################################################
# To use this script, just replace the yourData with your actual data and
# run the program
###############################################################################

#Replace with your data
yourData <- c(0.94, 1.26, 1.44, 1.49, 1.63, 1.80, 2.00, 2.00, 2.56,
              2.58, 3.24, 3.39, 3.53, 3.77, 4.36, 4.41, 4.60, 4.67,
              5.39, 6.25, 7.02, 7.89, 7.97, 8.00, 8.28, 8.83, 8.91,
              8.96, 9.92, 11.36, 12.15, 14.40, 16.00, 18.61, 18.75, 19.05,
              21.00, 21.41, 23.27, 24.71, 25.00, 28.75, 30.23, 35.45, 36.35
)


#Run the function creation code
kernelDensityEstimator <- function (Data){
  #Lets you fit 3 graphs on 1 page
  par (mfrow = c(3,1))
  
  #Creates a variable with the number of plotting points to increase efficiency
  plottingPoints <- length(Data)*4
  
  #Calculates the estimated probability density function using Silverman's bandwidth
   yourDataDensity<-density(Data, bw = 'nrd', n=plottingPoints, kernel = 'g', na.rm = TRUE)
   
  #Plots the estimated probability density function
   plot(yourDataDensity, type = 'l', main = 'Estimated PDF of Your Data', xlab = 'Your Data Values',
        ylab = 'Calculated Probability Density')
   
##### Plots the cumulative distribution function of the data #####
   
   #Finds the x and y attributes of your data
   xYourData <- yourDataDensity$x
   yYourData <- yourDataDensity$y
     
   #Create vectors of 0 to allow for efficient processing
       areaUnderTheCurve <-rep (0, plottingPoints)
       cdfOfYourData <- rep(0, plottingPoints)
       
   #For-loop that does a Riemann sum of your data's PDF
       for (i in 1:plottingPoints){
           areaUnderTheCurve[i] <-abs((yYourData[i] + yYourData[i+1])/2) * (xYourData[i+1]-xYourData[i])
           cdfOfYourData[i] <- sum(areaUnderTheCurve)
       }
   
    #Plot the CDF
       plot(xYourData, cdfOfYourData, main = 'Estimated Kernel CDF of Your Data',
            xlab = 'Your Data Values', ylab = 'Estimated Cumulative Density', type = 'l')
       
    #Plot the Quantile Function
       plot (cdfOfYourData, xYourData, main = 'Estimated Quantile Function of Your Data',
             xlab = 'Calculated Percentiles', ylab = 'Your Data Values', type = 'l')
       
  
}


kernelDensityEstimator (yourData)
######## To with other data, just replace yourData and type in kernelDensityEstimator (yourData) ############
