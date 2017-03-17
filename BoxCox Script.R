yourData <-c(0.6, 0.7, 1.1, 1.3, 1.8, 2.0, 2.3, 2.7, 2.9, 3.1,
             3.9, 4.3, 4.4, 4.9, 5.2, 5.4, 6.1, 6.8, 7.1, 8.0,
             9.4, 10.3, 12.9, 15.9, 16.0, 22.0, 22.2, 22.5, 23.0, 23.1,
             23.9, 26.5, 26.7, 28.4, 28.5, 32.2, 40.2, 42.5, 47.2, 48.3,
             55.8, 57.0, 57.2, 64.9, 67.6, 71.3, 79.5, 114.5, 128.6, 293.5)

BoxCox <- function (y, lowerLimit, upperLimit){
n <- length(y)  #declare length of your data

yt0<- log(y)    #Solve for the case where theta = 0
s <- sum(yt0)
yt0 <- log(y)
varyt0 <- var(yt0)

Lt0 <- -1*s - .5*n*(log(6.28*varyt0)+1)

th <- 0 #initialize the list of thetas
Lt <- 0
t <- lowerLimit #Start the theta at the lower limit
i <- 0
while(t < upperLimit) #End the loop at the chosen upper limit
{t <- t+.001
i <- i+1
th[i] <- t
yt <- (y^t -1)/t
varyt <- var(yt)
Lt[i] <- (t-1)*s - .5*n*(log(6.28*varyt)+1)
if(abs(th[i])<1.0e-10)Lt[i]<-Lt0
if(abs(th[i])<1.0e-10)th[i]<-0
}
# The following outputs the values of the likelihood and theta and yields
# the value of theta where likelihood is a maximum
out <- cbind(th,Lt)
Ltmax<- max(Lt)
imax<- which(Lt==max(Lt))
thmax<- th[imax]

#postscript("boxcox,plot.ps",height=8,horizontal=F)

plot(th,Lt,lab=c(30,50,7),main="Box-Cox Transformations",
     xlab="Theta",
     ylab="Objective Function, Lt(Theta)", type = 'l')

#the following plots a 95\% c.i. for theta

cic <- Ltmax-.5*qchisq(.95,1)  

del<- .03
iLtci <- which(abs(Lt-cic)<=del)
iLtciL<- min(iLtci)
iLtciU<- max(iLtci)
thLci<- th[iLtciL] #theta lower CI
thUci<- th[iLtciU]
abline(h=cic)
abline(v=thLci)
abline(v=thUci)
abline(v = thmax)

print ('Estimated Theta:')
print (thmax)
print ('95% CI Limits')
print (thLci)
print (thUci)
}


