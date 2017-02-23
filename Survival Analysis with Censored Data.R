#Kaplan Meier Survival Estimate
#Estimates the survival (1-F(t)) function for data that has censoring


library(MASS)
library(survival)

#1. Load your Data
Times<- c(180, 632, 2240, 195, 76, 70, 18, 700, 210, 1296, 23, 13, 1990)

#2. Load a vector containing 1s and 0s.
#   In this case, 0s are the censored data
Status<-c(rep(1,length(treatment1Times)-2), 0, 0)


#3. Fit the survival model 
Surv(Times, event = Status)

#4. Plot the survival model
Survival <- survfit(Surv(treatment1Times, treatment1Status)~1, 
                              conf.type='log-log')

#5. Print summary statistics. Remember that survival is 1-F(t)
#   Note: s(t)  for a quantile is the first  value for which s(t) is under
#         the desired quantile
summary(treatment1Survival)

print(treatment1Survival, print.rmean=TRUE)

plot(treatment1Survival, conf.int=FALSE, log = FALSE, main = 'Kaplan-Meier Estimate of Survival Function for Treatment1',
     xlab = 'Time to Cure', ylab = 'Survival Function')