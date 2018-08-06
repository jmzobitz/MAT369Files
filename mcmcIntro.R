#### Function mcmcIntro.R
### Modified: 10/6/15
### Purpose: Run a simple MCMC estimator to determine how you can estimate the carrying capacity for a logistic model equation.

##  The logistic model is regressionFormula = y = p1/(1+exp(p2+p3*x))
## p1 = carrying capacity
## p2 = pre determined from the data
### p3 = pre determined from the data


### 1) Define the data that we will be using:
### Here we will estimate the values of the logistic model for the following dataset:
# http://bscheng.com/2014/05/07/modeling-logistic-growth-data-in-r/
library('ggplot2')
#Data
mass<-c(6.25,10,20,23,26,27.6,29.8,31.6,37.2,41.2,48.7,54,54,63,66,72,72.2,
        76,75) #Wilson’s mass in pounds
days<-c(31,62,93,99,107,113,121,127,148,161,180,214,221,307,
                    452,482,923, 955,1308) #days since Wilson’s birth
data<-data.frame(mass,days) #create the data frame


### 2) Define a likelihood function.  
likelihood <- function(p1){
  time = data$days
  yMeasured = data$mass
  
  p2 = -2.461935;
  p3 = 0.017032;
  
  predictedMass = p1/(1+exp(-(p2+p3*time))) 
  
  # If you don't know the uncertiainty on your measurements, here is a quick way to determine the value:
  error=sd(predictedMass-yMeasured)
  ### Determine the likelihood of the values, and then sum them up.
  
  singlelikelihoods = dnorm(predictedMass, mean = yMeasured, sd = error)
  sumll = prod(singlelikelihoods)
  
  return(sumll)
}

# 3) Define a function that is the ratio of the likelihood we wish to estimate.
# Our decision will be based on the following: accept if w
decisionValue <- function(oldParam,newParam){
  
  oldLikelihood = likelihood(oldParam)
  newLikelihood = likelihood(newParam)
  
  return(newLikelihood/oldLikelihood)
}

  

