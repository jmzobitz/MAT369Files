# Each function needs the following:

# 1) equation that describes your dynamical system
# 2) model to pair with data (call this function model)
# 3) likelihood function to describe model-data mismatch (call this function likelihood)

library(deSolve)  ### needed because we are solving des

dynamics <- function (t, data, parameter) {
  
  ### List all your parameters here in the same order listed in "defaults.param"
  Ksa=parameter[1]
  bsa=parameter[2]
 
  Kschi = parameter[3] 
  bschi = parameter[4]
  
  # Define the interaction terms
  # alpha is the influence of Schizosaccharomyces on Saccharomyces
  # beta is the influence of Saccharomyces on Schizosaccharomyces
  alpha = parameter[5] 
  beta = parameter[6] 
  
  saccharomyces = data[1]
  schizosaccharomyces = data[2]
  ### This is your rate equation
  DsaDt = bsa * saccharomyces * (Ksa -(saccharomyces + alpha *schizosaccharomyces) )/ Ksa 
  DschiDt = bschi * schizosaccharomyces * (Kschi -(schizosaccharomyces + beta *schizosaccharomyces) )/ Kschi  
  
  ### You don't need to bother with the remaining items
  outVal = list(c(DsaDt,DschiDt))
  
  return(outVal)
}




model <- function(parameter,data) {
  
  currParam = setNames(parameter$value, parameter$name)
  xInit = c(sa=data[1,2], schi=data[1,3]);  # The first value will always be the initial condition
  timeVec=data$time;
  
  soln = ode(func=dynamics,
             times=timeVec, ## times
             y=xInit, ## init. conditions (named)
             parms=currParam) ## parameter values (named)
  
  soln=setNames(soln, colnames(data))  # Set the column names of the solution
  
  return(soln)  # return the solution as a vector to make things easy
}

#### Stuff specific to the particular likelihood function
# Likelihood function for zombie parameters
likelihood <- function(parameter,data){

  pred = model(parameter,data)
  # Record the measured and predicted values as a list
  measuredValues = unlist(data.frame(data[,2:3]))
  predictedValues = unlist(data.frame(pred[,2:3]))

  error = sd(predictedValues-measuredValues)  # This just estimates the error to be the empirical st. dev.  If you have uncertainties for your measurements, let's talk about how to code that in.
  singlelikelihoods = dnorm(measuredValues, mean = predictedValues, sd = error, log = T)
  sumll = sum(singlelikelihoods)
  
  return(sumll)
}

