# Each function needs the following:

# 1) equation that describes your dynamical system
# 2) model to pair with data (call this function model)
# 3) likelihood function to describe model-data mismatch (call this function likelihood)

library(deSolve)  ### needed because we are solving des

dynamics <- function (t, data, parameter) {
  
  ### List all your parameters here in the same order listed in "defaults.param"
  k=parameter[1]
  b=parameter[2]
 

  
  H = data[1]
  Z = data[2]
  C = data[3]
  ### This is your rate equation
  dH = -k*H*Z       # the derivative of H wrt time
  dZ = k*H*Z-b*Z    # the derivative of Z wrt time
  dC = b*Z          # the derivative of C wrt time 
  
  ### You don't need to bother with the remaining items
  outVal = list(c(dH,dZ,dC))
  
  return(outVal)
}




model <- function(parameter,data) {
  
  currParam = setNames(parameter$value, parameter$name)
  xInit = c(H=71, Z=1,C=0); # Let's set up some initial conditions at time t=0
  
  timeVec= data$Hour  # time vector - let's determine the values of H, Z, and C at times in vt

  
  soln = ode(func=dynamics,
             times=timeVec, ## times
             y=xInit, ## init. conditions (named)
             parms=currParam) ## parameter values (named)

  soln=setNames(soln, colnames(data))  # Set the column names of the solution
  
  return(soln)  # return the solution as a vector to make things easy
}

#### Stuff specific to the particular likelihood function
# Likelihood (or objective) function for zombie parameters
likelihood <- function(parameter,data){

  pred = model(parameter,data)
  # Record the measured and predicted values as a list
  measuredValues = unlist(data.frame(data[,2:3]))  # We only want the humans and zombies (first two columns of our measurements)
  predictedValues = unlist(data.frame(pred[,2:3]))  # We only want the humans and zombies (first two columns of our data)
  
  error = sd(predictedValues-measuredValues)  # This just estimates the error to be the empirical st. dev.  If you have uncertainties for your measurements, let's talk about how to code that in.
  singlelikelihoods = dnorm(measuredValues, mean = predictedValues, sd = error, log = T)
  sumll = sum(singlelikelihoods)
  
  return(sumll)

 
}

