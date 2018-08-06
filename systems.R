### Function systems.R
### Modified: 9/15/15
### Purpose: Solve a two dimensional differential equation using some of the built-in functionality

### Output:  
### screen of single variable plot of solution for each variable (versus time, plus phase line diagram of solution)

### The following values can be modified as needed
#### 1) Information about the equation (parameters, initial conditions)
parameters = c(r = .5, k = 1, e = 1, d = 1)   # parameters: a named vector
initialCondition = c(V = 1, P = 3)   # initial condition.  Be sure to name your variables

### 2) Time values
deltaT = 0.1    # timestep length
timeSteps = 200   # must be a number greater than 1 

### 3) You need to have a function of your dynamics dy/dt = f(t,y)
# R function to calculate the value of the derivatives at each time value
# Use the names of the variables as defined in the vectors above
dynamics <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    ### --> Start of input that you modify.
    ### Your differential equation will go in the following lines
     dV = r * V - k * V * P
     dP = e  * V * P - d * P
    return(list(c(dV, dP)))    ### You might need to modify dV or dP depending on your variables
    ### --> End of the input that you modify
  })
}



############ everything else below the line just plots the solution
# Solve the differential equation
library('deSolve')
time = seq(from=0,by=deltaT,length.out=timeSteps)  # the output time vector
out <- ode(y = initialCondition, times = time, func = dynamics, parms = parameters) ## Integration with 'ode'

## Plot the solution
out.df = as.data.frame(out) # required by ggplot: data object must be a data frame

library(ggplot2)
library(grid)
library(gridExtra)


# First plot
p1 <- ggplot(out.df, aes(x=time, y=get(names(initialCondition)[1]))) +
  geom_line(size=0.75,color='blue')+geom_point(size=3,color='darkblue')+
  labs(x="Time",y=names(initialCondition)[1])+
  
  ### Expand the graph to make sure axes cross at (0,0)
  expand_limits(y=0)

# Second plot
p2 <- ggplot(out.df, aes(x=time, y=get(names(initialCondition)[2]))) +
  geom_line(size=0.75,color='red')+geom_point(size=3,color='darkred')+
  labs(x="Time",y=names(initialCondition)[2])+
  
  ### Expand the graph to make sure axes cross at (0,0)
  expand_limits(y=0)

# Third plot
p3 <- ggplot(out.df, aes(x=get(names(initialCondition)[1]), y=get(names(initialCondition)[2]))) +
  geom_point(size=3,color='purple')+
  labs(x=names(initialCondition)[1],y=names(initialCondition)[2])+
  
  ### Expand the graph to make sure axes cross at (0,0)
  expand_limits(y=0)

grid.arrange( p1,p2,p3, ncol=2)

### Clear the workspace variables to remove clutter
rm(list=ls())