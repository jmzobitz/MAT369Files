### Function eulerSystems.R
### Modified: 8/16/15
### Purpose: Solve a multi-dimensional dimensional differential equation with Euler's method

### Output:  
### time: Output vector of times
### population: Estimated solution from Euler's method.

### The following values can be modified as needed

#### 1) Information about the equation (parameters, initial conditions)
parameters = c(r = 2, k = 0.5, e = 0.1, d = 1)   # parameters: a named vector
initialCondition =matrix( c(1, 3), nrow=1, ncol=2)   # initial condition.  Multiple conditions can be found 
variableNames=c('V','P')


### 2) Time values
deltaT = 0.05    # timestep length
timeSteps = 200   # must be a number greater than 1 


### 3) You need to have a function of your dynamics dy/dt = f(t,y)
# R function to calculate the value of the derivatives at each time value
# Use the names of the variables as defined in the vectors above
dynamics <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    dV = r*V - k*V*P
    dP = e*k*V*P - d*P
    return(c(dV, dP))
  })
}


############ everything else below the line just plots the solution
# Solve the differential equation
nSolns = dim(initialCondition)[1]  # Determine how many initial conditions we have
nVars = dim(initialCondition)[2]  # Determine how many variables we have
time = seq(from=0,by=deltaT,length.out=timeSteps)  # the output time vector

outSolutions=array(0,dim=c(1,4))  # Vector of solutions  time solution variableName Run

for (j in 1:nSolns) {
  
  newP =  initialCondition[j,] # The updated value
  names(newP)=variableNames;  # Assign the variable name to the 
  
  outMatrix = matrix(0,nrow=nVars,ncol=4);
  outMatrix[,1]=time[1];
  outMatrix[,2]=initialCondition[j,];
  outMatrix[,3]=variableNames;
  outMatrix[,4]=j;
  outSolutions=rbind(outSolutions,outMatrix)
  
  
  
  for (i in 2:timeSteps) {
    oldP = newP
    newP =  dynamics(t,oldP,parameters)*deltaT+oldP   # Your differential equation goes here.
    #outSolutions=rbind(outSolutions,c(time[i],newP,j))
    
    outMatrix = matrix(0,nrow=nVars,ncol=4);
    outMatrix[,1]=time[i];
    outMatrix[,2]=newP;
    outMatrix[,3]=variableNames;
    outMatrix[,4]=j;
    outSolutions=rbind(outSolutions,outMatrix)
    
    
    
    
  }
  
}

# Remove the first row of outSolutions
outSolutions=outSolutions[-1,];
outSolutions=unname(outSolutions)

outSolutions = data.frame(time=as.numeric(outSolutions[,1]), value=as.numeric(outSolutions[,2]),variableName=outSolutions[,3],run=as.numeric(outSolutions[,4]))
# ggplot2 
library(ggplot2) 
library(grid)
library(gridExtra)


#pdf(file = outPlotName)
# First plot
outPlot= ggplot(data.frame(outSolutions), aes(x=time, y=value,color=variableName)) +
  geom_point(size=3)+
  labs(title="Euler's Method Solution",x="Time",y="")+
  ### Expand the graph to make sure axes cross at (0,0)
  expand_limits(y=0)

print(outPlot)



### Clean up workspace
rm(list=ls(all=TRUE))
