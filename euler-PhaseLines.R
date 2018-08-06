### Function euler.R
### Modified: 2/6/14
### Purpose: Solve a one dimensional differential equation with Euler's method

### Output:  
### time: Output vector of times
### population: Estimated solution from Euler's method.

### The following values can be modified as needed

#### 1) Information about the equation (parameters, initial conditions)
parameters = c(r = 1.2, k = 3)   # parameters: a named vector
initialCondition = c(11)   # initial condition
variableName='S'
### 2) Time values
deltaT = 0.1    # timestep length
timeSteps = 50   # must be a number greater than 1 

### 3) Output file names (a plot and values)
outPlotName = 'test.pdf'  # pdf name of the file you need
outSolnFileName = 'solution.txt'


### 4) You need to have a function of your dynamics dy/dt = f(t,y)
dynamics <- function (t, state,parameters) {
  with(as.list(c(state, parameters)), {
  
    ### Modify this line of code for your dynamics
    dSdt = r*S*(k-S)
    print(dSdt)
  return(dSdt)
})
}


############ everything else below the line just plots the solution
nSolns = length(initialCondition)  # Determine how many initial conditions we have
time = seq(from=0,by=deltaT,length.out=timeSteps)  # the output time vector

outSolutions=array(0,dim=c(1,3))  # Vector of times out

for (j in 1:nSolns) {
  
  newP =  initialCondition[j] # The updated value
  names(newP)=variableName;  # Assign the variable name to the 

  outSolutions=rbind(outSolutions,c(time[1],initialCondition[j],j))
  
  
  
  for (i in 2:timeSteps) {
    oldP = newP
    newP =  dynamics(t,oldP,parameters)*deltaT+oldP   # Your differential equation goes here.
    outSolutions=rbind(outSolutions,c(time[i],newP,j))
  }
  
}

# Remove the first row of outSolutions
outSolutions=outSolutions[-1,];
outSolutions=unname(outSolutions)
# ggplot2 
library(ggplot2) 
colnames(outSolutions)=c("time","value","run")

# create factors with value labels 

#pdf(file = outPlotName)

outPlot=ggplot(data.frame(outSolutions), aes(x=time, y=value,group=run))+
        geom_line(size=0.75,color='blue')+geom_point(size=3,color='darkblue')+
  
  labs(title="Euler's Method Solution",x="Time",y=variableName)+

  ### Expand the graph to make sure axes cross at (0,0)
  expand_limits(y=0)
print(outPlot)
#dev.off()


write.table(format(outSolutions,digits=3), file = outSolnFileName, sep = "\t",qmethod = "double",quote=FALSE,row.names=FALSE,col.names=TRUE)
