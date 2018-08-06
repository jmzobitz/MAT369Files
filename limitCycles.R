### Function limitCycles.R
### Modified: 10/6/15
### Purpose: Examine phase plane and solution curves for a Hopf bifurcation
### Adapted from phasePlane.R by Danny Kaplan
### Output:  
### phase plane diagram of linear system


### The following values can be modified as needed
#### 1) Information about the equation (parameters, initial conditions)
initialCondition = c(x = 0.2, y=0.6)   # initial condition.  Be sure to name your variables
parameters = c(mu=1)   # parameters: a named vector (you will mess with this later)


############ everything else below the line just plots the solution
### 1) Specify information about the phaseplane setup
numArrows = 20    # number of arrows plotted on each row and column
xWindow = c(-1.5,1.5)   # x axis limits.  Must be of the form c(minVal,maxVal)
yWindow = c(-1.5,1.5)   # y axis limits.  Must be of the form c(minVal,maxVal)
xLabel = 'x'   # x axis label for plot
yLabel = 'y'   # x axis label for plot

### 2) Time values for the integration  (can be adjusted, but you won't need to mess with them here)
deltaT = 0.1    # timestep length
timeSteps = 100   # must be a number greater than 1 

### 3) You need to have a function of your dynamics dy/dt = f(t,y) for the numerical solver
# R function to calculate the value of the derivatives at each time value
# Use the names of the variables as defined in the vectors above
dynamics <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    ### --> Start of input that you modify.
    ### Your differential equation will go in the following lines
    dxdt = -y-x*(x^2+y^2-mu)
    dydt = x-y*(x^2+y^2-mu);
    return(list(c(dxdt, dydt)))    ### You might need to modify dV or dP depending on your variables
    ### --> End of the input that you modify
  })
}

### 4) Your nonlinear function of the system - we need this for the phase plane.  It is the same as dynamics
### for now, I just need to figure out how to combine the two.

nonlinearFn <- function() {
  function(x,y=NULL){
    if (is.null(y)) {
      y<- x[2]; x <- x[1];
    }
    
    ### Your two dimensional equation goes in here
      dxdt = -y-x*(x^2+y^2-parameters[1])
    dydt = x-y*(x^2+y^2-parameters[1]);
    ######
    
    return( c(dxdt, dydt) );
  }
}


library(reshape2)
phasearrows <- function(fun,xlims,ylims,deltaT=1,timeSteps=10,resol=10, col='black', add=F) {
  if (add==F) {
    plot(1,xlim=xlims, ylim=ylims, type='n');
  }
  x <- matrix(seq(xlims[1],xlims[2], length=resol), byrow=T, resol,resol);
  y <- matrix(seq(ylims[1],ylims[2], length=resol),byrow=F, resol, resol);
  npts <- resol*resol;
  xspace <- abs(diff(xlims))/(resol*5);
  yspace <- abs(diff(ylims))/(resol*5);
  
  
  
  x <- x + matrix(runif(npts, -xspace, xspace),resol,resol);
  y <- y + matrix(runif(npts, -yspace, yspace),resol,resol);
  
  
  z <- fun(x,y);
  
  z1 <- matrix(z[1:npts], resol, resol);
  z2 <- matrix(z[(npts+1):(2*npts)], resol, resol);
  maxx <- max(abs(z1));
  maxy <- max(abs(z2));
  dt <- min( abs(diff(xlims))/maxx, abs(diff(ylims))/maxy)/resol;
  lens <- sqrt(z1^2 + z2^2);
  lens2 <- lens/max(lens); 
  delta_x = dt*z1/((lens2)+.1);
  delta_y = dt*z2/((lens2)+.1);
  
  xEndVals = x + delta_x;
  yEndVals = y + delta_y;
  
  # Solve the differential equation
  library('deSolve')
  time = seq(from=0,by=deltaT,length.out=timeSteps)  # the output time vector
  out <- ode(y = initialCondition, times = time, func = dynamics, parms = parameters, method = "ode45") ## Integration with 'ode'
  

  dataValues = data.frame(xVal=melt(x)[,3], yVal=melt(y)[,3], xEndVal=melt(xEndVals)[,3],yEndVal=melt(yEndVals)[,3],xSoln=out[,2],ySoln=out[,3])
  
  library(grid) # needed for arrow function
  library(ggplot2)
  
  p = ggplot(dataValues, aes(x = xVal, y = yVal))+
    geom_segment(aes(xend = xEndVal, yend = yEndVal), arrow = arrow(length = unit(0.1,"cm")))+
    xlab(xLabel)+
    ylab(yLabel) +
    geom_point(data=dataValues, aes(x=xSoln, y=ySoln),size=3,color='purple')
  #+geom_line(data=dataValues, aes(x=xSoln, y=ySoln),size=1,color='purple')

  
  print(p)
  

  
}


phasearrows(nonlinearFn(),xWindow,yWindow,deltaT,timeSteps,numArrows)




