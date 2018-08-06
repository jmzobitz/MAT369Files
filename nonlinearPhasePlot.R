### Function nonlinearPhasePlot.R
### Modified: 10/6/15
### Purpose: Solve a two dimensional differential equation using some of the built-in functionality
### Adapted from phasePlane.R by Danny Kaplan
### Output:  
### phase plane diagram of linear system



### 1) Specify information about the plot
numArrows = 20    # number of arrows plotted on each row and column
xWindow = c(-1,5)   # x axis limits.  Must be of the form c(minVal,maxVal)
yWindow = c(-1,5)   # y axis limits.  Must be of the form c(minVal,maxVal)
xLabel = 'N'   # x axis label for plot
yLabel = 'P'   # x axis label for plot

### 2) Your nonlinear function of the system

nonlinearFn <- function() {
  function(x,y=NULL){
    if (is.null(y)) {
      y<- x[2]; x <- x[1];
    }
    
    ### Your two dimensional equation goes in here
    dxdt = x-1*x*y;
    dydt = -y+x*y;
    ######
    
    return( c(dxdt, dydt) );
  }
}

############ everything else below the line just plots the solution
library(reshape2)
phasearrows <- function(fun,xlims,ylims,resol=10, col='black', add=F) {
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

  dataValues = data.frame(xVal=melt(x)[,3], yVal=melt(y)[,3], xEndVal=melt(xEndVals)[,3],yEndVal=melt(yEndVals)[,3])

  library(grid) # needed for arrow function
  library(ggplot2)
  
  p = ggplot(dataValues, aes(x = xVal, y = yVal))+
  geom_segment(aes(xend = xEndVal, yend = yEndVal), arrow = arrow(length = unit(0.1,"cm")))+
  xlab(xLabel)+
  ylab(yLabel)
  

  print(p)
}


phasearrows(nonlinearFn(),xWindow,yWindow,numArrows)



