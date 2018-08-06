### Function gauseLikelihood.R
### Modified: 10/15/15
### Purpose: Likelihood function plot of Sacchromyces data (yeast) from Gause 1932 "Experimental studies on the struggle for coexistence"
### Output: contour plot of likelihood function


library(ggplot2)
library(reshape2) # for melt


### Show data that we want to plot
gauseData=data.frame(time = c(6, 16, 24, 29, 40, 48, 53),volume = c(0.37, 8.87, 10.66, 12.50, 13.27, 12.87, 12.70))

### Make a quick ggplot of the data
# Contour plot of likelihood function
sPlot = ggplot(gauseData, aes(time, volume))+
  geom_point(size=6)+
labs(title="Saccharomyces volume from Gause (1932)",x="Time",y="Volume")
print(sPlot)

## Time data is already embedded in the logistic function.  
# Volume data is part of the likelihood

# Identify the ranges of the parameters that we wish to investigate
kParam=seq(5,20,length.out=100)
bParam=seq(-1,0,length.out=100)

nContours = 20;   # Number of contours used in our expression

### Here is the function used for the logistic model in Gause et al 1932.  k and b are the parameters
logistic <- function(k,b){
  # The value of a is determined by the initial condition
  a = (k-0.45)/0.45
  time = c(6, 16, 24, 29, 40, 48, 53);
  yOut = k/(1+exp(b*time))
  return(yOut)
}

#### Stuff specific to the particular likelihood function
# Likelihood function for zombie parameters
likelihood <- function(k,b){  
  volume = c(0.37, 8.87, 10.66, 12.50, 13.27, 12.87, 12.70);
  predVolume = logistic(k,b)
  error = sd(predVolume-volume)
 singlelikelihoods = dnorm(volume, mean = predVolume, sd = error, log = F)
 return(prod(singlelikelihoods))
  # Note: if you want to do the log likelihood, it is an easy switch.  Comment out the two lines above and uncomment the rest
 # singlelikelihoods = dnorm(volume, mean = predVolume, sd = error, log = T)
  #return(-sum(singlelikelihoods))
  
}


### Apply the likelihood to each element of the matrix
vecLikelihood <- Vectorize(likelihood,vectorize.args = c('k','b'))

zValues = outer(kParam,bParam,vecLikelihood)
### We assume that the data are normally distributed with mean 0 and standard deviation sigma
rownames(zValues)=kParam
colnames(zValues)=bParam
zValues3d <- melt(zValues)
names(zValues3d) <- c("k", "b", "likelihoodVal");


# Contour plot of likelihood function
v = ggplot(zValues3d, aes(k, b, z = likelihoodVal))+
  geom_tile(aes(fill = likelihoodVal)) + stat_contour()+
  labs(title="Contour Plot of Likelihood Function")

print(v)