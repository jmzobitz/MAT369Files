

### Author: JMZ, modified from Logan and Wolesesnky "Mathematical Methods in Biology"
### Purpose: Simluate a wildebeast population undergoing harvesting with stochastic environmental effects
### due to rainfall.
### This corresponds to Exercise 8 on page 318
### Modified 11/12/15

### Variable parameters for the wildebeast population

library(reshape2)
library(ggplot2)
harvestRate=0.5;  ### The harvesting rate of the wildebeast.

nYears = 5;  ### Number of years we run the population
nSimulations = 2  # Number of simulations we try

### Fixed parameters for the simulation
initialWildebeast=250000;	### Initial number of wildebeasts
r=1.1323;	### The growth rate
rainfall= c(100, 36, 100, 104, 167, 107, 165, 71, 91, 77, 134, 192, 235, 159, 211, 257, 204, 300, 187, 84, 99, 163, 97, 228, 208)  ### Rainfall rates for a 34 year period 




### Set up the results
wildebeastResults=array(0,dim=c(nYears,nSimulations));
wildebeastResults[1,]=initialWildebeast;
for (j in 1:nSimulations) {
for (i in 2:nYears) {
  currRainfall = sample(rainfall,1);
  carryingCapacity = 20748*currRainfall
  wildebeastResults[i,j]=((1-harvestRate)*r*carryingCapacity*wildebeastResults[i-1,j])/(carryingCapacity+(r-1)*wildebeastResults[i-1,j]);
  
}
}

### Now make a plot
melted=melt(wildebeastResults)
colnames(melted)=c("generations","iteration","population")
### Ensemble plot
ensemblePlot=ggplot(melted,aes(x=generations,y=population,group=iteration)) +
  geom_line() + geom_abline(intercept = 150000,slope=0,color='blue',size=1.5) +
  theme(plot.title = element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=20)) +
  labs(x = "Years",y = "Population")


print(ensemblePlot)


print (
  ggplot(inputData,aes(x=time,y=value,group=simulation)) +
    geom_line() +
    theme(plot.title = element_text(size=20),
          axis.title.x=element_text(size=20),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.y=element_text(size=20)) +
    labs(x = labels[1],y = labels[2])
)









