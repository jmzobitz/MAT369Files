### Author: JMZ, modified from Logan and Wolesesnky "Mathematical Methods in Biology"
### Purpose: Create a sandhill crane model of discrete dynamics x(t+1)=r*x(t), as detailed on page 311
### r = 1+b-d, where b is the birth rate, d is the death rate.
### b and d are drawn from normally distributed random variables.

### Changeable parameters
s0 = 100;  ### Initial population of sandhill cranes
floodRate = 5/25;

### Fixed parameters
numberYears=20;  	### Number of years we run our simulation
years = 0:numberYears
nSimulations = 100

### Set up vector of results
sandhills=array(0,dim=c(length(years),nSimulations));
sandhills[1,]=s0;  # Set the first index equal to the initial amount

### Loop through the each years and simulation, varying the birth and death rate
for (j in 1:nSimulations) {
  for (i in 2:length(years)) {
    if (runif(1)< floodRate) {r=0.575}   # Catastrophic years the net growth rate is lowered
    else {r=1.4}   # Normal years the net growth rate is 1.4
    sandhills[i,j]=r*sandhills[i-1,j];  ### Update current year from last years population x[t]=r*x[t], r = 1-b-d
    
    
  }
}

### Now loop through and do 
quantVals = c(0.025,0.5,0.975);  # The CI we want to utilize
# Now loop through everything
outCI=array(dim = c(length(quantVals),length(years)));

for (i in 1:length(years)) {
  outCI[,i] = quantile(sandhills[i,],quantVals);
  
}




###






#### Make a plot of the solution
# Plot your results
library(ggplot2)

data=data.frame(years=years,
                F =outCI[2,],
                L =outCI[1,],
                U =outCI[3,])
### Ensemble plot
sandhillPlot=ggplot(data,aes(x=years,y=F)) +
  geom_ribbon(aes(ymin=L,ymax=U),alpha=0.2,colour='grey') +
  geom_line(size=1.5) +
  theme(plot.title = element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=20)) +
  labs(x = "Years",y = "Sandhill Crane Population")

print(sandhillPlot)


