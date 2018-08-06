### Author: JMZ, modified from Logan and Wolesesnky "Mathematical Methods in Biology"
### Purpose: Create a simple random walk model and plot distribution of locations


### Fixed parameters
numberSteps=200;  	### Number of years we run our simulation
steps = 0:numberSteps
nSimulations = 1000

### Set up vector of results
x=array(0,dim=c(length(steps),nSimulations));

### Loop through the each years and simulation, varying the birth and death rate
for (j in 1:nSimulations) {
  for (i in 2:numberSteps) {
    if (runif(1)< 0.5) {x[i,j]=x[i-1,j]+1}   # Move right
    else {x[i,j]=x[i-1,j]-1}   # Move left
  }
}

### Now loop through and do 
quantVals = c(0.025,0.5,0.975);  # The CI we want to utilize
# Now loop through everything
outCI=array(dim = c(length(quantVals),length(steps)));

for (i in 1:length(steps)) {
  outCI[,i] = quantile(x[i,],quantVals);
  
}


#### Make a plot of the solution
# Plot your results
library(ggplot2)

data=data.frame(steps=steps,
                F =outCI[2,],
                L =outCI[1,],
                U =outCI[3,])
### Ensemble plot
walkPlot=ggplot(data,aes(x=steps,y=F)) +
  geom_ribbon(aes(ymin=L,ymax=U),alpha=0.2,colour='grey') +
  geom_line(size=1.5) +
  theme(plot.title = element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=20)) +
  labs(x = "Iteration",y = "x Position")

print(walkPlot)


