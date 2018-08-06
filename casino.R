
### Author: JMZ, modified from Logan and Wolesesnky "Mathematical Methods in Biology"
### Purpose: Simluate a casino game where you win or lose a dollar with a given probability
### This corresponds to Exercise 4 on page 318
### Modified 11/12/15

library(reshape2)
library(ggplot2)

houseWinProbability=0.52;  ### The probability of winning the card game
initialCash = 100 ### Initial cash on hand
nTimes = 200;	### the number of times you play

nSimulations = 100 ### the number of simulations

### Set up vector of results
winnings=array(0,dim=c(nTimes,nSimulations));
winnings[1,]=initialCash  # Set the first row of all simulations to 100.
### Loop through the each years and simulation, varying the birth and death rate
for (j in 1:nSimulations) {
  for (i in 2:nTimes) {
    if (runif(1)< houseWinProbability) {winnings[i,j]=winnings[i-1,j]-1}   # LOSING :-(
    else {winnings[i,j]=winnings[i-1,j]+1}   # WINNING :-)
    
    if (winnings[i,j]==0) break   # If you go broke, then exit the loop
  }
}

### Now loop through and make an ensemble plot of the results
quantVals = c(0.025,0.5,0.975);  # The CI we want to utilize
# Now loop through everything
outCI=array(dim = c(length(quantVals),nTimes));

for (i in 1:nTimes) {
  outCI[,i] = quantile(winnings[i,],quantVals);
  
}


#### Make a plot of the solution
# Plot your results
library(ggplot2)
library(reshape2)

data=data.frame(steps=1:nTimes,
                F =outCI[2,],
                L =outCI[1,],
                U =outCI[3,])
### Ensemble plot
ensemblePlot=ggplot(data,aes(x=steps,y=F)) +
  geom_ribbon(aes(ymin=L,ymax=U),alpha=0.2,colour='grey') +
  geom_line(size=1.5) +
  theme(plot.title = element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=20)) +
  labs(x = "Iteration",y = "Money")

print(ensemblePlot)

####
data=data.frame(steps=1:nTimes,winnings =winnings)
melted=melt(winnings)
colnames(melted)=c("steps","iteration","winnings")
### Ensemble plot
ensemblePlot=ggplot(melted,aes(x=steps,y=winnings,group=iteration)) +
  geom_line() +
  theme(plot.title = element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=20)) +
  labs(x = "Games",y = "Winnings")

print(ensemblePlot)










