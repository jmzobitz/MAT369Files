
### Author: JMZ, modified from Logan and Wolesesnky "Mathematical Methods in Biology"
### Purpose: Simulate the dynamics of population that produces variable number of offspring generation to generation
### This corresponds to Exercise 5 on page 318
### Modified 11/12/15


library(reshape2)
library(ggplot2)

initialPopulation = 10 ### Initial population
nRealizations = 1;  ### the number of realizations of the dynamics

nGenerations = 200 ### the number of Generations

### Set up vector of results
population=array(0,dim=c(nGenerations,nRealizations));
population[1,]=initialPopulation  # Set the first row of the initial population to p0.
### Loop through the each years and simulation, varying the birth and death rate
for (j in 1:nRealizations) {
  for (i in 2:nGenerations) {
    r = runif(population[i-1,j])   ### Variable testing how many offspring are made
    newPopulation = array(0,dim=population[i-1,j])  ### Set up a vector of new populations, depending on 
    
    newPopulation[which(r<0.25)]=2  # Two offspring are made if r < 0.25
    newPopulation[which((0.25 <= r & r < 0.75))]=1  # One offspring are made if 0.25 <= r < 0.75
    
    # else no more offspring are made (all other values of newPopulation are 0)
    
    population[i,j]=sum(newPopulation)   # Update the population
    
    if (population[i,j]==0) break   # If you go extinct, then exit the loop
  }
}


#### Make a plot of the solution
# Plot your results
library(ggplot2)
library(reshape2)


####
melted=melt(population)
colnames(melted)=c("generations","iteration","population")
### Ensemble plot
ensemblePlot=ggplot(melted,aes(x=generations,y=population,group=iteration)) +
  geom_line() +
  theme(plot.title = element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=20)) +
  labs(x = "Generation",y = "Population")

print(ensemblePlot)











