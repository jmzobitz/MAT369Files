### Author: JMZ, modified from Logan and Wolesesnky "Mathematical Methods in Biology"
### Purpose: Create a sandhill crane model of discrete dynamics x(t+1)=r*x(t), as detailed on page 311
### r = 1+b-d, where b is the birth rate, d is the death rate.
### b and d are drawn from normally distributed random variables.

### Changeable parameters
s0 = 100;  ### Initial population of sandhill cranes

### Fixed parameters
numberYears=20;		### Number of years we run our simulation
years = 0:numberYears
nSimulations = 1

### Set up vector of results
sandhills=array(0,dim=c(length(years),nSimulations));
sandhills[1]=s0;  # Set the first index equal to the initial amount

### Loop through the each years, varying the birth and death rate
for (i in 2:length(years)) {
  b=rnorm(1,mean=0.5,sd=0.03) ### Birth rate is a random variable of mean 0.5, st dev 0.03.
  d=rnorm(1,mean=0.1,sd=0.08)  ### Death rate is a random variable of mean 0.1, st dev 0.08.
  
  r = 1+b-d;  # The net growth rate
sandhills[i]=r*sandhills[i-1];	### Update current year from last years population x[t]=r*x[t]

}

#### Make a plot of the solution
# Plot your results
library(ggplot2)


dataValues=data.frame(years=years,sandhills=sandhills)
sandhillPlot=ggplot(dataValues, aes(x=years, y=sandhills)) + geom_line(size=2,color='blue') +
  theme(plot.title = element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=20)) +
  labs(x = "Years",y = "Sandhills")

print(sandhillPlot)

