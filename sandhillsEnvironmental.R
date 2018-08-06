### Author: JMZ, modified from Logan and Wolesesnky "Mathematical Methods in Biology"
### Purpose: Create a sandhill crane model of discrete dynamics x(t+1)=r*x(t), as detailed on page 311
### r = 1+b-d, where b is the birth rate, d is the death rate.
### b and d are drawn from normally distributed random variables.

### Changeable parameters
s0 = 100;  ### Initial population of sandhill cranes
floodRate = 9*1/25   # Catastrophic flood rate

### Fixed parameters
numberYears=200;  	### Number of years we run our simulation
years = 0:numberYears


### Set up vector of results
sandhills=array(0,dim=length(years));
sandhills[1]=s0;  # Set the first index equal to the initial amount

### Loop through the each years, varying the birth and death rate
for (i in 2:length(years)) {
  if (runif(1)< floodRate) {r=0.575}   # Catastrophic years the net growth rate is lowered
  else {r=1.4}   # Normal years the net growth rate is 1.4
  sandhills[i]=r*sandhills[i-1];	### Update current year from last years population x[t]=r*x[t], r = 1-b-d
  
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


