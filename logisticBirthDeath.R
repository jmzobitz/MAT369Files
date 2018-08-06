### Code for simulating a logistic differential equation.
### Author: JMZ
### Modified: 11/25/15

### Assumes that the equation has random noise in the transmissing


source('summarizeDynamics.R')   # File that plots all of the values
### Set up the model

T=5;   ### Stopping time
dt=0.05;  ### deltaT

### Stochastic parameters
simulations = 10;	### Number of simulations of the process

### Set up initial conditions for the model
p0=10;  			### Initial population size

### Identify the parameters of the model
r=0.06;    		### Growth rate
K=100;				### Carrying capacity


### Set up the time and population vectors
t=seq(0,T,by=dt);  ### Define a vector of time points to evaluate solution
population=array(p0,dim=c(length(t),simulations));  # 
# Set the row names in our simulation matrix to our time values (helpful when we make the plots later)
row.names(population)=t;


### Main loop of the stochastic process
for (j in 1:simulations) {   # Loop through each simulation
  
  for (i in 2:length(t) ) {  # Loop through the time values
    p=population[i-1,j];   ### p stands for the population from the previous timestep
    
    stochasticPart=sqrt(r*p*K+r*p^2)*sqrt(dt)*rnorm(1);  ### Assume the population growth term is stochastic
    
    
    # Now update each of the determinsitc dynamics
    dpdt = r*p*(K-p);   # dp/dt    

    # Update dynamics according to Euler's method  y[new] = y[old] + ydot * dt + any stochastics
    population[i,j]=p+dpdt*(dt)+stochasticPart
    
    
    # Apply a reality constraint so the population stays at 0 if it falls below that
    population[i,j]=max(population[i,j],0)   
    
  }
}

# ### Now plot the results 
labels=c("Time","Population")
summarizeDynamics(t,population,labels);

