### Script for modifying results from Munz et al 2009.
### Author: JMZ
### Modified: 8/25/11

source('summarizeDynamics.R')   # File that plots all of the values
### Set up the model

T=10;   ### Stopping time
dt=0.5;  ### deltaT


### Stochastic parameters
sigma=0.004;			### Standard deviation of random noise
simulations = 10;	### Number of simulations of the process


### Set up initial conditions for the model
N=500;  ### Initial population size  (S + I + Z = N)
  
### Identify the parameters of the model

birthRate = 0;  ### This is capital pi in Munz et al 2009
transmission = 0.0095; ### This is parameter beta in Munz et al 2009
nonZombieDeath = 0.0001; ### This is parameter delta in Munz et al 2009
resurrection =  0.1; ### This is parameter xi in Munz et al 2009
defeat = 0.005; ### This is parameter alpha in Munz et al 2009


t=seq(0,T,by=dt);  ### Define a vector of time points to evaluate solution

susceptibles=array(N-1,dim=c(length(t),simulations));  # At least one person is infected
infecteds=array(1,dim=c(length(t),simulations));
recovered=array(0,dim=c(length(t),simulations));

# Set the row names in our simulation matrix to our time values (helpful when we make the plots later)
row.names(susceptibles)=t;
row.names(infecteds)=t;
row.names(recovered)=t;

### Main loop of the stochastic process
for (j in 1:simulations) {   # Loop through each simulation
  
  for (i in 2:length(t) ) {  # Loop through the time values
    s=susceptibles[i-1,j];   ### s stands for the susceiptible people in Munz et al 2009
    z=infecteds[i-1,j];  ### z stands for the zombies from Munz et al 2009
    r=recovered[i-1,j];  ### r stands for the removed from Munz et al 2009
    
    stochasticPart=s*z*sigma*sqrt(dt)*rnorm(1);  ### Assume the transmission rate is stochastic
    
    # Now update each of the determinstic dynamics
    dsdt = birthRate - transmission*s*z-nonZombieDeath*s;   # ds/dt
    dzdt = transmission*s*z+resurrection*r-defeat*s*z;   # dz/dt
    drdt = nonZombieDeath*s+defeat*s*z-resurrection*r;      # dr/dt
 
    # Update dynamics according to Euler's method  y[new] = y[old] + ydot * dt + any stochastics
    susceptibles[i,j]=s+dsdt*(dt)-stochasticPart
    infecteds[i,j]=z+dzdt*(dt)+stochasticPart 
    recovered[i,j]=r+drdt*(dt)
    
    # Apply a reality constraint so the population stays at 0 if it falls below that
    susceptibles[i,j]=max(susceptibles[i,j],0)
    infecteds[i,j]=max(infecteds[i,j],0)
    recovered[i,j]=max(recovered[i,j],0)
    
  }
  
}

# ### Now plot the results 
labels=c("Time","Susceptibles")
summarizeDynamics(t,susceptibles,labels);

labels=c("Time","Infecteds")
summarizeDynamics(t,infecteds,labels);

labels=c("Time","Removed")
summarizeDynamics(t,recovered,labels);
