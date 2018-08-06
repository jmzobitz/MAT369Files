#### Function mcmcMaster.R
### Modified: 10/30/15
### Purpose: Run a simple MCMC estimator to determine that is coupled to a differential equation model.  File names are all relative to the current directory.


### STEP 1: Determine the name of the data file used in the parameter estimation. 
dataFileName = '2012datacsv.csv'
  
### STEP 2: Specify where specific model functions and parameters are located
paramFileName = "defaults.param" # Location of file for parameters


### STEP 3: identify the type of estimation. Superfast is used for debugging code.
### Normal is regular.  Either uncomment one or the other.
estimateMode='superfast'
#estimateMode='normal'

### STEP 4: Specify where specific model functions and parameters are located
source("model.R")  # NEEDED TO SPECIFY SOME MODEL


######### NOW WE ARE READY FOR PARAMETER ESTIMATION. YOU DO NOT NEED TO MODIFY ANYTHING BELOW THIS LINE

print("Doing parameter estimation.  Please be patient.")
# Call main code to do the mcmc estimation (NEEDED)
source("MCMC/mcmcFunctions.R")  # NEEDED
source("MCMC/mcmcMetro.R")
print("Parameter estimation complete. Running model estimates.")

# # 3) Run the output file to model ensemble runs
source("MCMC/mcmcOutput.R")
# 
# 
# ### Clean up the memory
closeAllConnections()
rm(list=ls(all=TRUE))
# 


