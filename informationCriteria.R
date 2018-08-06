### Function regressionPlot.R
### Modified: 10/6/15
### Purpose: Do a regression of values

### Output:  
### 1) Printed display of regression formula statistics
### 2) Plot of data, along with fitted formula and forecast uncertainty.


# 1) Identify the name of the data set you wish to run a regression on and labels for the axis.  This will usually be listed in ``Data'' under the global environment.  If it is not there you will need to import it into your workspace:
datasetName =  iris

# 2) Identify your dependent and independent variables in the data.  Note we use $ to signify column names for the data.  y = dependent variable, x = independent variable

y = iris$Sepal.Length
x = iris$Petal.Width

# 3) Identify the basic formula of the regression and plotting formula. Keep the variable name `regressionFormula = ', but everything after can be modified.

regressionFormula = y ~ 1 + x
  
# 4) Identify the plot names for the regression plot
indepVariableName = 'Petal Width (mm)'
depVariableName = 'Sepal Length (mm)'




##########################
#### Everything else below plots the solution and the results.  You can tweak as needed, but proceed with caution ### 
##########################

# Define the likelihood function


# Determine the linear fit according to your regression formula and print the summary
fit=lm(regressionFormula, data = datasetName)
nData = length(y)
nParams = length(fit$coefficients)
print(paste0('Number of parameters (P): ',nParams))
print(paste0('Number of data points (N): ',nData))
print(logLik(fit))

