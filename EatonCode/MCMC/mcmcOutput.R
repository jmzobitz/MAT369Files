# Take an estimate chain and do the following:
#
# Summarize the results with the mean parameters and the histograms (one big plot ...)
# Plot the data with the 95% CI of the parameters (DONE)
# Pairwise correlations of parameters (DONE)

### Save the plot

# Sample chain NSAMPLE times
# run the model for each of those parameters
# then calculate the CI for each timestep

#load(paramEstimateFileName)
#load(dataFileName)


# Write the parameter output to a file (estimated parameters only)

estimateSummaryFileName = paste0(outFigureFileLocation,'estimationSummary.txt')

capture.output(print('Names of Estimated parameters:'), file =estimateSummaryFileName) 
capture.output(print(estimateChain$param$name[which(estimateChain$param$changeable==1)]), file =estimateSummaryFileName,append=TRUE) 

capture.output(summary(estimateChain$chainInfo[,which(estimateChain$param$changeable==1)]), file =estimateSummaryFileName,append=TRUE) 

######
# x and y are already predefined  x = inputs  y = output
paramValues = estimateChain$chainInfo # A matrix where each row is an iteration, each column a parameter



### Now start to make some plots
library(ggplot2)



### Print a boxplot for each of the estimated parameters.
### boxplot  --> edit this for additional parameters
# x and y are already predefined  x = inputs  y = output
paramValues = data.frame(estimateChain$chainInfo[,1]) # A matrix where each row is an iteration, each column a parameter
changeableParams = which(estimateChain$param$changeable==1)

for (i in 1:length(changeableParams)) {
  paramName=estimateChain$param$name[changeableParams[i]]
  saveFileName=paste0(outFigureFileLocation,paramName,'.boxplot.png')


  parameter1 = ggplot(data.frame(estimateChain$chainInfo[,changeableParams[i]]),aes(x=1,y=var1))+geom_boxplot() + scale_x_continuous(breaks=NULL) + theme(axis.title.x=element_blank()) + ylab('Parameter')
  print(parameter1)
  ggsave(saveFileName)
  
  
}


### Pairwise parameter correlations
source("MCMC/correlationMatrixFunctions.R")  # Helper functions to do the correlation plots

parameterData = as.data.frame(estimateChain$chainInfo[,which(estimateChain$param$changeable==1)])
colnames(parameterData) = estimateChain$param$name[which(estimateChain$param$changeable==1)]

png(paste0(outFigureFileLocation,'pairwiseParameterPlot.png'))

pairs(parameterData,pch='.',upper.panel=panel.cor,
      diag.panel=panel.hist,
      lower.panel=panel.lm)

dev.off()






