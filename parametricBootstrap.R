# Non parametric bootstrap estimate
# Make sure the globalTempData.csv set is loaded in

tempData=globalTempData$globalTemp
order(tempData,decreasing=TRUE)[1:10]
nSamples=1000

nYears = length(tempData)  # How many years is our actual dataset?

yearsBack = 10  # how many years do we look back at the data?


hotIndex=array(dim=nSamples)  # Set up a vector that tracks how many years of the last 10 were some of the hottest.
xBar=mean(tempData)  # Mean of the data
stdX = sd(tempData)  # Average of the data
rnorm(1,mean=xBar,sd=stdX)
for (i in 1:nSamples) {
  newDataSet=rnorm(nYears,mean=xBar,sd=stdX)  # Generate a new temperature dataset
  orderedDataSet=order(newDataSet,decreasing=TRUE) # Order the dataset
  hotIndex[i] = length(which(orderedDataSet[1:yearsBack]>=(nYears-yearsBack))) #count how many times the hottest years occured toward the end
  
}
hist(hotIndex)   # Make a histogram of values
print(table(hotIndex))  # Make a table of values
