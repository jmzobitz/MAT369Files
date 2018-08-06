

nSamples=10000

meanSampleData = array(dim=nSamples)

tempData=globalTempData$globalTemp

mean(tempData)


for (i in 1:nSamples) {
 meanSampleData[i]=mean(sample(tempData,replace=TRUE))

  
}
hist(meanSampleData)
quantile(meanSampleData,c(0.025,0.5,0.975))


##########################
order(tempData)


