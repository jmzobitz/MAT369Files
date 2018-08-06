# A Condensed MCMC Metropolis Hastings method for teaching.  Some of the user defined constructs are set as defaults here to make it easier for use in the classroom.


### Call up an MCMC library (easier to do stuff, but not really necessary?) - maybe for post hoc analyze?
library(coda)

# determine if we are running a superfast mode

if (estimateMode=='superfast') {
  
  NUM_AT_ONCE = 100 #NEEDED
  NCHAINS = 1 # NEEDED
  BURN_ITER = 700 # NEEDED Number of runs we do after convergences, for burn-in
  ESTIMATE_ITER = 800 # Number of runs that we actually record data
  MAX_ITER = 1500  # NEEDED
  CHAIN_TUNING = TRUE;    # Do we tune parameters while we start from chains?
  ESTIMATE_TUNING = FALSE;    # Do we tune parameters while we estimate?
  
} else {
  NUM_AT_ONCE = 10000 #NEEDED
  NCHAINS = 5 # NEEDED
  BURN_ITER = 70000 # NEEDED Number of runs we do after convergences, for burn-in
  ESTIMATE_ITER = 80000 # Number of runs that we actually record data
  MAX_ITER = 150000  # NEEDED
  CHAIN_TUNING = TRUE;    # Do we tune parameters while we start from chains?
  ESTIMATE_TUNING = FALSE;    # Do we tune parameters while we estimate?
  
}

percentIter = 0.5    # Percentage of the iterations we will randomly sample after parameter estimation

# Set the seed in the random number generator to the system time:
seedValue = as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31);
set.seed(seedValue)

### Call up data file
data=read.table(dataFileName, header = TRUE, sep = ",",blank.lines.skip=TRUE)

### Call up parameter file
paramInfo=read.table(paramFileName, header = TRUE, na.strings = "*", sep = "",comment.char="#",blank.lines.skip=TRUE)

### Identify the output info
paramEstimateFileName = "Outputs/parameterEstimate.Rda"  # NEEDED Name of the file where we will save the data frame of all of the parameter estimates
outFileName = "Outputs/estimateLogFile.txt"  # NEEDED Name of file where chain info is written
outFigureFileLocation='Figures/'  # NEEDED for mcmcSoilOutput - directory where all the output info is written.



### Start writing to log file

#file.create(outFileName);
write(c("Starting Parameter Estimation"), outFileName)
write(c(paste("Input parameter file: ",paramFileName)), outFileName,append=TRUE)
write(c(paste("Input data file: ",dataFileName)), outFileName,append=TRUE)
write(c(paste("Output parameter estimate file: ",paramEstimateFileName)), outFileName,append=TRUE)
write(c(""), outFileName,append=TRUE)
write(c(paste("Number of Chains: ",NCHAINS)), outFileName,append=TRUE)
write(c(paste("Max Iterations per Chain: ", MAX_ITER)), outFileName,append=TRUE)
write(c(paste("Burn Iterations / Estimate Iterations: ",BURN_ITER, " / ", ESTIMATE_ITER)), outFileName,append=TRUE)
write(c(paste("MCMC Random Number Generator Seed value: ",seedValue)), outFileName,append=TRUE)
write(c(""), outFileName,append=TRUE)



### Run for NUM_AT_ONCE, check convergence, and then see if we do again

### I guess the <<- assigns a global value to the variable.
	A_STAR<<-0.4  # target acceptance rate
	DEC<<-0.99  # how much to decrease temp. by on rejection
	INC <<- DEC^((A_STAR - 1)/A_STAR);
 	# want INC^A_STAR * DEC^(1 - A_STAR) = 1

	THRESH<<-0.05  # how close we have to get to A_STAR before stop adjusting temperatures





maxLL = -99999;	# set this to a ridiculusly low number so we will accept the first one

#fileConn<-file(outFileName)

paramMaxLL = paramInfo$value[paramInfo$changeable ==1];
bestChain = 1;

for (i in 1:NCHAINS) {
	acceptVal = 0.0;
	iterCurr = 0;
	write(c(paste("Chain", i, "of ", NCHAINS)), outFileName,append=TRUE)
	write(paste("Start Chain Time:", date()),outFileName,append=TRUE)
		# Run chain until convergence, then check max LL
		while ( abs(acceptVal-A_STAR)>THRESH & iterCurr < MAX_ITER ) {
			iterCurr =iterCurr+ NUM_AT_ONCE;
			paramCurr = initialStart(paramInfo)
      
			newChain= run_metropolis_MCMC(paramCurr, NUM_AT_ONCE,data,CHAIN_TUNING)
			acceptVal = newChain$acceptance
			#acceptVal = (acceptVal * (iterCurr-NUM_AT_ONCE) + newChain$acceptance * NUM_AT_ONCE ) / iterCurr;
			write(c(paste("Iteration: ", iterCurr, round(acceptVal,digits=3), "% accepted at ", date())), outFileName,append=TRUE)
      
      paramCurr = newChain$param
			
			
		}
		if (maxLL < max(newChain$likelihood,na.rm=TRUE)) {  # if we have a new max LL, then we will take that as the best chain.
			outChain = newChain;
			maxLL = max(newChain$likelihood,na.rm=TRUE)
			paramMaxLL = newChain$chainInfo[which(newChain$likelihood==maxLL)[1],]

			bestChain = i;
      ##outChain$param$value = paramMaxLL 
		}
		 
		# Write Chain info
	write(c(""), outFileName,append=TRUE)
	write(c(paste("Parameter", "CurrentValue", "MaxLLvalue","Knob")), outFileName,append=TRUE)
	
		outInfo = data.frame(newChain$param$name[newChain$param$changeable ==1], round(newChain$param$value[newChain$param$changeable ==1],digits=3), round(paramMaxLL[newChain$param$changeable ==1],digits=3),round(newChain$param$knob[newChain$param$changeable ==1],digits=3));
		write.table(outInfo, file=outFileName,sep="\t",append=TRUE,row.names=FALSE,quote=FALSE,col.names=FALSE)
		write(c("",paste("Number of Iterations: ", iterCurr)), outFileName,append=TRUE)
		write(c(paste("Acceptance: ", round(acceptVal,digits=3))), outFileName,append=TRUE)
		write(c(paste("Best Chain: ", bestChain)), outFileName,append=TRUE)
		write(c(paste("maxLL of Chain / best maxLL: ", round(max(newChain$likelihood,na.rm=TRUE),digits=5)," / ", round(maxLL,digits=5))), outFileName,append=TRUE)
		write(paste("End Chain Time:", date()),outFileName,append=TRUE)
		write(c("***********************",""), outFileName,append=TRUE)


	
}


### Now do MCMC for the burn in, and then the remainder of the time

write(c(paste("Starting parameter estimation from maxLL parameters from chain: ", bestChain),""), outFileName,append=TRUE)
write(paste("Start Chain Time:", date()),outFileName,append=TRUE)
write(c(""), outFileName,append=TRUE)


burnChain = run_metropolis_MCMC(outChain$param, BURN_ITER,data,ESTIMATE_TUNING)

write(c("Finished burn-in chain; starting estimate chain."), outFileName,append=TRUE)
write(c(paste("Burn chain acceptance: ", BURN_ITER, round(burnChain$acceptance,digits=3), "%")), outFileName,append=TRUE)
write(c(""), outFileName,append=TRUE)
estimateChain = run_metropolis_MCMC(burnChain$param, ESTIMATE_ITER,data,ESTIMATE_TUNING)
write(paste("End Chain Time:", date()),outFileName,append=TRUE)
write(c(paste("Estimate chain acceptance: ", ESTIMATE_ITER, round(estimateChain$acceptance,digits=3), "%")), outFileName,append=TRUE)
write(c(""), outFileName,append=TRUE)
### Prepare parameters for analysis
### Calculate quantiles
quantVals = c(0.025,0.5,0.975);	# The CI we want to utilize
paramQuantiles = apply(estimateChain$chainInfo, 2, quantile, probs = quantVals,na.rm=TRUE)

### Calculate mean
paramMean = apply(estimateChain$chainInfo, 2, mean,na.rm=TRUE)

### Calculate max likelihood parameters from the estimate chain
paramMaxLL = estimateChain$chainInfo[which(estimateChain$likelihood==max(estimateChain$likelihood,na.rm=TRUE)),]

## Only take the first row if we have more than occurence of the max
if (is.null(dim(paramMaxLL))==FALSE) {
paramMaxLL=paramMaxLL[1,]

}


### Save parameters and likelihood
paramValuesOut=data.frame(estimateChain$param$name,paramMaxLL,paramMean,t(paramQuantiles))

names(paramValuesOut)= c("name", "maxLL", "mean", quantVals)



maxLL = max(estimateChain$likelihood,na.rm=TRUE)
meanLL = mean(estimateChain$likelihood,na.rm=TRUE)

llIn = estimateChain$param;
llIn$value = paramMean;

llMeanParam = likelihood(llIn,data)+prior(llIn)

likelihoodInfoOut = data.frame(maxLL,meanLL,llMeanParam);
names(likelihoodInfoOut) = c("maxLL", "meanLL", "llWithMeanParam");


### Save the parameter estimation
save(estimateChain,paramValuesOut,likelihoodInfoOut,file=paramEstimateFileName);


