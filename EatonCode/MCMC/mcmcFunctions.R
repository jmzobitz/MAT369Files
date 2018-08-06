### Auxillary functions for MCMC parameter estimation

# 1) initialStart: Gives a random initial start to parameters and adds in a tunable knob for simulated annealing
#    INPUT:
#    OUTPUT:
# 2) proposalFunction: MH jump algorithm to determine for the parameter estimation
# 3) run_metropolis_MCMC: the main MCMC estimator, forging ahead through iterations
### Primary source: http://theoreticalecology.wordpress.com/2011/12/09/mcmc-chain-analysis-and-convergence-diagnostics-with-coda-in-r/

### Prior likelihood values, calculates the log probability density from the prior, assuming a uniform priod density
# necessary to calculate the prior probabilities here?  They do cancel out when we have uniform PDFs, but I guess they could matter in other cases.



prior <- function(param)  {
  # super cool way to do this all at once!
  priorProb = dunif(param$value[param$changeable==1],min=param$minVal[param$changeable==1], max=param$maxVal[param$changeable==1], log = T)
  
  return(sum(priorProb))
}



### Random initial start
initialStart <- function(paramInfo)	{
    param = paramInfo;	# Make a local copy
    nChangeable=sum(param$changeable==1)	# number of changeable parameters , since they are all 0 or 1, this should work.
    
    # super cool way to do this all at once!
    param$value[param$changeable == 1] = runif(nChangeable,min=param$minVal[param$changeable==1], max=param$maxVal[param$changeable==1])
    nParams=length(param$value)		# number of parameters

	param$knob = array(1,dim=c(nParams,1));		# add in a row of "temperatures" for simulated annealing
  
  return(param)
}



### Main MCMC routine.
run_metropolis_MCMC <- function(paramInfo, iterations,data,tuning){
	# tuning = TRUE ==> simulated annealing
	# tuning = FALSE ==> keep the knob as it is
  # saveValues = TRUE ==> Write out accepted values
  # saveValues == FALSE ==> Returns a dummy value
	
	if (tuning == FALSE) {
		incVal = 1
		decVal = 1
	} else {
		incVal = INC
		decVal = DEC
	}
	
  
    param = paramInfo;	# Make a local copy, and this keeps the current value of the parameters
    nParams=length(param$value)		# number of parameters
	  

    paramSample = sample(param$name[param$changeable==1],iterations,replace = TRUE)
    randomAccept = -rexp(iterations);  # Vector of random acceptance probabilities
    tune = runif(iterations)-0.5;
	  seedSave = .Random.seed;   # Save the current state of the RNG seed, if it gets reset it in the model
    
    accept = 0;
    chain = param$value;
  
    likelihoodVal=likelihood(param,data)+prior(param);
    paramSampleOutIndex = 1;  
    
    oldLL = likelihoodVal[1];
      for (i in 2:iterations){
      
		    oldParam = param;
		    acceptFlag = 0;
		   ###########
		    
 		    paramToChange = param[param$name==paramSample[i],];
 		    paramRange = paramToChange$maxVal - paramToChange$minVal
		    paramToChange$value = (paramToChange$knob * paramRange * tune[i])+paramToChange$value
 		    ### Select a random value, making sure it is tuned correctly, but also keeping in the bounds of the parameter
         
 		    if ( (paramToChange$value < paramToChange$minVal) | (paramToChange$value > paramToChange$maxVal) ) {acceptFlag = 9999}
 		    else  {
           param$value[param$name==paramSample[i]]=paramToChange$value
           newLL=likelihood(param,data)+prior(param)
           
           # double check to make sure we don't have pesky NaN affecting things.
           # most likely LL will be Inf of NaN --> no probability of occurrence.
                    
           probab = (newLL-oldLL);
           
           if ((is.na(probab)==TRUE) | (is.finite(probab)==FALSE) ) {acceptFlag = 9999}
          		    
 		    }

        if (acceptFlag != 9999) {         
          if ( ( randomAccept[i] < probab) ) {acceptFlag = 1}
        }
        # Evaluate if accept or reject
        
        if (acceptFlag == 1) {
          chain= rbind(chain,param$value)	# accept - 
				
		      param$knob[param$name==paramSample[i]]= max(param$knob[param$name==paramSample[i]]*incVal,1e-8);
          
		      accept = accept+1;
          likelihoodVal = rbind(likelihoodVal,newLL)
          paramSampleOutIndex = c(paramSampleOutIndex, i);
         
         
          oldLL = newLL;
         }  else {
  		    # Adjust the knob
           
           param$knob[param$name==paramSample[i]]= max(param$knob[param$name==paramSample[i]]*decVal,1e-8);
 		      # Return to the old value
		      param$value=oldParam$value;

  }
	
  
}
  
	.Random.seed = t(seedSave);  # Load back up to seed the current state of RNG for MCMC
	outList <- list("chainInfo" = mcmc(chain), "param" = param, "acceptance" = (accept/iterations), "likelihood"=likelihoodVal,"paramSample"=paramSample[paramSampleOutIndex])
	
   return(outList)
}



