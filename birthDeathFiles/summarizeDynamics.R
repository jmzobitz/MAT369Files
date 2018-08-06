#### Function created to make a confidence interval from a time value.
### Assumes that the data are melted already
### Returns a data frame of time, the forecast value (F), lower CI (2.5%, L), upper CI (97.5% U)

summarizeDynamics <- function(t,inputData,labels) {
  
  ### Prepare the data in a form that can be easily plotted
  library(reshape2);
  library(ggplot2);
  
  inputDataMatrix = inputData;
  ### The function "melt" puts the data from a matrix format to a vector for easy grouping
  inputData=melt(inputData);
  names(inputData)=c("time","simulation","value")
  
  
  ### Do a line plot
  print (
    ggplot(inputData,aes(x=time,y=value,group=simulation)) +
      geom_line() +
      theme(plot.title = element_text(size=20),
            axis.title.x=element_text(size=20),
            axis.text.x=element_text(size=15),
            axis.text.y=element_text(size=15),
            axis.title.y=element_text(size=20)) +
      labs(x = labels[1],y = labels[2])
  )
  
  
  
  
  
  
  
  ### Now loop through and do 
  quantVals = c(0.025,0.5,0.975);  # The CI we want to utilize
  # # Now loop through everything
  outCI=array(dim = c(length(quantVals),length(t)));
  # 
  for (i in 1:length(t)) {
    outCI[,i] = quantile( inputDataMatrix[i,],quantVals);
    
  }
  
  data=data.frame(t=t,
                  F =outCI[2,],
                  L =outCI[1,],
                  U =outCI[3,])
  
  print(
    ggplot(data,aes(x=t,y=F)) +
      geom_ribbon(aes(ymin=L,ymax=U),alpha=0.2,colour='grey') +
      geom_line(size=1.5) +
      theme(plot.title = element_text(size=20),
            axis.title.x=element_text(size=20),
            axis.text.x=element_text(size=15),
            axis.text.y=element_text(size=15),
            axis.title.y=element_text(size=20)) +
      labs(x = labels[1],y = labels[2])
    
    
  )
  

  
}


#