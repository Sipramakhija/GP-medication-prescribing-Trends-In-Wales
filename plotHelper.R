#All the plot codes is mentioned in this script 


library(ggplot2)
library(ggExtra)
library(gghighlight)

#plot period summary
plotPeriodSummary <- function(periodSummary){
  plot <- plot(x=periodSummary$period, y=periodSummary$total_cost)
  
  return (plot)
}


#Plotting the  Dementia  Cost & Percent of Patients diagnosed
plotDementia <- function(demSpend){
  plot <- ggplot(demSpend, aes(x=Disorderpercent, y=totalcost,size = totalcost,color = Disorderpercent)) +
    geom_point()+scale_y_log10()+ggtitle("Disorderpercent vs Cost for Dementia")
  
  return (plot)
}
#Plotting the  Depression Cost & Percent of Patients diagnosed
plotDepression <- function(depressionSpend){
  plot <- ggplot(depressionSpend, aes(x=Disorderpercent, y=totalcost,size = totalcost,color = Disorderpercent)) +
    geom_point()+scale_y_log10()+ggtitle("Disorderpercent vs Cost for Depression")
  
  return(plot)
}
#Plotting the  Epilepsy Cost & Percent of Patients diagnosed
plotEpilepsy <- function(epilepsySpend){
  plot <- ggplot(epilepsySpend, aes(x=Disorderpercent, y=totalcost,size = totalcost,color = Disorderpercent)) +
    geom_point()+scale_y_log10()+ggtitle("Disorderpercent vs Cost for Epilepsy")
  
  return(plot)
}

#Plotting the  Mental Health Cost & Percent of Patients diagnosed
plotMentalHealth <- function(mhSpend){
  plot <- ggplot(mhSpend, aes(x=Disorderpercent, y=totalcost,size = totalcost,color = Disorderpercent)) +
    geom_point()+scale_y_log10()+ggtitle("Disorderpercent vs Cost for Mental Health")
  
  return(plot)
}

#Creating a distribution function to observe the data distribution across the diseases percent & overall Cost with the spend 
getDistribution <- function(value){
  result <- qqnorm(value)
  qqline(value)
  hist(value)
  
  return(result)
}
