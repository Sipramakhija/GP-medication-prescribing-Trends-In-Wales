
install.packages("strings")


library("stringr")


# top 5 expensive drugs
getTopFiveExpensiveDrugs <- function(data){
  result <- data %>% 
    filter(practiceid == practiceid)%>%
    arrange(desc(totalcost))%>%head(5)%>%
    select(practiceid,bnfname,totalcost)
    
    return(result)
}

#filtering least the prescribed drugs  
getLeastPrescribedDrug <- function(qtyCostPractice){
  result <- qtyCostPractice %>% filter(practiceid == practiceid)%>%arrange(desc(quantity))%>%tail(10)%>%
    select(practiceid,bnfname,quantity)
  
  return(result)
  
}

# get last 10 prescribed dug cost
getLastTenPrescribedDrugCost <- function(qtyCostPractice){
  result <- qtyCostPractice %>% filter(practiceid == practiceid)%>%arrange(desc(totalcost))%>%
    tail(10)%>%select(practiceid,bnfname,totalcost)   
    
    return(result)
}

