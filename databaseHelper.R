library(GetoptLong)
library(dplyr)


#List out the table structure
listTableStructure <- function(con){
  structure <- dbGetQuery(con, "
           select column_name, ordinal_position,data_type,character_maximum_length,numeric_precision from INFORMATION_SCHEMA.COLUMNS
           where table_schema = 'public'and table_name = 'address';")
  return(structure)
}

#List of Expensive drugs 
getExpensiveDrug <- function(con){
  result <- dbGetQuery(con, "select practiceid, bnfname,sum(quantity) as quantity, actcost * quantity as totalcost
                      from gp_data_up_to_2015  group by practiceid,bnfname,totalcost")
    
    return(result)
}

#Get quantity and cost of practice
getQtyCostPractice <- function(con){
  result <- dbGetQuery(con, "select practiceid,bnfname,sum(quantity) as quantity, actcost * quantity as totalcost
                        from gp_data_up_to_2015  group by practiceid,bnfname,totalcost")
  
  return(result)
}

# Get period summary
getPeriodSummary <- function(con){
  result <- dbGetQuery(con, "select period,sum(items) as total_items,sum(actcost) as total_cost 
                              from gp_data_up_to_2015 group by period")
  
  return(result)
}

#Creating function to calculate percent of patients,list of drugs prescribed, & Cost of the medicine spent 

# Get percent of patients
getPercentofPatients <- function(con, a,b){
  result <- dbGetQuery(con,"select * from qof_indicator c inner join qof_achievement t on c.indicator = t.indicator")%>%  
    filter(str_detect(area, a), indicator == b) %>% group_by(orgcode)%>%
    mutate(Disorderpercent = ratio * 100)%>%select(orgcode,Disorderpercent)
  
  return(result)
  
}

# Get list of prescribed drugs
getDrugsPrescribedList <- function(con, t){
  result <- dbGetQuery(con,"select * from bnf")%>%
    filter(str_detect(sectiondesc,t))
  return(result)
  
}

#Get Cost medicine (change the total cost to cost/all cost)
getCostPerMedicine <- function(con, a,b){
  result <-dbGetQuery(con,"select * from gp_data_up_to_2015")%>%separate(bnfcode,into = c('bnf1','bnf2'),sep=9)%>%
    inner_join(a,by= c('bnf1' ='bnfchemical'))%>%group_by(practiceid)%>%
    mutate(totalcost = quantity *actcost)%>%group_by(practiceid)%>%
    summarise(practiceid,totalcost = sum(totalcost))%>%filter(row_number()==1)%>%
    inner_join(b,by=c('practiceid'='orgcode'))
  return(result )
  
}
