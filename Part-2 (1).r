#Part-2
#Open_end_Analysis-Prescription Cost Analysis and association between the cost and disorder percent 
#This analysis shows the cost analysis related to the Central Nervous system disorders across the practice.

library(RPostgreSQL)
library(DBI)
library(GetoptLong)
library(tidyverse)
library(remotes)
library(devtools)
library(dplyr)
library(pacman)

#library(dplyr)
library(bigrquery)



drv = dbDriver("PostgreSQL");
con <- dbConnect(
  #drv,              #From the RPostgreSQL library.  
  RPostgres::Postgres(),  # From the RPostgres/DBI library.
  dbname = "gp_practice_data",
  host = "localhost", port = 5432,
  user = "postgres", password = rstudioapi::askForPassword())

#Confirming the established Connection with PostgreSQL
print(dbListTables(con))

#Listing the structure of the table
dbGetQuery(con, "
           select column_name, ordinal_position,data_type,character_maximum_length,numeric_precision from INFORMATION_SCHEMA.COLUMNS
           where table_schema = 'public'and table_name = 'address';")


#Top 5 expensive Drugs 
GP_Data<- dbGetQuery(con, "select practiceid, bnfname,sum(quantity) as quantity, actcost * quantity as totalcost
                      from gp_data_up_to_2015  group by practiceid,bnfname,totalcost")

ExpensiveDrugs<- GP_Data %>% 
  filter(practiceid == practiceid)%>%
  arrange(desc(totalcost))%>%head(5)%>%
  select(practiceid,bnfname,totalcost)    
ExpensiveDrugs



#Quatity and total cost for the practice is
gp <- dbGetQuery(con, "select practiceid,bnfname,sum(quantity) as quantity, actcost * quantity as totalcost
                      from gp_data_up_to_2015  group by practiceid,bnfname,totalcost")

#filtering least the prescribed drugs  
leastprescribed <- gp %>% filter(practiceid == practiceid)%>%arrange(desc(quantity))%>%tail(10)%>%
  select(practiceid,bnfname,quantity)

#print last 10 most prescribed drugs
leastprescribed

# printing the cost last 10 drugs
Last10cost <- gp %>% filter(practiceid == practiceid)%>%arrange(desc(totalcost))%>%
  tail(10)%>%select(practiceid,bnfname,totalcost)    
Last10cost

period_summary <- dbGetQuery(con, "select period,sum(items) as total_items,sum(actcost) as total_cost 
                              from gp_data_up_to_2015 group by period")

period_summary$period = as.factor(period_summary$period)

plot(x=period_summary$period, y=period_summary$total_cost)


# creating function to calculate percent of patients,list of drugs prescribed, & Cost of the medicine spent 

#Function to calculate percent of patients
Patients<- function(a,b){
        t<- dbGetQuery(con,"select * from qof_indicator c inner join qof_achievement t on c.indicator = t.indicator")%>%  
                filter(str_detect(area, a), indicator == b) %>% group_by(orgcode)%>%
                mutate(Disorderpercent = ratio * 100)%>%select(orgcode,Disorderpercent)
  
  return(t)

}


#List of drugs prescribed
Medicine<- function(t){
  d<-dbGetQuery(con,"select * from bnf")%>%
    filter(str_detect(sectiondesc,t))
    return(d)

}


#Cost of the Per medicine (change the total cost to cost/allcost)
Cost<-function(a,b){
  z<-dbGetQuery(con,"select * from gp_data_up_to_2015")%>%separate(bnfcode,into = c('bnf1','bnf2'),sep=9)%>%
    inner_join(a,by= c('bnf1' ='bnfchemical'))%>%group_by(practiceid)%>%
    mutate(totalcost = quantity *actcost)%>%group_by(practiceid)%>%
    summarise(practiceid,totalcost = sum(totalcost))%>%filter(row_number()==1)%>%
    inner_join(b,by=c('practiceid'='orgcode'))
  return(z)
  
}

#################################################################################
#################################################################################
###################################################################################
#Using the function to create tables for Central Nervous Sytsem Related Diseases, 
#Only the 4 diseases has been considered for the analysis 

#1-Dementia
Dementia_Patients <-Patients("Dementia","DEM001")

Dem_Drugs<-Medicine("Dementia")

Dem_spend<-Cost(Dem_Drugs,Dementia_Patients)

ggplot(Dem_spend, aes(x=Disorderpercent, y=totalcost,size = totalcost,color = Disorderpercent)) +
  geom_point()+scale_y_log10()+ggtitle("Disorderpercent vs Cost for Dementia")


#2-Depression

Depression_Patients<- Patients("Depression","DEP003W")

Depression_Drugs<-Medicine("Antidepressant Drugs")

Depression_Spend<-Cost(Depression_Drugs,Depression_Patients)

#Plotting
ggplot(Depression_Spend, aes(x=Disorderpercent, y=totalcost,size = totalcost,color = Disorderpercent)) +
  geom_point()+scale_y_log10()+ggtitle("Disorderpercent vs Cost for Depression")



#3.Epilepsy:

Epilepsy_Patients<- Patients("Epilepsy","EP001")

Epilepsy_Drugs<-Medicine("Antiepileptic Drugs")

Epilepsy_Spend<- Cost(Epilepsy_Drugs,Epilepsy_Patients)

ggplot(Epilepsy_Spend, aes(x=Disorderpercent, y=totalcost,size = totalcost,color = Disorderpercent)) +
  geom_point()+scale_y_log10()+ggtitle("Disorderpercent vs Cost for Epilepsy")


#4.Mental Health:

MH_Patients<- Patients("Mental Health","MH001")

MH_Drugs<- Medicine("Drugs Used In Psychoses & Rel.Disorders")

MH_Spend<- Cost(MH_Drugs,MH_Patients)

#plotting the Disorder percent vs Cost for Mental Health

ggplot(MH_Spend, aes(x=Disorderpercent, y=totalcost,size = totalcost,color = Disorderpercent)) +
  geom_point()+scale_y_log10() + ggtitle("Disorderpercent vs Cost for Mental Health")

#Hypothesis Testing for the Association whether there is an association between the patients percent and spend in the medication
#Assuming that 
#Null Hypothesis:No Association between the mental Health condition or diseases with the spend in the medication
#Alternate hypothesis: Association between the mental Health condition or diseases with the spend in the medication

#Creating a distribution function to observe the data distribution across the diseases percent & overall Cost with the spend 
#
distribution<-function(x){
  y<-qqnorm(x)
  qqline(x)
  hist(x)
  return(y)
 
} 
#Dementia Distribution
Dem_dis<- distribution(Dem_spend$totalcost)

#applying log for normal distribution
N_Dem_dis<-log(Dem_spend$totalcost)

#Dementia Patients Distribution
Dem_Patients_Percent_Dis< distribution(Dem_spend$Disorderpercent)

#As the data is not normal distributed, applying log
N_Dem_Patients_Percent_Dis<- log(Dem_spend$Disorderpercent)

#applying the two-way t-test to the see the p.value
t.test(N_Dem_Patients_Percent_Dis,N_Dem_dis,alternative = "two.sided")

#Depression Distribution 
Dep_dis<- distribution(Depression_Spend$totalcost)

#applying log for normal distribution
N_Dep_dis<-log(Depression_Spend$totalcost)

#Depression Patients distribution
Dep_Patients_Percent<- distribution(Depression_Spend$Disorderpercent)

#Applying log for normal distribution
N_Dep_Patients_Percent<-log(Depression_Spend$Disorderpercent)

#applying the two-way t-test to observe whether the p.value is 0.5
t.test(N_Dep_Patients_Percent,N_Dep_dis,alternative = "two.sided")

wilcox.test(N_Dep_Patients_Percent,N_Dep_dis)


#Epilepsy Distribution
Epi_Dis<-distribution(Epilepsy_Spend$totalcost)

#applying log for normal distribution
N_Epi_dis<- log(Epilepsy_Spend$totalcost)

#Epilepsy Patients Distribution 
Epi_Patients_Percent<- (Epilepsy_Spend$Disorderpercent)

#applying log for normal distribution
N_Epi_Patients_Percent<- log(Epilepsy_Spend$Disorderpercent)

t.test(N_Epi_Patients_Percent,N_Epi_dis,alternative = "two.sided")
#P value < 2.2e-16, rejecting the null hypothesis

#Mental health spend and total cost 
MH_Dis<-distribution(MH_Spend$totalcost)

#applying log for normal distribution
N_MH_Dis<-log(MH_Spend$totalcost)

#Mental health Patients Distribution
MH_Patients_Dis<-distribution(MH_Spend$Disorderpercent)

#applying log for normal distribution
N_MH_Patients_Dis<-log(MH_Spend$Disorderpercent)

t.test(N_MH_Patients_Dis,N_MH_Dis,alternative = "two.sided")
#p-value < 2.2e-16-Rejecting the null hypothesis 

-------------------------------------------------------------
-------------------------------------------------------------
  

