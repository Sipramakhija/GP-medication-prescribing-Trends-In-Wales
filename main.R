#Part-2- Codes by Sipra Makhija
#Open_end_Analysis-Prescription Cost Analysis and association between the cost and disorder percent 
#This analysis shows the cost analysis related to the Central Nervous system disorders across the practice.
#The other scripts has codes which are already mentioned in this script, there is no requirement to run the codes differently
#Once the main File is run, all the packages will be installed & load independently.

#First Step is to install the Required packages
#install.packages("RPostgreSQL", "DBI","Getoplong","tidyverse",
                 #"remotes","devtools","dplyr","ggplot2","gghighlight")


#Load the source files 
#The main script depends on these sources, please load the whole repository while running the codes.
source("connectionHelper.R")
source("databaseHelper.R")
source("dataHelper.R")
source("plotHelper.R")


#Load the library 
library(tidyverse)
library(remotes)
library(devtools)
library(dplyr)



#Setting up driver for connecting to the SQL Database
drv = connectDriver("PostgreSQL");

#Establishing the connection with the PostgreSQL 
con <- connectDB(drv,"gp_practice_data", "localhost", 5432,"postgres")

#Confirming the established Connection with PostgreSQL
#Listing the structure of the table

print(listTableStructure(con))

#Section-1
#Analyzing the overall cost 
#Top 5 expensive Drugs Prescribed across the practice
expensiveDrug <- getExpensiveDrug(con)

topFiveExpensiveDrugs<- getTopFiveExpensiveDrugs(expensiveDrug) 

#Printing the list of top 5 expensive 
print(topFiveExpensiveDrugs)


#Quantity and total cost for the practice 
qtyCostPractice <- getQtyCostPractice(con)

#Filtering the least  prescribed drugs  
leastPrescribedDrug <- getLeastPrescribedDrug(qtyCostPractice)

#Print last 10 most prescribed drugs
print(leastPrescribedDrug)

#Period wise/year wise spend on the medication
periodSummary <- getPeriodSummary(con)
periodSummary$period = as.factor(periodSummary$period)
print(plotPeriodSummary(periodSummary))

#____________________________________________________________________________________________________________________________
#Section-2 Central Nervous system related diseases & Spend on the medication,
#The diseases are selected randomly not on based any criteria 
#Creating the table for Patients, List of Drugs and prescribed medicine for Dementia, Depression, Epilepsy & Mental Health

#1-Number of dementia Diagnosed person 
dementiaPatients <-getPercentofPatients(con,"Dementia","DEM001")

#Drugs Prescribed for Dementia 
demdrugs<-getDrugsPrescribedList(con,"Dementia")

#Spend in Dementia Drugs
demSpend<-getCostPerMedicine(con,demdrugs,dementiaPatients)

#Plotting the Dementia Patients Percent vs Cost for Dementia
print(plotDementia(demSpend))


#2-Depression Diagnosed person 
depressionPatients<- getPercentofPatients(con, "Depression","DEP003W")

#Drugs Prescribed for Drugs 
depressionDrugs<-getDrugsPrescribedList(con, "Antidepressant Drugs")

#Spend in Depression Drugs 
depressionSpend<-getCostPerMedicine(con, depressionDrugs,depressionPatients)

#Plotting the Depression Patients percent vs Cost of medicine for Depression
print(plotDepression(depressionSpend))


#3.Epilepsy Diagnosed person 
epilepsyPatients<- getPercentofPatients(con,"Epilepsy","EP001")

#Drugs Prescribed for Epilepsy
epilepsyDrugs<-getDrugsPrescribedList(con, "Antiepileptic Drugs")

#Spend in Epilepsy Drugs
epilepsySpend<- getCostPerMedicine(con, epilepsyDrugs,epilepsyPatients)

#plotting the Patients percent vs Cost for Epilepsy
print(plotEpilepsy(epilepsySpend))


#4.Mental Health diagnosed person
mhPatients<- getPercentofPatients(con ,"Mental Health","MH001")

#List of drugs prescribed for Mental Health
mhDrugs<- getDrugsPrescribedList(con ,"Drugs Used In Psychoses & Rel.Disorders")

#Spend on Epilepsy Drugs 
mhSpend<- getCostPerMedicine(con, mhDrugs,mhPatients)

#plotting the Disorder percent vs Cost for Mental Health
print(plotMentalHealth(mhSpend))

#_____________________________________________________________________________________________________________________________
#Section-3- Hypothesis formation & Testing 

#Hypothesis Testing for the Association whether there is an association between the patients percent and spend in the medication
#Assuming that 
#Null Hypothesis:No Association between the mental Health condition or diseases with the spend in the medication
#Alternate hypothesis: Association between the mental Health condition or diseases with the spend in the medication

#Disease wise distribution

#Dementia Distribution
dementiaDistribution <- getDistribution(demSpend$totalcost)

#Applying log for normal distribution
nDementiaDistribution <- log(demSpend$totalcost)

#Dementia Patients Distribution
dementiaPatientDistribution <- getDistribution(demSpend$Disorderpercent)

#Applying the log for normal distributed 
nDementiaPatientDistribution <- log(demSpend$Disorderpercent)

#Applying the two-way t-test to the generate the p.value
t.test(nDementiaPatientDistribution,nDementiaDistribution,alternative = "two.sided")
#p-value < 2.2e-16-Rejecting the null hypothesis

#Epilepsy Distribution
epilepsyDistribution <-getDistribution(epilepsySpend$totalcost)

#Applying log for normal distribution
nEpilepsyDistribution <- log(epilepsySpend$totalcost)

#Epilepsy Patients Distribution 
epilepsyPatientDistribution<- (epilepsySpend$Disorderpercent)

#applying log for normal distribution
nEpilepsyPatientDistribution <- log(epilepsySpend$Disorderpercent)

#applying the two-way t-test to observe whether the p.value is 0.5
t.test(nEpilepsyPatientDistribution,nEpilepsyDistribution,alternative = "two.sided")
#P value < 2.2e-16, rejecting the alternate hypothesis

#Mental health spend and total cost 
mentalHealthDistribution <- getDistribution(mhSpend$totalcost)


#applying log for normal distribution
nMentalHealthDistribution <-log(mhSpend$totalcost)

#Mental health Patients Distribution
mentalHealthPatientDistribution <- getDistribution(mhSpend$Disorderpercent)

#Applying log for normal distribution
nMentalHealthPatientDistribution <- log(mhSpend$Disorderpercent)

#applying the two-way t-test to observe whether the p.value is 0.5
t.test(nMentalHealthPatientDistribution,nMentalHealthDistribution,alternative = "two.sided")
#p-value < 2.2e-16 rejecting the Alternate Hypothesis

#Depression Distribution 
depressionDistribution <- getDistribution(depressionSpend$totalcost)

#Applying log for normal distribution
nDepressionPatientDistribution <- log(depressionSpend$Disorderpercent)
nonInf<- replace(nDepressionPatientDistribution, is.infinite(nDepressionPatientDistribution), 0)
print(nonInf)

#Depression Patients distribution
depressionPatientDistribution <- getDistribution(depressionSpend$Disorderpercent)

#Applying log for normal distribution
nDepressionPatientDistribution <- log(depressionSpend$Disorderpercent)


#applying the two-way t-test to observe whether the p.value is 0.5
t.test(nonInf,nDepressionDistribution,alternative = "two.sided")
#p-value < 2.2e-16 rejecting the Alternate Hypothesis


# ------------------------------------------------------------------------------------------------------
#End of Codes

#References
#1-R styles from Google 
#2-Stack overflow 
#3-PostgreSQL Tutorial 
#4-Class notes
