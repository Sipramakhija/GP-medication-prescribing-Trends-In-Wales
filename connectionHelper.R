#This script allows the connection of the SQL to R for accessing the database 
#Install the required packages 


#Load the Library 
library(RPostgreSQL)
library(RPostgres)
library(DBI)

# Load database driver (driver)
connectDriver <- function(driver){
  drv <- dbDriver(driver)
  return(drv)
}

# Connect to database
connectDB <- function(driver, dbName, host, port, user){
  cat("Attempting a new connection to the database: ", dbName, "...\n ")
  # Connect to PostgreSQL database
  con <- dbConnect( 
    # driver,
    RPostgres::Postgres(),
                   host = host, port = port,
                   password = rstudioapi::askForPassword(),
                   user = user,
                   dbname = dbName )
  
  #Confirming the established Connection with PostgreSQL
  if (dbIsValid(con) == TRUE) {
    cat("Connection to ", dbName, " was Successful.")
  }
  return(con)
}

# disconnect database
disconnect_Database <- function(connection, driver) {
  dbDisconnect(connection)
  dbListConnections(driver)
  dbUnloadDriver(driver)
}

