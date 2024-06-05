# install.packages("RODBC")
# install.packages("dygraphs")
# install.packages("xts")
# install.packages("odbc")
library(xts)
library(odbc)
library(DBI)
library(tidyverse)
library(dygraphs)


# Connect to the database
moorings_db <- dbConnect(odbc(), 
                 Driver = "SQL Server", 
                 Server = "KCITSQLPRNRPX01", 
                 Database = "MarineBuoy", 
                 Trusted_Connection = "True")

# Lists the tables in the MarineBuoy database
dbListTables(moorings_db)

# Grabs the tables with ranges and identifying information
siterange <- dbReadTable(moorings_db, "SiteRange")
sitenums <- dbReadTable(moorings_db, "Site")
sensorrange <- dbReadTable(moorings_db, "SensorRange")

# Writes these tables to csv files 
write_csv(siterange,
          "Site_Range.csv")
write_csv(sitenums,
          "Site_ID.csv")
write_csv(sensorrange,
          "Sensor_Range.csv")



# Testing - grabbing data from Dockton ------------------------------------

dbSendQuery(moorings_db,
            "SELECT * FROM Dockton")

?dbSendQuery