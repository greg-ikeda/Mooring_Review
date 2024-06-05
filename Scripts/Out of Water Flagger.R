#### Setup #### 
rm(list = ls())
graphics.off()

# Load packages
library(tidyverse)
library(lubridate)
library(here)

# Load Data and set up directories ---------------------------------------------------------------

save_folder <- paste0(here(), "/Output")

fpath <- choose.files(default = "C:\\Users\\gikeda\\OneDrive - King County\\Downloads\\test",
                      caption = "Select the King County Mooring File")
mooringdata <- read_tsv(fpath, comment = "*")

# Load data as UTC timestamp ---------------------------------------------------------
# Note: UnixTimeStamp calculation does not apply a correction factor when converting the Datetime column when in UTC
mooringdata_UTC <- mooringdata %>%
  mutate(Datetime = mdy_hms(Date, tz = "UTC"), # Specify timezone as UTC, to force the conversion to UnixTimestamp to be in UTC
         year = as.double(format(as.Date(Datetime, format="%Y-%m-%d"), format = "%Y")),
         month = as.factor(format(as.Date(Datetime, format="%Y-%m-%d"), format = "%m")),
         day = as.double(format(as.Date(Datetime, format="%Y-%m-%d"), format = "%d")),
         hour = as.double(format(as.POSIXct(Datetime, format="%H:%M:%S"), format = "%H")),
         monthday = as.factor(format(as.Date(Datetime, format="%Y-%m-%d"), format = "%m-%d"))) %>%
  select(Datetime, year, month, day, hour, everything()) 

qual_cols <-which(grepl(pattern = 'Qual_1', colnames(mooringdata)), arr.ind = TRUE) 
qual_cols <- colnames(mooringdata[qual_cols])

top_out_of_water_UTC <- mooringdata_UTC %>%
  mutate(UnixTimeStamp = as.numeric(Datetime)*1000, # multiply by 1000 to get the same # of digits as used in the database.
         Qual_1_Water_Temperature = 444,
         Qual_1_Salinity = 444,
         Qual_1_DO = 444,
         Qual_1_Chlorophyll_Fluorescence = 444,
         Qual_1_Turbidity = 444,
         Qual_1_Sonde_pH = 444,
         Qual_1_Water_Density = 444) %>%
  filter(`1_Depth_m` < 0) %>% # Filter for only samples where the top sonde depth is negative, indicating it exited the water
  select(UnixTimeStamp, Datetime, qual_cols)

write_csv(top_out_of_water_UTC,
          paste0(here("output", "SeaAq_Flags_UTC.csv")))


# Thomas' request for some data columns for testing -----------------------
# Same as above, but includes Depth, Salinity, Temperature, [DO], and DO %sat

test_out_of_water <- mooringdata_UTC %>%
  mutate(UnixTimeStamp = as.numeric(Datetime)*1000,
         Qual_1_Water_Temperature = 444,
         Qual_1_Salinity = 444,
         Qual_1_DO = 444,
         Qual_1_Chlorophyll_Fluorescence = 444,
         Qual_1_Turbidity = 444,
         Qual_1_Sonde_pH = 444,
         Qual_1_Water_Density = 444) %>%
  filter(`1_Depth_m` < 0) %>%
  select(UnixTimeStamp, Datetime, all_of(qual_cols),
         `1_Depth_m`, `1_Salinity_PSU`, `1_Water_Temperature_degC`, `1_Dissolved_Oxygen_mg/L`, `1_Dissolved_Oxygen_%Sat`)

write_csv(test_out_of_water,
          paste0(here("output", "SeaAq_Flags_UTC_with_params.csv")))
# 
# # Out of water df PST ---------------------------------------------------------
# mooringdata_PST <- mooringdata %>%
#   mutate(Datetime = mdy_hms(Date, tz = "Etc/GMT+8"),
#          year = as.double(format(as.Date(Datetime, format="%Y-%m-%d"), format = "%Y")),
#          month = as.factor(format(as.Date(Datetime, format="%Y-%m-%d"), format = "%m")),
#          day = as.double(format(as.Date(Datetime, format="%Y-%m-%d"), format = "%d")),
#          hour = as.double(format(as.POSIXct(Datetime, format="%H:%M:%S"), format = "%H")),
#          monthday = as.factor(format(as.Date(Datetime, format="%Y-%m-%d"), format = "%m-%d"))) %>%
#   select(Datetime, year, month, day, hour, everything()) 
# 
# qual_cols <-which(grepl(pattern = 'Qual_1', colnames(mooringdata)), arr.ind = TRUE) 
# qual_cols <- colnames(mooringdata[qual_cols])
# 
# top_out_of_water_PST_df <- mooringdata_PST %>%
#   mutate(UnixTimeStamp = as.numeric(Datetime)*1000,
#          Qual_1_Water_Temperature = 444,
#          Qual_1_Salinity = 444,
#          Qual_1_DO = 444,
#          Qual_1_Chlorophyll_Fluorescence = 444,
#          Qual_1_Turbidity = 444,
#          Qual_1_Sonde_pH = 444,
#          Qual_1_Water_Density = 444) %>%
#   filter(`1_Depth_m` < 0) %>%
#   select(UnixTimeStamp, `1_Depth_m`, Datetime, qual_cols)
# 
# top_out_of_water_PST <- top_out_of_water_PST_df %>%
#   select(UnixTimeStamp, qual_cols)
# 
# write_csv(top_out_of_water_PST,
#           paste0(here("output", "SeaAq_Flags_PST.csv")))
# 
# 
# 
# 
# # Thomas' Data ------------------------------------------------------------
# 
# library(readxl)
# DB_Query_for_Greg_Jan3_2022 <- read_excel("C:/Users/gikeda/OneDrive - King County/Downloads/DB_Query_for_Greg_Jan3_2022.xlsx")
# View(DB_Query_for_Greg_Jan3_2022)
# 
