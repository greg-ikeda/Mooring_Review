# To do:
# Add in discrete data if available. Use the download_discrete and import_discrete functions from kcmarine. Might not be viable for the 10-day plots
# Add a way to easily overlay vertical lines for cleaning dates. 
# Use R markdown to create a dashboard with mooring review specs
# Create a Shiny app that shows three interactive plots vertically stacked for data comparison (e.g. DO + Chl + S)
  # Selection tools allow you to pick which plots appear in this   
# Include transparent overlay of past year's data
# Include transparent boxes that denote each 24 hour period
# Automated HTTP requests that pull data from the mooring site without the need to download. 

#### Setup #### 
rm(list = ls())
graphics.off()

# Load packages
library(tidyverse)
library(lubridate)
library(kcmarine)
library(gghighlight)
library(patchwork)
library(weathermetrics)
library(plotly) #Enables interactive plots
library(htmlwidgets)
library(stringr)
library(beepr)

# Where to save QC output?
# save_folder <- choose.dir(caption = "Select the folder to save stuff")
save_folder <- r"(C:\Users\gikeda\OneDrive - King County\Documents\January 2023\Mooring IT Fix\Dockton Data)"
setwd(save_folder)


# Dockton ------------------------------------------------------------
logger_file_dockton <- choose.files(caption = "Select Logger file", )
kingcounty_file_dockton <- choose.files(caption = "Select King County file")
hydrosphere_file_dockton <- choose.files(caption = "Select Hydrosphere file")
hydrosphere_dockton <- read_csv(hydrosphere_file_dockton, comment = "*")
kingcounty_dockton <- read_tsv(kingcounty_file_dockton, comment = "*")
logger_dockton <- read_csv(logger_file_dockton, comment = "*", skip = 1)

start_date <- "2022-09-01 00:00:00 PDT"
end_date <- "2022-09-20 00:00:00 PDT"

zzhydrosphere_clean_dockton <- hydrosphere_dockton %>%
  mutate(Datetime_string = paste(Date, Time),   
         Datetime = ymd_hms(Datetime_string, tz = "America/Los_Angeles"),
         # Datetime = ymd_hms(Datetime_string),
         UnixDatetime = as.numeric(Datetime),
         temp = signif(`HCEP(TEMP)`, digits = 3),
         sal = signif(`HCEP(SAL)`, digits = 3),
         oxy = signif(`HCEP(OXY)`, digits = 3),
         spar = signif(SP_110, digits = 3)) %>%
  select(Datetime, temp, sal, oxy, UnixDatetime, everything()) %>%
  filter(Datetime >= as.POSIXct(start_date),
         Datetime <= as.POSIXct(end_date))

zzkingcounty_clean_dockton <- kingcounty_dockton %>%
  mutate(Datetime = mdy_hms(`Date`, tz = "America/Los_Angeles"),
  # mutate(Datetime = mdy_hms(`Date`),
         UnixDatetime = as.numeric(Datetime),
         temp = signif(`Water_Temperature_degC`, digits = 3),
         sal = signif(`Salinity_PSU`, digits = 3),
         oxy = signif(`Dissolved_Oxygen_mg/L`, digits = 3),
         spar = signif(`Surf_PAR_umol/s/sqm`, digits = 3)) %>%
  select(Datetime, temp, sal, oxy, spar, UnixDatetime, everything()) %>%
  filter(Datetime >= as.POSIXct(start_date),
         Datetime <= as.POSIXct(end_date)) 

zzlogger_clean_dockton <- logger_dockton %>%
  mutate(date_time = paste(logger_dockton$Date, logger_dockton$Time),
         Datetime = as.POSIXct(date_time, format = "%m/%d/%Y %H:%M:%S",  tz = "America/Los_Angeles"),
         # Datetime = as.POSIXct(date_time, format = "%m/%d/%Y %H:%M:%S"),
         UnixDatetime = as.numeric(Datetime),
         temp = signif(logger_dockton$`HCEP(TEMP)`, digits = 3),
         sal = signif(logger_dockton$`HCEP(SAL)`, digits = 3),
         oxy = signif(logger_dockton$`HCEP(OXY)`, digits = 3),
         spar = signif(logger_dockton$`SP-110`, digits = 3)) %>%
  select(Datetime, temp, sal, oxy, spar, UnixDatetime, everything()) %>%
  filter(Datetime >= as.POSIXct(start_date),
         Datetime <= as.POSIXct(end_date))

# Seattle Aquarium --------------------------------------------------------
# kingcounty_file_sea_aq <- choose.files(caption = "Select King County file")
# hydrosphere_file_sea_aq <- choose.files(caption = "Select Hydrosphere file")
# hydrosphere_sea_aq <- read_csv(hydrosphere_file_sea_aq, comment = "*")
# kingcounty_sea_aq <- read_tsv(kingcounty_file_sea_aq, comment = "*")
# 
# zzhydrosphere_clean_seaAQ <- hydrosphere_sea_aq %>%
#   mutate(Datetime_string = paste(Date, Time),   
#          Datetime = ymd_hms(Datetime_string, tz = "America/Los_Angeles"),
#          UnixDatetime = as.numeric(Datetime),
#          temp = signif(`HCEP1(TEMP)`, digits = 3),
#          sal = signif(`HCEP1(SAL)`, digits = 3),
#          oxy = signif(`HCEP1(OXY)`, digits = 3)) %>%
#   select(Datetime, temp, sal, oxy, UnixDatetime, everything()) %>%
#   filter(Datetime >= as.POSIXct("2022-09-01 00:00:00 PDT"),
#          Datetime <= as.POSIXct("2022-10-26 00:00:00 PDT"))
# 
# zzkingcounty_clean_seaAQ <- kingcounty_sea_aq %>%
#   mutate(Datetime = mdy_hms(`Date`, tz = "America/Los_Angeles"),
#          UnixDatetime = as.numeric(Datetime),
#          temp = signif(`1_Water_Temperature_degC`, digits = 3),
#          sal = signif(`1_Salinity_PSU`, digits = 3),
#          oxy = signif(`1_Dissolved_Oxygen_mg/L`, digits = 3)) %>%
#   select(Datetime, temp, sal, oxy, UnixDatetime, everything()) %>%
#   filter(Datetime >= as.POSIXct("2022-09-01 00:00:00 PDT"),
#          Datetime <= as.POSIXct("2022-10-26 00:00:00 PDT")) 



# # Point Williams ----------------------------------------------------------
# kingcounty_file_pw <- choose.files(caption = "Select King County file POINT WILLIAMS")
# hydrosphere_file_pw <- choose.files(caption = "Select Hydrosphere file POINT WILLIAMS")
# hydrosphere_pw <- read_csv(hydrosphere_file_pw, comment = "*")
# kingcounty_pw <- read_tsv(kingcounty_file_pw, comment = "*")
# 
# zzhydrosphere_clean_pw <- hydrosphere_pw %>%
#   mutate(Datetime_string = paste(Date, Time),   
#          Datetime = ymd_hms(Datetime_string, tz = "America/Los_Angeles"),
#          UnixDatetime = as.numeric(Datetime),
#          temp = signif(`HCEP(TEMP)`, digits = 3),
#          sal = signif(`HCEP(SAL)`, digits = 3),
#          oxy = signif(`HCEP(OXY)`, digits = 3),
#          spar = signif(`PAR_SQ-215-SS`, digits = 3)) %>%
#   select(Datetime, temp, sal, oxy, spar, UnixDatetime, everything()) %>%
#   filter(Datetime >= as.POSIXct("2022-09-01 00:00:00 PDT"),
#          Datetime <= as.POSIXct("2022-09-26 00:00:00 PDT"))
# 
# zzkingcounty_clean_pw <- kingcounty_pw %>%
#   mutate(Datetime = mdy_hms(`Date`, tz = "America/Los_Angeles"),
#          UnixDatetime = as.numeric(Datetime),
#          temp = signif(`Water_Temperature_degC`, digits = 3),
#          sal = signif(`Salinity_PSU`, digits = 3),
#          oxy = signif(`Dissolved_Oxygen_mg/L`, digits = 3)) %>%
#   select(Datetime, temp, sal, oxy, UnixDatetime, everything()) %>%
#   filter(Datetime >= as.POSIXct("2022-09-01 00:00:00 PDT"),
#          Datetime <= as.POSIXct("2022-09-26 00:00:00 PDT")) 

# Plots -------------------------------------------------------------------

#Dockton Plot
dockton_plot <- ggplot(zzkingcounty_clean_dockton)+
  geom_point(aes(x = Datetime,
                 y = spar))+
  geom_point(data = zzhydrosphere_clean_dockton,
             aes(x = Datetime,
                 y = spar,
                 alpha = 2),
             color = "red")+
  geom_point(data = zzlogger_clean_dockton,
             aes(x = Datetime,
                 y = spar,
                 alpha = 2),
             color = "blue")+
  # geom_line(aes(x = Datetime,
  #               y = spar),
  #           alpha = 0.2)+
  # geom_line(data = zzhydrosphere_clean_dockton,
  #           aes(x = Datetime,
  #               y = spar,
  #               alpha = 0.2),
  #           color = "red")+
  # geom_line(data = zzlogger_clean_dockton,
  #           aes(x = Datetime,
  #               y = spar,
  #               alpha = 0.2),
  #           color = "blue")+
  ggtitle("Dockton (Logger = Blue, Hydrosphere = Red, KC Website = Black)")
ggplotly(dockton_plot)

#PW Plot
PW_plot <- ggplot(zzkingcounty_clean_pw)+
  geom_point(aes(x = Datetime,
                y = temp))+
  geom_point(data = zzhydrosphere_clean_pw,
             aes(x = Datetime,
                 y = temp,
                 alpha = 2),
             color = "red")
  # ggtitle("Dockton (Logger = Blue, Hydrosphere = Red, KC Website = Black)")
ggplotly(PW_plot)

htmlwidgets::saveWidget(ggplotly(dockton_plot), title = "Dockton Data", file = "Dockton_comparison.html")

#Seattle Aquarium Plot
sea_aq_plot <- ggplot()+
  geom_point(data = zzkingcounty_clean_seaAQ,
             aes(x = Datetime,
                y = temp),
             color = "blue")+
  geom_point(data = zzhydrosphere_clean_seaAQ,
             aes(x = Datetime,
                 y = temp,
                 alpha = 2),
             color = "red")+
  ylim(0,25)
ggplotly(sea_aq_plot)


seattle_aquarium <- ggplot(data = zzhydrosphere_clean_seaAQ)+
  geom_point(aes(x = Datetime,
                 y = temp))
ggplotly(seattle_aquarium)
htmlwidgets::saveWidget(ggplotly(seattle_aquarium), title = paste("Seattle Aquarium Temperature"),  "Seattle_Aquarium.html")



#Dockton and SeaAQ Hydrosphere SPAR
hs_both <- ggplot()+
  geom_point(data = zzhydrosphere_clean_pw,
             aes(x = Datetime,
                 y = spar),
             color = "red")+
  geom_line(data = zzhydrosphere_clean_pw,
             aes(x = Datetime,
                 y = spar),
             color = "red",
            alpha = 0.3)+
  geom_point(data = zzhydrosphere_clean_dockton,
             aes(x = Datetime,
                 y = spar),
             color = "blue")+
  geom_line(data = zzhydrosphere_clean_dockton,
             aes(x = Datetime,
                 y = spar),
             color = "blue",
            alpha = 0.3)
ggplotly(hs_both)

#Save HTML Widgets
htmlwidgets::saveWidget(ggplotly(g), title = paste("Dockton Temperature"),  "Dockton.html")




# Slicing PAR Data in dataframes ------------------------------------------
# Dockton
dockton_PAR_max <- zzlogger_clean_dockton %>%
  group_by(Date) %>%
  summarize(max_SPAR = max(spar))
dockton_par_max <- left_join(zzlogger_clean_dockton, dockton_PAR_max) %>%
  filter(spar == max_SPAR) %>%
  select(Datetime, spar)

# Point Williams
pointwilliams_PAR_max <- zzhydrosphere_clean_pw %>%
  group_by(Date) %>%
  summarize(max_SPAR = max(spar))
pw_spar_max <- left_join(zzhydrosphere_clean_pw, pointwilliams_PAR_max) %>%
  filter(spar == max_SPAR) %>%
  group_by(Datetime) %>%
  summarize(max_SPAR = mean(max_SPAR)) %>%
  select(Datetime, spar)
pw_spar_max <- pw_spar_max %>%
  mutate(station = "Point Williams") 


