#### Setup and import data #### 
rm(list = ls())
graphics.off()

# Where to save QC output?
# save_folder <- choose.dir(caption = "Select the folder to save stuff")
save_folder <- "C:/Users/gikeda/R"
setwd(save_folder)

# Load packages
library(tidyverse)
library(lubridate)
library(kcmarine)
library(gghighlight)
library(patchwork)
library(weathermetrics)
library(plotly) #Enables interactive plots
library(htmlwidgets)

# Load in mooring data and format datetime. Extract day from the data
fpath <- choose.files(caption = "Select file")

test <- read_tsv(fpath)




mooringdata <- read_tsv(fpath, skip = 49)
mooringdata <- mooringdata %>%
  mutate(Datetime = mdy_hms(Date),
         day = as.double(format(as.Date(Datetime,format="%Y-%m-%d"), format = "%d")),
         hour = as.double(format(as.POSIXct(Datetime,format="%H:%M:%S"), format = "%H"),
                          ) %>% #Extracts the day from the datetime format 

  select(Datetime, everything())

startdate <- "10/1/2019 12:00:00 AM"
enddate <-"11/29/2019 12:00:00 PM"
# cleaning_date_1 <- "5/3/2022 12:00:00 AM"
# cleaning_date_2 <- "2/23/2022 2:20:00 PM"
# cleaning_date_3 <- "2/9/2022 10:30:00 AM"
# cleaning_date_4 <- "10/28/2021 12:00:00 AM"
# cleaning_date_5 <- "6/22/2022 9:00:00 AM"



p <- ggplot(data = mooringdata)+ 
  geom_point(mapping = aes(x = Datetime, y = Surface_PAR), size = 0.5)+
  # xlim(mdy_hms(c(startdate, enddate)))+
  ylim(-100, 1000)+
  xlab("")

ggplotly(p)

saveWidget(ggplotly(p), file = "Dockton_SPAR.html")

ggsave(paste0(save_folder, "2008-2019_Surface_PAR"), 
       dpi = 300, height = 6, width = 10)

p1 <- p + geom_vline(xintercept = as.numeric(mdy_hms(c(cleaning_date_1, 
                                                       cleaning_date_2,
                                                       cleaning_date_3,
                                                       cleaning_date_4, 
                                                       cleaning_date_5))))




ggsave(plot = p, paste0(save_folder, "2008-2019_Surface_PAR"), 
       dpi = 300, height = 6, width = 10)



wind_speed <- ggplot(data = mooringdata)+ 
  geom_point(mapping = aes(x = Datetime, y = `Wind_Speed_m/sec`))+
  xlim(mdy_hms(c("6/21/2022 12:00:00 AM","6/23/2022 12:00:00 PM")))+
  xlab("")

salinity <- ggplot(data = mooringdata)+ 
  geom_point(mapping = aes(x = Datetime, y = Salinity_PSU))+
  xlim(mdy_hms(c("6/21/2022 12:00:00 AM","6/23/2022 12:00:00 PM")))+
  xlab("")

DO <- ggplot(data = mooringdata)+ 
  geom_point(mapping = aes(x = Datetime, y = `Dissolved_Oxygen_mg/L` ))+
  xlim(mdy_hms(c("6/21/2022 12:00:00 AM","6/23/2022 12:00:00 PM")))+
  xlab("")

hcep_ph <- ggplot(data = mooringdata)+ 
  geom_point(mapping = aes(x = Datetime, y = Sonde_pH))+
  xlim(mdy_hms(c("6/21/2022 12:00:00 AM","6/23/2022 12:00:00 PM")))+
  xlab("")

ggplot(data = mooringdata)+
  geom_point(mapping = aes(x = Datetime, y = SeaFET_External_pH_final_recalc))+
  xlab("")
external_sal <- ggplot(data = mooringdata)+ 
  geom_point(mapping = aes(x = Datetime, y = SeaFET_External_pH_recalc_w_salinity))+
  xlab("")
# internal_raw <- ggplot(data = mooringdata)+ 
#   geom_point(mapping = aes(x = Datetime, y = SeaFET_Internal_pH_raw))+
#   xlab("")
internal_recalc <- ggplot(data = mooringdata)+ 
  geom_point(mapping = aes(x = Datetime, y = SeaFET_Internal_pH_recalculated))+
  xlab("")
seafet_temp <- ggplot(data = mooringdata)+ 
  geom_point(mapping = aes(x = Datetime, y = SeaFET_Temperature_degC))+
  xlab("")
gridExtra::grid.arrange(external_sal, internal_raw, seafet_temp, nrow=3)









ggplotly(turb)
gridExtra::grid.arrange(turb, wind_speed, nrow=2)
gridExtra::grid.arrange(salinity, DO, hcep_ph, nrow=3)


#----------------------------------------------------------------------------------------------
# 
# # 
# # #Bin Average based on depth
# depth <- mooringdata$'Depth (m)'
# hour <- mooringdata$hour
# mooringdata_binavg <- mooringdata %>%
#   mutate(bins = cut(depth, 50, labels = c("surface", 2:50))) %>% #adds a label to only the  surface bin. Simply labels the rest as "2 - 50".
#   mutate(timeofday = cut(hour, 2, labels = c('morning', 'afternoon'))) %>% #Bin average based on time of day
#   select(bins, timeofday, everything()) 
# # 
# 
# 
# Mooring_Data <- mooringdata %>%
#   mutate(DateTime = as.POSIXct(paste(Date, Time), 
#                                 format = "%Y-%m-%d %:%M:%S",
#                                 tz = "America/Los_Angeles"),
#          ph_delta = `HCEP(PH)` - `SeaFET(PH_EXT)`) %>%
#   select(DateTime, everything()) %>%
#   arrange(DateTime)
#  
# 
# 
# ggplot(data = Mooring_Data)+ 
#   theme_bw() + 
#   geom_line(aes(x = DateTime, y = `SeaFET(INT_VOLT)`), color = 'blue')
# 
# ggplot(data = Mooring_Data)+ 
#   theme_bw() + 
#   geom_line(aes(x = DateTime, y = `HCEP(PRES)`), color = 'blue')
# 
# ggplot(data = Mooring_Data)+ 
#   theme_bw() + 
#   geom_line(aes(x = DateTime, y = `HCEP(FL)`), color = 'blue')+
#   ylim(-1,45)
# 
# 
# 
# ggplot(data = Mooring_Data)+ 
#   theme_bw() + 
#   geom_line(aes(x = DateTime, y = `SeaFET(TEMP)`), color = 'blue')
# 
# 
# 
# ggplot(data = Mooring_Data)+ 
#   theme_bw() + 
#   geom_line(aes(x = DateTime, y = `SeaFET(INT_HUMID)`), color = 'blue')+
#   geom_smooth(aes(x = DateTime, y = `SeaFET(INT_HUMID)`), color = 'blue')
# 
# 
# HCp1 <- ggplot(data = Mooring_Data)+ 
#   theme_bw() + 
#   geom_line(aes(x = DateTime, y = `HCEP(PRES)`), color = 'blue')
# HCp2 <- ggplot(data = Mooring_Data)+ 
#   theme_bw() + 
#   geom_line(aes(x = DateTime, y = `HCEP(TEMP)`), color = 'blue')
# HCp3 <- ggplot(data = Mooring_Data)+ 
#   theme_bw() + 
#   geom_line(aes(x = DateTime, y = `HCEP(COND)`), color = 'red')
# HCp4 <- ggplot(data = Mooring_Data)+ 
#   theme_bw() + 
#   geom_line(aes(x = DateTime, y = `HCEP(OXY)`), color = 'blue')
# HCp5 <- ggplot(data = Mooring_Data)+ 
#   theme_bw() + 
#   geom_line(aes(x = DateTime, y = `HCEP(PH)`), color = 'blue')
# HCp6 <- ggplot(data = Mooring_Data)+ 
#   theme_bw() + 
#   geom_line(aes(x = DateTime, y = `HCEP(FL)`), color = 'blue')+
#   ylim(-0.5, 45)
# HCp7 <- ggplot(data = Mooring_Data)+ 
#   theme_bw() + 
#   geom_line(aes(x = DateTime, y = `HCEP(TURB)`), color = 'red')
# gridExtra::grid.arrange(HCp1,HCp2,HCp3,HCp4,HCp5,HCp6,HCp7, nrow=7)
# 
# 
# SFp1 <- ggplot(data = Mooring_Data)+ 
#   theme_bw() + 
#   geom_line(aes(x = DateTime, y = `SeaFET(INT_VOLT)`), color = 'blue')
# SFp2 <- ggplot(data = Mooring_Data)+ 
#   theme_bw() + 
#   geom_line(aes(x = DateTime, y = `SeaFET(EXT_VOLT)`), color = 'blue')
# SFp3 <- ggplot(data = Mooring_Data)+ 
#   theme_bw() + 
#   geom_line(aes(x = DateTime, y = `SeaFET(PH_INT)`), color = 'red')
# SFp4 <- ggplot(data = Mooring_Data)+ 
#   theme_bw() + 
#   geom_line(aes(x = DateTime, y = `SeaFET(PH_EXT)`), color = 'blue')
# SFp5 <- ggplot(data = Mooring_Data)+ 
#   theme_bw() + 
#   geom_line(aes(x = DateTime, y = `HCEP(PH)`), color = 'blue')
# SFp6 <- ggplot(data = Mooring_Data)+ 
#   theme_bw() + 
#   geom_line(aes(x = DateTime, y = `HCEP(FL)`), color = '#023020')+
#   labs(x = "")+
#   ylim(-0.5 , 45)
# SFp7 <- ggplot(data = Mooring_Data)+ 
#   theme_bw() + 
#   geom_line(aes(x = DateTime, y = `HCEP(TURB)`), color = 'red')
# 
# (allph <- ggplot(Mooring_Data)+
#   geom_line(aes(x = DateTime+22, y = `HCEP(PH)`), color = 'black')+
#   geom_line(aes(x = DateTime+22, y = `SeaFET(PH_INT)`), color = 'red')+
#   geom_line(aes(x = DateTime+22, y = `SeaFET(PH_EXT)`), color = 'blue')+
#   labs(x = "", y = "HC and SeaFET pH"))
# ph_delta <- ggplot(Mooring_Data, aes(x = DateTime, y = ph_delta))+
#                      geom_line()+
#                      geom_smooth()
#                    
#   
# gridExtra::grid.arrange(SFp1,SFp2,SFp3,SFp4,SFp5,SFp6,SFp7, nrow=7)
# gridExtra::grid.arrange(
#   allph, 
#   SFp6, 
#   HCp4, 
#   nrow = 3)  
# 
# gridExtra::grid.arrange(
#   allph, 
#   ggplot(data = Mooring_Data)+ 
#     theme_bw() + 
#     geom_line(aes(x = DateTime, y = `SeaFET(INT_VOLT)`), color = 'red'), 
#   ggplot(data = Mooring_Data)+ 
#     theme_bw() + 
#     geom_line(aes(x = DateTime, y = `SeaFET(EXT_VOLT)`), color = 'blue'), 
#   ggplot(data = Mooring_Data)+ 
#     theme_bw() + 
#     geom_line(aes(x = DateTime, y = `SeaFET(TEMP)`), color = 'black'), 
#   nrow = 4)  

