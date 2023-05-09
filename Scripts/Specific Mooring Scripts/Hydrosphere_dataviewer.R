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
# choose.dir(caption = "Select directory")
save_folder <- r"(C:\Users\gikeda\OneDrive - King County\Downloads)"
setwd(save_folder)



# Load Data  ----------------------------------------------------------
hydrosphere_file <- choose.files(caption = "Select Hydrosphere file")
hydrosphere <- read_csv(hydrosphere_file, comment = "*")

station <- gsub(".*\\\\","",hydrosphere_file)
station <- gsub(" ","",station)
station <- gsub("\(*\)","",station)


if(station == "PointWilliams"){
  zzhydrosphere_clean <- hydrosphere %>%
    mutate(Datetime_string = paste(`Date(America/Los_Angeles)`, `Time(America/Los_Angeles)`),   
           Datetime = mdy_hms(Datetime_string),
           UnixDatetime = as.numeric(Datetime),
           temp = signif(`HCEP(TEMP)`, digits = 3),
           sal = signif(`HCEP(SAL)`, digits = 3),
           oxy = signif(`HCEP(OXY)`, digits = 3),
           spar = signif(`PAR_SQ-215-SS`, digits = 3)) %>%
    select(Datetime, temp, sal, oxy, spar, UnixDatetime, everything())
  
  zzkingcounty_clean <- kingcounty %>%
    mutate(Datetime = mdy_hms(`Date`),
           UnixDatetime = as.numeric(Datetime),
           temp = signif(`Water_Temperature_degC`, digits = 3),
           sal = signif(`Salinity_PSU`, digits = 3),
           oxy = signif(`Dissolved_Oxygen_mg/L`, digits = 3)) %>%
    select(Datetime, temp, sal, oxy, UnixDatetime, everything())
  
  # Plot the data
  plot <- ggplot(zzkingcounty_clean)+
    geom_point(aes(x = Datetime,
                   y = `Surf_PAR_umol/s/sqm`),
               size = 2)+
    geom_line(aes(x = Datetime,
                  y = `Surf_PAR_umol/s/sqm`,
                  alpha = 0.2),
              color = "black") +
    geom_point(data = zzhydrosphere_clean,
               aes(x = Datetime,
                   y = `PAR_SQ-215-SS`,
                   alpha = 0.5),
               color = "red") +
    geom_line(data = zzhydrosphere_clean,
              aes(x = Datetime,
                  y = `PAR_SQ-215-SS`,
                  alpha = 0.2),
              color = "red") +
    ggtitle(paste(station, "(Hydrosphere = Red, KC Website = Black)"))
  ggplotly(plot)

  
} else if(station == "Dockton"){
  zzhydrosphere_clean <- hydrosphere %>%
    mutate(Datetime_string = paste(`Date(America/Los_Angeles)`, `Time(America/Los_Angeles)`),   
           Datetime = mdy_hms(Datetime_string, tz = "America/Los_Angeles"),
           UnixDatetime = as.numeric(Datetime),
           temp = signif(`HCEP(TEMP)`, digits = 3),
           sal = signif(`HCEP(SAL)`, digits = 3),
           oxy = signif(`HCEP(OXY)`, digits = 3)) %>%
    select(Datetime, temp, sal, oxy, UnixDatetime, everything())
  
  zzkingcounty_clean <- kingcounty %>%
    mutate(Datetime = mdy_hms(`Date`, tz = "America/Los_Angeles"),
           UnixDatetime = as.numeric(Datetime),
           temp = signif(`Water_Temperature_degC`, digits = 3),
           sal = signif(`Salinity_PSU`, digits = 3),
           oxy = signif(`Dissolved_Oxygen_mg/L`, digits = 3),
           spar = signif(`Surf_PAR_umol/s/sqm`, digits = 3)) %>%
    select(Datetime, temp, sal, oxy, spar, UnixDatetime, everything())
  
  plot <- ggplot(zzkingcounty_clean)+
    geom_point(aes(x = Datetime,
                   y = oxy))+
    geom_line(aes(x = Datetime,
                  y = oxy,
                  alpha = 0.2),
              color = "black") +
    geom_point(data = zzhydrosphere_clean,
               aes(x = Datetime,
                   y = oxy,
                   alpha = 2),
               color = "red") +
    geom_line(data = zzhydrosphere_clean,
              aes(x = Datetime,
                  y = oxy,
                  alpha = 0.2),
              color = "red") +
    ggtitle("Dockton (Hydrosphere = Red, KC Website = Black)")
  ggplotly(plot)
  
} else if(station =="SeattleAquarium"){
  zzhydrosphere_clean <- hydrosphere %>%
    mutate(Datetime_string = paste(`Date(America/Los_Angeles)`, `Time(America/Los_Angeles)`),   
           Datetime = mdy_hms(Datetime_string, tz = "America/Los_Angeles"),
           UnixDatetime = as.numeric(Datetime),
           temp = signif(`HCEP1(TEMP)`, digits = 3),
           sal = signif(`HCEP1(SAL)`, digits = 3),
           oxy = signif(`HCEP1(OXY)`, digits = 3)) %>%
    select(Datetime, temp, sal, oxy, UnixDatetime, everything())
  
  zzkingcounty_clean <- kingcounty %>%
    mutate(Datetime = mdy_hms(`Date`, tz = "America/Los_Angeles"),
           UnixDatetime = as.numeric(Datetime),
           temp = signif(`1_Water_Temperature_degC`, digits = 3),
           sal = signif(`1_Salinity_PSU`, digits = 3),
           oxy = signif(`1_Dissolved_Oxygen_mg/L`, digits = 3)) %>%
    select(Datetime, temp, sal, oxy, UnixDatetime, everything())

  # Plot the data
  plot <- ggplot(zzkingcounty_clean)+
    geom_point(aes(x = Datetime,
                   y = oxy),
               size = 2)+
    geom_line(aes(x = Datetime,
                  y = oxy,
                  alpha = 0.2),
              color = "black") +
    geom_point(data = zzhydrosphere_clean,
               aes(x = Datetime,
                   y = oxy),
               alpha = 0.8,
               color = "red") +
    geom_line(data = zzhydrosphere_clean,
              aes(x = Datetime,
                  y = oxy,
                  alpha = 0.2),
              color = "red") +
    ggtitle(paste(station, "(Hydrosphere = Red, KC Website = Black)"))
  ggplotly(plot)
  
  
} else if(station == "QuartermasterYachtClub"){
  zzhydrosphere_clean <- hydrosphere %>%
    mutate(Datetime_string = paste(`Date(America/Los_Angeles)`, `Time(America/Los_Angeles)`),   
           Datetime = mdy_hms(Datetime_string, tz = "America/Los_Angeles"),
           UnixDatetime = as.numeric(Datetime),
           temp = signif(`HCEP(TEMP)`, digits = 3),
           sal = signif(`HCEP(SAL)`, digits = 3),
           oxy = signif(`HCEP(OXY)`, digits = 3)) %>%
    select(Datetime, temp, sal, oxy, UnixDatetime, everything())
  
  zzkingcounty_clean <- kingcounty %>%
    mutate(Datetime = mdy_hms(`Date`, tz = "America/Los_Angeles"),
           UnixDatetime = as.numeric(Datetime),
           temp = signif(`Water_Temperature_degC`, digits = 3),
           sal = signif(`Salinity_PSU`, digits = 3),
           oxy = signif(`Dissolved_Oxygen_mg/L`, digits = 3)) %>%
    select(Datetime, temp, sal, oxy, UnixDatetime, everything())
  
  # Plot the data
  plot <- ggplot(zzkingcounty_clean)+
    geom_point(aes(x = Datetime,
                   y = temp),
               size = 2)+
    geom_line(aes(x = Datetime,
                  y = temp,
                  alpha = 0.2),
              color = "black") +
    geom_point(data = zzhydrosphere_clean,
               aes(x = Datetime,
                   y = temp,
                   alpha = 0.5),
               color = "red") +
    geom_line(data = zzhydrosphere_clean,
              aes(x = Datetime,
                  y = temp,
                  alpha = 0.2),
              color = "red") +
    ggtitle(paste(station, "(Hydrosphere = Red, KC Website = Black)"))
  ggplotly(plot)
}




# Scratch -------------------------------------------------------------------
# 
winter_KC <- zzkingcounty_clean %>%
  filter(Datetime > as.POSIXct("2022-01-01 00:00:00"),
         Datetime < as.POSIXct("2022-03-01 00:00:00"))
spring_KC <- zzkingcounty_clean %>%
  filter(Datetime > as.POSIXct("2022-03-01 00:00:00"),
         Datetime < as.POSIXct("2022-06-01 00:00:00"))
summer_KC <- zzkingcounty_clean %>%
  filter(Datetime > as.POSIXct("2022-06-01 00:00:00"),
         Datetime < as.POSIXct("2022-09-01 00:00:00"))
fall_KC <- zzkingcounty_clean %>%
  filter(Datetime > as.POSIXct("2022-09-01 00:00:00"),
         Datetime < as.POSIXct("2023-01-31 00:00:00"))


winter_HS <- zzhydrosphere_clean %>%
  filter(Datetime > as.POSIXct("2022-01-01 00:00:00"),
         Datetime < as.POSIXct("2022-03-01 00:00:00"))
spring_HS <- zzhydrosphere_clean %>%
  filter(Datetime > as.POSIXct("2022-03-01 00:00:00"),
         Datetime < as.POSIXct("2022-06-01 00:00:00"))
summer_HS <- zzhydrosphere_clean %>%
  filter(Datetime > as.POSIXct("2022-06-01 00:00:00"),
         Datetime < as.POSIXct("2022-09-01 00:00:00"))
fall_HS <- zzhydrosphere_clean %>%
  filter(Datetime > as.POSIXct("2022-09-01 00:00:00"),
         Datetime < as.POSIXct("2023-01-31 00:00:00"))
# 
# # Seasons
# 
# # Winter
plot <- ggplot(winter_KC)+
  geom_point(aes(x = Datetime,
                 y = temp),
             size = 2)+
  geom_line(aes(x = Datetime,
                y = temp,
                alpha = 0.2),
            color = "black") +
  geom_point(data = winter_HS,
             aes(x = Datetime,
                 y = temp,
                 alpha = 0.5),
             color = "red") +
  geom_line(data = winter_HS,
            aes(x = Datetime,
                y = temp,
                alpha = 0.2),
            color = "red") +
  ggtitle(paste(station, " Winter (Hydrosphere = Red, KC Website = Black)"))
ggplotly(plot)

# Spring
plot <- ggplot(spring_KC)+
  geom_point(aes(x = Datetime,
                 y = temp),
             size = 2)+
  geom_line(aes(x = Datetime,
                y = temp,
                alpha = 0.2),
            color = "black") +
  geom_point(data = spring_HS,
             aes(x = Datetime,
                 y = temp,
                 alpha = 0.5),
             color = "red") +
  geom_line(data = spring_HS,
            aes(x = Datetime,
                y = temp,
                alpha = 0.2),
            color = "red") +
  ggtitle(paste(station, " Spring (Hydrosphere = Red, KC Website = Black)"))
ggplotly(plot)

# Summer
plot <- ggplot(summer_KC)+
  geom_point(aes(x = Datetime,
                 y = temp),
             size = 2)+
  geom_line(aes(x = Datetime,
                y = temp,
                alpha = 0.2),
            color = "black") +
  geom_point(data = summer_HS,
             aes(x = Datetime,
                 y = temp,
                 alpha = 0.5),
             color = "red") +
  geom_line(data = summer_HS,
            aes(x = Datetime,
                y = temp,
                alpha = 0.2),
            color = "red") +
  ggtitle(paste(station, " Summer (Hydrosphere = Red, KC Website = Black)"))
ggplotly(plot)

# Fall
plot <- ggplot(fall_KC)+
  geom_point(aes(x = Datetime,
                 y = temp),
             size = 2)+
  geom_line(aes(x = Datetime,
                y = temp,
                alpha = 0.2),
            color = "black") +
  geom_point(data = fall_HS,
             aes(x = Datetime,
                 y = temp,
                 alpha = 0.5),
             color = "red") +
  geom_line(data = fall_HS,
            aes(x = Datetime,
                y = temp,
                alpha = 0.2),
            color = "red") +
  ggtitle(paste(station, " Fall (Hydrosphere = Red, KC Website = Black)"))
ggplotly(plot)

# Sept - Dec
timerange_KC <- zzkingcounty_clean %>%
filter(Datetime > as.POSIXct("2022-09-01 00:00:00"),
       Datetime < ymd_hms("2022-11-30 00:00:00"))
timerange_HS <- zzhydrosphere_clean %>%
filter(Datetime > as.POSIXct("2022-09-01 00:00:00"),
       Datetime < ymd_hms("2022-11-30 00:00:00"))

plot <- ggplot(timerange_KC)+
  geom_point(aes(x = Datetime,
                 y = `Surf_PAR_umol/s/sqm`),
             size = 2)+
  geom_line(aes(x = Datetime,
                y = `Surf_PAR_umol/s/sqm`,
                alpha = 0.2),
            color = "black") +
  geom_point(data = timerange_HS,
             aes(x = Datetime,
                 y = `PAR_SQ-215-SS`,
                 alpha = 0.5),
             color = "red") +
  geom_line(data = timerange_HS,
            aes(x = Datetime,
                y = `PAR_SQ-215-SS`,
                alpha = 0.2),
            color = "red") +
  ggtitle(paste(station, " Fall (Hydrosphere = Red, KC Website = Black)"))
ggplotly(plot)
