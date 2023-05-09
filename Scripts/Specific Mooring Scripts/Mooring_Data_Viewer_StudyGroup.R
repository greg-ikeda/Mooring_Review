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

# Where to save QC output?
# save_folder <- choose.dir(caption = "Select the folder to save stuff")
save_folder <- "//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/Marine Group/Greg_Temp/"
setwd(save_folder)
fpath <- choose.files(caption = "Select file")
mooringdata <- read_tsv(fpath, comment = "*")
station <- gsub(".*\\\\","",fpath)
station <- gsub("_.*","",station)
dir.create(paste0(as.character(station), "_", Sys.Date(), "_Mooring_QC"))
save_folder <- paste0(save_folder, as.character(station), "_", Sys.Date(), "_Mooring_QC")
setwd(save_folder)
file.copy(from = fpath,
          to = save_folder)
file.remove(from = fpath)


#### Load data and strip out the header #### 
mooringdata <- mooringdata %>%
  mutate(Datetime = mdy_hms(Date),
         year = as.double(format(as.Date(Datetime,format="%Y-%m-%d"), format = "%Y")),
         month = as.factor(format(as.Date(Datetime,format="%Y-%m-%d"), format = "%m")),
         day = as.double(format(as.Date(Datetime,format="%Y-%m-%d"), format = "%d")),
         hour = as.double(format(as.POSIXct(Datetime,format="%H:%M:%S"), format = "%H")),
         monthday = as.factor(format(as.Date(Datetime,format="%Y-%m-%d"), format = "%m-%d"))) %>%
  select(Datetime, year, month, day, hour, everything()) 

# Strip out qualifier data, battery voltage, and raw SeaFET data 
remove_cols <- which(grepl(pattern = 'ID|Qual|_V', colnames(mooringdata)), arr.ind = TRUE)
mooringdata_light <- mooringdata[-remove_cols]

#### Look at the last 30 days of data #### 
start <- Sys.Date() - 30
end <- Sys.Date()
last_month <- mooringdata_light %>%
  filter(Datetime > start,
         Datetime < end)
dir.create(paste0("monthly_", start, "-", end))
thirtyday_save_directory <- paste0(save_folder,"/", "monthly_", start, "-", end)


# Strip out the dates from the plotted data
remove_var <- which(grepl(pattern = 'monthday|Datetime|year|month|day|hour|Date', colnames(last_month)), arr.ind = TRUE)
variables <- colnames(last_month[-remove_var])

# Create plots for 30-day data
myplots <- vector("list")
count <- 0
for (variable in variables){
  variable <- as.name(variable)
  count <- count+1
  # lower_lim <- (as.numeric(summary(last_month[[variable]])[2])) - (log10(as.numeric(summary(last_month[[variable]])[2])))
  # upper_lim <- (as.numeric(summary(last_month[[variable]])[4])) + (log2(as.numeric(summary(last_month[[variable]])[4])))
  myplots[[count]] <- print(ggplot(last_month)+
                              geom_line(aes_string(x = 'Datetime',
                                                   y = as.name(variable)),
                                        alpha = 0.3,
                                        size = 0.8)+
    labs(title = paste0(variable, " (30 Days) ", station))+
    geom_point(aes_string(x = 'Datetime',
                         y = as.name(variable)),
               size = 0.7,
               color = 'blue'))
    # ylim(c(lower_lim, upper_lim))
  if(grepl("%", variable) == TRUE){
    variable <- str_replace_all(variable, "%", "percent" )
  } else if(grepl("/", variable) == TRUE){
    variable <- str_replace_all(variable, "/", "÷" )
  } else{
    variable <- variable
  }
  ggsave(path = thirtyday_save_directory, paste0(as.character(count), "_", as.character(variable), "_", start, "-", end, "_mooring_review.png"),
         dpi = 300, height = 8.5, width = 16)
  htmlwidgets::saveWidget(ggplotly(myplots[[count]]), title = paste0(variable), file = paste0(thirtyday_save_directory, "/", as.character(count), "_", as.character(variable), ".html"))
}


#### Look at the last 10 days of data #### 
tenday_start <- Sys.Date() - 10
tenday_end <- Sys.Date()
week_start <- Sys.Date() - 7
last_tendays <- mooringdata_light %>%
  filter(Datetime > tenday_start,
         Datetime < tenday_end)
dir.create(paste0("weekly_", tenday_start, "-", tenday_end))
weekly_save_directory <- paste0(save_folder,"/", "weekly_", tenday_start, "-", tenday_end)

# Strip out the dates from the plotted data
remove_var <- which(grepl(pattern = 'monthday|Datetime|year|month|day|hour|Date', colnames(last_tendays)), arr.ind = TRUE)
variables <- colnames(last_tendays[-remove_var])

# Create plots for last week of data #
myplots <- vector("list")
count <- 0
for (variable in variables){
  variable <- as.name(variable)
  count <- count+1
  myplots[[count]] <- print(ggplot(last_tendays)+
                              geom_line(aes_string(x = 'Datetime',
                                                   y = as.name(variable)),
                                        alpha = 0.3,
                                        size = 0.8)+
                              labs(title = paste0(variable, " (10 Days) ", station))+
                            geom_point(aes_string(x = 'Datetime',
                                                  y = as.name(variable)),
                                       size = 0.7,
                                       color = 'blue'))
  if(grepl("%", variable) == TRUE){
    variable <- str_replace_all(variable, "%", "percent" )
  } else if(grepl("/", variable) == TRUE){
    variable <- str_replace_all(variable, "/", "÷" )
  } else{
    variable <- variable
  }
  ggsave(path = weekly_save_directory, paste0(as.character(count), "_", as.character(variable), "_", start, "-", end, "_mooring_review.png"), 
         dpi = 300, height = 8.5, width = 16)
  htmlwidgets::saveWidget(ggplotly(myplots[[count]]), title = paste0(variable), file = paste0(weekly_save_directory, "/", as.character(count), "_", as.character(variable), ".html"))
}

