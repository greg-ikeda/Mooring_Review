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

#### Ony thing to change - define how many days you would like to plot. Default is days back from today #######
daysback <- 150
# daysfwd <- Sys.Date()
daysfwd <- Sys.Date()

start_date <- "2022-12-25"
end_date <- "2023-1-25"

# start_date <- "2022-03-07"
# end_date <- "2022-09-10"

# Where to save QC output?
# save_folder <- choose.dir(caption = "Select the folder to save stuff")
save_folder <- r"(C:\Users\gikeda\OneDrive - King County\Documents\January 2023\Mooring IT Fix\Dockton Data)"
setwd(save_folder)
fpath <- choose.files(caption = "Select file")
mooringdata <- read_csv(fpath, comment = "*")
filename <- gsub(".*\\\\","",fpath) 
station <- gsub(".csv","",filename)
dir.create(paste0("Hydrosphere_",as.character(station), "_", Sys.Date(), "_Mooring_QC"))
save_folder <- paste0(save_folder,"Hydrosphere_",as.character(station), "_", Sys.Date(), "_Mooring_QC")
setwd(save_folder)
file.copy(from = paste0(fpath),
          to = save_folder,
          overwrite = TRUE)
file.remove(from = fpath)

#### Load data and strip out the header #### 
mooringdata <- mooringdata %>%
  mutate(Datetime = ymd_hms(paste(Date, Time)),
         year = as.double(format(as.Date(Datetime,format="%Y-%m-%d"), format = "%Y")),
         month = as.factor(format(as.Date(Datetime,format="%Y-%m-%d"), format = "%m")),
         day = as.double(format(as.Date(Datetime,format="%Y-%m-%d"), format = "%d")),
         hour = as.double(format(as.POSIXct(Datetime,format="%H:%M:%S"), format = "%H")),
         minute = as.double(format(as.POSIXct(Datetime,format="%H:%M:%S"), format = "%M")),
         second = as.double(format(as.POSIXct(Datetime,format="%H:%M:%S"), format = "%S")),
         monthday = as.factor(format(as.Date(Datetime,format="%Y-%m-%d"), format = "%m-%d"))) %>%
  select(Datetime, year, month, day, hour, everything())

# Grab only HCEP data
HCEP_cols <- which(grepl(pattern = 'HCEP', colnames(mooringdata)), arr.ind = TRUE)
HCEP_columns <- colnames(mooringdata[HCEP_cols])
HCEP_data <- mooringdata %>%
  select(Datetime, all_of(HCEP_columns))

#### Define Date Range and Plot Data #### 
# userdef_start <- Sys.Date() - daysback
# userdef_end <- daysfwd
userdef_start <- as.POSIXct(start_date)
userdef_end <- as.POSIXct(end_date)
userdef <- HCEP_data %>%
  filter(Datetime > userdef_start,
         Datetime < userdef_end)
dir.create(paste0(as.character(daysback),"_days_", userdef_start, "-", userdef_end))
userdef_save_directory <- paste0(save_folder,"/", as.character(daysback),"_days_", userdef_start, "-", userdef_end)

# Strip out the dates from the plotted data
remove_var <- which(grepl(pattern = 'monthday|Datetime|year|month|day|hour|Date', colnames(userdef)), arr.ind = TRUE)
variables <- colnames(userdef[-remove_var])

# Create plots for last week of data #
myplots <- vector("list")
count <- 0
# variables <- c("Rain_Fall_in")

t <- 

for (variable in variables){
  variable <- as.name(variable)
  count <- count+1
  myplots[[count]] <- print(ggplot(userdef)+
                              geom_line(aes_string(x = 'Datetime',
                                                   y = as.name(variable)),
                                        alpha = 0.3,
                                        size = 0.8)+
                              labs(title = paste0(variable, " (", daysback, " Days) ", station))+
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
  # ggsave(path = userdef_save_directory, paste0(as.character(count), "_", as.character(variable), "_", userdef_start, "-", userdef_end, "_mooring_review.png"), dpi = 300, height = 8.5, width = 16)
  # htmlwidgets::saveWidget(ggplotly(myplots[[count]]), title = paste0(variable), file = paste0(userdef_save_directory, "/", as.character(count), "_", as.character(variable), ".html"))
}

ggplotly(myplots[[3]])

beep(5)

