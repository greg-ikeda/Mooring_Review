## This version is designed to run on all Hydrosphere files in a specific directory.

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
library(svDialogs)
library(here)

plotstack <- function(index1, index2){
  print(myplots[[index1]] + myplots[[index2]] +plot_layout(nrow = 2))
  ggsave(path = stacked_folder, 
         paste0(vars[index1], "_", vars[index2], '.png'), 
         dpi = 300, height = 8.5, width = 16)
  # Sys.sleep(3)
}

# Identify Files ----------------------------------------------------------

dir <- dirname(choose.files(default = "C:\\Users\\gikeda\\OneDrive - King County\\Downloads\\test",
                            caption = "Select file in the directory of Hydrosphere files"))
files <- list.files(dir, pattern = ".csv")

# Load Data and set up directories ---------------------------------------------------------------

save_folder <- paste0(here(), "/Output")
archive <- paste0(here(), "/Archive")
setwd(save_folder)

for(file in files){
  fpath <- paste0(dir,"/",file)
  mooringdata <- read_csv(fpath, comment = "*")
  
  station <- basename(fpath)
  station <- gsub(".csv", "", station)
  station <- gsub("(\\d)", "", station)
  station <- gsub("[[:punct:]]", "", station)
  
  dir.create(paste0(as.character(station), "_", Sys.Date(), "_Mooring_QC"))
  save_folder <- paste0(save_folder, "/", as.character(station), "_", Sys.Date(), "_Mooring_QC")
  setwd(save_folder)
  file.copy(from = fpath,
            to = save_folder)
  file.copy(from = fpath,
            to = archive)
  
  
  #### Prep data and strip out the header #### 
  mooringdata <- mooringdata %>%
    mutate(Datetime = paste(`Date`, `Time`),
           Datetime = ymd_hms(Datetime),
           year = as.double(format(as.Date(Datetime, format="%Y-%m-%d"), format = "%Y")),
           month = as.factor(format(as.Date(Datetime, format="%Y-%m-%d"), format = "%m")),
           day = as.double(format(as.Date(Datetime, format="%Y-%m-%d"), format = "%d")),
           hour = as.double(format(as.POSIXct(Datetime, format="%H:%M:%S"), format = "%H")),
           monthday = as.factor(format(as.Date(Datetime, format="%Y-%m-%d"), format = "%m-%d"))) %>%
    select(Datetime, year, month, day, hour, everything()) 
  
  # Strip out qualifier data, battery voltage, and raw SeaFET data 
  remove_cols <- which(grepl(pattern = 'Datetime|year|month|day|hour|UnixTimestamp|Date|Time|HCEP\\(SNDV\\)|HCEP\\(NUM\\)|HCEP\\(ERR\\)|monthday|SystemBattery|HCEP\\(FSD\\)|HCEP\\(TSD\\)|HCEP\\(SPC\\)', colnames(mooringdata)), arr.ind = TRUE)
  mooringdata_light <- mooringdata[-remove_cols] %>%
    mutate(Datetime = mooringdata$Datetime) %>%
    select(Datetime, everything())
  
  
  #### Define Date Range and Plot Data #### 
  
  daysfwd <- Sys.Date()
  start_date <- min(mooringdata_light$Datetime)
  # end_date <- "2022-12-28"
  end_date <- max(mooringdata_light$Datetime)
  daysback <- as.numeric(as.numeric(end_date - start_date))
  
  userdef_start <- as.Date(start_date)
  userdef_end <- as.Date(end_date)
  userdef_start_string <- paste0(userdef_start)
  userdef_end_string <- paste0(userdef_end)
  
  
  userdef <- mooringdata_light %>%
    filter(Datetime >= userdef_start,
           Datetime <= userdef_end +1)
  dir.create(paste0(userdef_start_string, "--", userdef_end_string))
  userdef_save_directory <- paste0(save_folder,"/", userdef_start_string, "--", userdef_end_string)
  
  # Strip out the dates from the plotted data
  remove_var <- which(grepl(pattern = 'monthday|Datetime|year|month|day|hour|Date', colnames(userdef)), arr.ind = TRUE)
  variables <- colnames(userdef[-remove_var])
  
  # Create Figures for each param -------------------------------------------
  myplots <- vector("list")
  count <- 0
  
  if(userdef_end - userdef_start < 21){
    xdate_breaks <- "2 days"
  } else if(userdef_end - userdef_start > 33){
    xdate_breaks <- "5 days"
  } else{
    xdate_breaks <- "1 week"
  }
  
  for (variable in variables){
    variable <- as.name(variable)
    count <- count+1
    myplots[[count]] <- print(ggplot(userdef)+
                                geom_line(aes_string(x = 'Datetime',
                                                     y = as.name(variable)),
                                          alpha = 0.3,
                                          size = 0.8)+
                                labs(title = paste0(variable, " (", userdef_start, " - ", userdef_end, ") ", station))+
                                geom_point(aes_string(x = 'Datetime',
                                                      y = as.name(variable)),
                                           size = 0.7,
                                           color = 'blue')+
                                scale_x_datetime(date_breaks = xdate_breaks, 
                                                 date_labels = "%m-%d",
                                                 expand = c(0, 0))+
                                theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)))
    if(grepl("%", variable) == TRUE){
      variable <- str_replace_all(paste0(variable), "%", "percent" )
    } else if(grepl("/", variable) == TRUE){
      variable <- str_replace_all(paste0(variable), "/", "-per-" )
    } else{
      variable <- variable
    }
    ggsave(path = userdef_save_directory, 
           paste0(as.character(count), "_", as.character(variable), "_", userdef_start, "-", userdef_end, "_mooring_review.png"), 
           dpi = 300, height = 8.5, width = 16)
    htmlwidgets::saveWidget(ggplotly(myplots[[count]]), 
                            title = paste0(variable), 
                            file = paste0(userdef_save_directory, "/", as.character(count), "_", as.character(variable), ".html"))
    # Sys.sleep(1)
  }
  
  # Deletes the additional folders created by htmltools
  created_dirs <- list.dirs(userdef_save_directory)
  unlink(created_dirs[2:length(created_dirs)], recursive = TRUE)
  
  file.remove(from = fpath)
  
  # Stacked Plots -------------------------------------------------
  # 
  # dir.create(paste0(userdef_save_directory, "/stacked"))
  # stacked_folder <- paste0(userdef_save_directory, "/stacked")
  # 
  # vars <- c()
  # for(i in c(1:length(variables))){
  #   if(grepl("%", variables[i]) == TRUE){
  #     vars[i] <- str_replace_all( variables[i], "%", "percent" )
  #   } else if(grepl("/",  variables[i]) == TRUE){
  #     vars[i] <- str_replace_all( variables[i], "/", "-per-" )
  #   } else{
  #     vars[i] <-  variables[i]
  #   }
  # }
  
  # plotstack <- function(index1, index2){
  #   print(myplots[[index1]] + myplots[[index2]] +plot_layout(nrow = 2))
  #   ggsave(path = stacked_folder, 
  #          paste0(vars[index1], "_", vars[index2], '.png'), 
  #          dpi = 300, height = 8.5, width = 16)
  #   # Sys.sleep(3)
  # }
  
  # if(station == "PennCoveSurface"){
  #   # Turbidity and Chl
  #   plotstack(9, 8)
  #   # pH and Chl
  #   plotstack(7, 8)
  #   # Nitrate and Chl
  #   plotstack(14, 8)
  #   # DO and Chl
  #   plotstack(6, 8)
  #   # DO and pH
  #   plotstack(6, 7)
  #   # DO and Salinity
  #   plotstack(6, 10)
  #   # Temperature and Salinity
  #   plotstack(3, 10)
  # } else if(station == "CoupevilleWharf"){
  #   # Turbidity and Chl
  #   plotstack(7, 6)
  #   # pH and Chl
  #   plotstack(5, 6)
  #   # Nitrate and Chl
  #   plotstack(12, 6)
  #   # DO and Chl
  #   plotstack(4, 6)
  #   # DO and pH
  #   plotstack(4, 5)
  #   # DO and Salinity
  #   plotstack(4, 8)
  #   # Temperature and Salinity
  #   plotstack(1, 8)
  # } else {
  #   # Turbidity and Chl
  #   plotstack(7, 6)
  #   # pH and Chl
  #   plotstack(5, 6)
  #   # Nitrate and Chl
  #   plotstack(12, 6)
  #   # DO and Chl
  #   plotstack(4, 6)
  #   # DO and pH
  #   plotstack(4, 5)
  #   # DO and Salinity
  #   plotstack(4, 8)
  #   # Temperature and Salinity
  #   plotstack(1, 8)
  # }
  
  shell.exec(userdef_save_directory)
}
