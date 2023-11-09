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

# Load Data and set up directories ---------------------------------------------------------------

# downloads <- "C:\\Users\\gikeda\\OneDrive - King County\\Downloads"
# shell.exec(downloads)



save_folder <- paste0(here(), "/Output")
archive <- paste0(here(), "/Archive")
setwd(save_folder)

fpath <- choose.files(default = "C:\\Users\\gikeda\\OneDrive - King County\\Downloads\\test",
                      caption = "Select the King County Mooring File")
mooringdata <- read_tsv(fpath, comment = "*")

station <- gsub(".*\\\\","",fpath)
station <- gsub("_.*","",station)

dir.create(paste0(as.character(station), "_", Sys.Date(), "_Mooring_QC"))
save_folder <- paste0(save_folder, "/", as.character(station), "_", Sys.Date(), "_Mooring_QC")
setwd(save_folder)
file.copy(from = fpath,
          to = save_folder)
file.copy(from = fpath,
          to = archive)


#### Load data and strip out the header #### 
mooringdata <- mooringdata %>%
  mutate(Datetime = mdy_hms(Date, tz = "UTC"),
         year = as.double(format(as.Date(Datetime, format="%Y-%m-%d"), format = "%Y")),
         month = as.factor(format(as.Date(Datetime, format="%Y-%m-%d"), format = "%m")),
         day = as.double(format(as.Date(Datetime, format="%Y-%m-%d"), format = "%d")),
         hour = as.double(format(as.POSIXct(Datetime, format="%H:%M:%S"), format = "%H")),
         monthday = as.factor(format(as.Date(Datetime, format="%Y-%m-%d"), format = "%m-%d"))) %>%
  select(Datetime, year, month, day, hour, everything()) 

# Strip out qualifier data, battery voltage, and raw SeaFET data 
remove_cols <- which(grepl(pattern = 'ID|Qual|_V', colnames(mooringdata)), arr.ind = TRUE)
mooringdata_light <- mooringdata[-remove_cols]

#### Define Date Range and Plot Data #### 

daysfwd <- Sys.Date()
start_date <- min(mooringdata_light$Datetime)
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
  # beep(1)
  # Sys.sleep(1)
}

# Deletes the additional folders created by htmltools
created_dirs <- list.dirs(userdef_save_directory)
unlink(created_dirs[2:length(created_dirs)], recursive = TRUE)

file.remove(from = fpath)

# Stacked Plots -----------------------------------------------------------

dir.create(paste0(userdef_save_directory, "/stacked"))
stacked_folder <- paste0(userdef_save_directory, "/stacked")

vars <- c()
for(i in c(1:length(variables))){
  if(grepl("%", variables[i]) == TRUE){
    vars[i] <- str_replace_all( variables[i], "%", "percent" )
  } else if(grepl("/",  variables[i]) == TRUE){
    vars[i] <- str_replace_all( variables[i], "/", "-per-" )
  } else{
    vars[i] <-  variables[i]
  }
}

plotstack <- function(index1, index2){
  print(myplots[[index1]] + myplots[[index2]] +plot_layout(nrow = 2))
  ggsave(path = stacked_folder, 
         paste0(vars[index1], "_", vars[index2], '.png'), 
         dpi = 300, height = 8.5, width = 16)
  Sys.sleep(3)
}

if(station == "Dockton"){
  # Salinity and DO
  plotstack(3, 5)
  # Salnity and Temperature
  plotstack(3, 2)
  # DO and Chl
  plotstack(5, 6)
  # DO and pH
  plotstack(5, 8)
  # Density and Salinity
  plotstack(9, 3)
  # Chl and Turbidity
  plotstack(6, 7)
  # Rainfall and Salinity
  plotstack(13, 3)
  # Fluorescence and pH
  plotstack(6, 8)
} else if(station == "SeattleAquarium"){
  # Salinity and DO 1
  plotstack(5, 8)
  # Salinity and DO 2
  plotstack(6, 10)
  # Salnity and Temperature 1
  plotstack(5, 3)
  # Salnity and Temperature 2
  plotstack(6, 4)
  # DO and Chl 1 
  plotstack(8, 11)
  # DO and Chl 
  plotstack(9, 12)
  # DO and pH 1
  plotstack(8, 15)
  # DO and pH 2
  plotstack(9, 16)
  # Density and Salinity 1
  plotstack(5, 17)
  # Density and Salinity 2
  plotstack(6, 18)
  # Chl and Turbidity 1
  plotstack(11, 13)
  # Chl and Turbidity 2
  plotstack(12, 14)
  # Fluorescence and pH 1
  plotstack(11, 15)
  # Fluorescence and pH 1
  plotstack(12, 16)
} else if(station == "QuartermasterYachtClub"){
  # Salinity and DO
  plotstack(3, 5)
  # Salnity and Temperature
  plotstack(3, 2)
  # DO and Chl
  plotstack(5, 6)
  # DO and pH
  plotstack(5, 7)
  # Density and Salinity
  plotstack(8, 3)
  # Chl and Turbidity
  plotstack(6, 9)
  # Fluorescence and pH
  plotstack(6, 7)
  # HC-EP pH and SeaFET pH
  plotstack(10, 7)
} else if(station == "PointWilliams"){
  # Salinity and DO
  plotstack(3, 5)
  # Salnity and Temperature
  plotstack(3, 2)
  # DO and Chl
  plotstack(5, 6)
  # DO and pH
  plotstack(5, 8)
  # Density and Salinity
  plotstack(11, 3)
  # Chl and Turbidity
  plotstack(6, 7)
  # Fluorescence and pH
  plotstack(6, 8)
  # HC-EP pH and SeaFET pH
  plotstack(8, 21)
  # Nitrate and Chl
  plotstack(9, 6)
}

# beep(5)
shell.exec(userdef_save_directory)


# Identify missing data -----------------------------------------------------------------
miising_data <- mooringdata %>%
filter(if_any(everything(), ~ . == 999))


# if(station == "SeattleAquarium"){
#   missing <- mooringdata %>%
#     filter(Qual_1_Water_Temperature == 999)
#   write_csv(missing, paste0(station,"_missingvalues.csv"))
# } else{
#   missing <- mooringdata %>%
#     filter(Qual_Water_Temperature == 999)
#   write_csv(missing, paste0(station,"_HCEP_missingvalues.csv"))
# }
