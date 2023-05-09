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

#### Ony thing to change - define how many days you would like to plot. Default is days back from today #######

daysfwd <- Sys.Date()
start_date <- "2022-09-01"
end_date <- "2022-09-30"
# end_date <- paste0(Sys.Date())
daysback <- as.numeric(parse_date(end_date) - parse_date(start_date))

# Where to save QC output?
# save_folder <- choose.dir(caption = "Select the folder to save stuff")
save_folder <- "C:/Users/gikeda/R/Temp/Mooring_Data/"
archive <- "C:/Users/gikeda/R/Temp/Mooring_Data/Archive"
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
file.copy(from = fpath,
          to = archive)
file.remove(from = fpath)

#### Load data and strip out the header #### 
mooringdata <- mooringdata %>%
  mutate(Datetime = mdy_hms(Date, tz = "America/Los_Angeles"),
         year = as.double(format(as.Date(Datetime,format="%Y-%m-%d"), format = "%Y")),
         month = as.factor(format(as.Date(Datetime,format="%Y-%m-%d"), format = "%m")),
         day = as.double(format(as.Date(Datetime,format="%Y-%m-%d"), format = "%d")),
         hour = as.double(format(as.POSIXct(Datetime,format="%H:%M:%S"), format = "%H")),
         monthday = as.factor(format(as.Date(Datetime,format="%Y-%m-%d"), format = "%m-%d"))) %>%
  select(Datetime, year, month, day, hour, everything()) 

# Strip out qualifier data, battery voltage, and raw SeaFET data 
remove_cols <- which(grepl(pattern = 'ID|Qual|_V', colnames(mooringdata)), arr.ind = TRUE)
mooringdata_light <- mooringdata[-remove_cols]

#### Define Date Range and Plot Data #### 
# userdef_start <- Sys.Date() - daysback
# userdef_end <- daysfwd
userdef_start <- as.POSIXct(start_date)
userdef_end <- as.POSIXct(end_date)
userdef <- mooringdata_light %>%
  filter(Datetime > userdef_start,
         Datetime < userdef_end)
dir.create(paste0(userdef_start, "--", userdef_end))
userdef_save_directory <- paste0(save_folder,"/", userdef_start, "--", userdef_end)

# Strip out the dates from the plotted data
remove_var <- which(grepl(pattern = 'monthday|Datetime|year|month|day|hour|Date', colnames(userdef)), arr.ind = TRUE)
variables <- colnames(userdef[-remove_var])

# Create Figures for each param -------------------------------------------
myplots <- vector("list")
count <- 0
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
                                         color = 'blue'))
  if(grepl("%", variable) == TRUE){
    variable <- str_replace_all(variable, "%", "percent" )
  } else if(grepl("/", variable) == TRUE){
    variable <- str_replace_all(variable, "/", "-per-" )
  } else{
    variable <- variable
  }
  ggsave(path = userdef_save_directory, 
         paste0(as.character(count), "_", as.character(variable), "_", userdef_start, "-", userdef_end, "_mooring_review.png"), 
         dpi = 300, height = 8.5, width = 16)
  htmlwidgets::saveWidget(ggplotly(myplots[[count]]), 
                          title = paste0(variable), 
                          file = paste0(userdef_save_directory, "/", as.character(count), "_", as.character(variable), ".html"))
  beep(1)
  Sys.sleep(1)
}

# Deletes the additional folders created by htmltools
created_dirs <- list.dirs(userdef_save_directory)
unlink(created_dirs[2:length(created_dirs)], recursive = TRUE)

# Extra and Scratch Notes -------------------------------------------------

# 
# t <- ggplot(userdef)+
#   geom_line(aes_string(x = 'Datetime',
#                        y = 'Water_Temperature_degC'),
#             alpha = 0.3,
#             size = 0.8)+
#   # annotate("rect", 
#   #          xmin = as.POSIXct("2021-03-14"),
#   #          xmax = as.POSIXct("2021-03-15"),
#   #          ymax = max(userdef$Water_Temperature_degC),
#   #          ymin= min(userdef$Water_Temperature_degC),
#   #          alpha = 0.1,
#   #          fill = "blue")+
#   geom_point(aes_string(x = 'Datetime',
#                         y = 'Water_Temperature_degC'),
#              size = 0.7,
#              color = 'blue')
# htmlwidgets::saveWidget(ggplotly(t), 
#                         file = "test.html")
# ggplotly(t)
# 
# ggsave(path = userdef_save_directory, 
#        paste0("_mooring_review.png"), 
#        dpi = 300, height = 8.5, width = 16)


# start_date <- dlgInput("Enter the start date in format yyyy-mm-dd", Sys.info()["user"])$res
# end_date <- dlgInput("Enter the end date in format yyyy-mm-dd", Sys.info()["user"])$res
beep(5)


# Identify missing data -----------------------------------------------------------------
# 
# if(station == "SeattleAquarium"){
#   missing <- mooringdata %>%
#     filter(Qual_1_Water_Temperature == 999)
#   write_csv(missing, paste0(station,"_missingvalues.csv"))
# } else{
#   missing <- mooringdata %>%
#     filter(Qual_Water_Temperature == 999)
#   write_csv(missing, paste0(station,"_missingvalues.csv"))
# }
# 


# Scratch -----------------------------------------------------------------
# Identifies missing data from non-CTD params
# keepcols <- which(grepl(pattern = 'Qual', colnames(mooringdata)), arr.ind = TRUE)
# qual_columns <- colnames(mooringdata[keepcols])
# missing_SUNA <- mooringdata %>%
#   filter(`Qual_SUNA_Nitrite+Nitrate_mgN/L_raw` == 999)
# write_csv(missing_SUNA, "missing_SUNA.csv")
# missing_airtemp <- mooringdata %>%
#   filter(`Qual_Air_Temperature` == 999)
# write_csv(missing_airtemp, "missing_airtemp.csv")
# missing_windspeed <- mooringdata %>%
#   filter(`Qual_Wind_Speed` == 999)
# write_csv(missing_windspeed, "missing_windspeed.csv")
# missing_winddir <- mooringdata %>%
#   filter(`Qual_Wind_Direction` == 999)
# write_csv(missing_winddir, "missing_winddir.csv")
# missing_surfPAR <- mooringdata %>%
#   filter(`Qual_Surf_PAR` == 999)
# write_csv(missing_surfPAR, "missing_surfPAR.csv")
# missing_seafet <- mooringdata %>%
#   filter(`Qual_SeaFET_Internal_pH_raw` == 999)
# write_csv(missing_seafet, "missing_seafet.csv")
# missing_humidity <- mooringdata %>%
#   filter(`Qual_Rel_Hum` == 999)
# write_csv(missing_humidity, "missing_humidity.csv")
# 

# 
# data_list <- c()
# colcount <- 0
# for(i in c(1:length(qual_columns))){
#   smoogle <- mooringdata %>%
#     filter(qual_columns[i] == 999)
#   
#   print(ggplot(smoogle)+
#     geom_bar(aes(x = monthday)))
#   # Sys.sleep(1)
# }
# 
# 
# test <- mooringdata %>%
#   filter(qual_columns[1] == 999)
# test2 <- mooringdata %>%
#   filter(Qual_Depth == 999)
# 
# testname <- as.name(qual_columns[1])
# 
# mooringdata$testname
# 
# test3 <- mooringdata %>%
#   filter(as.name(qual_columns[1]) == 999)
# 
# g <- ggplot(missing)+
#   geom_bar(aes(x = monthday))
# ggplotly(g)
