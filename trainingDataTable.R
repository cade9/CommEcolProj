# Purpose: The following extracts the dates of all processed files so that you can create 
# and print a word table to take notes on training and test data
# these tables just include the dates and are later than edited using excel, so the file name will
# still be the same as the output listed here, but will contain extra columns.
# 
# Author: Cade
# Date = 4/10/2017
##########################################################
require(lubridate)
#### Landsat 8 ####
#list all the rasters with a 34
# this is actually not processed data, but the data that we downloaded and is prior to processing 
# True, False rids us of headers
list.raster.34= list.files(path = "./Data/L8_SR", pattern = "LC08044034", full.names = T)[c(TRUE,FALSE)]
date = str_sub(list.raster.34,-12,-5)
date2 <- data.frame( "Date" = ymd(date),
                     "Date_raw" = date)
write.csv(date2, "L8_dates_processed.csv", row.names = F)

#### Landsat 5 ####
list.raster.34= list.files(path = "./Data/L5_SR", pattern = "LT05044033", full.names = T)[c(TRUE,FALSE)]
date = str_sub(list.raster.34,-12,-5)
date2 <- data.frame( "Date" = ymd(date),
                     "Date_raw" = date)
write.csv(date2, "L5_dates_processed.csv", row.names = F)
