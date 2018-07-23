  # TODO: Cleaned up version of add_unqID_toshp.R
# DATE: 11/26/2016
# This adds a class column and a unqID column to the shapefiles that have been created from photo-interpretation 
# Comments added for julia: 3/1/2017
# NOTES: line 18 can be changed for particular date using 
# my.shp = list.files(".", pattern="0421.*shp$") # select a particular date
# Author: cade
###############################################################################
#### Packages Required ####
require(raster)
require(rgdal)
library(tidyverse)
library(stringr)
library(lubridate)
library(rgeos)
require(rgeos)
require(maptools)
require(sp)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Completed for 2014-2015 Landsat 8 imagery on 4/10/2018
# output is 
# "./Data/Vector/ROI_shp"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Part 1: Unzip files ##### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## List all shapefiles that do not have any extra information 
my.shp = list.files("./Data/Vector/ROIS", pattern=".shp$")
myDsn <- "./Data/Vector/ROIS"

'/change out directory'
out.dir ="./Data/Vector/ROI_shp" #does not overwrite the shapefiles

##################################################################################
## loop to add coordinates 
for (i in 2:length(my.shp)) {
  
  # file name # EX: "L8"   "2014" "0113" "EMR"  "shp"
  myname = unlist(strsplit(my.shp[i],"[_|.]")) #i 
  myname
  
  # sensor 
  my.sensor = myname[1]
  
  # date # "20140113"
  # combine year and date into one 
  s.dates = str_c(myname[2],myname[3], collapse ="") 
  s.dates 
  
  # class # "EMR"
  my.class = myname[4]
  my.class
  
  # use to read in shapefile 
  layer.name = unlist(strsplit(my.shp[i],"[.]")) #i
  layer.name = layer.name[1]
  layer.name
  
  # read in shapefile
  roi = readOGR(dsn = myDsn, layer = layer.name)
  proj4string(roi)=CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") #change projection
  
  # add class column
  roi$Class = my.class
  
  # add date column
  roi$date <- s.dates
  
  # add uniqueID
  coordNE <- as.data.frame(coordinates(roi)) # read coordinates and turn into datagram
  colnames(coordNE) <- c("E","N")# change column names
  coordNE$x <- paste0(my.sensor,"_",s.dates,"_",my.class,"_",coordNE$E,"E_",coordNE$N,"N")
  roi$unqID <- coordNE$x
  # write out new shapefile
  writeOGR(roi,dsn=out.dir, layer = paste0(layer.name), driver= "ESRI Shapefile",overwrite_layer= T)
}

