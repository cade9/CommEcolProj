# Purpose: Crops, merges, and masks Landsat 5, Landsat 7 and Landsat 8 
# path row: path/row 44/33 and 44/34  
# Date: 7/1/2015
# Updated: 
# Author: cade
#######################################################################
#### Packages Required ####
require(raster)
require(rgdal)
library(tidyverse)
library(stringr)
library(lubridate)
library(rgeos)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Part 1: rewrite Shruti file ##### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Other images 
# ./2015/201509_02_63_mosaic_v2_complete_cade.img ~ shruti classifcation just copied 
# shrutiClass <- raster("./2015/201509_02_63_mosaic_v2_complete_cade.img") ~ shrutis written to geotiff
# writeRaster(shrutiClass,"201509_02_63_mosaci_v2_rewrite",format="GTiff", overwrite=T)
# "./2015/201509_02_63_mosaic_v2_complete_cadeENVI" ~ used ENVI to rewrite

# set directories for hyperspectral classifcation and multispectral classifcation
hyperDir <-"E:\\cade\\Classification_CSTARS"
multiDir <- "E:\\cade\\Classification_redo\\Classification_50_2614"

# read in shrutis AVIRIS classifcaiton
shrutiClass <- raster(paste0(hyperDir,"./2015/201509_02_63_mosaic_v2_complete_cadeENVI"))
NAvalue(shrutiClass) <- 0 # set 0 to NA values 

# read in the closest classifcation image 
s5_0903 <- raster("./S5_0903_redo_50.tif") # thesis classifcation S5 
l8_0812 <- raster("E:\\cade\\SPOT5Take5_11_25_2016\\L8_Classification\\L8_class50\\L8_224_class50.tif")



r <- sampleRandom(shrutiClass, size = 6000 , xy = TRUE, sp=TRUE, na.rm = TRUE)




###### Examples ####
# 
# shrutiClass[shrutiClass %in% c(2,3,5,7,8,10,12)] <- 1 #
# shrutiClass[shrutiClass %in% c(6,11,13,14)] <- 2 #
# shrutiClass[shrutiClass %in% c(9)] <- 3 #
# shrutiClass[shrutiClass %in% c(15)] <- 4 #
# Create a random raster 
# r <- raster(ncol=30,nrow=20)
# r[] <- 1:ncell(r)
# 
# x <- sampleRandom(r, ncell(r)*.3, asRaster=TRUE)
# x <- sampleStratified(shrutiClass, size = 4, na.rm=TRUE, sp=T, xy = T)  This doesnt work because apparently 
# some of the classifcations do not have enough values 