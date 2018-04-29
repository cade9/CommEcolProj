# Purpose: Plot L8 images for presentation purposes 
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
# Complete: 4/6/2017
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
outDir = "E:\\cade\\CommEcolProj\\Output\\RGBDisplay"
imgDir = "E:\\cade\\CommEcolProj\\Output"
imgList = list.files(imgDir, pattern = "LC08.*_SR\\.tif$", full.names =F)
for (i in 1:length(imgList)){
  ref <- brick(paste0(imgDir,"\\",imgList[i])) #i
  rDate <- substr(imgList[i],11,18) # i 
   
  jpeg(filename= paste0(outDir,"\\L8_" , rDate,"_TrueColor.jpeg"))
  plotRGB(ref, r= 5, g=4, b=3, stretch = 'lin') # true color
  dev.off()
}
