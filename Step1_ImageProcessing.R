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
source("./R/LandsatUntar.R")
source("./R/clipMask.R")
source("./R/brickClip.R")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Part 1: Unzip files ##### 
# Complete: 4/6/2017
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Untar and unzip files
myDatasets <- list.files("./Data/L8_SR", pattern = ".tar.gz", full.names = T)
lapply(myDatasets, landsatUntar)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Part 2: Lists files #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
outDir= "./Output/"

########### Bounding box ####
boundingBox = readOGR(dsn="Data/Vector",layer ="s5t5_clipbox2")

########## Lists of rasters layers ####
#### List surface reflectance layers previous-- deleted  ####
# myDirs <- dir("./Data/test", pattern = "SR$", full.names = T)
# ## Select folders that are Landsat 5 and Landsat 8
# L5 <- list.files(myDirs[1], full.names = T)
# L8 <- list.files(myDirs[2], full.names = T)

#### List surface reflectance folders ####
#List raster stacks
list.raster.33 = list.files(path = "./Data/L8_SR", pattern = "LC08044033", full.names = T)[c(TRUE,FALSE)]
list.raster.34= list.files(path = "./Data/L8_SR", pattern = "LC08044034", full.names = T)[c(TRUE,FALSE)]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Part 3: Define Cloud Mask Values  #####
# CLIP AND MERGE THE TWO TILE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Read in mask change values 
L5MaskReclass <- read_csv("./Data/ancillary/L5_sr_cloud_qa_values.csv")
L8MaskReclass <- read_csv("./Data/ancillary/L8_pixel_qa_values.csv")
L5MaskValues <- unique(L5MaskReclass$Pixel_Value[L5MaskReclass$NewValue == 0])
L5ClearValues <- unique(L5MaskReclass$Pixel_Value[L5MaskReclass$NewValue == 1])
L8MaskValues <- unique(L8MaskReclass$Pixel_Value[L8MaskReclass$NewValue == 0])
L8ClearValues <- unique(L8MaskReclass$Pixel_Value[L8MaskReclass$NewValue == 1])

for (i in 17:length(list.raster.33)) {
  ### Masks Processing ###
  
  # clip the masks to clip area
  mask33 <- clipMask(list.raster.33[i], boundingBox) # i
  mask34 <- clipMask(list.raster.34[i], boundingBox) # i
  # merge the masks
	mergeMask <- merge(mask34,mask33,overlap = TRUE, ext = boundingBox) #13
	rm(mask33)
	rm(mask34)
	# change the values of the clouds for L8
	mergeMask[mergeMask %in% L8MaskValues] <- NA  # set clouds to 0
	mergeMask[mergeMask %in% L8ClearValues] <- 1 # set clear to 1

	### Spectral Stacking ### 
	# load reflectance 33 and 34 as a brick
	sf_33 <- brickClip(list.raster.33[i], boundingBox) #i 
	sf_34 <- brickClip(list.raster.34[i], boundingBox) #i
	# merge reflectance
	mergeRef <- merge(sf_34,sf_33, overlap = TRUE)
	rm(sf_33)
	rm(sf_34)
	
	#mask reflectance
	finalRef <- mergeRef * mergeMask
	
	rastername = str_sub(list.raster.33[i],-22,-1) #i
	rastername2=paste0(outDir,rastername,"_finalSR")
	writeRaster(finalRef, rastername2, format="GTiff", overwrite=T, dataType = dataType(finalRef))
	writeRaster(mergeRef, paste0(outDir,rastername,"_SR"),format="GTiff", overwrite=T, dataType = dataType(mergeRef))
	writeRaster(mergeMask, paste0(outDir,rastername,"_CCmask"),format="GTiff", overwrite=T, dataType = dataType(mergeMask))
	
	rm(mergeRef)
	rm(mergeMask)
	rm(finalRef)
	removeTmpFiles(h=0)
}


