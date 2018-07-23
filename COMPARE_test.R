# Purpose: Compares CSTAR classification to masters thesis and community ecology project 
# Date: 05/23/2018
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
require(caret)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Part 1: Convert Shruti Classification to 4 classes and masked ##### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This was completed in ENVI using the post classification toolset andt 
# the ROI tool
# 1) In main envi. Tools -> post classification -> reassign groups 
#
#
# http://www.harrisgeospatial.com/Support/SelfHelpTools/HelpArticles/HelpArticles-Detail/TabId/2718/ArtMID/10220/ArticleID/18580/How-to-change-the-classification-values-pixel-values-in-an-ENVI-Classic-classification-image.aspx

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Part 2: Read in rasters ##### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### Main images ####
# set directories for hyperspectral classifcation and multispectral classifcation
hyperDir <-"E:\\cade\\Classification_CSTARS"
multiDir <- "E:\\cade\\Classification_redo\\Classification_50_2614"

# read in shrutis AVIRIS classification
shrutiClass <- raster(paste0(hyperDir,"/2015/201509_02_63_mosaic_v2_complete_Copy_regrouped_renumbered_arcmap.tif"))
#NAvalue(shrutiClass) <- 0 # set 9 to NA values 
#shrutiClass[shrutiClass == 0 ] <- NA
#na is -9999

# read in the closest classifcation image 
# list of raster images
rasterList <- c(paste0(multiDir,"/S5_0903_redo_50.tif"), # thesis classifcation 
                paste0(multiDir,"/S5_0908_redo_50.tif"), # thesis classifcaition 
                "E:\\cade\\SPOT5Take5_11_25_2016\\L8_Classification\\L8_class50\\L8_224_class50.tif",
                "E:\\cade\\CommEcolProj\\Output\\Classified_Apr29\\L8_20150812_classified_60_Apr29.tif")

# need a list of the shape files and to extract the extent below 
# shpDir <- "E:\\cade\\CommEcolProj\\Data\\Vector\\DifferentArea_CropBoxes"
# shpList <- list.files(shpDir, pattern = "shp$")[c(1,2,5,9)]
# shpList <- gsub(".shp","", shpList)

# previous issues with masking and croping the different files 
# cropped all in arcmap using extract by mask
# created new liberty island shapefile entiled may24

shrutiCropList <- list.files(paste0(hyperDir,"/2015"), pattern = "\\.*tif$", full.names = T)[-c(1,7)]

# out  directory
dirOut <- "E:\\cade\\CommEcolProj\\Output\\CompareAVIRIS"
cm.all3 <- NULL

for(i in 1:length(rasterList)){
  multRast <- raster(rasterList[i]) #i
  
  
  for(j in 2:length(shrutiCropList)){
    # Extent of shapefile 
    #shpFile <- readOGR(shpDir, shpList[j]) #j 
    #shpExt <- extent(shpFile)
    #shrutiClassCrop <- crop(mask(shrutiClass, shpFile), shpFile)
    shrutiClassCrop <- raster(shrutiCropList[j]) #j 
    NAvalue(shrutiClassCrop) <- 0
    #shrutiClassCrop[shrutiClassCrop == 0] <- NA
    
    # random sample shrutiClass
    #rSample <- sampleRandom(shrutiClass, size = 5 , xy = TRUE, sp=TRUE, na.rm = TRUE, ext = shpExt)
    rSample <- sampleStratified(shrutiClassCrop, size = 60000 ,exp = 20, xy = TRUE, sp=TRUE, na.rm = TRUE) 
    #rSample <- raster::extract(shrutiClass, x1)
    
    # Extract at a lower resolution image
    testExt <- raster::extract(multRast, rSample, sp= TRUE)
    testExt <- na.omit(testExt)
    
    # change names
    names(testExt)[4] <- "shrutiClass" 
    
    # confusion matrix
    mod_RF_conMatrix <- confusionMatrix(data = testExt[[5]],
                                        reference = testExt$shrutiClass) 
    mod_RF_conMatrix
    
    rasterName <- names(testExt)[5]
    subArea = shpList[j] #j
    
    ## save cm output
    cm <-capture.output(print(mod_RF_conMatrix), 
                        file = paste0(dirOut,"\\",rasterName,"_",subArea,"_CMoutputMay23.txt"), append = TRUE) 
    
    ## add accuracy to a table use str(cm) to determine what the variables are called
    overall <- mod_RF_conMatrix$overall
    overall.accuracy <- overall['Accuracy']
    ## will compute the accuracy statistics for all
    byC <- mod_RF_conMatrix$byClass
    prod.EMR <- byC['Class: 1','Sensitivity']
    prod.FLT <- byC['Class: 2','Sensitivity']
    prod.SAV <- byC['Class: 3','Sensitivity']
    prod.water <- byC['Class: 4','Sensitivity']
    user.EMR <- byC['Class: 1','Pos Pred Value']
    user.FLT <- byC['Class: 2','Pos Pred Value']
    user.SAV <- byC['Class: 3','Pos Pred Value']   
    user.water <- byC['Class: 4','Pos Pred Value']
    
    
    cm.all= data.frame(rasterName, subArea, overall.accuracy, prod.EMR, user.EMR, prod.FLT, user.FLT, prod.SAV, user.SAV, prod.water, user.water, row.names= NULL)
    cm.all3 = rbind(cm.all,cm.all3)
    
    rm(rSample)
    rm(testExt)
    rm(shrutiClassCrop)
  }
  rm(multRast)
  removeTmpFiles(h=0)
}

write.csv(cm.all3,paste0(dirOut,"\\compareAVIRIScm_may23.csv"))








#### Graveyard ####

s5_0903 <- raster(paste0(multiDir,"./S5_0903_redo_50.tif")) # thesis classifcation S5 date 0903 
s5_0908 <- raster(paste0(multiDir,"./S5_0908_redo_50.tif")) # thesis classificantion s5 date 0908
l8_0812 <- raster("E:\\cade\\SPOT5Take5_11_25_2016\\L8_Classification\\L8_class50\\L8_224_class50.tif")
l8_0812_apr29 <- raster("E:\\cade\\CommEcolProj\\Output\\Classified_Apr29\\L8_20150812_classified_60_Apr29.tif")




test_0812 <- raster::extract(l8_0812, rSample, sp= TRUE)
classKey <- read_csv("E:\\cade\\Classification_CSTARS\\reclassifyShruti.csv") %>%
  select(shrutiClass,shrutiNewClass)
new <- merge(test_0812, classKey, by.x = "X201509_02_63_mosaic_v2_complete_cadeENVI", by.y = "shrutiClass",all.x = TRUE)
new <- na.omit(new)
# test for new 
# extract observed data from the rasters by test dates 

mod_RF_conMatrix <- confusionMatrix( data = new$L8_224_class50,
                                     reference = new$shrutiNewClass) 
mod_RF_conMatrix



rsample <- sampleStratified(shrutiClass, size = 10000, na.rm=TRUE, sp=T, xy = T)

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

# randomly unselect 70% of the data