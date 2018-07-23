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
NAvalue(shrutiClass) <- 0 # set 9 to NA values 
#shrutiClass[shrutiClass == 0 ] <- NA
#na is -9999

# read in the closest classifcation image 
# list of raster images
rasterList <- c(paste0(multiDir,"/S5_0903_redo_50.tif"), # thesis classifcation 
                paste0(multiDir,"/S5_0908_redo_50.tif"), # thesis classifcaition 
                "E:\\cade\\SPOT5Take5_11_25_2016\\L8_Classification\\L8_class50\\L8_224_class50.tif",
                paste0(hyperDir,"/2015/201509_02_63_mosaic_v2_complete_Copy_regrouped_renumbered_arcmap.tif"),
                "E:\\cade\\CommEcolProj\\Output\\Classified_Apr29\\L8_20150812_classified_60_Apr29.tif")

# need a list of the shape files and to extract the extent below 
shpDir <- "E:\\cade\\CommEcolProj\\Data\\Vector\\DifferentArea_CropBoxes"
shpList <- list.files(shpDir, pattern = "shp$")[c(1,2,5,6,9)]
shpList <- gsub(".shp","", shpList)

# previous issues with masking and croping the different files 
# cropped all in arcmap using extract by mask
# created new liberty island shapefile entiled may24

#shrutiCropList <- list.files(paste0(hyperDir,"/2015"), pattern = "\\.*tif$", full.names = T)[-c(1,7)]

# out  directory
dirOut <- "E:\\cade\\CommEcolProj\\Output\\CompareAVIRIS"
cm.all3 <- NULL
pixelCount <- NULL
L8Rast <- raster(rasterList[5]) 

### So we really want to run this code probably like 100 times and then plot a boxplot
for(x in 1:100){
  for(i in 1:length(shpList)){
    # ShapefileCrop
    shpFile <- readOGR(shpDir, shpList[1]) #i 
    shpExt <- extent(shpFile)
    subArea = shpList[i] #i
    #read in landsat
    L8RastCrop <- crop(mask(L8Rast,shpFile), shpFile)
    
    # count pixels
    layer.freq <- freq(L8RastCrop, useNA="no")
    n1 <- ceiling(min(layer.freq[,"count"]) * 0.6)
    n2 <- data.frame(subregion = names(shrutiClass),
                     numberSamples = n1)
    nCount <- rbind(n2,nCount)
    
    
    # sample pixels
    rSample <- sampleStratified(L8RastCrop, size = n1, xy = TRUE, sp=TRUE, na.rm = TRUE) 
    
    for(j in 1:4){
      rast <- raster(rasterList[j]) #j
      NAvalue(rast) <- 0

      # Extract at a lower resolution image
      testExt <- raster::extract(rast, rSample, sp= TRUE)
      testExt <- na.omit(testExt)
      
      # change names
      names(testExt)[4] <- "L8Class" 
      
      # confusion matrix
      mod_RF_conMatrix <- confusionMatrix(data = testExt[[5]],
                                          reference = testExt$L8Class) 
      mod_RF_conMatrix
      
      rasterName <- names(testExt)[5]
      
      ## save cm output
      cm <-capture.output(print(mod_RF_conMatrix), 
                          file = paste0(dirOut,"\\",rasterName,"_",subArea,"_CMoutputMay27_it",x,".txt"), append = TRUE) 
      
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
      
      iteration = x
      
      cm.all= data.frame(rasterName, subArea, overall.accuracy, prod.EMR, user.EMR, prod.FLT, user.FLT, prod.SAV, user.SAV, prod.water, user.water, row.names= NULL,iteration)
      cm.all3 = rbind(cm.all,cm.all3)
      
      
      rm(testExt)
      rm(rast)
    }
    rm(rSample)
    rm(L8RastCrop)
    removeTmpFiles(h=0)
  }
  
}


write.csv(cm.all3,paste0(dirOut,"\\compareAVIRIScm_may27.csv"))

pointPlot <- function (k, xVar, yVar) {
    ggplot(k, aes(x = k[xVar], k[yVar])) + geom_point() +  scale_x_continuous(name = xVar) +
    scale_y_continuous(name = yVar)
}

my.plots <- map(my.list$data,pointPlot,xVar = "Temperature",yVar = "Turbidity")

myPlot <- function (k, xVar, yVar) {
  ggplot(k, aes(x = k[xVar], y= k[yVar])) + geom_boxplot()
}

test <- cm.all3 %>% melt() %>% group_by(subArea, rasterName) %>%
  nest() 


my.plots <- map(test$data,myPlot,xVar = "variable",yVar = "value")
#### plot 

test2 <- filter(cm.all3, subArea== "ShermanIsland" & rasterName == "X201509_02_63_mosaic_v2_complete_Copy_regrouped_renumbered_arcmap")
test2 <- test2[,-12]
melt2 <- melt(test2)
boxplot(value~variable, data = melt2)

myPlot(melt2, xVar = "variable", yVar = "value")

p <- ggplot(melt2, aes(x = "variable", y = "value")) + geom_boxplot()


tes

