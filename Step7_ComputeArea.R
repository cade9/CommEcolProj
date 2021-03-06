# TODO: COMPUTES THE AREA  of each land cover 
# # page 187 and 204
# Author: cade
###############################################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Part 1:EXTRACT BY MASK IN ARCMAP####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## List classified files 
classDir <- "./Output/Classified_Apr29"
rastClassList <- list.files(classDir, pattern ="_Apr29\\.tif$",full.names = F)
# extract dates 
mydates.all <- substr(rastClassList,4,11) 
#stack rasters 
classifiedStack <- stack(paste0(classDir,"/",rastClassList))
names(classifiedStack) <- mydates.all
# write raster 
writeRaster(classifiedStack,paste0(classDir,"/","L8_classified_apr29_stack"), format="GTiff", overwrite=T)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Part 1: Extract dates and list classification files####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## List cropped files 
class.stack <- stack(paste0(classDir, "/L8_classified_apr29_stack_crop.tif"))
# rename brick
names(class.stack) <- mydates.all

#determine unique classes and pixel size
uniqueClasses <- unique(class.stack[[1]]) ## classes
resSat <- res(class.stack[[1]]) # pixel resolution

## List crop regions
shapesDir <- "./Data/Vector/DifferentArea_CropBoxes"
myShps <- list.files(shapesDir,pattern ="shp$")[c(1,2,4,9)]
myShps <- c("BigBreak","LibertyIsland","ShermanIsland","VeniceWard","shruti_waterways_merge")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Part 2: Loop Throught Extracts ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


for (i in 2:length(myShps)) {
  area.all <- data.frame(landcover = uniqueClasses)
  # read in shapefile
  roi <- readOGR(dsn =shapesDir, layer = myShps[i]) #i
  #mask by shapefile
  #rast.crop <- class.stack
  rast.crop <- mask(crop(class.stack, roi),roi, df=T) 
  area.all$Layer <- myShps[i] #i 
  for (j in 1:nlayers(rast.crop)){
    layer1 <- rast.crop[[j]] #j
    #mydate = paste0(unlist(strsplit(names(layer1),"_"))[2],"_area_km2")
    mydate = names(layer1)
    layer.freq <- freq(layer1, useNA="no")
    area_km2 <- data.frame(area_km2 = layer.freq[,"count"] * prod(resSat) *1e-06)
    names(area_km2) <- mydate
    area.all <- cbind(area_km2,area.all)
  }
  write.csv(area.all,paste0("./Output/AreaCalc_Apr29/",myShps[i],".csv"))
}









setwd("E:\\cade\\SPOT5Take5_11_25_2016\\S5_Classification")
#write.csv(area.all, "area_classes_50up_layerstack_ww.csv",row.names=FALSE)

#rename classes
area.all$landcover <- c("EMR","FLT","SAV","Water")
#melt to get in appropriate form for plotting
area.all.melt <- melt(area.all, id.vars='landcover')
names(area.all.melt) <- c("landcover","day","area_km2")
area.all.melt2 <- area.all.melt[rev(rownames(area.all.melt)),] # rev order 

##ggplot
p <-ggplot(area.all.melt2, aes(x=day,y=area_km2))
p + geom_area(aes(colour = landcover, fill = landcover), position="stack",size=3)


# http://stackoverflow.com/questions/4651428/making-a-stacked-area-plot-using-ggplot2 







### original for a single type 
final.area <- data.frame(landcover = uniqueClasses,)

layer1 <- class.stack[[1]]
resSat <-res(layer1)
uniqueClasses <- unique(layer1)

layer.freq <- freq(layer1, useNA="no")
area_km2 <- layer.freq[,"count"] * prod(resSat) *1e-06
layer1stuff <- data.frame(landcover = uniqueClasses, area_km2 = area_km2)

