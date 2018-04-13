# TODO: Add this extracts the data from the files 
# 
# Author: cade
###############################################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create the test dataset for 2014 - 2015 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Test for for 2014-2015 Landsat 8 imagery on 4/10/2018 #### 
# move the data to the correct place. Used the SR data with no final except 20140422

# List all the L8 files that have SR appended to them 
L8 <- list.files("./Output", pattern = "LC08.*_SR\\.tif$")

# list the dates that are 2014 and 2015
L8dates <- str_subset(L8, "2014|2015")

# Move the correct files over
newDir <- "./Data/L8_2014-2015_test/" # new directory
origDir <- "./Output/" # old directory
file.copy(paste0(origDir,L8date),paste0(newDir,L8dates))

# delete 20140505 by hand
file.remove(paste0(newDir,L8dates[4]))
# copy 20150422_finalSF by hand
# L8dates[15]
file.remove(paste0(newDir,L8dates[15]))
file.copy(paste0(origDir,"LC080440332015042201T1_finalSR.tif"),paste0(newDir,"LC080440332015042201T1_finalSR.tif") )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



##############################
## raster list
rast.dir = "./Data/L8_2014-2015_test" #where rasters live
my.raster = list.files(rast.dir,pattern =".tif$")	  # delete ones with no training data

## shapefile list
shp.dir <- "./Data/Vector/ROI_shp"
my.shp = list.files(shp.dir, pattern="*.shp$") 
################################################### PART1: LOOP  ###########################################

shp.ex = NULL #empty list for extracting of shapefiles, must append to this list

# loop for extraction
for (i in 1:length(my.raster)) {
	for (j in 1:length(my.shp)){
		
		# date for raster
		#r.dates = unlist(strsplit(my.raster[1],"[_]")) #i   ~~ will have to change if you change the names 
		r.dates = substr(my.raster[i], 11,18) #i
		r.dates
		
		# read in raster
		my.stack = stack(paste0(rast.dir,"\\",my.raster[i])) #i
		names(my.stack) <-c("Aerosol", "UltraBlue","Blue","Green","Red","NIR","SWIR","SWIR2")
		
		# shapefile name 
		myname = unlist(strsplit(my.shp[j],"[_|.]")) #j
		
		# shapefile date
		s.dates = str_c(myname[2],myname[3], collapse ="") 
		s.dates 
		
		# shapefile class
		my.class = myname[4]
		my.class
		
		# use to read in shapefile
		layer.name = unlist(strsplit(my.shp[j],"[.]")) #j
		layer.name = layer.name[1]
		layer.name
		
		# read in roi
		roi = readOGR(dsn =shp.dir, layer = layer.name)
		roi = roi[,!names(roi) %in% "ID"] # delete layer name
		
		if(r.dates == s.dates){
			my.extract <- raster::extract(my.stack, roi, sp =T) #when you convert from sp to df it adds coord columns
			my.extract$Date = paste(r.dates)
			my.extract = as.data.frame(my.extract, row.names = NULL)
			my.extract <- my.extract[, c("Date", "Class", "Aerosol","UltraBlue","Blue","Green","Red","NIR","SWIR","SWIR2","unqID","coords.x1","coords.x2")]
			shp.ex= rbind(my.extract,shp.ex)
		}
		}
}


################################################### PART2: WRITE TO CSV  ###########################################
out.dir<- "./Data/L8_2014-2015_test/"
write.csv(shp.ex, file = paste0(out.dir,"L8_20142015_traingData.csv"),row.names=FALSE)

###did not actually run 
#shp.ex.sp <-SpatialPoints(coords=shp.ex[,c("coords.x1","coords.x2")]) #page 73
#writeOGR(shp.ex.sp,dsn=out.dir, layer = "all_training", driver= "ESRI Shapefile",overwrite_layer= T)


