# TODO: Applying a randomForest classification model to the Spot5 Take5 Images
# This is after all the data have been redone
# DATE: 11/27/2016
# Author: cade
###############################################################################
## require packages
require(raster)
require(RStoolbox)
require(rgdal)
require(randomForest)
require(caret)
require(e1071)
###### working directories #######
outdir = "./Output/Classified"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Part 1: Load in training data csv ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### load in training data full ######
shp.ex <- read.csv("./Data/L8_2014-2015_test/L8_20142015_traingData.csv" ,header = T)
shp.ex <- na.omit(shp.ex)
dim(shp.ex) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Part 2: Convert test and trainig data to a spatial points ####
# dataframe
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### read it in as spatial dataframe and reproject
shp.ex.sp2 <- SpatialPointsDataFrame(shp.ext[,c("coords.x1","coords.x2")],
                                     data = shp.ext[1:11])
proj4string(shp.ex.sp2)=CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
#writeOGR(shp.ex.sp2, dsn = getwd(), "L8_20142015_test_trainingDate", driver ="ESRI Shapefile")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Part 3: Split the training and test data ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# this will be how much data the test data gets, so if you want 30% then enter .3
splitType = 0.3

# split by date and by class 
df.sp <- split(shp.ex.sp2, list(shp.ex.sp2$Date, shp.ex.sp2$Class))

# sample a certain percentage denoted by splitType
my.samples <- lapply(df.sp, function(x) x[sample(1:nrow(x), size=splitType*nrow(x), FALSE),])
# combine all the test samples
testData <- do.call(rbind, my.samples)
# using the uniqueID set all other samples to test
trainData <- shp.ex.sp2[!(shp.ex.sp2$unqID %in% test.50$unqID),]

##### write training and test csv and shapefiles 
test.df <- as.data.frame(test.50)
train.df <- as.data.frame(train.50)
write.csv(test.df, "./Data/L8_2014-2015_test/L8_test_60_1000.csv")
write.csv(train.df, "./Data/L8_2014-2015_test/L8_train_60_1000.csv")
writeOGR(testData , dsn= "./Data/L8_2014-2015_test","testData_60_100", driver= "ESRI Shapefile")
writeOGR(trainData, dsn= "./Data/L8_2014-2015_test","trainData_60_100", driver= "ESRI Shapefile")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Part 4: Create randomForest model ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### Random Forest Model Test the Training data
mod_RF <- randomForest(Class ~ UltraBlue + Blue + Green + Red + NIR + SWIR + SWIR2, data = trainData, importance = T, ntree=1000)
mod_RF
# save model
saveRDS(mod_RF, "mod_RF_updated_50_1000.rds")

#### NOT NEEDED: Typical RandomForest test statistics ####
#apply to test data just as a check
mod_RF_test <- predict(mod_RF, testData)
#confusiom Matrix Random Forest
mod_RF_conMatrix <- confusionMatrix( data = mod_RF_test,
		reference = testData$Class)
mod_RF_conMatrix 




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Part 4: Apply random forest to imagery
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my.raster = list.files(path = "./Data/L8_2014-2015_test", pattern = ".tif$", full.names = T)

for (i in 1:length(my.raster)) {
	
	#load in raster and get date
	L8.raster <- brick(my.raster[i]) #i
	# rename raster 
	names(L8.raster) <-  c("Aerosol","UltraBlue", "Blue","Green","Red","NIR","SWIR","SWIR2")
	
	##apply prediction to image
	classified_rast <- predict(L8.raster,mod_RF)
	
	#create raster
	# get date of the raster for naming purposes
	r.date = unlist(strsplit(my.raster[i],"[/]")) #i
	r.date <- substr(r.date[4],11,18)  
	r.date
	rastername2=paste0("L8_",r.date, "_classified_60")
	## write raster 
	writeRaster(classified_rast, paste0(outdir,"\\",rastername2), format="GTiff",
			overwrite=T) #,datatype = "INT2S" 
	rm(L8.raster)
	rm(classified_rast)                          
	removeTmpFiles(h=0)
	
}


