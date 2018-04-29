# TODO: Compute Accuracy Statistics
# DATE: 11/26/2016
# Author: cade
###############################################################################

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Part 1: List Images and get test data####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#list classifed images
dirClassified <-  "./Output/Classified_Apr29"
my.raster = list.files(dirClassified, pattern=".tif$", full.names = T)

# get test data
testData <- readOGR(dsn='./Data/L8_2014-2015_test',"testData_40_Apr29")

## determine uniqueClasses
uniqueClasses <- unique(testData$Class)
#uniqueClasses <- rev(uniqueClasses)
uniqueClasses

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Part 2: Create and print confusion matrices for all dates####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cm.all3 <- NULL
for (i in 1:length(my.raster)) {
	#read in classified raster
	classified.rast <-  raster(my.raster[i]) #i
	
	# get raster date
	r.date = unlist(strsplit(my.raster[i],"[/]")) #i 
	r.date = r.date[4]
	r.date = substr(r.date,4,11) 
	r.date

	## separate out test.data by data and ensure it matches the raster data
	test.dates <-testData[testData$Date == r.date,] 
	
	# extract observed data from the rasters by test dates 
	obs <- raster::extract(classified.rast, test.dates) # numbered values 
	# convered observed data from numeric data to classfied value
	test.obs.factor <- uniqueClasses[obs] 
	# class my test data says that it is 
	val <- test.dates$Class
	
	## confusionMatrix of Random forest on the correct test data
	mod_RF_conMatrix <- confusionMatrix( data = test.obs.factor,
			reference = val) 
	mod_RF_conMatrix
	## save cm output
	'/ this saves both the confusion matrix as it is printed in the console and a table with all values (cm.all3)'
	cm <-capture.output(print(mod_RF_conMatrix), file = paste0(dirClassified,
					"\\",r.date,"_CMoutput.txt"), append = TRUE)
	
	## add accuracy to a table use str(cm) to determine what the variables are called
	overall <- mod_RF_conMatrix$overall
	overall.accuracy <- overall['Accuracy']
	## will compute the accuracy statistics for all
	byC <- mod_RF_conMatrix$byClass
	prod.EMR <- byC['Class: EMR','Sensitivity']
	prod.FLT <- byC['Class: FLT','Sensitivity']
	prod.SAV <- byC['Class: SAV','Sensitivity']
	prod.water <- byC['Class: water','Sensitivity']
	user.EMR <- byC['Class: EMR','Pos Pred Value']
	user.FLT <- byC['Class: FLT','Pos Pred Value']
	user.SAV <- byC['Class: SAV','Pos Pred Value']
	user.water <- byC['Class: water','Pos Pred Value']
	
	cm.all= data.frame(r.date, overall.accuracy, prod.EMR, user.EMR, prod.FLT, user.FLT, prod.SAV, user.SAV, prod.water, user.water, row.names= NULL)
	cm.all3 = rbind(cm.all,cm.all3)
	
	rm(classified.rast)                          
	removeTmpFiles(h=0)
}

write.csv(cm.all3, paste0(dirClassified,"/cm_20142015Test_apr29.csv"))
