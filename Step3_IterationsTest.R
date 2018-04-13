# TODO: This iterates through training plot classifcation 100 times for 3 split types. 70/30,60/40,30/30
# The final output is a graph of all of them
# this is based on RF_iterations_trial3 and RF_Classification_addedmissing_50_50.tif
# random sampling idea in part because: # http://stackoverflow.com/questions/23479512/stratified-random-sampling-from-data-frame-in-r 
#
# DATE: 11/27/2016
# Author: cade
###############################################################################

## require packages
require(RStoolbox)
require(rgdal)
require(randomForest)
require(caret)
require(e1071)

############ PART1: IMPORT ROI EXTRACTED DATA WITH SPECTRA --CLASSIFICATION TABLE ####################

shp.ext <- read.csv("./Data/L8_2014-2015_test/L8_20142015_traingData.csv")
dim(shp.ext) #7406 13
shp.ext <- na.omit(shp.ext) #7405
dim(shp.ext)
names(shp.ext)
# convert to spatialpointsDataFrame 
shp.ex.sp <-SpatialPointsDataFrame(shp.ext[,c("coords.x1","coords.x2")],
		data = shp.ext[1:11]) ## do not need to include coords data bc it will re-add when we change pack to dataframe

############### PART2: DIVIDE INTO TRAINING AND TEST DATA ########################################

######################### 70/30 Split ############################
cm.it.70= NULL
## loop 100 iterations
for (i in 1:100){
	
	df.sp <- split(shp.ex.sp, list(shp.ex.sp$Date, shp.ex.sp$Class)) #split it up by date and class
	my.samples <- lapply(df.sp,function(x) x[sample(1:nrow(x),size=0.3*nrow(x),FALSE),])
	test.70 <- do.call(rbind,my.samples)
	train.70 <- shp.ex.sp[!(shp.ex.sp$unqID %in% test.70$unqID),]
	dim(test.70)
	dim(train.70)
	
	###### randomForest #########
	#predict RF with test data just spectra
	df_RF <- randomForest(Class ~ UltraBlue + Blue + Green + Red + NIR + SWIR + SWIR2, data = train.70, importance = T)
	
	#apply to test data
	df_RF_test <- predict(df_RF, test.70) #, type="class")
	
	#confusionMatrix Random Forest
	df_RF_cm <- confusionMatrix( data = df_RF_test,
			reference = test.70$Class)	
	df_RF_cm
	#str(df_RF_cm) #determine how to pull out certain portions of the confusion matrix
	
	#pull out portions of confusion matrix and append to cm.it.70
	overall <- df_RF_cm$overall
	overall.accuracy <- overall['Accuracy'] #overall accuracy
	byC <- df_RF_cm$byClass
	
	prod.EMR <- byC['Class: EMR','Sensitivity']
	prod.FLT <- byC['Class: FLT','Sensitivity']
	prod.SAV <- byC['Class: SAV','Sensitivity']
	prod.water <- byC['Class: water','Sensitivity']
	user.EMR <- byC['Class: EMR','Pos Pred Value']
	user.FLT <- byC['Class: FLT','Pos Pred Value']
	user.SAV <- byC['Class: SAV','Pos Pred Value']
	user.water <- byC['Class: water','Pos Pred Value']
	
	cm.df.70= data.frame(overall.accuracy, prod.EMR, user.EMR, prod.FLT,
			user.FLT, prod.SAV, user.SAV, prod.water, user.water, row.names= NULL)
	cm.it.70 = rbind(cm.df.70,cm.it.70)
	#cm.it.70= cbind(overall.accuracy, pos.EMR,pos.FLT,pos.SAV,pos.water,neg.EMR
	#, neg.FLT, neg.SAV, neg.water, cm.it.70)	
}
cm.it.70
cm.it.70$Iterate= c(1:100) 

######################### 60/40 Split ############################
cm.it.60= NULL
## loop 100 iterations
for (i in 1:100){
	
	df.sp <- split(shp.ex.sp, list(shp.ex.sp$Date, shp.ex.sp$Class)) #split it up by date and class
	my.samples <- lapply(df.sp,function(x) x[sample(1:nrow(x),size=0.4*nrow(x),FALSE),])
	test.40 <- do.call(rbind,my.samples)
	train.60 <- shp.ex.sp[!(shp.ex.sp$unqID %in% test.40$unqID),]
	dim(test.40)
	dim(train.60)
	
	###### randomForest #########
	#predict RF with test data just spectra
	df_RF <- randomForest(Class ~ UltraBlue + Blue + Green + Red + NIR + SWIR + SWIR2, data = train.60, importance = T)
	
	#apply to test data
	df_RF_test <- predict(df_RF, test.40) #, type="class")
	
	#confusionMatrix Random Forest
	df_RF_cm <- confusionMatrix( data = df_RF_test,
			reference = test.40$Class)	
	df_RF_cm
	#str(df_RF_cm) #determine how to pull out certain portions of the confusion matrix
	
	#pull out portions of confusion matrix and append to cm.it.70
	overall <- df_RF_cm$overall
	overall.accuracy <- overall['Accuracy'] #overall accuracy
	byC <- df_RF_cm$byClass
	
	prod.EMR <- byC['Class: EMR','Sensitivity']
	prod.FLT <- byC['Class: FLT','Sensitivity']
	prod.SAV <- byC['Class: SAV','Sensitivity']
	prod.water <- byC['Class: water','Sensitivity']
	user.EMR <- byC['Class: EMR','Pos Pred Value']
	user.FLT <- byC['Class: FLT','Pos Pred Value']
	user.SAV <- byC['Class: SAV','Pos Pred Value']
	user.water <- byC['Class: water','Pos Pred Value']
	
	cm.df.60= data.frame(overall.accuracy, prod.EMR, user.EMR, prod.FLT,
			user.FLT, prod.SAV, user.SAV, prod.water, user.water, row.names= NULL)
	cm.it.60 = rbind(cm.df.60,cm.it.60)
}
cm.it.60
cm.it.60$Iterate= c(1:100) 


######################### 50/50 Split ############################
cm.it.50= NULL
## loop 100 iterations
for (i in 1:100){
  
  df.sp <- split(shp.ex.sp, list(shp.ex.sp$Date, shp.ex.sp$Class)) #split it up by date and class
  my.samples <- lapply(df.sp,function(x) x[sample(1:nrow(x),size=0.5*nrow(x),FALSE),])
  test.50 <- do.call(rbind,my.samples)
  train.50 <- shp.ex.sp[!(shp.ex.sp$unqID %in% test.50$unqID),]
  dim(test.50)
  dim(train.50)
  
  ###### randomForest #########
  #predict RF with test data just spectra
  df_RF <- randomForest(Class ~ UltraBlue + Blue + Green + Red + NIR + SWIR + SWIR2, data = train.50, importance = T)
  
  #apply to test data
  df_RF_test <- predict(df_RF, test.50) #, type="class")
  
  #confusionMatrix Random Forest
  df_RF_cm <- confusionMatrix( data = df_RF_test,
                               reference = test.50$Class)	
  df_RF_cm
  #str(df_RF_cm) #determine how to pull out certain portions of the confusion matrix
  
  #pull out portions of confusion matrix and append to cm.it.70
  overall <- df_RF_cm$overall
  overall.accuracy <- overall['Accuracy'] #overall accuracy
  byC <- df_RF_cm$byClass
  
  prod.EMR <- byC['Class: EMR','Sensitivity']
  prod.FLT <- byC['Class: FLT','Sensitivity']
  prod.SAV <- byC['Class: SAV','Sensitivity']
  prod.water <- byC['Class: water','Sensitivity']
  user.EMR <- byC['Class: EMR','Pos Pred Value']
  user.FLT <- byC['Class: FLT','Pos Pred Value']
  user.SAV <- byC['Class: SAV','Pos Pred Value']
  user.water <- byC['Class: water','Pos Pred Value']
  
  cm.df.50= data.frame(overall.accuracy, prod.EMR, user.EMR, prod.FLT,
                       user.FLT, prod.SAV, user.SAV, prod.water, user.water, row.names= NULL)
  cm.it.50 = rbind(cm.df.50,cm.it.50)
}
cm.it.50
cm.it.50$Iterate= c(1:100) 

# add column to each that has a "70/30" and so on
names(cm.it.70)
cm.it.70$split <- "70/30"
names(cm.it.60)
cm.it.60$split <- "60/40"
names(cm.it.50)
cm.it.50$split <- "50/50"

cm.it.all <- rbind(cm.it.70,cm.it.60,cm.it.50)
boxplot(overall.accuracy~split, data = cm.it.all, main= "Testing stability of randomForest Classifier", xlab = "Split Type", ylab = "Overall Accuracy")
#boxplot(count ~ spray, data = InsectSprays, col = "lightgray")
means <- tapply(cm.it.all$overall.accuracy,cm.it.all$split,median)
points1 <-  tapply(cm.it.all$overall.accuracy,cm.it.all$split)
points(means,col="red",pch=18)

boxplot(user.SAV~split, data = cm.it.all, main= "Testing stability of randomForest Classifier", xlab = "Split Type", ylab = "Overall Accuracy")



