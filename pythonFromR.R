require(arcgisbinding)
arc.check_product()
library(help = "arcgisbinding")
require(reticulate)

os <- import("os")
glob <- import("glob")
arc <- import("arcpy")
py_help(arcpy)
source_python("E:\\cade\\CommEcolProj\\maskArea.py")


mRast <- "E:\\cade\\CommEcolProj\\test\\L8_20130416_classified_60_Jun6.tif"
shp <- "E:\\cade\\CommEcolProj\\test\\shruti_waterways_merge.shp"
outDir <- "E:\\cade\\CommEcolProj\\test"
nameAppend = "shruti" 
maskArea(mRast,shp,outDir,nameAppend)


# https://github.com/R-ArcGIS/r-bridge 
# https://rstudio.github.io/reticulate/articles/calling_python.html
# CALLING PYTHON FROM R ^^ SUPER USEFUL


## other usefule links
# https://r-arcgis.github.io/assets/arcgisbinding-vignette.html
# https://esricanada-ce.github.io/r-arcgis-tutorials/4-Building-an-R-Script-Tool.pdf 
# https://www.r-bloggers.com/run-python-from-r/ 
# https://www.r-bloggers.com/run-python-from-r/ 


# WHAT THE PYTHON CODE LOOKS LIKE
#import arcpy
#import glob
#import os

# from arcpy import env
# from arcpy.sa import *
#   arcpy.CheckOutExtension('Spatial')
# 
# def maskArea(myStack, myArea, outDir):
#   outExtractByMask = ExtractByMask(myStack,myArea)
# #outname = os.path.join(outDir,str(myStack),"_croppedArea.tif")
# outname = outDir + '\\' + "test" +"_croppedArea.tif"
# outExtractByMask.save(outname)

