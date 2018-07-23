'''
Created on Jul 6, 2016
Website used to help build this 
http://gis.stackexchange.com/questions/94092/script-to-extract-by-mask-a-list-of-raster-files
https://rstudio.github.io/reticulate/articles/calling_python.html  --- how to run a python script from R

http://gis.stackexchange.com/questions/157558/batch-extract-by-mask-but-customize-individual-output-names
@author: cade

'''
import arcpy
import glob
import os
import re


from arcpy import env
from arcpy.sa import *
arcpy.CheckOutExtension('Spatial')

def maskArea(myStack, myArea, outDir,cropAppend ):
    outExtractByMask = ExtractByMask(myStack,myArea)
    #outname = os.path.join(outDir,str(myStack),"_croppedArea.tif")
    name1 = re.split('\\\\|/|\\.',myStack)
    name2 = name1[len(name1)-2]
    outname = outDir + '\\' + name2 + "_" + cropAppend + ".tif"
    outExtractByMask.save(outname)


    
