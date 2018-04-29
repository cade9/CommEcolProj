# TODO: Using the area calculations make plots of areal change over time 
# date: 4/29/2018
# Author: cade
##############################################################################
require(tidyverse)
require(reshape2)
require(ggplot2)
require(lubridate)
require(scales)
require(ggthemes)
###################
setwd("C:\\Users\\cade\\Documents\\PhDMerced\\Spr18Courses\\CommunityEcology\\csvarea")
myFiles <- list.files(pattern =".csv")

for (i in 1:length(myFiles)){
  myArea <- read_csv(myFiles[i]) # i 
  myArea
  myArea$landcover <- c("EMR","FLT","SAV","water")
  names(myArea) <- sub("X","",names(myArea))
  
  meltArea <- melt(myArea)
  meltArea <- na.omit(meltArea)
  names(meltArea) <- c("Class","layer","Date","Area")
  meltArea$Date <- ymd(meltArea$Date)
  
  MyScatterplot <- meltArea %>%
    ggplot(aes(x = Date, y = Area, col = Class)) +
    geom_point() + geom_line() +scale_x_date(breaks = pretty_breaks(10)) + 
    scale_y_continuous( name ="Area (km^2)" ) + # breaks = seq(0, 10, by = 2)
    scale_color_manual(values=c("#FFAA00", "#C500FF", "#55FF00","#0070FF")) +
    theme_classic()
  MyScatterplot
  
  nameArea <- unlist(strsplit(myFiles[i],"[.]")) #i
  nameArea1 <- nameArea[1]
  ggsave(paste0("areaplot_colorCorrect_",nameArea1,".png"))
  
}







p1 <- ggplot(meltArea,aes(x = Year, Y = Area))
p1 + geom_point()
p1

myArea <- lapply(myFiles,read_csv)