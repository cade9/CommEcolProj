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
##############################################
areaDir <- "E:\\cade\\CommEcolProj\\Output\\AreaCalc_Apr29"
myFiles <- list.files(areaDir, pattern =".csv", full.names = F)

for (i in 1:length(myFiles)){
  # read in area csv and remove the unnecessary columns 
  myArea <- read_csv(paste0(areaDir,"/",myFiles[i])) %>% select(-c(X1)) # i 
  # write the landcovers correctly
  myArea$landcover <- c("EMR","FLT","SAV","Water")
  # remove x
  names(myArea) <- sub("X","",names(myArea))
  
  # melt data talbe into the correct format
  meltArea <- melt(myArea)
  meltArea <- na.omit(meltArea)
  names(meltArea) <- c("Class","layer","Date","Area")
  meltArea$Date <- ymd(meltArea$Date)
  
  # plot the scatter of all 4 variables
  MyScatterplot <- meltArea %>%
    ggplot(aes(x = Date, y = Area, col = Class)) +
    geom_point() + geom_line() +scale_x_date(breaks = pretty_breaks(10)) + 
    scale_y_continuous( name ="Area (km^2)" ) + # breaks = seq(0, 10, by = 2)
    scale_color_manual(values=c("#FFAA00", "#C500FF", "#55FF00","#0070FF")) +
    theme_classic()
  MyScatterplot
  
  nameArea <- unlist(strsplit(myFiles[i],"[.]")) #i
  nameArea1 <- nameArea[1]
  ggsave(paste0(areaDir,"/areaplot_colorCorrect_",nameArea1,".png"))
  
  # plot all  without water 
  noWater <- meltArea %>% filter(!Class == "Water") %>%
    ggplot(aes(x = Date, y = Area, col = Class)) +
    geom_point() + geom_line() +scale_x_date(breaks = pretty_breaks(10)) + 
    scale_y_continuous( name ="Area (km^2)" ) + # breaks = seq(0, 10, by = 2)
    scale_color_manual(values=c("#FFAA00", "#C500FF", "#55FF00")) +
    theme_classic()
  noWater
  ggsave(paste0(areaDir,"/areaplot_colorCorrect_",nameArea1,"_nowater.png"))
  
  # plot all 4 without water 
  noWater2 <- meltArea %>% filter(!Class == "Water") %>%
    ggplot(aes(x = Date, y = Area, col = Class)) +
    geom_point() + geom_smooth() +scale_x_date(breaks = pretty_breaks(10)) + 
    scale_y_continuous( name ="Area (km^2)" ) + # breaks = seq(0, 10, by = 2)
    scale_color_manual(values=c("#FFAA00", "#C500FF", "#55FF00")) +
    theme_classic()
  noWater2
  ggsave(paste0(areaDir,"/areaplot_colorCorrect_",nameArea1,"_nowater_smooth.png"))

}







p1 <- ggplot(meltArea,aes(x = Year, Y = Area))
p1 + geom_point()
p1

myArea <- lapply(myFiles,read_csv)