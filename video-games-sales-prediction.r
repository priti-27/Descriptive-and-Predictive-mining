rm(list=ls()) #remove all variable stored previously

vg<- read.csv("C:/Users/priya/Desktop/Game Research/Video_Games_Sales_as_at_22_Dec_2016.csv")
library(reshape2)
library(dplyr)
library(plotly)
library(caret)
library(corrplot)
library(RColorBrewer)
vg<- read.csv("C:/Users/priya/Desktop/Game Research/Video_Games_Sales_as_at_22_Dec_2016.csv")
vg$Year_of_Release <- as.numeric(as.character(vg$Year_of_Release))
vg$User_Score <- as.numeric(as.character(vg$User_Score))
vg[vg==""] <-NA
vg$Genre <- as.character(vg$Genre)
vg$Genre[vg$Genre=="Role-Playing"] <-"RolePlaying"
vg <- vg %>% filter(vg$Year_of_Release<=2016)
vg <- vg%>% arrange(desc(Year_of_Release))