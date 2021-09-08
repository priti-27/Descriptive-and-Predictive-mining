Game data visualization (R)

##Library data loding
=================================================================

# Visualizations
#additional themes and theme components for ggplot2
library(hrbrthemes)

#is an extension of the grimmer of graphics, as implementation by ggplot2
#package that adds support for declaring animations using an api familiar to user of  ggplot2
library(gganimate)

#for graphics use or 2 different thing merge code(average and coefficient )
library(gapminder)

#Same user no of time repeated for different purpose
library(babynames)

#Extra themes Scales and Geoms for ggplot
library(ggthemes)

#The cowplot package is a simple add-on to ggplot. It provides various features that help with creating publication-quality figures, 
#such as a set of themes, functions to align plots and arrange them into complex compound figures, 
#and functions that make it easy to annotate plots and or mix plots with images.
library(cowplot)

#ggplot2 is an R package used for statistical computing and data representation using data visualization.
#It follows underlying graphics called Grammar of Graphics which includes certain rules and independent 
#components which can be used to represent data in various formats.
library(ggplot2)

# Statistics
library(DescTools)
==================================================
  # Loading the data 
  data <- read.csv("C:/Users/priya/Desktop/Game Research/gamedata/vgsales.csv", stringsAsFactors = FALSE)
  
  # Removing the Rank column
  data$Rank <- NULL
  # Filtering only the records of interest for this study, removing the records with Year = NaN and records with the year above 2016
  data <- data[data$Year != "N/A" & data$Year != "2017" & data$Year != "2020", ]
  data$Year <- factor(data$Year)
  
  # Viewing the first 6 DataFrame records
  head(data, 6)
  ==================================================================
 =================================================================
    
    
    