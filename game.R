
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

# Data Manipulation
library(dplyr)

# Statistics
library(DescTools)
==================================================
  # Loading the database 
  data <- read.csv("C:/Users/priya/Desktop/Game Research/gamedata/vgsales.csv", stringsAsFactors = FALSE)
  
  # Removing the Rank column
  data$Rank <- NULL
  # Filtering only the records of interest for this study, removing the records with Year = NaN and records with the year above 2016
  data <- data[data$Year != "N/A" & data$Year != "2017" & data$Year != "2020", ]
  data$Year <- factor(data$Year)
  
  # Viewing the first 6 DataFrame records
  head(data, 6)
  ==================================================================
    summary(data)
  =================================================================
    ## 2.Discriptiv analysis
    #A.Frequency Distribution
    # year : year -> year of the game release 
    
    freq_year <- data.frame(cbind(Frequency = table(data$Year), Percent = prop.table(table(data$Year)) * 100))
  freq_year <- freq_year[order(freq_year$Frequency, decreasing=TRUE), ]
  freq_year    
  
  options(repr.plot.width = 14, repr.plot.height = 10)
  df <- head(freq_year, 10)
  a <- ggplot(data = df, mapping = aes(x = Frequency, y = row.names(df))) +
    geom_bar(stat = "identity", mapping = aes(fill = row.names(df), color = row.names(df)), alpha = .7, size = 1.1) +
    geom_label(mapping = aes(label=Frequency), fill = "#006400", size = 6, color = "white", fontface = "bold", hjust=.7) +
    ggtitle("The 10 most frequent years in the database") +
    xlab(" ") +
    ylab("") +
    theme_ipsum() +
    coord_flip() +
    theme(plot.background = element_rect(color = "black", size = 1.1),
          plot.title = element_text(size = 24, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
          axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
          axis.text.x = element_text(size = 20, face = "bold"),
          axis.text.y = element_text(size = 20, face = "bold"),
          legend.position = "none")
  
  df1 <- tail(freq_year, 10)
  b <- ggplot(data = df1, mapping = aes(x = Frequency, y = row.names(df1))) +
    geom_bar(stat = "identity", mapping = aes(fill = row.names(df1), color = row.names(df1)), alpha = .7, size = 1.1) +
    geom_label(mapping = aes(label=Frequency), fill = "red", size = 6, color = "white", fontface = "bold", hjust=.7) +
    ggtitle("The 10 least frequent years in the database") +
    xlab(" ") +
    ylab("") +
    theme_ipsum() +
    coord_flip() +
    theme(plot.background = element_rect(color = "black", size = 1.1),
          plot.title = element_text(size = 24, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
          axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
          axis.text.x = element_text(size = 20, face = "bold"),
          axis.text.y = element_text(size = 20, face = "bold"),
          legend.position = "none")
  plot_grid(a, b, nrow = 2, ncol = 1)
  
  ==================================================================================
    ##NA_sales
    
    options(repr.plot.width = 14, repr.plot.height = 10)
  df <- head(freq_year, 10)
  a <- ggplot(data = df, mapping = aes(x = Frequency, y = row.names(df))) +
    geom_bar(stat = "identity", mapping = aes(fill = row.names(df), color = row.names(df)), alpha = .7, size = 1.1) +
    geom_label(mapping = aes(label=Frequency), fill = "#006400", size = 6, color = "white", fontface = "bold", hjust=.7) +
    ggtitle("The 10 most frequent years in the database") +
    xlab(" ") +
    ylab("") +
    theme_ipsum() +
    coord_flip() +
    theme(plot.background = element_rect(color = "black", size = 1.1),
          plot.title = element_text(size = 24, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
          axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
          axis.text.x = element_text(size = 20, face = "bold"),
          axis.text.y = element_text(size = 20, face = "bold"),
          legend.position = "none")
  
  df1 <- tail(freq_year, 10)
  b <- ggplot(data = df1, mapping = aes(x = Frequency, y = row.names(df1))) +
    geom_bar(stat = "identity", mapping = aes(fill = row.names(df1), color = row.names(df1)), alpha = .7, size = 1.1) +
    geom_label(mapping = aes(label=Frequency), fill = "red", size = 6, color = "white", fontface = "bold", hjust=.7) +
    ggtitle("The 10 least frequent years in the database") +
    xlab(" ") +
    ylab("") +
    theme_ipsum() +
    coord_flip() +
    theme(plot.background = element_rect(color = "black", size = 1.1),
          plot.title = element_text(size = 24, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
          axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
          axis.text.x = element_text(size = 20, face = "bold"),
          axis.text.y = element_text(size = 20, face = "bold"),
          legend.position = "none")
  
  plot_grid(a, b, nrow = 2, ncol = 1)
  
  ==================================================================================##Eu_Sales
    options(repr.plot.width = 14, repr.plot.height = 6)
  a <- ggplot(data = data, mapping = aes(x = EU_Sales)) +
    geom_histogram(bins = 80, fill = "#00CED1", color = "#7FFF00") +
    xlab("Sales in Europe (in millions)") +
    ylab("Frequency") +
    ggtitle("Sales in Europe (in millions) Histogram") +
    theme_minimal() +
    theme(plot.title = element_text(size = 24, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
          axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
          axis.text.x = element_text(size = 20, face = "bold"),
          axis.text.y = element_text(size = 20, face = "bold"),
          legend.position = "none")
  
  df <- data[data$EU_Sales < 2, ]
  b <- ggplot(data = df, mapping = aes(x = EU_Sales)) +
    geom_histogram(bins = 80, fill = "#00CED1", color = "#7FFF00") +
    xlab("Sales in Europe (in millions)") +
    ylab("") +
    ggtitle("Sales in Europe < 2 millions") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 24, hjust = .5, face = "bold"),
      axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
      axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
      axis.text.x = element_text(size = 20, face = "bold"),
      axis.text.y = element_text(size = 20, face = "bold"),
      legend.position = "none")
  
  plot_grid(a, b, nrow = 1, ncol = 2)
  ==================================================================================
    ##JP_Sales
    
    options(repr.plot.width = 14, repr.plot.height = 6)
  a <- ggplot(data = data, mapping = aes(x = JP_Sales)) +
    geom_histogram(bins = 80, fill = "#4B0082", color = "#FF00FF") +
    xlab("Sales in Japan (in millions)") +
    ylab("Frequency") +
    ggtitle("Sales in Japan (in millions) Histogram") +
    theme_minimal() +
    theme(plot.title = element_text(size = 24, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
          axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
          axis.text.x = element_text(size = 20, face = "bold"),
          axis.text.y = element_text(size = 20, face = "bold"),
          legend.position = "none")
  
  df <- data[data$JP_Sales < 2, ]
  b <- ggplot(data = df, mapping = aes(x = JP_Sales)) +
    geom_histogram(bins = 80, fill = "#4B0082", color = "#FF00FF") +
    xlab("Sales in Japan (in millions)") +
    ylab("") +
    ggtitle("Sales in Japan < 2 millions") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 24, hjust = .5, face = "bold"),
      axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
      axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
      axis.text.x = element_text(size = 20, face = "bold"),
      axis.text.y = element_text(size = 20, face = "bold"),
      legend.position = "none")
  plot_grid(a, b, nrow = 1, ncol = 2)
  
  ==================================================================================
    ##other_sales
    
    options(repr.plot.width = 14, repr.plot.height = 6)
  a <- ggplot(data = data, mapping = aes(x = Other_Sales)) +
    geom_histogram(bins = 80, fill = "#800000", color = "black") +
    xlab("Sales in the rest of the world (in millions)") +
    ylab("Frequency") +
    ggtitle("Sales in the rest of the world") +
    theme_minimal() +
    theme(plot.title = element_text(size = 24, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
          axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
          axis.text.x = element_text(size = 20, face = "bold"),
          axis.text.y = element_text(size = 20, face = "bold"),
          legend.position = "none")
  
  df <- data[data$Other_Sales < 2, ]
  b <- ggplot(data = df, mapping = aes(x = Other_Sales)) +
    geom_histogram(bins = 80, fill = "#800000", color = "black") +
    xlab("Sales in the rest of the world (in millions)") +
    ylab("") +
    ggtitle("Sales in the rest of the world < 2") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 24, hjust = .5, face = "bold"),
      axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
      axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
      axis.text.x = element_text(size = 20, face = "bold"),
      axis.text.y = element_text(size = 20, face = "bold"),
      legend.position = "none")
  
  plot_grid(a, b, nrow = 1, ncol = 2)
  ==================================================================================
    ##Global_sales
    
    options(repr.plot.width = 14, repr.plot.height = 6)
  a <- ggplot(data = data, mapping = aes(x = Global_Sales)) +
    geom_histogram(bins = 80, fill = "orange", color = "#FF0000") +
    xlab("Total worldwide sales (in millions)") +
    ylab("Frequency") +
    ggtitle("Total worldwide sales (in millions)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 24, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
          axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
          axis.text.x = element_text(size = 20, face = "bold"),
          axis.text.y = element_text(size = 20, face = "bold"),
          legend.position = "none")
  
  df <- data[data$Global_Sales < 2, ]
  b <- ggplot(data = df, mapping = aes(x = Global_Sales)) +
    geom_histogram(bins = 80, fill = "orange", color = "#FF0000") +
    xlab("Total worldwide sales (in millions)") +
    ylab("") +
    ggtitle("Total worldwide sales < 2") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 24, hjust = .5, face = "bold"),
      axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
      axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
      axis.text.x = element_text(size = 20, face = "bold"),
      axis.text.y = element_text(size = 20, face = "bold"),
      legend.position = "none")
  
  plot_grid(a, b, nrow = 1, ncol = 2)
  ==================================================================================#Distribution of qualitative variable 
    #Name  
    #5most frequent game in database
    
    freq_name <- data.frame(cbind(Frequency = table(data$Name), Percent = prop.table(table(data$Name)) * 100))
  freq_name <- head(freq_name[order(freq_name$Frequency, decreasing = T), ], 5)
  freq_name
  
  a <- ggplot(data = freq_name, mapping = aes(x = row.names(freq_name), y = Frequency)) +
    geom_segment(aes(xend=row.names(freq_name), yend=0, color = row.names(freq_name)), size = 2.5, alpha = .5) +
    geom_point(mapping = aes(fill = row.names(freq_name)), size = 5, shape = 21) +
    coord_flip() +
    theme_economist() +
    xlab("") +
    ylab("") +
    theme(plot.background = element_rect(fill = "#F8F8FF", color = "purple"),
          axis.title.x = element_text(size = 16, hjust = .5, face = "italic"),
          axis.title.y = element_text(size = 16, hjust = .5, face = "italic"),
          axis.text.x = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 16, face = "bold"),
          legend.position = "none")
  
  b <- ggplot(data = freq_name, mapping = aes(x = row.names(freq_name), y = Frequency)) +
    geom_segment(aes(xend=row.names(freq_name), yend=0, color = row.names(freq_name)), size = 2.5, alpha = .5) +
    geom_point(mapping = aes(fill = row.names(freq_name)), size = 5, shape = 21) +
    theme_economist() +
    xlab("") +
    ylab("") +
    theme(plot.background = element_rect(fill = "#F8F8FF", color = "purple"),
          axis.title.x = element_text(size = 16, face = "italic"),
          axis.title.y = element_text(size = 16, hjust = .5, face = "italic"),
          axis.text.x = element_text(size = 16, face = "bold"),
          axis.text.y = element_text(size = 16, face = "bold"),
          legend.position = "none")
  
  plot_grid(a, b + coord_polar(), ncol = 2, nrow = 1)
  
  ==================================================================================##plateform
    
    unique(data$Platform)
  freq_platform <- data.frame(cbind(Frequency = table(data$Platform), Percent = prop.table(table(data$Platform)) * 100))
  freq_platform <- head(freq_platform[order(freq_platform$Frequency, decreasing = T), ], 5)
  freq_platform
  a <- ggplot(data = freq_platform, mapping = aes(x = row.names(freq_platform), y = Frequency)) +
    geom_bar(stat = "identity", aes(fill = row.names(freq_platform)), size = 1, alpha = .5, color = "black") +
    geom_label(mapping = aes(label = Frequency), fill = "purple", color = "white", size = 6, fontface = "bold") +
    coord_flip() +
    theme_economist() +
    ylab("Frequency") +
    xlab("") +
    theme(plot.background = element_rect(fill = "#F0E68C", color = "orange", size = 1),
          axis.title.y = element_text(size = 16, hjust = .5, face = "italic"),
          axis.title.x = element_text(size = 20, hjust = .5, vjust = -2, face = "italic"),
          axis.text.x = element_text(size = 14, face = "bold"),
          axis.text.y = element_text(size = 16, face = "bold"),
          legend.position = "none")
  
  b <- ggplot(data = freq_name, mapping = aes(x = row.names(freq_platform), y = Frequency)) +
    geom_bar(stat = "identity", aes(fill = row.names(freq_platform)), color = "black", size = 1, alpha = .5) +
    theme_economist() +
    xlab("") +
    ylab("") +
    theme(plot.background = element_rect(fill = "#F0E68C", color = "orange", size = 1),
          axis.title.y = element_text(size = 16, face = "italic"),
          axis.title.x = element_text(size = 20, hjust = .5, face = "italic"),
          axis.text.x = element_text(size = 16, face = "bold"),
          axis.text.y = element_text(size = 16, face = "bold"),
          legend.position = "none")
  
  plot_grid(a, b + coord_polar(), ncol = 2, nrow = 1)
  ==================================================================================
    #gener
    
    freq_genre <- data.frame(cbind(Frequency = table(data$Genre), Percent = prop.table(table(data$Genre)) * 100))
  freq_genre <- freq_genre[order(freq_genre$Frequency, decreasing = T), ]
  freq_genre
  
  options(repr.plot.width = 14, repr.plot.height = 6)
  ggplot(data = freq_genre, mapping = aes(x = Frequency, y = row.names(freq_genre))) +
    geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_genre), color = row.names(freq_genre)), alpha = .7, size = 1.1) +
    geom_label(mapping = aes(label=Frequency), fill = "#B22222", size = 6, color = "white", fontface = "bold", hjust=.7) +
    ggtitle("Genre Frequency Distribution") +
    xlab(" ") +
    ylab("") +
    theme_ipsum() +
    coord_flip() +
    theme(
      plot.title = element_text(size = 24, hjust = .5, face = "bold"),
      axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
      axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
      axis.text.x = element_text(size = 20, face = "bold", angle = 20),
      axis.text.y = element_text(size = 20, face = "bold"),
      legend.position = "none")
  ================================================================================= 
    #Publisher
    
    unique(data$Publisher)
  
  # 10 most Frequent publisher in database
  
  freq_published <- data.frame(cbind(Frequency = table(data$Publishe), Percent = prop.table(table(data$Publishe)) * 100))
  freq_published <- head(freq_published[order(freq_published$Frequency, decreasing = T), ], 10)
  freq_published
  
  options(repr.plot.width = 14, repr.plot.height = 10)
  ggplot(data = freq_published, mapping = aes(x = Frequency, y = row.names(freq_published))) +
    geom_line(group = 1, size = 1, color = "blue", linetype = "dashed") +
    geom_label(mapping = aes(label=Frequency, fill = row.names(freq_published)), size = 7, color = "white", fontface = "bold", hjust=.7) +
    ggtitle("Publisher distribution") +
    xlab("Frequency") +
    ylab("") +
    theme_economist() +
    theme(plot.background = element_rect(fill = "#98FB98", color = "blue"),
          plot.title = element_text(size = 24, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
          axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
          axis.text.x = element_text(size = 18, face = "bold"),
          axis.text.y = element_text(size = 18, face = "bold"),
          legend.position = "none")
  
  #B.central trend Measures
  
  options(repr.plot.width = 14, repr.plot.height = 6)
  a <- ggplot(data = df_means, mapping = aes(x = Mean, y = row.names(df_means))) +
    geom_line(group = 1, size = 1.2, linetype = "dashed", color = "blue") +
    geom_point(size = 5, shape = 21, stroke = 1.5, mapping = aes(fill = row.names(df_means))) +
    theme_minimal() +
    ylab("") +
    theme(plot.title = element_text(size = 24, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
          axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
          axis.text.x = element_text(size = 18, face = "bold"),
          axis.text.y = element_text(size = 18, face = "bold"),
          legend.position = "none")
  
  b <- ggplot(data = df_means, mapping = aes(x = Mean, y = row.names(df_means))) +
    geom_line(group = 1, size = 1.2, linetype = "dashed", color = "blue") +
    geom_point(size = 5, stroke = 1.5, shape = 21, mapping = aes(fill = row.names(df_means))) +
    theme_minimal() +
    ylab("") +
    xlab("") +
    theme(plot.title = element_text(size = 24, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
          axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
          axis.text.x = element_text(size = 18, face = "bold"),
          axis.text.y = element_text(size = 18, face = "bold"),
          legend.position = "bottom",
          legend.title = element_text(color = "white"),
          legend.text = element_text(size = 12, face = "bold"))
  
  plot_grid(a, b + coord_polar(), nrow = 1, ncol = 2)
  
  ## Median
  df_median <- data.frame(Median = c(median(data$NA_Sales), median(data$EU_Sales), median(data$JP_Sales), median(data$Other_Sales), median(data$Global_Sales)))
  row.names(df_median) <- c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")
  df_median
  
  ##Mode
  Mode <- function(x){
    freq <- table(x)
    return(names(freq)[freq == max(freq)])  
  }
  
  df_mode <- data.frame(Mode = c(Mode(data$NA_Sales), Mode(data$EU_Sales), Mode(data$JP_Sales), Mode(data$Other_Sales), Mode(data$Global_Sales)))
  row.names(df_mode) <- c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")
  df_mode
  
  df_meds <- data.frame(Mean = df_means$Mean, Median = df_median, Mode = df_mode)
  df_meds
  
  ##c.Seprating Measures
  
  #percentile
  
  percentile <- c()
  for(i in 1:99){
    
    percentile <- c(percentile, i / 100)
    
  }
  
  df_percentiles <- data.frame(NA_Sales = quantile(data$NA_Sales, percentile), EU_Sales = quantile(data$EU_Sales, percentile), JP_Sales = quantile(data$JP_Sales, percentile),
                               Other_Sales = quantile(data$Other_Sales, percentile), Global_Sales = quantile(data$Global_Sales, percentile))
  df_percentiles
  
  ##d.Dispersion Measure 
  
  df_dm <- data.frame(DM = c(MeanAD(data$NA_Sales), MeanAD(data$EU_Sales), MeanAD(data$JP_Sales), MeanAD(data$Other_Sales), MeanAD(data$Global_Sales)))
  row.names(df_dm) <- c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")
  df_dm
  
  df_var <- data.frame(Variance = c(var(data$NA_Sales), var(data$EU_Sales), var(data$JP_Sales), var(data$Other_Sales), var(data$Global_Sales)))
  row.names(df_var) <- c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")
  df_var
  
  df_std <- data.frame(std = c(sqrt(var(data$NA_Sales)), sqrt(var(data$EU_Sales)), sqrt(var(data$JP_Sales)), sqrt(var(data$Other_Sales)), sqrt(var(data$Global_Sales))))
  row.names(df_std) <- c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")
  df_std
  
  df_dispersion <- data.frame(DM = df_dm$DM, Variance = df_var$Variance, std = df_std$std)
  row.names(df_dispersion) <- c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")
  df_dispersion
  
  ggplot(data = df_dispersion) +
    geom_bar(stat = "identity", mapping = aes(x = row.names(df_dispersion), y = Variance, fill = "Variance"), alpha = .9, size = 1, color = "blue") +
    geom_bar(stat = "identity", mapping = aes(x = row.names(df_dispersion), y = std, fill = "std"), alpha = .4, size = 1, color = "green") +
    geom_bar(stat = "identity", mapping = aes(x = row.names(df_dispersion), y = DM, fill = "DM"), alpha = .9, size = 1, color = "red") +
    xlab("") +
    ylab("") +
    theme_minimal() +
    theme(plot.title = element_text(size = 24, hjust = .5, face = "bold"),
          axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
          axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
          axis.text.x = element_text(size = 18, face = "bold"),
          axis.text.y = element_text(size = 18, face = "bold"),
          legend.position = "bottom",
          legend.title = element_text(color = "white"),
          
          ==================================================================================
            ## 3.exploratory analysis
            ##A.analysis of the world best-selling game
            
            # NA_Sales
            t_v_name_NA <- aggregate(list(NA_Sales = data$NA_Sales), list(Name = data$Name), sum)
          t_v_name_NA <- t_v_name_NA[order(t_v_name_NA$NA_Sales, decreasing = T), ]
          
          # EU_Sales
          t_v_name_EU <- aggregate(list(EU_Sales = data$EU_Sales), list(Name = data$Name), sum)
          t_v_name_EU <- t_v_name_EU[order(t_v_name_EU$EU_Sales, decreasing = T), ]
          
          # JP_Sales
          t_v_name_JP <- aggregate(list(JP_Sales = data$JP_Sales), list(Name = data$Name), sum)
          t_v_name_JP <- t_v_name_JP[order(t_v_name_JP$JP_Sales, decreasing = T), ]
          
          # Other_Sales
          t_v_name_Other <- aggregate(list(Other_Sales = data$Other_Sales), list(Name = data$Name), sum)
          t_v_name_Other <- t_v_name_Other[order(t_v_name_Other$Other_Sales, decreasing = T), ]
          
          # Global_Sales
          t_v_name_Global <- aggregate(list(Global_Sales = data$Global_Sales), list(Name = data$Name), sum)
          t_v_name_Global <- t_v_name_Global[order(t_v_name_Global$Global_Sales, decreasing = T), ]
          
          options(repr.plot.width = 14, repr.plot.height = 25)
          a <- ggplot(data = head(t_v_name_NA, 10), mapping = aes(x = Name, y = NA_Sales)) +
            geom_bar(stat = "identity", mapping = aes(fill = Name, color = Name), size = 1.1, alpha = .7) +
            geom_label(mapping = aes(label = NA_Sales), size = 6, fontface = "bold") +
            xlab("") +
            ylab("Sales in North America (in millions)") +
            ggtitle("The 10 best selling games in North America") +
            theme_ipsum() +
            theme(legend.position = "none",
                  plot.title = element_text(size = 22, face = "bold", hjust = .5),
                  axis.text.x = element_text(size = 15, face = "bold", angle = 20),
                  axis.text.y = element_text(size = 18, face = "bold"),
                  axis.title.y = element_text(size = 20))
          
          b <- ggplot(data = head(t_v_name_EU, 10), mapping = aes(x = Name, y = EU_Sales)) +
            geom_bar(stat = "identity", mapping = aes(fill = Name, color = Name), size = 1.1, alpha = .7) +
            geom_label(mapping = aes(label = EU_Sales), size = 6, fontface = "bold") +
            xlab("") +
            ylab("Sales in Europe (in millions)") +
            ggtitle("The 10 best selling games in Europe") +
            theme_ipsum() +
            theme(legend.position = "none",
                  plot.title = element_text(size = 22, face = "bold", hjust = .5),
                  axis.text.x = element_text(size = 15, face = "bold", angle = 20),
                  axis.text.y = element_text(size = 18, face = "bold"),
                  axis.title.y = element_text(size = 20))
          
          c <- ggplot(data = head(t_v_name_JP, 10), mapping = aes(x = Name, y = JP_Sales)) +
            geom_bar(stat = "identity", mapping = aes(fill = Name, color = Name), size = 1.1, alpha = .7) +
            geom_label(mapping = aes(label = JP_Sales), size = 6, fontface = "bold") +
            xlab("") +
            ylab("Sales in Japan (in millions)") +
            ggtitle("The 10 best selling games in Japan") +
            theme_ipsum() +
            theme(legend.position = "none",
                  plot.title = element_text(size = 22, face = "bold", hjust = .5),
                  axis.text.x = element_text(size = 15, face = "bold", angle = 20),
                  axis.text.y = element_text(size = 18, face = "bold"),
                  axis.title.y = element_text(size = 20))
          
          d <- ggplot(data = head(t_v_name_Other, 10), mapping = aes(x = Name, y = Other_Sales)) +
            geom_bar(stat = "identity", mapping = aes(fill = Name, color = Name), size = 1.1, alpha = .7) +
            geom_label(mapping = aes(label = Other_Sales), size = 6, fontface = "bold") +
            xlab("") +
            ylab("Sales in the rest of the world (in millions)") +
            ggtitle("The 10 best selling games in rest of the world") +
            theme_ipsum() +
            theme(legend.position = "none",
                  plot.title = element_text(size = 22, face = "bold", hjust = .5),
                  axis.text.x = element_text(size = 15, face = "bold", angle = 20),
                  axis.text.y = element_text(size = 18, face = "bold"),
                  axis.title.y = element_text(size = 20))
          plot_grid(a, b, c, d, nrow = 4, ncol = 1)
          
          ## best selling game in world from 1980 to 2016
          
          a <- c()
          for(i in 1:nrow(t_v_name_Global)){
            a <- c(a, i)
          }
          
          row.names(t_v_name_Global) <- a
          head(t_v_name_Global, 10)
          
          options(repr.plot.width = 20, repr.plot.height = 8)
          a <- ggplot(data = head(t_v_name_Global, 5), mapping = aes(x = Name, y = Global_Sales)) +
            geom_bar(stat = "identity", mapping = aes(fill = Name, color = Name), size = 1, alpha = .7) +
            geom_label(mapping = aes(label = Global_Sales), color = "white", fill = "blue", size = 6, fontface = "bold") +
            xlab("") +
            ylab("") +
            ggtitle("The best selling games in the world from 1980 to 2016") +
            theme_ipsum() +
            coord_flip() +
            theme(legend.position = "none",
                  plot.title = element_text(size = 25, face = "bold", hjust = -2, vjust = 4),
                  axis.text.x = element_text(size = 20, face = "bold"),
                  axis.text.y = element_text(size = 18, face = "bold"),
                  axis.title.y = element_text(size = 20))
          
          b <- ggplot(data = head(t_v_name_Global, 5), mapping = aes(x = Name, y = Global_Sales)) +
            geom_line(size = 2, alpha = .7, group = 1) +
            geom_point(mapping = aes(fill = Name), shape = 21, size = 5) +
            theme_ipsum() +
            xlab("") +
            ylab("") +
            theme(legend.position = "none",
                  axis.text.x = element_text(size = 15, face = "bold"),
                  axis.text.y = element_text(size = 18, face = "bold"),
                  axis.title.y = element_text(size = 20))
          
          plot_grid(a, b + coord_polar(), nrow = 1, ncol = 2)
          
          
          df_top_5 <- data[data$Name == "Wii Sports" | data$Name == "Grand Theft Auto V" | data$Name == "Super Mario Bros." | data$Name == "Tetris" | data$Name == "Mario Kart Wii", ]
          
          options(repr.plot.width = 14, repr.plot.height = 7)
          ggplot(data = df_top_5, mapping = aes(x = Year, y = Global_Sales)) +
            geom_bar(stat = "identity", mapping = aes(fill = Name, color = Name), size = 1, alpha = .8) +
            facet_wrap(~Name) +
            theme_bw() +
            xlab("") +
            ylab("Sales in the world (in millions)") +
            theme(
              legend.position = "none",
              strip.text.x = element_text(margin = margin(7, 7, 7, 7), size = 20, face = "bold", color = "white"),
              strip.background = element_rect(fill = "#BC8F8F", color = "black"),
              plot.title = element_text(size = 22, face = "bold", hjust = .5),
              axis.text.x = element_text(size = 11, face = "bold"),
              axis.text.y = element_text(size = 15, face = "bold"),
              axis.title.y = element_text(size = 20))
          
          
          ## B.Number of sales per plateform
          # NA_Sales
          p_name_NA <- aggregate(list(NA_Sales = data$NA_Sales), list(Platform = data$Platform), sum)
          p_name_NA <- p_name_NA[order(p_name_NA$NA_Sales, decreasing = T), ]
          
          # EU_Sales
          p_name_EU <- aggregate(list(EU_Sales = data$EU_Sales), list(Platform = data$Platform), sum)
          p_name_EU <- p_name_EU[order(p_name_EU$EU_Sales, decreasing = T), ]
          
          # JP_Sales
          p_name_JP <- aggregate(list(JP_Sales = data$JP_Sales), list(Platform = data$Platform), sum)
          p_name_JP <- p_name_JP[order(p_name_JP$JP_Sales, decreasing = T), ]
          
          # Other_Sales
          p_name_Other <- aggregate(list(Other_Sales = data$Other_Sales), list(Platform = data$Platform), sum)
          p_name_Other <- p_name_Other[order(p_name_Other$Other_Sales, decreasing = T), ]
          
          # Global_Sales
          p_name_Global <- aggregate(list(Global_Sales = data$Global_Sales), list(Platform = data$Platform), sum)
          p_name_Global <- p_name_Global[order(p_name_Global$Global_Sales, decreasing = T), ]
          
          options(repr.plot.width = 20, repr.plot.height = 23)
          a <- ggplot(data = p_name_NA, 10, mapping = aes(x = Platform, y = NA_Sales)) +
            geom_line(size = 1.1, alpha = .7, group = 1, linetype = 2, color = "purple") +
            geom_label(mapping = aes(label = NA_Sales, fill = Platform), color = "black", size = 6, fontface = "bold", alpha = .8) +
            xlab("") +
            ylab("Sales in North America") +
            ggtitle("Number of sales per platform in North America") +
            theme_minimal() +
            theme(
              legend.position = "none",
              plot.title = element_text(size = 25, face = "bold", hjust = .5),
              axis.text.x = element_text(size = 15, face = "bold"),
              axis.text.y = element_text(size = 20, face = "bold", hjust = .5),
              axis.title.y = element_text(size = 23, hjust = .5))
          
          b <- ggplot(data = p_name_EU, 10, mapping = aes(x = Platform, y = EU_Sales)) +
            geom_line(size = 1.1, alpha = .7, group = 1, linetype = 2, color = "purple") +
            geom_label(mapping = aes(label = EU_Sales, fill = Platform), color = "black", size = 6, fontface = "bold", alpha = .8) +
            xlab("") +
            ylab("Sales in Europe") +
            ggtitle("Number of sales per platform in Europe") +
            theme_minimal() +
            theme(
              legend.position = "none",
              plot.title = element_text(size = 25, face = "bold", hjust = .5),
              axis.text.x = element_text(size = 15, face = "bold"),
              axis.text.y = element_text(size = 20, face = "bold", hjust = .5),
              axis.title.y = element_text(size = 23, hjust = .5))
          
          c <- ggplot(data = p_name_JP, 10, mapping = aes(x = Platform, y = JP_Sales)) +
            geom_line(size = 1.1, alpha = .7, group = 1, linetype = 2, color = "purple") +
            geom_label(mapping = aes(label = JP_Sales, fill = Platform), color = "black", size = 6, fontface = "bold", alpha = .8) +
            xlab("") +
            ylab("Sales in Japan") +
            ggtitle("Number of sales per platform in Japan") +
            theme_minimal() +
            theme(
              legend.position = "none",
              plot.title = element_text(size = 25, face = "bold", hjust = .5),
              axis.text.x = element_text(size = 15, face = "bold"),
              axis.text.y = element_text(size = 20, face = "bold", hjust = .5),
              axis.title.y = element_text(size = 23, hjust = .5))
          
          d <- ggplot(data = p_name_Other, 10, mapping = aes(x = Platform, y = Other_Sales)) +
            geom_line(size = 1.1, alpha = .7, group = 1, linetype = 2, color = "purple") +
            geom_label(mapping = aes(label = Other_Sales, fill = Platform), color = "black", size = 6, fontface = "bold", alpha = .8) +
            xlab("") +
            ylab("Sales in the rest of the world") +
            ggtitle("Number of sales per platform in rest of the world") +
            theme_minimal() +
            theme(
              legend.position = "none",
              plot.title = element_text(size = 25, face = "bold", hjust = .5),
              axis.text.x = element_text(size = 15, face = "bold"),
              axis.text.y = element_text(size = 20, face = "bold", hjust = .5),
              axis.title.y = element_text(size = 23, hjust = .5))
          
          plot_grid(a, b, c, d, nrow = 4, ncol = 1)
          
          ##The 10 plateformwith the highest number of game sales in the WorldPhones
          
          a <- c()
          for(i in 1:nrow(p_name_Global)){
            a <- c(a, i)
          }
          
          row.names(p_name_Global) <- a
          head(p_name_Global, 10)
          
          options(repr.plot.width = 14, repr.plot.height = 6)
          ggplot(data = head(p_name_Global, 10), mapping = aes(x = Platform, y = Global_Sales)) +
            geom_segment(aes(xend=Platform, yend=0, color = Platform), size = 2.3, alpha = .8) +
            geom_point(mapping = aes(fill = Platform), size = 7, shape = 21) +
            geom_line(group = 1, size = 1.1, linetype = 10, color = "red") +
            xlab("") +
            ylab("") +
            ggtitle("The 10 platforms with the highest number of game sales in the world") +
            theme_minimal() +
            theme(plot.title = element_text(size = 25, face = "bold", hjust = .5),
                  axis.title.x = element_text(size = 16, hjust = .5, face = "italic"),
                  axis.title.y = element_text(size = 16, hjust = .5, face = "italic"),
                  axis.text.x = element_text(size = 20, face = "bold"),
                  axis.text.y = element_text(size = 20, face = "bold"),
                  legend.position = "none")
          
          d_top_10 <- data[data$Platform == "PS2" | data$Platform == "X360" | data$Platform == "PS3" | data$Platform == "Wii" | data$Platform == "DS" | data$Platform == "PS" | data$Platform == "GBA"
                           | data$Platform == "PSP" | data$Platform == "PS4" | data$Platform == "PC", ]
          d_top_10$Year <- as.numeric(levels(d_top_10$Year))[d_top_10$Year]
          
          options(repr.plot.width = 14, repr.plot.height = 10)
          ggplot(data = d_top_10, mapping = aes(x = Year, y = Global_Sales)) +
            geom_bar(stat = "identity", mapping = aes(fill = Platform, color = Platform), size = .1, alpha = .8) +
            facet_wrap(~Platform) +
            theme_bw() +
            xlab("") +
            ylab("Sales in the world (in millions)") +
            theme(
              legend.position = "none",
              strip.text.x = element_text(margin = margin(7, 7, 7, 7), size = 20, face = "bold", color = "white"),
              strip.background = element_rect(fill = "#F4A460", color = "black"),
              plot.title = element_text(size = 22, face = "bold", hjust = .5),
              axis.text.x = element_text(size = 20, face = "bold"),
              axis.text.y = element_text(size = 20, face = "bold"),
              axis.title.y = element_text(size = 20))
          
          
          ## c.game sales by genre
          # NA_Sales
          g_name_NA <- aggregate(list(NA_Sales = data$NA_Sales), list(Genre = data$Genre), sum)
          g_name_NA <- g_name_NA[order(g_name_NA$NA_Sales, decreasing = T), ]
          
          # EU_Sales
          g_name_EU <- aggregate(list(EU_Sales = data$EU_Sales), list(Genre = data$Genre), sum)
          g_name_EU <- g_name_EU[order(g_name_EU$EU_Sales, decreasing = T), ]
          
          # JP_Sales
          g_name_JP <- aggregate(list(JP_Sales = data$JP_Sales), list(Genre = data$Genre), sum)
          g_name_JP <- g_name_JP[order(g_name_JP$JP_Sales, decreasing = T), ]
          
          # Other_Sales
          g_name_Other <- aggregate(list(Other_Sales = data$Other_Sales), list(Genre = data$Genre), sum)
          g_name_Other <- g_name_Other[order(g_name_Other$Other_Sales, decreasing = T), ]
          
          # Global_Sales
          g_name_Global <- aggregate(list(Global_Sales = data$Global_Sales), list(Genre = data$Genre), sum)
          g_name_Global <- g_name_Global[order(g_name_Global$Global_Sales, decreasing = T), ]
          
          options(repr.plot.width = 20, repr.plot.height = 20)
          a <- ggplot(data = g_name_NA, mapping = aes(x = Genre, y = NA_Sales)) +
            geom_segment(aes(xend=Genre, yend=0, color = Genre), size = 2.3, alpha = .8) +
            geom_point(mapping = aes(fill = Genre), size = 7, shape = 21) +
            geom_line(group = 1, size = 1.1, linetype = 10, color = "red") +
            xlab("") +
            ylab("") +
            ggtitle("Number of sales by gender in North America (in millions)") +
            theme_minimal() +
            theme(plot.title = element_text(size = 25, face = "bold", hjust = .5),
                  axis.title.x = element_text(size = 16, hjust = .5, face = "italic"),
                  axis.title.y = element_text(size = 16, hjust = .5, face = "italic"),
                  axis.text.x = element_text(size = 20, face = "bold"),
                  axis.text.y = element_text(size = 20, face = "bold"),
                  legend.position = "none")
          
          b <- ggplot(data = g_name_EU, mapping = aes(x = Genre, y = EU_Sales)) +
            geom_segment(aes(xend=Genre, yend=0, color = Genre), size = 2.3, alpha = .8) +
            geom_point(mapping = aes(fill = Genre), size = 7, shape = 21) +
            geom_line(group = 1, size = 1.1, linetype = 10, color = "red") +
            xlab("") +
            ylab("") +
            ggtitle("Number of sales by gender in Europe (in millions)") +
            theme_minimal() +
            theme(plot.title = element_text(size = 25, face = "bold", hjust = .5),
                  axis.title.x = element_text(size = 16, hjust = .5, face = "italic"),
                  axis.title.y = element_text(size = 16, hjust = .5, face = "italic"),
                  axis.text.x = element_text(size = 20, face = "bold"),
                  axis.text.y = element_text(size = 20, face = "bold"),
                  legend.position = "none")
          
          c <- ggplot(data = g_name_JP, mapping = aes(x = Genre, y = JP_Sales)) +
            geom_segment(aes(xend=Genre, yend=0, color = Genre), size = 2.3, alpha = .8) +
            geom_point(mapping = aes(fill = Genre), size = 7, shape = 21) +
            geom_line(group = 1, size = 1.1, linetype = 10, color = "red") +
            xlab("") +
            ylab("") +
            ggtitle("Number of sales by gender in Japan (in millions)") +
            theme_minimal() +
            theme(plot.title = element_text(size = 25, face = "bold", hjust = .5),
                  axis.title.x = element_text(size = 16, hjust = .5, face = "italic"),
                  axis.title.y = element_text(size = 16, hjust = .5, face = "italic"),
                  axis.text.x = element_text(size = 20, face = "bold"),
                  axis.text.y = element_text(size = 20, face = "bold"),
                  legend.position = "none")
          
          d <- ggplot(data = g_name_Other, mapping = aes(x = Genre, y = Other_Sales)) +
            geom_segment(aes(xend=Genre, yend=0, color = Genre), size = 2.3, alpha = .8) +
            geom_point(mapping = aes(fill = Genre), size = 7, shape = 21) +
            geom_line(group = 1, size = 1.1, linetype = 10, color = "red") +
            xlab("") +
            ylab("") +
            ggtitle("Number of sales by gender in rest of the world (in millions)") +
            theme_minimal() +
            theme(plot.title = element_text(size = 25, face = "bold", hjust = .5),
                  axis.title.x = element_text(size = 16, hjust = .5, face = "italic"),
                  axis.title.y = element_text(size = 16, hjust = .5, face = "italic"),
                  axis.text.x = element_text(size = 20, face = "bold"),
                  axis.text.y = element_text(size = 20, face = "bold"),
                  legend.position = "none")
          
          plot_grid(a, b, c, d, nrow = 4, ncol = 1)
          
          ## Examples of games by genre
          
          a <- c()
          for(i in 1:nrow(g_name_Global)){
            a <- c(a, i)
          }
          
          row.names(g_name_Global) <- a
          g_name_Global
          
          options(repr.plot.width = 14, repr.plot.height = 6)
          ggplot(data = g_name_Global, mapping = aes(x = Genre, y = Global_Sales)) +
            geom_bar(stat = "identity", mapping = aes(fill = Genre), alpha = .7, size = 1, color = "black") +
            geom_label(mapping = aes(label=Global_Sales), fill = "purple", size = 6, color = "white", fontface = "bold", hjust=.7) +
            ggtitle("Best selling genres in the world") +
            xlab(" ") +
            ylab("") +
            theme_ipsum() +
            theme(
              plot.title = element_text(size = 24, hjust = .5, face = "bold"),
              axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
              axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
              axis.text.x = element_text(size = 20, face = "bold", angle = 20),
              axis.text.y = element_text(size = 20, face = "bold"),
              legend.position = "none")
          
          g_top_10 <- data[data$Genre == "Action" | data$Genre == "Sports" | data$Genre == "Shooter" | data$Genre == "Role-Playing" | data$Genre == "Racing", ]
          g_top_10$Year <- as.numeric(levels(g_top_10$Year))[g_top_10$Year]
          
          
          options(repr.plot.width = 14, repr.plot.height = 10)
          ggplot(data = g_top_10, mapping = aes(x = Year, y = Global_Sales)) +
            geom_bar(stat = "identity", mapping = aes(fill = Genre, color = Genre), size = .1, alpha = .8) +
            facet_wrap(~Genre) +
            theme_bw() +
            xlab("") +
            ylab("Sales in the world (in millions)") +
            theme(
              legend.position = "none",
              strip.text.x = element_text(margin = margin(7, 7, 7, 7), size = 20, face = "bold", color = "white"),
              strip.background = element_rect(fill = "black", color = "red"),
              plot.title = element_text(size = 22, face = "bold", hjust = .5),
              axis.text.x = element_text(size = 20, face = "bold"),
              axis.text.y = element_text(size = 20, face = "bold"),
              axis.title.y = element_text(size = 20))
          
          ggplot(data = g_top_10, aes(x=Year, y=Global_Sales, group=Genre, color=Genre)) +
            geom_line() +
            geom_point() +
            ggtitle("Sale of games by genre from 1980 to 2016 (in millions)") +
            theme_ipsum() +
            ylab("Sale of games") +
            xlab("") +
            transition_reveal(Year) 
          
          ##d.Number of sales per publisher 
          
          options(repr.plot.width = 18, repr.plot.height = 20)
          a <- ggplot(data = head(pu_name_NA, 10), mapping = aes(x = Publisher, y = NA_Sales)) +
            geom_bar(stat = "identity", aes(fill = Publisher, color = Publisher), size = 1.2, alpha = .8) +
            geom_label(mapping = aes(label=NA_Sales), fill = "#FF8C00", size = 6, color = "white", fontface = "bold", hjust=.7) +
            xlab("") +
            ylab("") +
            ggtitle("Number of sales by Publisher in North America (in millions)") +
            theme_minimal() +
            theme(plot.title = element_text(size = 25, face = "bold", hjust = .5),
                  axis.title.x = element_text(size = 16, hjust = .5, face = "italic"),
                  axis.title.y = element_text(size = 16, hjust = .5, face = "italic"),
                  axis.text.x = element_text(size = 20, face = "bold", angle = 10),
                  axis.text.y = element_text(size = 20, face = "bold"),
                  legend.position = "none")
          
          b <- ggplot(data = head(pu_name_EU, 10), mapping = aes(x = Publisher, y = EU_Sales)) +
            geom_bar(stat = "identity", aes(fill = Publisher, color = Publisher), size = 1.2, alpha = .8) +
            geom_label(mapping = aes(label=EU_Sales), fill = "#FF8C00", size = 6, color = "white", fontface = "bold", hjust=.7) +
            xlab("") +
            ylab("") +
            ggtitle("Number of sales by Publisher in Europe (in millions)") +
            theme_minimal() +
            theme(plot.title = element_text(size = 25, face = "bold", hjust = .5),
                  axis.title.x = element_text(size = 16, hjust = .5, face = "italic"),
                  axis.title.y = element_text(size = 16, hjust = .5, face = "italic"),
                  axis.text.x = element_text(size = 20, face = "bold", angle = 10),
                  axis.text.y = element_text(size = 20, face = "bold"),
                  legend.position = "none")
          
          c <- ggplot(data = head(pu_name_JP, 10), mapping = aes(x = Publisher, y = JP_Sales)) +
            geom_bar(stat = "identity", aes(fill = Publisher, color = Publisher), size = 1.2, alpha = .8) +
            geom_label(mapping = aes(label=JP_Sales), fill = "#FF8C00", size = 6, color = "white", fontface = "bold", hjust=.7) +
            xlab("") +
            ylab("") +
            ggtitle("Number of sales by Publisher in Japan (in millions)") +
            theme_minimal() +
            theme(plot.title = element_text(size = 25, face = "bold", hjust = .5),
                  axis.title.x = element_text(size = 16, hjust = .5, face = "italic"),
                  axis.title.y = element_text(size = 16, hjust = .5, face = "italic"),
                  axis.text.x = element_text(size = 20, face = "bold", angle = 10),
                  axis.text.y = element_text(size = 20, face = "bold"),
                  legend.position = "none")
          
          d <- ggplot(data = head(pu_name_Other, 10), mapping = aes(x = Publisher, y = Other_Sales)) +
            geom_bar(stat = "identity", aes(fill = Publisher, color = Publisher), size = 1.2, alpha = .8) +
            geom_label(mapping = aes(label=Other_Sales), fill = "#FF8C00", size = 6, color = "white", fontface = "bold", hjust=.7) +
            xlab("") +
            ylab("") +
            ggtitle("Number of sales by Publisher in rest of the world (in millions)") +
            theme_minimal() +
            theme(plot.title = element_text(size = 25, face = "bold", hjust = .5),
                  axis.title.x = element_text(size = 16, hjust = .5, face = "italic"),
                  axis.title.y = element_text(size = 16, hjust = .5, face = "italic"),
                  axis.text.x = element_text(size = 20, face = "bold", angle = 10),
                  axis.text.y = element_text(size = 20, face = "bold"),
                  legend.position = "none")
          
          plot_grid(a, b, c, d, nrow = 4, ncol = 1)
          
          #10.publishers with the most sales
          a <- c()
          for(i in 1:nrow(pu_name_Global)){
            a <- c(a, i)
          }
          
          row.names(pu_name_Global) <- a
          head(pu_name_Global, 10)
          
          options(repr.plot.width = 14, repr.plot.height = 7)
          ggplot(data = head(pu_name_Global, 10), mapping = aes(x = Publisher, y = Global_Sales)) +
            geom_bar(stat = "identity", mapping = aes(fill = Publisher), alpha = .7, size = 1, color = "black") +
            geom_label(mapping = aes(label=Global_Sales), fill = "purple", size = 6, color = "white", fontface = "bold", hjust=.7) +
            ggtitle("The 10 publishers with the most sales (in millions)") +
            xlab(" ") +
            ylab("") +
            theme_ipsum() +
            theme(
              plot.title = element_text(size = 24, hjust = .5, face = "bold"),
              axis.title.x = element_text(size = 24, hjust = .5, face = "italic"),
              axis.title.y = element_text(size = 24, hjust = .5, face = "italic"),
              axis.text.x = element_text(size = 20, face = "bold", angle = 15),
              axis.text.y = element_text(size = 20, face = "bold"),
              legend.position = "none")
          
          pu_top_10 <- data[data$Publisher == "Nintendo" | data$Publisher == "Electronic Arts" | data$Publisher == "Activision" | data$Publisher == "Sony Computer Entertainment" | data$Publisher == "Ubisoft" | data$Publisher == "Take-Two Interactive" | 
                              data$Publisher == "THQ" | data$Publisher == "Konami Digital Entertainment" | data$Publisher == "Sega" | data$Publisher == "Namco Bandai Games", ]
          pu_top_10$Year <- as.numeric(levels(pu_top_10$Year))[pu_top_10$Year]
          
          options(repr.plot.width = 17, repr.plot.height = 10)
          ggplot(data = pu_top_10, mapping = aes(x = Year, y = Global_Sales)) +
            geom_bar(stat = "identity", mapping = aes(fill = Publisher, color = Publisher), size = .1, alpha = .8) +
            facet_wrap(~Publisher) +
            theme_bw() +
            xlab("") +
            ylab("Sales in the world (in millions)") +
            theme(
              legend.position = "none",
              strip.text.x = element_text(margin = margin(7, 7, 7, 7), size = 20, face = "bold", color = "#4B0082"),
              strip.background = element_rect(fill = "#FFB6C1", color = "green"),
              plot.title = element_text(size = 22, face = "bold", hjust = .5),
              axis.text.x = element_text(size = 20, face = "bold"),
              axis.text.y = element_text(size = 20, face = "bold"),
              axis.title.y = element_text(size = 20))
          
          ggplot(pu_top_10, aes(Year, Global_Sales, colour = Publisher)) +
            geom_point(alpha = 0.7, show.legend = FALSE) +
            scale_size(range = c(2, 12)) +
            scale_x_log10() +
            facet_wrap(~Publisher) +
            # Here comes the gganimate specific bits
            labs(title = 'Year: {frame_time}', x = 'Year', y = 'Sales in the world') +
            transition_time(Year) +
            ease_aes('linear')
          
          npu_top_10 <- data[data$Publisher == "Nintendo" | data$Publisher == "Electronic Arts" | data$Publisher == "Activision" | data$Publisher == "Ubisoft", ]
          npu_top_10$Year <- as.numeric(levels(npu_top_10$Year))[npu_top_10$Year]
          
          ggplot(data = npu_top_10, aes(x=Year, y=Global_Sales, group=Publisher, color=Publisher)) +
            geom_line() +
            geom_point() +
            ggtitle("Sale of games by Publisher from 1980 to 2016 (in millions)") +
            theme_ipsum() +
            ylab("Sale of games") +
            xlab("") +
            transition_reveal(Year) 
          
          ## E.global sales Number per year 
          df_global <- aggregate(list(Global_Sales = data$Global_Sales), list(Year = data$Year), sum)
          df_global <- df_global[order(df_global$Global_Sales), ]
          
          a <- c()
          
          for(i in 1:nrow(df_global)){
            a <- c(a, i)
          }
          row.names(df_global) <- a
          df_global
          options(repr.plot.width = 20, repr.plot.height = 11)
          a <- ggplot(data = df_global, mapping = aes(x = Year, y = Global_Sales)) +
            geom_line(size = 1, linetype = 10, color = "blue", group = 1) +
            geom_point(size = 6, shape = 21, mapping = aes(fill = Year)) +
            xlab("") +
            ylab("Global Sales") +
            ggtitle("Number of sales by Year in world (in millions)") +
            theme_classic() +
            theme(legend.position = "none",
                  strip.text.x = element_text(margin = margin(7, 7, 7, 7), size = 20, face = "bold", color = "#4B0082"),
                  strip.background = element_rect(fill = "#FFB6C1", color = "green"),
                  plot.title = element_text(size = 22, face = "bold", hjust = .5),
                  axis.text.x = element_text(size = 15),
                  axis.text.y = element_text(size = 20, face = "bold"),
                  axis.title.y = element_text(size = 23))
          
          b <- ggplot(data = df_global, mapping = aes(x = Year, y = Global_Sales)) +
            geom_segment(aes(xend=Year, yend=0, color = Year), size = 2.3, alpha = .8) +
            geom_point(mapping = aes(fill = Year), size = 5, shape = 21) +
            geom_line(group = 1, size = 1.1, linetype = 10, color = "red") +
            xlab("") +
            ylab("Global Sales") +
            theme_classic() +
            theme(plot.title = element_text(size = 25, face = "bold", hjust = .5),
                  axis.title.x = element_text(size = 16, hjust = .5, face = "italic"),
                  axis.title.y = element_text(size = 16, hjust = .5, face = "italic"),
                  axis.text.x = element_text(size = 15),
                  axis.text.y = element_text(size = 20, face = "bold"),
                  legend.position = "none")
          c <- ggplot(data = df_global, mapping = aes(x = Year, y = Global_Sales)) +
            geom_line(size = 1, linetype = 10, color = "blue", group = 1) +
            geom_label(mapping = aes(label=Global_Sales), fill = "blue", size = 6, color = "white", fontface = "bold", alpha = .5) +
            xlab("") +
            ylab("Global Sales") +
            theme_classic() +
            theme(legend.position = "none",
                  strip.text.x = element_text(margin = margin(7, 7, 7, 7), size = 20, face = "bold", color = "#4B0082"),
                  strip.background = element_rect(fill = "#FFB6C1", color = "green"),
                  plot.title = element_text(size = 22, face = "bold", hjust = .5),
                  axis.text.x = element_text(size = 15),
                  axis.text.y = element_text(size = 20, face = "bold"),
                  axis.title.y = element_text(size = 23))
          plot_grid(a, b, c, ncol = 1, nrow = 3) 
======================================================================================================================================================
  library(arules),
  rules <- apriori(data),
  summary(rules),
  
  rules <- apriori(data,parameter = list(minlen = 2, maxlen = 3, support = .3)),
  inspect(rules),
  
  rules <- apriori(data,parameter = list(minlen = 2 ,maxlen = 3, conf = .2),
                   appearance = list(rhs = c ("Global_Sales=[0.01,0.09)]"),defalut = "lhs"))
  inspect(rules)
  
  ==================
    
    library(MASS)
chisq.test(table(data$Platform,data$Genre))

------------------------------------------
  table(data$Platform,data$Genre)
  
  