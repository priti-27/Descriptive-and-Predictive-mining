---
title: "script.rmd"
author: "RStudio"
date: "16/05/2021"
output: html_document
---

```{r setup, include=FALSE}
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

knitr::opts_chunk$set(echo = TRUE)
```
```{r, warning=FALSE,message=FALSE}
NR <- vg%>% select(Year_of_Release) %>%
  group_by(Year_of_Release)%>% 
  summarise(Count=n())

plot_ly(data=NR,x=~Year_of_Release)%>%
  add_trace(y=~Count,name="Number of release",mode="lines",type = 'scatter',
color = '#000000') %>%
layout(title = "Fig.1 Number of games released per year",
    yaxis = list(title="Games released (count)"))

```

```{r, warning=FALSE,message=FALSE}
Sales_NA <- vg%>% select(Year_of_Release,NA_Sales) %>%
  group_by(Year_of_Release)%>% 
  summarise(Sum_NA_Sales=sum(NA_Sales))
Sales_EU <- vg%>% select(Year_of_Release,EU_Sales) %>%
  group_by(Year_of_Release)%>% 
  summarise(Sum_EU_Sales=sum(EU_Sales))
Sales_JP <- vg%>% select(Year_of_Release,JP_Sales) %>%
  group_by(Year_of_Release)%>% 
  summarise(Sum_JP_Sales=sum(JP_Sales))
Sales_OH <- vg%>% select(Year_of_Release,Other_Sales) %>%
  group_by(Year_of_Release)%>% 
  summarise(Sum_OH_Sales=sum(Other_Sales))

Sales_evo <- Reduce(function(x,y) merge(x,y,all=TRUE,by="Year_of_Release"),list(Sales_NA,Sales_EU,Sales_JP,Sales_OH))

plot_ly(data=Sales_evo,x=~Year_of_Release)%>%
  add_trace(y=~Sum_NA_Sales,name="North America Sales",mode="lines",type = 'scatter') %>%
  add_trace(y=~Sum_EU_Sales,name="Europe Sales",mode="lines",type = 'scatter') %>%
  add_trace(y=~Sum_JP_Sales,name="Japan Sales",mode="lines",type = 'scatter') %>%
  add_trace(y=~Sum_OH_Sales,name="Other Sales",mode="lines",type = 'scatter') %>%
  layout(title = "Fig.2 Total units sold by year of release and by region",
  yaxis = list(title="Sales (in millions of units)"))
```



```{r, warning=FALSE,message=FALSE}
MSales_NA <- vg%>% select(Year_of_Release,NA_Sales) %>%
  group_by(Year_of_Release)%>% 
  summarise(Mean_NA_Sales=mean(NA_Sales),SD_NA_Sales =sd(NA_Sales))
MSales_EU <- vg%>% select(Year_of_Release,EU_Sales) %>%
  group_by(Year_of_Release)%>% 
  summarise(Mean_EU_Sales=mean(EU_Sales),SD_EU_Sales =sd(EU_Sales))
MSales_JP <- vg%>% select(Year_of_Release,JP_Sales) %>%
  group_by(Year_of_Release)%>% 
  summarise(Mean_JP_Sales=mean(JP_Sales),SD_JP_Sales =sd(JP_Sales))
MSales_OH <- vg%>% select(Year_of_Release,Other_Sales) %>%
  group_by(Year_of_Release)%>% 
  summarise(Mean_OH_Sales=mean(Other_Sales),SD_OH_Sales =sd(Other_Sales))

MSales_evo <- Reduce(function(x,y) merge(x,y,all=TRUE,by="Year_of_Release"),list(MSales_NA,MSales_EU,MSales_JP,MSales_OH))

plot_ly(data=MSales_evo,x=~Year_of_Release)%>%
  add_trace(y=~Mean_NA_Sales,name="North America Sales",
            mode="lines",type = 'scatter',
            error_y = ~list(value = SD_NA_Sales,
                            color = '#000000')) %>%
  add_trace(y=~Mean_EU_Sales,name="Europe Sales",
            mode="lines",type = 'scatter',
            error_y = ~list(value = SD_EU_Sales,
                            color = '#000000')) %>%
  add_trace(y=~Mean_JP_Sales,name="Japan Sales",
            mode="lines",type = 'scatter',
            error_y = ~list(value = SD_JP_Sales,
                            color = '#000000')) %>%
  add_trace(y=~Mean_OH_Sales,name="Other Sales",
            mode="lines",type = 'scatter',
            error_y = ~list(value = SD_OH_Sales,
                            color='#000000'))%>%
    layout(title = "Fig.3 Average units sold per game by year of release and by region",
    yaxis = list(title="Sales (in millions of units)"))
```

```{r, warning=FALSE,message=FALSE}
nintendoplatforms = c("3DS","DS","GB","GBA","N64","GC", "NES","SNES","Wii","WiiU")
sonyplatforms = c("PS","PS2","PSP","PS3","PS4","PSV")
segaplatforms = c("GEN","SCD","DC","GG","SAT")
msplatforms = c("XB","X360", "XOne")
otherplatforms = c("2600","3DO","NG","PCFX","TG16","WS")
pc= c('PC')

vg$Platformvendor[vg$Platform %in% nintendoplatforms] <- "Nintendo"
vg$Platformvendor[vg$Platform %in% sonyplatforms] <- "Sony"
vg$Platformvendor[vg$Platform %in% msplatforms] <- "Microsoft"
vg$Platformvendor[vg$Platform %in% segaplatforms] <- "Sega"
vg$Platformvendor[vg$Platform %in% pc] <- "PC"
vg$Platformvendor[vg$Platform %in% otherplatforms] <- "Other"

Platform_level <- vg%>% group_by(Platform)%>% 
  summarise(Sales = sum(Global_Sales))

Platform_level<- left_join(Platform_level,vg[,c("Platformvendor","Platform")])
Platform_level <- unique(Platform_level)
Platform_level<- Platform_level%>% arrange(Platformvendor,Sales)
Platform_level$Platform <-factor(Platform_level$Platform , levels = Platform_level$Platform)

Platform_level$color <- c(brewer.pal(3,'Greys'),
                          brewer.pal(9,'Blues'),"#000000",
                          brewer.pal(7,'Purples'),
                          brewer.pal(5,'Oranges'),
                          brewer.pal(6,'Greens'))
                          
N_platform <-vg%>%group_by(Platform,Year_of_Release) %>% 
  summarise(Sales=sum(Global_Sales))

N_platform<- N_platform%>%group_by(Year_of_Release)%>% 
  mutate(YearTotal = sum(Sales),Percent = Sales/YearTotal)
N_platform$Platform <-factor(N_platform$Platform , levels = Platform_level$Platform)

plot_ly(N_platform,x=~Year_of_Release,y=~Percent,
          color=~Platform,colors=Platform_level$color,
          hoverinfo='text',type='bar',
          text=~paste('Year : ', Year_of_Release,'<br>',
                      'Platform : ',Platform,'<br>',
                      'Sales (in millions of units): ', Sales,'<br>',
                      'Percentage in total game sales: ',round(Percent,2)*100,'%'))%>%
    layout(barmode='stack',
    title='Fig.4 Percentage of game platforms in total sales by year of release')


```

## Predictive analysis

```{r, warning=FALSE,message=FALSE}
D_model <-  vg[,c('NA_Sales',
                'Name',
                'Year_of_Release',
                'Publisher',
                'Platform',
                'Genre',
                'Critic_Score',
                'Critic_Count',
                'User_Score',
                'User_Count')]
```
### Data preprocessing

#### Data imputation and data splitting

```{r, warning=FALSE,message=FALSE}
naPerc<-apply(D_model,2, function(x){length(which(is.na(x)))/dim(D_model)[1]})
naPerc
```

```{r, warning=FALSE,message=FALSE}
# delete observations w missing val in Genre & Name
D_model <- D_model %>% filter(!is.na(Genre)&!is.na(Name))
preproc <- preProcess(D_model[,-1], method = c("bagImpute")) # alternative :knnImpute, median
X_train <- predict(preproc, D_model[D_model$Year_of_Release <= quantile(D_model$Year_of_Release, .9),-1])
X_test <- predict(preproc, D_model[D_model$Year_of_Release > quantile(D_model$Year_of_Release, .9),-1])
Y_test <- D_model[1:dim(X_test)[1],1]
length(Y_test)==dim(X_test[1])[1]
Y_train <- D_model[dim(X_test)[1]+1:dim(X_train)[1],1]
length(Y_train)==dim(X_train[1])[1]
D_train <- cbind(Y_train,X_train)
train_indx <- which(D_train$Year_of_Release<quantile(D_train$Year_of_Release,0.875))
```

```{r, warning=FALSE,message=FALSE}
P_avg_Sales  <-D_train%>%
  filter(Year_of_Release>=max(Year_of_Release)-3)%>% 
  group_by(Publisher)%>% 
  mutate(avgSales = mean(Y_train))%>%ungroup()%>%
  select(Publisher,avgSales)%>% unique()
  
D_train<- left_join(D_train,P_avg_Sales,by="Publisher")
D_train[is.na(D_train)]<-0
D_train <- D_train%>% select(-Publisher,-Name)
X_test <- left_join(X_test,P_avg_Sales,by="Publisher")
X_test[is.na(X_test)]<-0
X_test <- X_test%>% select(-Publisher,-Name)
```

```{r, warning=FALSE,message=FALSE}
cs <- preProcess(D_train[,c(5:9)],method=c("center","scale"))
D_train <- cbind(D_train[,c(1:4)],predict(cs,D_train[,c(5:9)]))
cs <- preProcess(X_test[,c(4:8)],method=c("center","scale"))
X_test <- cbind(X_test[,c(1:3)],predict(cs,X_test[,c(4:8)]))
```

Here is how the final data that will be used to train the models looks like

```{r, warning=FALSE,message=FALSE}
head(D_train)
```
As as sanity check, the predictors are not highly correlated.

```{r, warning=FALSE,message=FALSE}
cormax <- cor(D_train[,c(5:9)])
corrplot(cormax,method="number")
```
