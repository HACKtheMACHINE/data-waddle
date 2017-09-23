#### R Template for HACKtheMACHINE September 22nd and 23rd #####################

### Data Preprocessing #########################################################
## Installing required packages and calling libraries ##########################
install.packages("tidyverse")
install.packages("caTools")
install.packages("cluster")

library(magrittr) # adds pipe operator
library(tidyverse) # general data tidy package
library(caTools) # tools for data training
library(cluster) # adds hierarchical clustering 

## Importing dataset ###########################################################
classC_ship1_MRG <- read.csv("classC_ship1_MRG.csv") # imports data to data.frame

dataset <- classC_ship1_MRG

dataset$DateTime <- 
  as.POSIXct(dataset$DateTime, format = "%m/%d/%Y %H:%M:%S %p") %>%
  as.numeric

ggplot(data = dataset) + 
  aes(x = dataset$DateTime, y = dataset$MRG1.TN.GEAR.FWD.BRG.T, 
      color = dataset$Indicator) + 
  geom_point() + ggtitle("Reduction Gear Fwd Bearing Temp vs. Date") +
  xlab("Date") + ylab("MRG Temp") + 
  theme(axis.title.x = element_text(color = "DarkBlue", size = 16),
        axis.title.y = element_text(color = "DarkBlue", size = 16),
        plot.title = element_text(color = "DarkBlue", size = 20))

ggplot(data = dataset.MRG) + 
  aes(x = dataset.MRG$DateTime, y = dataset.MRG$MRG1.TN.GEAR.FWD.BRG.T, 
      color = dataset.MRG$Indicator) + 
  geom_point() + ggtitle("Reduction Gear Fwd Bearing Temp vs. Date") +
  xlab("Date") + ylab("MRG Temp") + 
  theme(axis.title.x = element_text(color = "DarkBlue", size = 16),
        axis.title.y = element_text(color = "DarkBlue", size = 16),
        plot.title = element_text(color = "DarkBlue", size = 20))

ggplot(data = dataset.MRG1) + 
  aes(x = dataset.MRG1$DateTime, y = dataset.MRG1$MRG1.TN.GEAR.FWD.BRG.T, 
      color = dataset.MRG1$Indicator) + 
  geom_point() + ggtitle("Reduction Gear Fwd Bearing Temp vs. Date") +
  xlab("Date") + ylab("MRG Temp") + 
  theme(axis.title.x = element_text(color = "DarkBlue", size = 16),
        axis.title.y = element_text(color = "DarkBlue", size = 16),
        plot.title = element_text(color = "DarkBlue", size = 20))

ggplot(data = dataset.MRG2) + 
  aes(x = dataset.MRG2$DateTime, y = dataset.MRG2$MRG1.TN.GEAR.FWD.BRG.T, 
      color = dataset.MRG2$Indicator) + 
  geom_point() + ggtitle("Reduction Gear Fwd Bearing Temp vs. Date") +
  xlab("Date") + ylab("MRG Temp") + 
  theme(axis.title.x = element_text(color = "DarkBlue", size = 16),
        axis.title.y = element_text(color = "DarkBlue", size = 16),
        plot.title = element_text(color = "DarkBlue", size = 20))

x1 <- grep("MRG1", names(dataset))
x1 <-c(56,x1)
x2 <- grep("MRG2", names(dataset))
x2 <-c(56,x2)

dataset.MRG1 <- subset(dataset, select = x1)
dataset.MRG2 <- subset(dataset, select = x2)
write.csv(dataset.MRG1, "classC_ship1_MRG1.csv")
write.csv(dataset.MRG1, "classC_ship1_MRG2.csv")

## Accounting for missing data #################################################
# Taking care of missing data (If there is no missing data, do not use)
dataset$data.column2 <- ifelse(is.na(dataset$data.column3),
                               ave(dataset$data.column3, 
                                   FUN = function(x) mean(x, na.rm = TRUE)),
                               dataset$data.column3)

## Scaling data features #######################################################
untouched <- select(dataset, data.column1)
dataset %<>% select(data.column2, data.column3, data.column4) # remove data we 
# don't want to scale
dataset %<>% scale # comment out if unnecessary
dataset %<>% cbind(unscaled, .) # return data to data.frame
rm(untouched) # removes unscaled vector

## Handling categorical data ###################################################
dataset$data.column1 %<>% factor(levels = c('Male', 'Female'), labels = c(0, 1))

## Splitting data into training and testing sets ###############################
set.seed(4) # ensures all parties get similar results
split <-dataset %>% sample.split(SplitRatio = 0.75)
data.train <- dataset %>% subset(split == TRUE)
data.test <- dataset %>% subset(split == FALSE)
rm(split)

### Data manipulation ##########################################################
## Creating linear regression based on one independent variable ################
y.pred <-
  lm(formula = data.column4 ~ data.column2, data = data.train ) %>% 
  predict(data.test)

### Data visualization #########################################################
## Creating a scatterplot based on some of the data ############################
ggplot(dataset, aes(data.column2, data.column4)) +
  geom_point(size = 4, color = "Red")
