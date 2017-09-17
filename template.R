#### R Template for HACKtheMACHINE September 22nd and 23rd #####################

### Data Preprocessing #########################################################
## Installing required packages and calling libraries ##########################
install.packages("magrittr")
install.packages("tidyverse")
install.packages("caTools")
install.packages("ggplot2")
install.packages("cluster")
library(magrittr) # adds pipe operator
library(tidyverse) # general data tidy package
library(caTools) # tools for data training
library(ggplot2) # graphics package (contained in tidyverse)
library(cluster) # adds hierarchical clustering 

## Importing dataset ###########################################################
dataset <- read.csv("dataset.csv") # imports data to data.frame

## Accounting for missing data #################################################
# Taking care of missing data (If there is no missing data, do not use)
dataset$annual.income <- ifelse(is.na(dataset$annual.income),
                                ave(dataset$annual.income, 
                                    FUN = function(x) mean(x, na.rm = TRUE)),
                                dataset$annual.income)

## Scaling data features #######################################################
unscaled <- select(dataset, gender, spending.score)
dataset %<>% select(age, annual.income) # remove data we don't want to scale
dataset %<>% scale # comment out if unnecessary
dataset %<>% cbind(unscaled, .) # return data to data.frame
rm(unscaled) # removes unscaled vector

## Handling categorical data ###################################################
dataset$gender %<>% factor(levels = c('Male', 'Female'), labels = c(0, 1))

## Splitting data into training and testing sets ###############################
set.seed(4) # ensures all parties get similar results
split <-dataset %>% sample.split(SplitRatio = 0.75)
data.train <- dataset %>% subset(split == TRUE)
data.test <- dataset %>% subset(split == FALSE)
rm(split)

### Data manipulation ##########################################################
## Creating linear regression based on one independent variable ################
y.pred <-
  lm(formula = spending.score ~ age, data = data.train ) %>% 
  predict(data.test)

### Data visualization #########################################################
## Creating a scatterplot based on some of the data ############################
ggplot(dataset, aes(age, spending.score)) +
  geom_point(size = dataset$annual.income, color = "Red")
