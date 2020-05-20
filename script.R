library(tidyverse)
library(dplyr)

#import data
attritionData <- read.csv("./dataset/Employee-Attrition.csv")
head(attritionData)
tail(attritionData)

#Data preparaing
attritionData <- attritionData %>% select(-EmployeeCount,-EmployeeNumber)

#Visualization
