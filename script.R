library(tidyverse)

#import data
attritionData <- read.csv("./dataset/Employee-Attrition.csv")
head(attritionData)

#Data preparaing
attritionData <- attritionData %>% select(-EmployeeCount,-EmployeeNumber)

#Visualization
