library(tidyverse)
library(dplyr)

#import data
attritionData <- read.csv("./dataset/Employee-Attrition.csv")
head(attritionData)
<<<<<<< HEAD
tail(attritionData)

=======
>>>>>>> c07e21c16ef9063b07fad7431d0e630dec21ca62
#Data preparaing
attritionData <- attritionData %>% select(-EmployeeCount,-EmployeeNumber)

#Visualization
