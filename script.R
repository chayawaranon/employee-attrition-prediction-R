#import library
library(tidyverse)
library(dplyr)
<<<<<<< HEAD

#import data

attritionData <- read.csv("./dataset/Employee-Attrition.csv")
head(attritionData)
tail(attritionData)

print('hello world')
#import data

=======
library(tidyr)

#import data
data <- read.csv('./dataset/HRDataset.csv')
>>>>>>> c2650c56c9a7090e697df3d0b73112739168e8c4

#Data preparaing

#filter null values
preparedData <- data %>% filter(!is.na(EmpID)) %>% select(c(3:33))

#convert to factor
for(index in c(1:7,9,10,26)){
  preparedData[,index] <- factor(preparedData[,index])
}


#Visualization
