#import data
library(tidyverse)

#import data
data <- read.csv('./dataset/HRDataset.csv')

#Data preparaing

#filter null values
preparedData <- data %>% filter(!is.na(EmpID)) %>% select(c(3:33))

#convert to factor
for(index in c(1:7,9,10,26)){
  preparedData[,index] <- factor(preparedData[,index])
}


#Visualization
