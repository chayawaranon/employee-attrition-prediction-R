library(tidyverse)
library(dplyr)
library(tidyr)

#import data
data <- read.csv('./dataset/HRDataset.csv')

#Data preparaing
preparedData <- data %>% filter(!is.na(EmpID)) %>% select(c(3:33))
for(index in c(1:7,9,10,26)){
  preparedData[,index] <- factor(preparedData[,index])
}


#Visualization
