#import data
library(tidyverse)

#import data
data <- read.csv('./dataset/HRDataset.csv')

#Data preparaing
preparedData <- data %>% filter(!is.na(EmpID)) %>% select(c(9:11,13:33),-28)

#convert to factor
for(index in c(1:7,9,10,26)){

  preparedData[,index] <- factor(preparedData[,index])
}

preparedData %>% separate(DOB,c("BirthDate","BirthMonth","BirthYear"),sep = "/") %>% 
  separate(DateofHire,c("HireDate","HireMonth","HireYear"),sep = "/") %>% 
  separate(DateofTermination,c("TerminateDate","TerminateMonth","TerminateYear"),sep = "/") -> preparedData

preparedData <- preparedData %>% select(-c("BirthDate","BirthMonth","HireDate","HireMonth","TerminateDate","TerminateMonth"))

for(i in c(1:nrow(preparedData))){
  if(is.na(preparedData$TerminateYear[i])){
    preparedData$TerminateYear[i] <- "19"
  }
}
preparedData$TerminateYear <- as.integer(preparedData$TerminateYear)
preparedData <- preparedData %>% mutate(Age = 119 - as.integer(BirthYear),WorkedYear = 19 - (as.integer(HireYear)-2000))

#Visualization
