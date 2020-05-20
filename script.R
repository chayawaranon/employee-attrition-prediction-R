#import library
library(tidyverse)

#import data
data <- read.csv('./dataset/HRDataset.csv')

#Data preparaing
preparedData <- data %>% filter(!is.na(EmpID)) %>% select(c(9:11,13:33),-28)
preparedData$HispanicLatino <- tolower(preparedData$HispanicLatino)
for(index in c(1,3,4,8:12,16,17,18,20)){
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
# Worked Year Histogram
preparedData %>% ggplot(aes(x = WorkedYear)) + geom_histogram(binwidth = 1,color = 'white')
# Age Histogram
preparedData %>% ggplot(aes(x = Age)) + geom_histogram(binwidth = 1,color = 'white') 
# number of special Projects Histogram
preparedData %>% ggplot(aes(x = SpecialProjectsCount)) + geom_histogram(binwidth = 1,color = 'white')
# Recent employee satisfaction Histogram
preparedData %>% ggplot(aes(x = EmpSatisfaction)) + geom_histogram(binwidth = 1,color = 'white')
# Engagement year Histogram
preparedData %>% ggplot(aes(x = EngagementSurvey)) + geom_histogram(binwidth = 0.5,color = 'white')
# position
preparedData %>% ggplot() + geom_bar(aes(x = Position, fill = Termd),position = 'stack')+coord_flip()
preparedData %>% ggplot() + geom_bar(aes(x = Position, fill = Termd),position = 'fill')+coord_flip()
# state
preparedData %>% ggplot() + geom_bar(aes(x = State, fill = Termd),position = 'stack')+coord_flip()
preparedData %>% ggplot() + geom_bar(aes(x = State, fill = Termd),position = 'fill')+coord_flip()
# sex
preparedData %>% ggplot() + geom_bar(aes(x = Sex))
preparedData %>% ggplot() + geom_bar(aes(x = Sex, fill = Termd),position = 'fill')
# marital
preparedData %>% ggplot() + geom_bar(aes(x = MaritalDesc))
preparedData %>% ggplot() + geom_bar(aes(x = MaritalDesc, fill = Termd),position = 'fill')
# critizen
preparedData %>% ggplot() + geom_bar(aes(x = CitizenDesc))
preparedData %>% ggplot() + geom_bar(aes(x = CitizenDesc, fill = Termd),position = 'fill')
