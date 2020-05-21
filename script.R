#import data
library(tidyverse)

#import data
data <- read.csv('./dataset/HRDataset.csv')

#Data preparaing
preparedData <- data %>% filter(!is.na(EmpID)) %>% select(-1,-2,-34,-35)
preparedData$HispanicLatino <- tolower(preparedData$HispanicLatino)
for(index in c(1:7,9:12,15:19,22:28)){
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
preparedData <- preparedData %>% mutate(Age = 119 - as.integer(BirthYear),WorkedYear = as.integer(TerminateYear) - (as.integer(HireYear)-2000))

#Visualization

preparedData %>% ggplot(aes(x = WorkedYear)) + geom_histogram(binwidth = 1,color = 'white') # Worked Year Histogram
preparedData %>% ggplot(aes(x = Age)) + geom_histogram(binwidth = 1,color = 'white') # Age Histogram
preparedData %>% ggplot(aes(x = SpecialProjectsCount)) + geom_histogram(binwidth = 1,color = 'white') # number of special Projects Histogram
preparedData %>% ggplot(aes(x = EmpSatisfaction)) + geom_histogram(binwidth = 1,color = 'white') # Recent employee satisfaction Histogram
preparedData %>% ggplot(aes(x = EngagementSurvey)) + geom_histogram(binwidth = 0.5,color = 'white') # Engagement year Histogram

#correlation plot

library(corrplot)
correlation <- cor(select(preparedData,c(8,29,30,32,33)))
corrplot(correlation,
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45)

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
preparedData %>% ggplot() + geom_bar(aes(x = Position))+coord_flip()
preparedData %>% ggplot() + geom_bar(aes(x = Position, fill = Termd),position = 'fill')+coord_flip()
# state
preparedData %>% ggplot() + geom_bar(aes(x = State))+coord_flip()
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

# department
preparedData %>% ggplot() + geom_bar(aes(x = Department))
preparedData %>% ggplot() + geom_bar(aes(x = Department, fill = Termd),position = 'fill')
# Work period
data.frame(workPeriod = ((preparedData$TerminateYear + 2000) - as.numeric(preparedData$HireYear))) %>% 
  ggplot() + geom_histogram(aes(x = workPeriod), bins = 12, color = 'white')
#PerformanceScore 
preparedData %>% ggplot() + geom_bar(aes(x = PerformanceScore ))
preparedData %>% ggplot() + geom_bar(aes(x = PerformanceScore , fill = Termd),position = 'fill')
#FromDiversityJobfair
preparedData %>% ggplot() + geom_bar(aes(x = FromDiversityJobFairID ))
preparedData %>% ggplot() + geom_bar(aes(x = FromDiversityJobFairID , fill = Termd),position = 'fill')
#PayRate
preparedData %>% ggplot(aes(x = PayRate)) + geom_histogram(color = 'white')

#Decision tree Model
library(rpart)
library(rpart.plot)

decisionData <- preparedData %>% select(-c(1:7,10,13,14,22,23,26,27,20),-TerminateYear)
set.seed(222)
test_index = sample(nrow(decisionData),0.25*nrow(decisionData))
decisionData_training <- decisionData[-test_index,]
decisionData_testing <- decisionData[test_index,]

# summary(decisionData_training)
# summary(decisionData_testing)
decisionTree <- rpart(Termd ~., data = decisionData_training)
rpart.plot(decisionTree)
decisionTree$variable.importance

head(predict(decisionTree,decisionData_testing))
pdf("decisionTree.pdf")
rpart.plot(decisionTree)
dev.off()

resp <- predict(decisionTree, decisionData_testing, type = 'class')

# Evaluation decision tree model
library(caret)
library(e1071)
confusionMatrix(resp,
                decisionData_testing$Termd,
                positive = "1",
                mode = "prec_recall"
                )

