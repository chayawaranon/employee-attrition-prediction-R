#import data
library(tidyverse)
data <- read.csv('./dataset/HRDataset.csv')

# ----------------------------------- Data preparaing ----------------------------------- #


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

#summary
summary(preparedData)

# ----------------------------------- Visualization ----------------------------------- #

#correlation plot
library(corrplot)
correlation <- cor(select(preparedData,c(8,29,30,32,33)))
corrplot(correlation,
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45)

correlation

# Worked Year Histogram
preparedData %>% ggplot(aes(x = WorkedYear)) + 
  geom_histogram(binwidth = 1,color = 'white', fill = 'purple') +
   ggtitle("Worked period of employee") +
   theme_minimal() 

# Age Histogram
preparedData %>%
  filter(Termd == 0) %>% ggplot(aes(x = Age)) + 
  geom_histogram(binwidth = 1,color = 'white', fill = 'purple') +
  ggtitle("Age of employees who are still working") +
  theme_minimal() 

# number of special Projects Histogram
preparedData %>% ggplot(aes(x = SpecialProjectsCount)) + 
  geom_histogram(binwidth = 1,color = 'white', fill = 'purple') +
  ggtitle("Special Project of each employee") +
  xlab('Special Project') +
  theme_minimal() 

# Recent employee satisfaction Histogram
preparedData %>% ggplot(aes(x = EmpSatisfaction)) + 
  geom_histogram(binwidth = 1,color = 'white', fill = 'purple') +
  ggtitle("Recent employee satisfaction") +
  xlab('Satisfaction') +
  theme_minimal()

# Engagement Survey Histogram
preparedData %>% ggplot(aes(x = EngagementSurvey)) + 
  geom_histogram(binwidth = 0.5,color = 'white', fill = 'purple') +
  ggtitle("Engagement Survey") +
  theme_minimal() 

# position
preparedData %>% ggplot() + geom_bar(aes(x = Position), fill = 'darkblue') + 
  ggtitle("Number of employees in each position") +
  theme_minimal() + 
  coord_flip() 
  
preparedData %>% ggplot() + geom_bar(aes(x = Position, fill = Termd),position = 'fill') + 
  ggtitle("Number of employees in each position" , subtitle = 'compared to termination of employee') +
  scale_fill_discrete(name = 'Terminate',label = c('No','Yes')) +
  theme_minimal() +
  coord_flip() 

 # state
preparedData %>% ggplot() + geom_bar(aes(x = State), fill = 'darkblue') + 
  ggtitle("Number of employee in each state") +
  theme_minimal() +
  coord_flip()
  
preparedData %>% ggplot() + geom_bar(aes(x = State, fill = Termd),position = 'fill') + 
  ggtitle("Number of employees in each state" , subtitle = 'compared to termination of employee') +
  scale_fill_discrete(name = 'Terminate', label = c('No','Yes')) +
  theme_minimal() +
  coord_flip()

# sex
preparedData %>% ggplot() + geom_bar(aes(x = Sex), fill = 'purple') +
  ggtitle("Number of employee in each gender") +
  theme_minimal() +
  xlab('Gender')

preparedData %>% ggplot() + geom_bar(aes(x = Sex, fill = Termd), position = 'fill') +
  ggtitle("Number of employees in each gender" , subtitle = 'compared to termination of employee') +
  scale_fill_discrete(name = 'Terminate', label = c('No','Yes')) +
  theme_minimal()

# marital
preparedData %>% ggplot() + 
  geom_bar(aes(x = MaritalDesc), fill = 'purple') +
  ggtitle("Number of employees in each marital status") + 
  xlab('Marital Status') +
  theme_minimal()

preparedData %>% ggplot() + geom_bar(aes(x = MaritalDesc, fill = Termd),position = 'fill') +
  ggtitle("Number of employees in each marital status" , subtitle = 'compared to termination of employee') +
  scale_fill_discrete(name = 'Terminate', label = c('No','Yes')) +
  xlab('Marital Status')+
  theme_minimal()

# critizen
preparedData %>% ggplot() + geom_bar(aes(x = CitizenDesc), fill = 'purple') +
  ggtitle("Number of employees in each citizen") +
  theme_minimal() +
  xlab('Citizen')

preparedData %>% ggplot() + geom_bar(aes(x = CitizenDesc, fill = Termd),position = 'fill') +
  ggtitle("Number of employees in each citizen" , subtitle = 'compared to termination of employee') +
  scale_fill_discrete(name = 'Terminate', label = c('No','Yes')) +
  theme_minimal() +
  xlab('Citizen')

# department
preparedData %>% ggplot() + geom_bar(aes(x = Department), fill = 'darkblue') +
  ggtitle("Number of employees in each department") +
  theme_minimal() 

preparedData %>% ggplot() + geom_bar(aes(x = Department, fill = Termd),position = 'fill') +
  ggtitle("Number of employees in each citizen" , subtitle = 'compared to termination of employee') +
  scale_fill_discrete(name = 'Terminate', label = c('No','Yes')) +
  theme_minimal() +
  xlab('Citizen')

#PerformanceScore 
preparedData %>% ggplot() + geom_bar(aes(x = PerformanceScore ), fill = 'darkblue') +
  ggtitle("Number of employees in each performance score") +
  theme_minimal()

preparedData %>% ggplot() + geom_bar(aes(x = PerformanceScore , fill = Termd),position = 'fill') +
  ggtitle("Number of employees in each performance score" , subtitle = 'compared to termination of employee') +
  scale_fill_discrete(name = 'Terminate', label = c('No','Yes')) +
  theme_minimal()

#FromDiversityJobfair
preparedData %>% ggplot() + geom_bar(aes(x = FromDiversityJobFairID ), fill = 'darkblue') +
  ggtitle("Number of employees from diversity job fair") +
  theme_minimal()

preparedData %>% ggplot() + geom_bar(aes(x = FromDiversityJobFairID , fill = Termd),position = 'fill') +
  ggtitle("Number of employees from diversity job fair" , subtitle = 'compared to termination of employee') +
  scale_fill_discrete(name = 'Terminate', label = c('No','Yes')) +
  theme_minimal()


#PayRate
preparedData %>% ggplot(aes(x = PayRate)) + geom_histogram(color = 'white', fill = 'purple', bins = 20) +
  ggtitle("Distribution of employees in each pay rate") +
  theme_minimal()

#RaceDesc
preparedData %>% ggplot() + geom_bar(aes(x = RaceDesc), fill = 'darkblue') +
  ggtitle("Number of employees in each race") +
  theme_minimal() +
  ylab('Race')

preparedData %>% ggplot() + geom_bar(aes(fill = Termd , y = RaceDesc),position = 'fill') +
  ggtitle("Number of employees in each race" , subtitle = 'compared to termination of employee') +
  scale_fill_discrete(name = 'Terminate', label = c('No','Yes')) +
  theme_minimal()

#HispanicLatino
preparedData %>% ggplot() + geom_bar(aes(x = HispanicLatino), fill = 'darkblue') +
  ggtitle("Number of employees who are Hispanic Latino") +
  theme_minimal()

preparedData %>% ggplot() + geom_bar(aes(fill = Termd , x = HispanicLatino),position = 'fill') +
  ggtitle("Number of employees who are Hispanic Latino" , subtitle = 'compared to termination of employee') +
  scale_fill_discrete(name = 'Terminate', label = c('No','Yes')) +
  theme_minimal()

#ManagerName
preparedData %>% ggplot() + geom_bar(aes(y = ManagerName), fill = 'darkblue') +
  ggtitle("Number of employees in each manager") +
  theme_minimal()

preparedData %>% ggplot() + geom_bar(aes(fill = Termd , y = ManagerName),position = 'fill') +
  ggtitle("Number of employees in each manager" , subtitle = 'compared to termination of employee') +
  scale_fill_discrete(name = 'Terminate', label = c('No','Yes')) +
  theme_minimal()


# ----------------------------------- Model ----------------------------------- #


#Decision tree Model
library(rpart)
library(rpart.plot)

decisionData <- preparedData %>% select(-c(1:7,10,13,14,20,22,23,26,27),-TerminateYear)

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

train_control<-trainControl(method="cv",
                            number=5,
                            search = "random")
model <-train(Termd~.,
              data=decisionData_training,
              trControl=train_control,
              method="rpart")

resp <- predict(decisionTree, decisionData_testing, type = 'class')
resp2 <- predict(model,decisionData_testing)

# Evaluation decision tree model
library(caret)
library(e1071)
confusionMatrix(resp,
                decisionData_testing$Termd,
                positive = "1",
                mode = "prec_recall"
                )

confusionMatrix(resp2,
                decisionData_testing$Termd,
                positive = "1",
                mode = "prec_recall"
)



