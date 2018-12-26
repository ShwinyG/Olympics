#TODO: Creat a new data set from the original. retain all the ones in the new dataset
#Read csv 
accuracy <- function(table){
  return((table[1,1] + table[2,2]) / (table[1,1] + table[1,2] + table[2,1] + table[2,2]))
}
sensitivy <- function(table){
  return((table[2,2]) / (table[1,2] + table[2,2]))
}
specificity <- function(table){
  return((table[1,1]) / (table[1,1] + table[1,2]))
}
F1 <- function(table){
  return(((2 * sensitivy(table) * specificity(table)) / (sensitivy(table) + specificity(table))))
}


olympics <- read.csv("~/Documents/R Studio Work/Final Project/olympics.csv", stringsAsFactors=FALSE)
#remove unecessary rows
olympics<-olympics[,-c(1,7,9,12)]
#creating a new data frame to omit the NAs 
olympicsOmit<-olympics
#defining lose as 0 and won as a 1 
olympicsOmit$Medal[which(is.na(olympicsOmit$Medal))]<-0
olympicsOmit$Medal[which(!(olympicsOmit$Medal==0))]<-1
#omitting NAs 
olympicsOmit <- na.omit(olympicsOmit)
#Veiwing tables to see if the 0 and 1 funcion worked 
table(olympicsOmit$Medal)
#Creating a new dataframe with even quantities of wins and losses to optimise our results 
subsetLoser<-subset(olympicsOmit, olympicsOmit$Medal==0)
table(subsetLoser$Medal)
library(dplyr)
logOlympics<-sample_n(subsetLoser, 30181)
View(logOlympics)
subsetWinner<-subset(olympicsOmit, olympicsOmit$Medal==1)
#Making a new table of the equal quantities 
glmOlympics<-rbind(logOlympics, subsetWinner)
table(glmOlympics$Medal)
#making the categorical variables into factors for glm 
glmOlympics$Name<-as.factor(glmOlympics$Name)
glmOlympics$Sex<-as.factor(glmOlympics$Sex)
glmOlympics$NOC<-as.factor(glmOlympics$NOC)
glmOlympics$Season<-as.factor(glmOlympics$Season)
glmOlympics$Sport<-as.factor(glmOlympics$Sport)
glmOlympics$Event<-as.factor(glmOlympics$Event)
class(glmOlympics$Season)
glmOlympics$Medal<-as.numeric(glmOlympics$Medal)
class(glmOlympics$Medal)


#splitting olympics into test and train data using CAtools 
library(caTools)
set.seed(100)
split <- sample.split(glmOlympics$Sport, SplitRatio = 0.7)
train <- subset(glmOlympics, split == TRUE)
test <- subset(glmOlympics, split == FALSE)                      

gLogisticModel<-glm(Medal~Height+Sex+Age+Event+NOC, data= glmOlympics)

summary(gLogisticModel)

pred<-predict(gLogisticModel, test, type="response")

pred1<-table(test$Medal, pred >= .5)

accuracy(pred1)
sensitivy(pred1)
specificity(pred1)
(pred1[1,1]+pred1[2,2])/(pred1[1,1]+pred1[1,2]+pred1[2,1]+pred1[2,2])
sens<- pred1[2,2]/(pred1[2,1]+pred1[2,2])
spec<- pred1[1,1]/(pred1[1,2]+pred1[1,1])
F1(pred1)

library(ROCR)
pred2<- prediction(pred, test$Medal)
perf<- performance(pred2, measure = "tpr", x.measure = "fpr") 



plot<-plot(perf, col=rainbow(10), main="glmROC")





library(pROC)
roc<-roc(test$Medal, as.numeric(pred >= 0.5))


test$Medal

auc(test$Medal, as.numeric(pred >= 0.5))

length(test$Medal) == length(pred)

sum(test$Medal)

                      