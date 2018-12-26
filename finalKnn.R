#Read csv 
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




knnOlympics<-glmOlympics
View(knnOlympics)
knnOlympics<-knnOlympics[, -c(8,10)]
View(knnOlympics)
hist(knnOlympics$Age)
knnOlympics$Agenumerical<-as.numeric(knnOlympics$Age)
sum(is.na(knnOlympics$Age))
knnOlympics$Age[knnOlympics$Agenumerical < 15] <- "below15"
knnOlympics$Age[knnOlympics$Agenumerical >=15 & knnOlympics$Agenumerical <= 20]<- "15-20"
knnOlympics$Age[knnOlympics$Agenumerical >20 & knnOlympics$Agenumerical <= 25]<- "20-25"
knnOlympics$Age[knnOlympics$Agenumerical >25 & knnOlympics$Agenumerical <= 30]<- "25-30"
knnOlympics$Age[knnOlympics$Agenumerical >30 & knnOlympics$Agenumerical <= 35]<- "30-35"
knnOlympics$Age[knnOlympics$Agenumerical >35 & knnOlympics$Agenumerical <= 40]<- "35-40"
knnOlympics$Age[knnOlympics$Agenumerical >40 & knnOlympics$Agenumerical <= 45]<- "40-45"
knnOlympics$Age[knnOlympics$Agenumerical >45 & knnOlympics$Agenumerical <= 50]<- "45-50"
knnOlympics$Age[knnOlympics$Agenumerical > 50] <- "above 70"
table(knnOlympics$Age)
hist(knnOlympics$Weight)
knnOlympics$Weightnumerical<- as.numeric(knnOlympics$Weight)
sum(is.na(knnOlympics$Weightnumerical))
knnOlympics$Weight[knnOlympics$Weight < 20] <- "below20"
knnOlympics$Weight[knnOlympics$Weightnumerical >=20 & knnOlympics$Weightnumerical <= 40]<- "20-40"
knnOlympics$Weight[knnOlympics$Weightnumerical >40 & knnOlympics$Weightnumerical <= 60]<- "40-60"
knnOlympics$Weight[knnOlympics$Weightnumerical >60 & knnOlympics$Weightnumerical <= 80]<- "60-80"
knnOlympics$Weight[knnOlympics$Weightnumerical >80 & knnOlympics$Weightnumerical <= 100]<- "80-100"
knnOlympics$Weight[knnOlympics$Weightnumerical >100 & knnOlympics$Weightnumerical <= 120]<- "100-120"
knnOlympics$Weight[knnOlympics$Weightnumerical >120 & knnOlympics$Weightnumerical <= 140]<- "120-140"
knnOlympics$Weight[knnOlympics$Weightnumerical >140 & knnOlympics$Weightnumerical <= 160]<- "140-160"
knnOlympics$Weight[knnOlympics$Weightnumerical >160 & knnOlympics$Weightnumerical <= 180]<- "160-180"
knnOlympics$Weight[knnOlympics$Weightnumerical > 180] <- "above 180"

#knnOlympics<-knnOlympics[,-c(8:9)]
library(caTools)
set.seed(100)
split <- sample.split(knnOlympics, SplitRatio = 75/100)
olympicsKnnTrain <- subset(knnOlympics, split == TRUE)
olympicsKnnTest <- subset(knnOlympics, split == FALSE)
View(olympicsKnnTest)




write.csv(olympicsKnnTest, file = "olympicsKnnTest.csv",row.names=FALSE, na="")
write.csv(olympicsKnnTrain, file = "olympicsKnnTrain.csv",row.names=FALSE, na="")


library(class)
table<-table(olympicsKnnTrain$Medal)
cl <- factor(c(rep("0", table[1]), rep("1", table[2])))
library(NbClust)
library(factoextra)
wss<-function(k){
  kmeans(knnOlympics,k, nstart = 10)$tot.withinss
}
  



View(knnOlympics)
#fv<-fviz_nbclust(knnOlympics, method="wss")


predknn<- prediction(table, olympicsKnnTest$Outcome)
perfknn<- performance(predknn, measure = "tpr", x.measure = "fpr") 
plot<-plot(perfknn, col=rainbow(10))

knn<-knn(olympicsKnnTrain, olympicsKnnTest, cl, k = 2, l = 0, prob = FALSE, use.all = TRUE)
predknn<-table(test1$Outcome, knn)

summary(pred1)
coefficients(pred1[,])

sensitivityknn<-56/(149+56)
specificityknn<-264/(264+131)

f1score<- 2/(1/(specificityknn)+(1/(sensitivityknn)))




library(e1071)
olympicsKnnTrain$Medal<-as.factor(olympicsKnnTrain$Medal)
olympicsKnnTest$Medal<-as.factor(olympicsKnnTest$Medal)

naiveBayesOlympics<-naiveBayes(Medal~., data=olympicsKnnTrain)

prednaive<-predict(naiveBayesOlympics, olympicsKnnTest)

table(prednaive, olympicsKnnTest$Medal)


length(prednaive)




library(ggplot2)

bp<- ggplot(knnOlympics, aes(x=0, y= "Age", fill = Age)) + geom_bar(width = 1, stat = "identity")
plot(bp)

pi<-bp + coord_polar("y", start=0)
plot(pi)

library(pROC)
croc<-roc(knnOlympics$Medal, as.numeric(predknn >= 0.5))





