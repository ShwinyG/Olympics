olympics <- read.csv("~/Documents/R Studio Work/Final Project/olympics.csv", stringsAsFactors=FALSE)
library(caTools)
olympicsna<-subset(olympics,!is.na(Height))
olympicsna<-subset(olympicsna,!is.na(Age))
olympicsna<-subset(olympicsna,!is.na(Weight))

olympics<-olympicsna

olympics[,-c(1,7,9,12)]
logolympics<- olympics

#Factoring olympics for columns that 
logolympics$Medal[which(is.na(logolympics$Medal))]  <- 0
logolympics$Medal[which(!(logolympics$Medal == 0))]  <- 1
t1 <- sample.split(logolympics, SplitRatio = 0.5)
train <- subset(logolympics, t1 == TRUE)
test <- subset(logolympics, t1 == FALSE)
olympicsna$Sex <- as.numeric(olympicsna$Sex == "M")
View(olympicsna)


set.seed(100)

logolympics$City<-as.factor(logolympics$City)
logolympics$Sport<-as.factor(logolympics$Sport)
logolympics$Season<-as.factor(logolympics$Season)

logolympics$Event<-as.factor(logolympics$Event)
logolympics$Games<-as.factor(logolympics$Games)
logolympics$NOC<-as.factor(logolympics$NOC)
logolympics$Team<-as.factor(logolympics$Team)

logolympics$Height<- as.numeric(logolympics$Height)
logolympics$Medal<- as.numeric(logolympics$Medal)
cor(olympicsna$Weight, olympics$Height)
#There is enough correlation between weight and Height so we can get rid of one of these variables 
#There are less levels for height so we are going to use height 
gLogisticModel<-glm(Medal~Height+Sex+Age+Event+Games+NOC, data= logolympics)

pred<-predict(gLogisticModel, test, type="response")
#pred<-round(pred)
View(pred)
pred1<-table(test$Medal, pred >= .5)
View(pred1)
(pred1[1,1]+pred1[2,2])/(pred1[1,1]+pred1[1,2]+pred1[2,1]+pred1[2,2])
sens<- pred1[2,2]/(pred1[2,1]+pred1[2,1])
spec<- pred1[1,1]/(pred1[1,2]+pred1[1,1])

ggplot(olympics, aes( x= Height, y= Weight)) + geom_point() + geom_smooth(method=lm) + ggtitle("Height to Weight, Corellation = .796")
cor(olympics$Height, olympics$Weight)


