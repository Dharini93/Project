train<-read.csv("bank.csv",header = T,sep = ",",na.strings = "")
train
str(train)
summary(train)
test <-read.csv("bank-additional.csv",header =T,sep = ",")
test
summary(test)
str(test)

colSums(is.na(train))
dim(train)

colSums(is.na(test))

################replacing misssing values in job column
unique(train$job)
levels <- levels(train$job)
levels[length(levels) + 1] <- "unknown"
train$job <- factor(train$job, levels = levels)
train$job[is.na(train$job)] <- "unknown"
unique(train$job)

################replacing missing values in marital.
str(train$marital)
unique(train$marital)
##table(train$marital)##replacing missing with mode
##table(train$marital,train$age)##age greater than 30 is married and less than 30 is single
levels<-levels(train$marital)
levels[length(levels) + 1]<-"unknown"
train$marital<-factor(train$marital,levels = levels)
train$marital[is.na(train$marital)]<-"unknown"
unique(train$marital)
str(train)
###############replacing missing values in education column
str(train$education)
unique(train$education)
levels<-levels(train$education)
levels[length(levels) + 1]<-"unknown"
train$education<-factor(train$education,levels = levels)
train$education[is.na(train$education)]<-"unknown"
unique(train$education)
##############replacing missing values in default column
table(train$default)
sum(is.na(train$default))
levels<-levels(train$default)
levels[length(levels)+1]<-"unknown"
train$default<-factor(train$default,levels = levels)
train$default[is.na(train$default)]<-"unknown"
table(train$default)
############# replacing missing values in housing
table(train$housing)
sum(is.na(train$housing))
levels<-levels(train$housing)
levels[length(levels) + 1]<-"unknown"
train$housing<-factor(train$housing,levels = levels)
train$housing[is.na(train$housing)]<-"unknown"
table(train$housing)
###################replacing missing values in loan
table(train$loan)
sum(is.na(train$loan))
levels<-levels(train$loan)
levels[length(levels) + 1]<-"unknown"
train$loan<-factor(train$loan,levels = levels)
train$loan[is.na(train$loan)]<-"unknown"
table(train$loan)
############replacing missing values in contact communication type
table(train$contact)
sum(is.na(train$contact))
#############replacing missing values in month
table(train$month)
sum(is.na(train$month))
################replacing missing values in day of week
table(train$day_of_week)
sum(is.na(train$day_of_week))
##############replacing missing values in duration
str(train$duration)
sum(is.na(train$duration))
plot(train$duration)
###################replacing values in campaign
sum(is.na(train$campaign))
str(train$campaign)
plot(train$campaign)
######identify outliers
boxplot(train$duration)
hist(train$duration)
#############new variable created for duration
train$dur <- train$duration + 10
train$dur
train$log_duration<-log(train$dur)
train$log_duration
hist(train$log_duration)

table(train$log_duration)
boxplot(train$log_duration)

test$dur <- test$duration
test$log_duration <- log(test$duration)
test$log_duration
#################Removing a variable
train <- train[-c(13)]
train
str(train)

#################Decision Tree
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)

dt_model<- rpart(y ~ ., data = train)
fancyRpartPlot(dt_model)

summary(dt_model)


#################Testing Decision Tree
prediction <- predict(dt_model, train, type = "class")
prediction

confusion.matrix <- prop.table(table(prediction, train$y))
confusion.matrix
confusionMatrix(prediction,train$y)

############Decision tree on test data
prediction<-predict(dt_model,test,type="class")
prediction

#############RandomForest
library(party)
library(randomForest)

rf_model <- randomForest(y~., data = train)
rf_model
