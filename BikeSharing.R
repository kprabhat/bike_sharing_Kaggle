setwd("F:/DA Projects/Kaggle/Bike Sharing")
library(ggplot2)
library(graphics)
library(caret)
library(randomForest)

set.seed(415)
train <- read.csv("train.csv")
test <- read.csv("test.csv")
View(train)
View(test)

test$casual <- 0
test$registered <- 0
test$count <- 0
# combine train and test data
data_all <- rbind(train, test)
View(data_all)
str(data_all)

# check for missing values
table(is.na(data_all))



# data exploration
# ggplot(data_all, aes(season)) + geom_histogram()
hist(data_all$season)
hist(data_all$weather)
hist(data_all$humidity)
hist(data_all$holiday)
hist(data_all$workingday)
hist(data_all$temp)
hist(data_all$atemp)
hist(data_all$windspeed)


# change categorical variables into factor
data_all$season <- as.factor(data_all$season)
data_all$holiday <- as.factor(data_all$holiday)
data_all$workingday <- as.factor(data_all$workingday)
data_all$weather <- as.factor(data_all$weather)



# date and hour extraction from datetime variable
Dates <- as.POSIXlt(data_all$datetime, format = "%m/%d/%Y %H:%M")
data_all$hour <- Dates$"hour"
data_all$date <- Dates$"mday"
# 0 for jan, 1 for feb and so on.. 11 for dec
data_all$month <- Dates$"mon"
data_all$year <- 1900 + Dates$"year"
# 0 for sunday, 1 for monday and so on...to 6 for saturday
data_all$weekday <- Dates$wday


# separate training and testing dataset
training <- data_all[data_all$date < 20, ]
testing <- data_all[data_all$date >= 20, ]


# hypothesis visualization
boxplot(training$count~training$hour,xlab="hour", ylab="count of total users")
boxplot(training$casual~training$hour,xlab="hour", ylab="casual users")
boxplot(training$registered~training$hour,xlab="hour", ylab="registered users")

boxplot(training$casual~training$weekday,xlab="weekdays", ylab="casual users")
boxplot(training$registered~training$weekday,xlab="weekdays", ylab="registered users")

boxplot(training$registered~training$weather,xlab="weather", ylab="registered users")
boxplot(training$casual~training$weather,xlab="weather", ylab="casual users")

boxplot(training$registered~training$temp,xlab="temp", ylab="registered users")
boxplot(training$casual~training$temp,xlab="temp", ylab="casual users")

boxplot(training$registered~training$year,xlab="year", ylab="registered users")
boxplot(training$casual~training$year,xlab="year", ylab="casual users")

boxplot(training$registered~training$windspeed,xlab="year", ylab="registered users")
boxplot(training$casual~training$windspeed,xlab="year", ylab="casual users")

boxplot(training$registered~training$humidity,xlab="humidity", ylab="registered users")
boxplot(training$casual~training$humidity,xlab="humidity", ylab="casual users")



# feature engineering
library(rattle)
library(rpart.plot)
library(RColorBrewer)
View(training)

dt1 <- rpart(registered~hour, data = training)
fancyRpartPlot(dt1)

dt2 <- rpart(casual~hour, data= training)
fancyRpartPlot(dt2)

# day part into hour bins
data_all$dp_reg <- 0
data_all$dp_reg[data_all$hour < 6.5] <- 1
data_all$dp_reg[data_all$hour>8 & data_all$hour<16] <- 3
data_all$dp_reg[data_all$hour == 7] <- 4
data_all$dp_reg[data_all$hour == 8] <- 5
data_all$dp_reg[data_all$hour==18 | data_all$hour==19] <- 6
data_all$dp_reg[data_all$hour==16 | data_all$hour==17] <- 7

data_all$dp_cas <- 0
data_all$dp_cas[data_all$hour<7.5] <- 1
data_all$dp_cas[data_all$hour>7.5 & data_all$hour<9.5] <- 2
data_all$dp_cas[data_all$hour>=20] <- 3
data_all$dp_cas[data_all$hour>9.5 & data_all$hour<20] <- 4


# temperature bins
t1 <- rpart(registered~temp, data = training)
fancyRpartPlot(t1)

t2 <- rpart(casual~temp, data= training)
fancyRpartPlot(t2)

data_all$tb_reg <- 0
data_all$tb_reg[data_all$temp<13] <- 1
data_all$tb_reg[data_all$temp>13 & data_all$temp<23] <- 2
data_all$tb_reg[data_all$temp>23 & data_all$temp<30] <- 3
data_all$tb_reg[data_all$temp>30] <- 4

data_all$tb_cas <- 0
data_all$tb_cas[data_all$temp<15] <- 1
data_all$tb_cas[data_all$temp>15 & data_all$temp<23] <- 2
data_all$tb_cas[data_all$temp>23 & data_all$temp<30] <- 3
data_all$tb_cas[data_all$temp>30] <- 4

# divide year into quarters
data_all$tp[data_all$year==2011 & data_all$month<3] <- 1
data_all$tp[data_all$year==2011 & data_all$month>2] <- 2
data_all$tp[data_all$year==2011 & data_all$month>5] <- 3
data_all$tp[data_all$year==2011 & data_all$month>8] <- 4
data_all$tp[data_all$year==2012 & data_all$month<3] <- 5
data_all$tp[data_all$year==2012 & data_all$month>2] <- 6
data_all$tp[data_all$year==2012 & data_all$month>5] <- 7
data_all$tp[data_all$year==2012 & data_all$month>8] <- 8
table(data_all$tp)

# daytype
data_all$daytype <- 0
data_all$daytype[data_all$holiday==0 & data_all$workingday==0] <- "Weekend"
data_all$daytype[data_all$holiday==0 & data_all$workingday==1] <- "Working day"
data_all$daytype[data_all$holiday==1 & data_all$workingday==0] <- "Holiday"

# weekend or weekday
data_all$weekend <- 0
data_all$weekend[data_all$weekday==0 | data_all$weekday==6] <- 1


# replace missing windspeed values by randomForest fitting on other factors
table(data_all$windspeed==0)
fac <- data_all$windspeed==0
wind_0 <- subset(data_all, fac)
wind_1 <- subset(data_all, !fac)

fit_ws <- randomForest(windspeed ~ season+ weather + humidity + month + temp + year + atemp, data=wind_1,importance=TRUE, ntree=250)
predictions_ws <- predict(fit_ws, wind_0)
wind_0$windspeed <- predictions_ws
data_all$windspeed[fac] <- predictions_ws


# change into factor
data_all$hour <- as.factor(data_all$hour)
data_all$date <- as.factor(data_all$date)
data_all$month <- as.factor(data_all$month)
data_all$year <- as.factor(data_all$year)
data_all$weekday <- as.factor(data_all$weekday)
data_all$dp_reg <- as.factor(data_all$dp_reg)
data_all$dp_cas <- as.factor(data_all$dp_cas)
data_all$tb_reg <- as.factor(data_all$tb_reg)
data_all$tb_cas <- as.factor(data_all$tb_cas)
data_all$daytype <- as.factor(data_all$daytype)
data_all$tp <- as.factor(data_all$tp)

# training and testing
training <- data_all[as.numeric(as.character(data_all$date)) < 20, ]
testing <- data_all[as.numeric(as.character(data_all$date)) >= 20, ]


# modelling
training$reg1=training$registered+1
training$cas1=training$casual+1
training$logcas=log(training$cas1)
training$logreg=log(training$reg1)
testing$logreg=0
testing$logcas=0

boxplot(training$logreg~train$weather,xlab="weather", ylab="registered users")
boxplot(training$logreg~train$season,xlab="season", ylab="registered users")



modfit_reg <- randomForest(logreg ~ hour +workingday + weekend+ weekday +holiday+ daytype +tb_reg+humidity+atemp+windspeed+season+weather+dp_reg+year+tp+ year, data=training,importance=TRUE, ntree=250)
predictions_reg <- predict(modfit_reg, testing)
testing$logreg <- predictions_reg

modfit_cas <- randomForest(logcas ~hour + daytype + weekend+ weekday + humidity+atemp+tb_cas+windspeed+season+weather+holiday+workingday+dp_cas+year+tp+ year, data=training,importance=TRUE, ntree=250)
predictions_cas <- predict(modfit_cas,testing)
testing$logcas <- predictions_cas

testing$registered <- exp(testing$logreg)-1
testing$casual <- exp(testing$logcas)-1
testing$count <- testing$casual+testing$registered
solution <- data.frame(datetime=as.POSIXlt(testing$datetime, format = "%m/%d/%Y %H:%M"), count=testing$count)
write.csv(solution, file="solution.csv",row.names=FALSE)
