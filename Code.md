##***Import Data***

## Loading Data with read.table command 
>crime.data<-read.table("C:\\Users\\Zeshan\\Documents\\AnalyticsProject\\crime.data.txt",header=TRUE)

## Checking the structure of Data with str and summary
>str(crime.data)
>summary(crime.data)

## Checking the head of data
>head(crime.data)

## ***Data Cleaning***

## Removing duplicate values with duplicated fucntion
> crime.data <- subset(crime.data, !duplicated(crime.data$Case.Description))

## Removing Null values from the data
>crime.data <- subset(crime.data,!is.na(crime.data$Ward))
>crime.data <- subset(crime.data,!is.na(crime.data$Loaction.Description))
>crime.data <- subset(crime.data, !is.na(crime.data$Block))
>crime.data <- subset(crime.data,!is.na(crime.data$Longitude))
>crime.data <- subset(crime.data,!is.na(crime.data$Latitude))
>crime.data <- subset(crime.data, !is.na(crime.data$Case.Description))


## Checking and then removing null vaues with specific which() fucntion
>crime.data[-which(is.na(crime.data$Location)), ]
>crime.data <- crime.data[-which(is.na(crime.data$Location)), ]

## Checking and removing imputed values due to data migration
> crime.data <- crime.data[crime.data$Case.Description != "Case.Description#",]

## Checking the head of the Date
> head(crime.data$Date)

## Changing the date to Date formatusing POSIXIT() funtion
>crime.data$date <- as.POSIXlt(crime.data$Date,format= "%m/%d/%Y %H:%M")

## Import library Chron to sort time from Date
>library(chron)

## Taking time out of Date with the help of time fucntion
crime.data$time<-times(format(crime.data$Date, "%H:%M:%S"))



## Creating time tags for time series model
>time.tag <- chron(times= c("00:00:00", "06:00:00", "12:00:00", "18:00:00","23:59:00"))

## Checking if time tag column has been created or not
>table(crime.data$time.tag)

## Spliting the time in to time tags with cut()
>crime.data$time.tag <- cut(crime.data$time, breaks= time.tag,labels= c("00-06","06-12", "12-18", "18-00"), include.lowest=TRUE)

## Recode the date variable by strping it from date and storing it to the date with specific format 
>crime.data$date <- as.POSIXlt(strptime(crime.data$date,format= "%Y-%m-%d"))

##Creating days for time series analysis
>crime.data$day <- weekdays(crime.data$date, abbreviate= TRUE)

## Grouping similar values in Primary.Type and saving it to new column "crime"
>crime.data$crime <- as.character(crime.data$Primary.Type)
>crime.data$crime <- ifelse(crime.data$crime %in% c("CRIM SEXUAL ASSAULT","PROSTITUTION", "SEX OFFENSE"), "SEX", crime.data$crime)
>crime.data$crime <- ifelse(crime.data$crime %in% c("MOTOR VEHICLE THEFT"),"MVT", crime.data$crime)
>crime.data$crime <- ifelse(crime.data$crime %in% c("GAMBLING", "INTERFERE WITH PUBLIC OFFICER", "INTERFERENCE WITH PUBLIC OFFICER", "INTIMIDATION","LIQUOR LAW VIOLATION", "OBSCENITY", "NON-CRIMINAL”, “PUBLIC PEACE VIOLATION","PUBLIC INDECENCY", "STALKING", "NON-CRIMINAL (SUBJECT SPECIFIED)"),"NONVIO", crime.data$crime)
>crime.data$crime <- ifelse(crime.data$crime == "CRIMINAL DAMAGE", "DAMAGE",crime.data$crime)
>crime.data$crime <- ifelse(crime.data$crime == "CRIMINAL TRESPASS","TRESPASS", crime.data$crime)
>crime.data$crime <- ifelse(crime.data$crime %in% c("NARCOTICS", "OTHERNARCOTIC VIOLATION", "OTHER NARCOTIC VIOLATION"), "DRUG", crime.data$crime)
>crime.data$crime <- ifelse(crime.data$crime == "DECEPTIVE PRACTICE","FRAUD", crime.data$crime)
>crime.data$crime <- ifelse(crime.data$crime %in% c("KIDNAPPING", "WEAPONS VIOLATION", "OFFENSE INVOLVING CHILDREN"), "VIO", crime.data$crime)


## Checking crime columns
>table(crime.data$crime)

## Converting Arrest to binary form
>crime.data$Arrest <- ifelse(as.character(crime.data$Arrest) == "Y", 1, 0)


## ***Visualizing***

## Importing ggplot library to create time series plot
>library(ggplot2)

## To install any library not present 
>utils:::menuInstallPkgs()

## Plot count of crime
>qplot(crime.data$time.tag, xlab="Time of day", main = "Crimes by time of day") + scale_y_continuous("Number of crimes")

##factor Day with 7 values of day
>crime.data$day <- factor(crime.data$day, levels= c("Mon", "Tue", "Wed","Thu", "Fri", "Sat", "Sun"))

##Plot day with count of crime 
>qplot(crime.data$day, xlab="day", main = "Crimes by time of day") + scale_y_continuous("Number of crimes")

##PLot week with count of crime
>qplot(crime.data$day, xlab="Time of week", main = "Crimes by time of week") + scale_y_continuous("Number of crimes")

##Factor month by its values
>crime.data$month <- factor(crime.data$month, levels= c("Jan", "Feb", "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

##Plot month with count of crime
>qplot(crime.data$Year, xlab="Time of month", main = "Crimes by time of month") + scale_y_continuous("Number of crimes")

##Plot Year with count of crime
>qplot(crime.data$Year, xlab="Time of year", main = "Crimes by time of year") + scale_y_continuous("Number of crimes")


##***Modelling***

##Checking the length of unique values of Beat
>length(unique(crime.agg$Beat))

##Checking the length of unique values required for the data modelling
>length(unique(crime.agg$date))


##Listing the crime data in to temp variable
>temp <- aggregate(crime.data$crime, by= list(crime.data$crime,crime.data$time.tag), FUN= length)

##Sort dates with unique values
>dates <- sort(as.character(unique(crime.agg$date)))

##Sort date by beats in roder to sync it with the columns
>names(temp)<-c("Beat","date")
>temp <- orderBy(~Beat, data= temp)

##model final data set and view it
>model.data <- aggregate(crime.agg[, c("count", "Arrest")], by=list(crime.agg$Beat, as.character(crime.agg$date)), FUN= sum)
>names(model.data) <- c("Beat", "date", "count", "Arrest")

##merge other unique values to the  model data
>model.data <- merge(temp, model.data, by= c("Beat", "date"), all.x= TRUE)

#View model
>View(model.data)

##Check fro null values again
model.data$count[is.na(model.data$count)] <- 0
>model.data$Arrest[is.na(model.data$Arrest)] <- 0

##now add week days and months to the data model
>model.data$day <- weekdays(as.Date(model.data$date), abbreviate= TRUE)
>model.data$month <- months(as.Date(model.data$date), abbreviate= TRUE)

##Create pastDays fucntions  to load past days fot time series

pastDays <- function(x) {c(0, rep(1, x))}

##Check model with past day
>model.data$past.crime.1 <- ave(model.data$count, model.data$Beat,FUN= function(x) filter(x, pastDays(1), sides= 1))


#Check model for past day 7
model.data$past.crime.7 <- ave(model.data$count, model.data$Beat,FUN= function(x) filter(x, pastDays(7), sides= 1))

##Check model for last 30 days (month basically)
>model.data$past.crime.30 <- ave(model.data$count, model.data$Beat,FUN= function(x) filter(x, pastDays(30), sides= 1))

##filling zeor values to the data with pastDays() funtions
>meanNA <- function(x){mean(x, na.rm= TRUE)}
>model.data$past.crime.1 <- ifelse(is.na(model.data$past.crime.1),meanNA(model.data$past.crime.1), model.data$past.crime.1)
>model.data$past.crime.7 <- ifelse(is.na(model.data$past.crime.7),meanNA(model.data$past.crime.7), model.data$past.crime.7)
>model.data$past.crime.30 <- ifelse(is.na(model.data$past.crime.30),meanNA(model.data$past.crime.30), model.data$past.crime.30)
>model.data$past.arrest.30 <- ave(model.data$Arrest, model.data$Beat,FUN= function(x) filter(x, pastDays(30), sides= 1))
>model.data$past.arrest.30 <- ifelse(is.na(model.data$past.arrest.30),meanNA(model.data$past.arrest.30), model.data$past.arrest.30)

##Create new column policing from arrest
>model.data$policing <- ifelse(model.data$past.crime.30 == 0, 0,model.data$past.arrest.30/model.data$past.crime.30)

#Create new column from past crime
>model.data$crime.trend <- ifelse(model.data$past.crime.30 == 0, 0,model.data$past.crime.7/model.data$past.crime.30)

##Create new column from months to seggregate the trend in months
>model.data$season <- as.factor(ifelse(model.data$month %in% c("Mar", "Apr","May"), "spring",ifelse(model.data$month %in% c("Jun", "Jul","Aug"), "summer",ifelse(model.data$month %in% c("Sep", "Oct","Nov"), "fall", "winter"))))

##Importing library for finding correalation matrix
>library(psych)

##Ploting correlation
>model.cor <- cor(model.data[, c("count", "past.crime.1", "past.crime.7","past.crime.30","policing", "crime.trend")])
>cor(model.cor)
>model.data <- orderBy(~date, data= model.data)

##Divinding data in to Train and Test with 90%/10% approach
>rows <- c(1:floor(nrow(model.data)*0.9))
>test.data <- model.data[-rows, ]
>model.data <- model.data[rows, ]

##Importing MASS Library
library(MASS)

##Creating neagtive Binomial model
>crime.model <- glm.nb(count ~past.crime.1 + past.crime.7 + past.crime.30 + policing + crime.trend + factor(day) + season, data= model.data)

##checking AIC value
>summary(crime.model)

##Creating Poisson Model
>crime.model.poisson <- glm.nb(count ~past.crime.1 + past.crime.7 + past.crime.30 + policing + crime.trend + factor(day) + season,family=poisson, data= model.data)

##Checking the AIC values
>summary(crime.model.poisson)

##Predicting model with predictfucntion(residual)
>crime.model.pred <- predict(crime.model, test.data, type= "response")

##Predicting model with predictfucntion(residual)
>crime.model.poisson.pred <- predict(crime.model.poisson, test.data, type= "response")

##Calculate the RMSE value negative binomial model
>sqrt(mean((test.data$count - crime.model.pred)^2))

##Calculate RMSE value for Poisson Model 
>sqrt(mean((test.data$count - crime.model.poisson.pred)^2))

##Create a plot for actual vs predicted for negative binomial model
>vali <- data.frame(test.data$count, crime.model.pred)
> names(vali) <- c("actual", "predicted")
> validate$bucket <- with(validate, cut(predicted, breaks=quantile(predicted, probs= seq(0, 1, 0.1)),include.lowest1= TRUE, labels= c(1:10)))
> vali2 <- aggregate(vali[, c("actual", "predicted")], by=list(vali$bucket), FUN=mean)

##Create a plot for actual vs predicted for Poisson model
>vali1 <- data.frame(test.data$count, crime.model.poisson.pred)
> names(vali1) <- c("actual", "predicted")
> validate$bucket <- with(vali1, cut(predicted, breaks=quantile(predicted, probs= seq(0, 1, 0.1)),include.lowest1= TRUE, labels= c(1:10)))
> vali12 <- aggregate(vali1[, c("actual", "predicted")], by=list(vali1$bucket), FUN=mean)
