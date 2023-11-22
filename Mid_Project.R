myData <- read.csv("E:/Dataset_midterm_Section(B).csv", header=TRUE,sep=",")
options(max.print=9999)
myData


library(dplyr)
myData$class <- na_if(myData$class, "")
myData


colSums(is.na(myData))


which(is.na(myData$age))
which(is.na(myData$Gender))
which(is.na(myData$class))


myNullLessData<- na.omit(myData)
myNullLessData


most_frequent_age <- names(which.max(table(myData$age)))
myData$age[is.na(myData$age)] <- most_frequent_age
most_frequent_age
myFrequencyAgeData = myData
myFrequencyAgeData
myData<- na.omit(myData)
myData


hist(myNullLessData$age, main = "Histogram of Age", xlab = "Age", ylab = "Number of People", 
     col = "lightblue", freq = TRUE, ylim = c(0, 250))



z_scores <- scale(myNullLessData$age)
age_outlier_indices <- which(abs(z_scores) > 2)
age_outlier_values <- myNullLessData$age[age_outlier_indices]
age_outlier_values


myNullLessData$age[age_outlier_indices] <- NA
myNullLessData


abnorlmal_age_delete <- na.omit(myNullLessData)
abnorlmal_age_delete


max_age <- max(abnorlmal_age_delete$age)
max_age


hist(myNullLessData$age,main = "Histogram of Age after outlier", xlab = "Age", ylab = "Number of People", 
     col = "lightblue", freq = TRUE)


hist(myNullLessData$fare, main = "Histogram of Fare", xlab = "Fare", ylab = "Frequency of room", 
     col = "red", freq = TRUE, ylim = c(0, 250),xlim = c(0, 600),breaks = 5)
max_fare <- max(myNullLessData$fare)
max_fare


sd(abnorlmal_age_delete$age)
sd(myNullLessData$fare)


myNullLessData$who <- gsub("^mannn$", "man", myNullLessData$who)
myNullLessData$who <- gsub("^womannn$", "woman", myNullLessData$who)
myNullLessData$who <- gsub("^womann$", "woman", myNullLessData$who)
myNullLessData$who
myNullLessData

 
myNullLessData$class <- factor(myNullLessData$class,
                       levels = c("First", "Second", "Third"),
                       labels = c(1,2,3))
myNullLessData


myNullLessDataStracture <- str(myNullLessData)


myNullLessDataSummary <- summary(myNullLessData)
myNullLessDataSummary

