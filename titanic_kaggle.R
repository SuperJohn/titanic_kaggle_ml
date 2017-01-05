# set the working directory
setwd("~/dev/titanic_kaggle/")

# read data files
test.data <- read.csv("~/dev/titanic_kaggle/test.csv", na.strings = c("NA",""))
train.data <- read.csv("~/dev/titanic_kaggle/train.csv", na.strings = c("NA",""))

# convert ints factors 
train.data$Survived = factor(train.data$Survived)
train.data$Pclass = factor(train.data$Pclass)
# test.data$Survived = factor(test.data$Survived)
# test.data$Pclass = factor(test.data$Pclass)


# DETECTING MISSING VALUES
sum(is.na(train.data$Embarked))

sapply(train.data, function(df){
    sum(is.na(df==TRUE) /length(df));
    })

install.packages("Amelia")
require(Amelia)

missmap(train.data, main="Missing Map")

Ameliaview() 

### IMPUTTING MISSING VALUES ###
table(train.data$Embarked, useNA = "always") ;
# Change NA Values to S, the most probable port
train.data$Embarked[which(is.na(train.data$Embarked))] = 'S' ; #using which to subset

# get table of salutations from names using grep
train.data$Name = as.character(train.data$Name) 
table_words = table(unlist(strsplit(train.data$Name, "\\s+")))
sort(table_words [grep('\\.',names(table_words))], decreasing = TRUE)

# find missing values
library(stringr)
tb = cbind(train.data$Age, str_match(train.data$Name, "[a-zA-z]+\\."))
table(tb[is.na(tb[,1]),2])

# impute mean value for missing ages
mean.mr = mean(train.data$Age[grepl(" Mr\\.", train.data$Name) & !is.na(train.data$Age)])
mean.mrs = mean(train.data$Age[grepl(" Mrs\\.", train.data$Name) & !is.na(train.data$Age)])
mean.dr = mean(train.data$Age[grepl(" Dr\\.", train.data$Name) & !is.na(train.data$Age)])
mean.miss = mean(train.data$Age[grepl(" Miss\\.", train.data$Name) & !is.na(train.data$Age)])
mean.master = mean(train.data$Age[grepl(" Master\\.", train.data$Name) & !is.na(train.data$Age)])

# assign missing value with the mean value of each title
train.data$Age[grepl(" Mr\\.", train.data$Name) & is.na(train.data$Age)] = mean.mr
train.data$Age[grepl(" Mrs\\.", train.data$Name) & is.na(train.data$Age)] = mean.mrs
train.data$Age[grepl(" Dr\\.", train.data$Name) & is.na(train.data$Age)] = mean.dr
train.data$Age[grepl(" Miss\\.", train.data$Name) & is.na(train.data$Age)] = mean.miss
train.data$Age[grepl(" Master\\.", train.data$Name) & is.na(train.data$Age)] = mean.master

### DATA VISUALIZATION ###
barplot(table(train.data$Survived), main="Passenger Survival", names = c("Perished", "Survived"))
barplot(table(train.data$Pclass), main = "Passenger Class", names = c("first", "seconds", "third"))
barplot(table(train.data$Sex), main = "Passenger Gender")
hist(train.data$Age, main = "Passenger Age", xlab = "Age")
barplot(table(train.data$SibSp), main = "Passenger Siblings")
barplot(table(train.data$Parch), main = "Passenger Parch")
hist(train.data$Fare, main = "Passenger Fare", xlab = "Fare")
barplot(table(train.data$Embarked), main = "Port of Embarkation")
counts = table( train.data$Survived, train.data$Sex )
counts
barplot(counts, col = c("darkblue", "red"), legend = c("Perished", "Survived"), main = "Passenger Survival by Sex")

# does Pclass affect survival rate? YUP!
counts = table( train.data$Survived, train.data$Pclass)
barplot(counts, col = c("darkblue", "red"), legend = c("Perished", "Survived"), main = "Passenger Survival by Class")

# Gender Composition of Class
counts = table( train.data$Sex, train.data$Pclass)
barplot(counts, col = c("darkblue", "red"), legend = rownames(counts), main = "Passenger Gender by Class")

# What does age distribution look like? - Age Histogram
hist(train.data$Age[which(train.data$Survived == "0")], main = "Passenger Age Histogram",xlab = "Age", ylab = "Count", col = "blue", breaks = seq(0,80,by=2))
hist(train.data$Age[which(train.data$Survived == "1")], col = "red", add = T, breaks = seq(0,80,by=2))

# what's the relationship between age and survival rate?
boxplot(train.data$Age ~ train.data$Survived , main = "Passenger Survival by Age" , xlab = "Survived", ylab = "Age" )

# categorize people into different age groups
train.child = train.data$Survived[train.data$Age < 13]
length(train.child[which(train.child == 1)]) / length(train.child)

train.youth = train.data$Survived[train.data$Age >= 15 & train.data$Age <25 ]
length(train.youth[which(train.youth == 1 )]) / length(train.youth)

train.adult = train.data$Survived[train.data$Age >= 25 & train.data$Age <65 ]
length(train.adult[which(train.adult == 1 )]) / length(train.adult)

train.senior = train.data$Survived[train.data$Age >= 65 ]
length(train.senior[which(train.senior == 1 )]) / length(train.senior)


# Junk Stuff
age_data <- table(train.data$Age, useNA = "always") ;
age_data <- as.data.frame(age_data)
age_data$var1 <- as.integer(age_data$var1) 
age_data$var1[which(is.na(age_data$var1))] = '0' ;
plot(age_data)
View(age_data)
rm(age_data)
