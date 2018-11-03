##################################################
### Unit 2 Assignment - Insurance
### MSDS 411-DL-56
### Brandi Beals

##################################################
### Load packages
library(corrplot)
library(fastDummies)

##################################################
### Set working directory & read data
setwd("C:/Users/Brara/Dropbox/Masters in Predictive Analytics/411-DL-56/W-unit 2 weeks 4 to 6/Unit 2 - Insurance/4 Homework")
# work setwd("C:/Users/bbeals/Dropbox (Personal)/Masters in Predictive Analytics/411-DL-56/W-unit 2 weeks 4 to 6/Unit 2 - Insurance/4 Homework")
insurance=read.csv("logit_insurance.csv",header=T)

str(insurance)
summary(insurance)
hist(insurance$TARGET_FLAG)
sum(insurance$TARGET_FLAG)/(sum(insurance$TARGET_FLAG)+sum(insurance$TARGET_FLAG==0)) # 26.38% of records have claims

### Convert data types
insurance$BLUEBOOK <- as.numeric(gsub("[^0-9.]", "", insurance$BLUEBOOK))
insurance$INCOME <- as.numeric(gsub("[^0-9.]", "", insurance$INCOME))
insurance$HOME_VAL <- as.numeric(gsub("[^0-9.]", "", insurance$HOME_VAL))
insurance$OLDCLAIM <- as.numeric(gsub("[^0-9.]", "", insurance$OLDCLAIM))

##################################################
### Exploratory data analysis
### Vehicle Details
hist(as.numeric(insurance$BLUEBOOK), breaks=30) # frequency decreases as prices rise, right-tailed
hist(insurance$CAR_AGE) # how can there be negative values? most cars are newer, with few over 20 years old
plot(insurance$CAR_USE) # most people use vehicles for private use
plot(insurance$CAR_TYPE) # most vehicles are SUVs, followed by minivan, pickup truck, sports car, van, and panel truck
plot(insurance$RED_CAR) # about a third of all cars are red

### Demographics & Family Life
hist(insurance$AGE) # normally distributed with a minor right-skew
plot(insurance$SEX) # slightly more people are female
plot(insurance$MSTATUS) # slightly more people are married
plot(insurance$PARENT1) # most people are not single parents
hist(insurance$HOMEKIDS) # most people have 0 kids, while those with kids have 2
hist(insurance$KIDSDRIV) # most people don't have any kids driving, with very few having 4

### Job & Education
hist(as.numeric(insurance$INCOME), breaks=50) # median income 54,000 (typical national average), right tailed
hist(as.numeric(insurance$HOME_VAL), breaks=50) # many customers do not own homes, right-tailed
plot(insurance$JOB) # some nulls, but mostly blue collar jobs, followed by clerical, professional, manager, lawyer, student, home maker, and doctor
hist(insurance$YOJ, breaks=20) # normally distributed with the exception of a peak at 0, some NAs
plot(insurance$EDUCATION) # most customers have a high school diploma with many having a bachelors or masters, few have less than high school or a phd
hist(insurance$TRAVTIME, breaks=20) # right-skewed with most travel time around 35 minutes
plot(insurance$URBANICITY) # most vehicles are located in urban areas

### Customer Information
hist(insurance$TIF, breaks=30) # most customers have been with the company only a few years and very few more than 20 years
hist(insurance$CLM_FREQ) # most customers haven't filed any claims, with a few filing 2, 1, and 3
hist(as.numeric(insurance$OLDCLAIM), breaks=30) # most customers have no claims, heavily right-tailed
hist(insurance$MVR_PTS, breaks=15) # most customers don't have any points and the frequency decreases with the increase in points
plot(insurance$REVOKED) # most people have not had their license revoked

##################################################
### Preparation and transformations
data <- insurance
data$PRIVATE_USE <- ifelse(data$CAR_USE=='Private',1,0)
data$RED_CAR <- ifelse(data$RED_CAR=='yes',1,0)
data$FEMALE <- ifelse(data$SEX=='z_F',1,0)
data$MARRIED <- ifelse(data$MSTATUS=='Yes',1,0)
data$SINGLE_PARENT <- ifelse(data$PARENT1=='Yes',1,0)
data$HOME_OWNER <- ifelse(data$HOME_VAL>0,1,0)
data$URBAN <- ifelse(data$URBANICITY=='Highly Urban/ Urban',1,0)
data$REVOKED <- ifelse(data$REVOKED=='Yes',1,0)
datanumeric <- data[,-c(1,3,9,11,12,13,14,16,19,26)]

### Correlation Matrix
corrplot(cor(datanumeric, use="complete.obs"), method="color", type="upper", tl.col="black", tl.cex=.7, 
         addCoef.col="black", number.cex=.8)

# being a home owner is highly correlated with home value
# women are negatively correlated with a red car
# being married and home ownership are correlated
# income and home value are correlated
# the number of claims and amount of the claims are correlated
# being married and a single parent are negatively correlated
# the number of kids at home and kids that can drive are positively correlated
# a customer's age and the number of kids at home are negatively correlated
# nothing is highly correlated with the response variable

### Imputation
# JOB


### Create dummy variables
datadummy <- data[,-c(1,3,9,11,12,16,26)]
datadummy <- dummy_cols(datadummy, remove_first_dummy=TRUE)
datadummy <- datadummy[,-c(8,9,13)]
corrplot(cor(datadummy, use="complete.obs"), method="color", type="upper", tl.col="black", tl.cex=.7, 
         addCoef.col="black", number.cex=.5)


basicmodel <- lm(TARGET_FLAG~., data=data)
summary(basicmodel)

### Remove Illogical Data
# data <- data[data$CAR_AGE>=0,]


##################################################
### Model creation

##################################################
### Model selection

