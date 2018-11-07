##################################################
### Unit 2 Assignment - Insurance
### MSDS 411-DL-56
### Brandi Beals

##################################################
### Load packages
library(corrplot)
library(fastDummies)
library(zoo)
library(InformationValue)
library(car)

### Functions
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

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
data$URBAN <- ifelse(data$URBANICITY=='Highly Urban/ Urban',1,0)
data$REVOKED <- ifelse(data$REVOKED=='Yes',1,0)

### Imputation
data$AGE_IMP <- ifelse(is.na(data$AGE)==TRUE,1,0)
data$YOJ_IMP <- ifelse(is.na(data$YOJ)==TRUE,1,0)
data$INCOME_IMP <- ifelse(is.na(data$INCOME)==TRUE,1,0)
data$HOME_VAL_IMP <- ifelse(is.na(data$HOME_VAL)==TRUE,1,0)
data$CAR_AGE_IMP <- ifelse(is.na(data$CAR_AGE)==TRUE,1,0)

data$AGE <- na.aggregate(data$AGE, data$EDUCATION, mean, na.rm = TRUE)
data$YOJ <- na.aggregate(data$YOJ, data$JOB, mean, na.rm = TRUE)
data$INCOME <- na.aggregate(data$INCOME, c(data$JOB,data$EDUCATION), mean, na.rm = TRUE)
data$HOME_VAL <- na.aggregate(data$HOME_VAL, c(data$JOB,data$MSTATUS,data$INCOME), mean, na.rm = TRUE)
data$CAR_AGE[is.na(data$CAR_AGE)] <- mean(data$CAR_AGE, na.rm = TRUE)

### Remove Illogical Data
data <- data[data$CAR_AGE>=0,]

# JOB - possibly impute using education and gender

### New fields and limiting columns
data$HOME_OWNER <- ifelse(data$HOME_VAL>0,1,0)
datanumeric <- data[,-c(1,3,9,11,12,13,14,16,19,26,32,33,34,35,36)]

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

### Create dummy variables
datadummy <- data[,-c(1,3,9,11,12,16,26)]
datadummy <- dummy_cols(datadummy, remove_first_dummy=TRUE)
datadummy <- datadummy[,-c(8,9,13)]

### Rename values
# JOB Unknown (N/A) Impute?
names(datadummy)[names(datadummy)=="EDUCATION_z_High School"] <- "EDUCATION_High_School"
names(datadummy)[names(datadummy)=="EDUCATION_<High School"] <- "EDUCATION_Less_High_School"
names(datadummy)[names(datadummy)=="JOB_z_Blue Collar"] <- "JOB_Blue_Collar"
names(datadummy)[names(datadummy)=="JOB_Home Maker"] <- "JOB_Home_Maker"
names(datadummy)[names(datadummy)=="JOB_"] <- "JOB_Unknown"
names(datadummy)[names(datadummy)=="CAR_TYPE_z_SUV"] <- "CAR_TYPE_SUV"
names(datadummy)[names(datadummy)=="CAR_TYPE_Sports Car"] <- "CAR_TYPE_Sports_Car"
names(datadummy)[names(datadummy)=="CAR_TYPE_Panel Truck"] <- "CAR_TYPE_Panel_Truck"

corrplot(cor(datadummy, use="complete.obs"), method="color", type="upper", tl.col="black", tl.cex=.7, 
         addCoef.col="black", number.cex=.5)

# positive correlation between having a masters degree and being a lawyer
# negative correlation between having a blue collar job and private use vehicle (meaning they use commercial vehicles)
# positive correlation between being female and driving an SUV
# positive correlation between having a panel truck and high bluebook value

##################################################
### Model creation
### Full logistic model (model 1)
model1 <- glm(TARGET_FLAG~., data=datadummy, family=binomial())
summary(model1)
model1prediction <- predict(model1, type = "response")
hist(model1prediction)
optCutOff1 <- optimalCutoff(datadummy$TARGET_FLAG, model1prediction) # 0.4668226
vif(model1)

### Limit logistic model (model 2)
model2 <- glm(TARGET_FLAG~KIDSDRIV+INCOME+TRAVTIME+BLUEBOOK+TIF+OLDCLAIM+CLM_FREQ+REVOKED+MVR_PTS+PRIVATE_USE+MARRIED+SINGLE_PARENT+URBAN+HOME_OWNER+
                CAR_TYPE_SUV+CAR_TYPE_Sports_Car+CAR_TYPE_Van+CAR_TYPE_Panel_Truck+CAR_TYPE_Pickup, data=datadummy, family=binomial())
summary(model2)
model2prediction <- predict(model2, type = "response")
hist(model2prediction)
optCutOff2 <- optimalCutoff(datadummy$TARGET_FLAG, model2prediction) # 0.4836009
vif(model2)

### Full probit model (model 3)
model3 <- glm(TARGET_FLAG~., data=datadummy, family=binomial(link="probit"))
summary(model3)
model3prediction <- predict(model3, type = "response")
hist(model3prediction)
optCutOff3 <- optimalCutoff(datadummy$TARGET_FLAG, model3prediction) # 0.4733581
vif(model3)

### Limit probit model (model 4)
model4 <- glm(TARGET_FLAG~KIDSDRIV+INCOME+TRAVTIME+BLUEBOOK+TIF+OLDCLAIM+CLM_FREQ+REVOKED+MVR_PTS+PRIVATE_USE+MARRIED+SINGLE_PARENT+URBAN+HOME_OWNER+
                CAR_TYPE_SUV+CAR_TYPE_Sports_Car+CAR_TYPE_Van+CAR_TYPE_Panel_Truck+CAR_TYPE_Pickup, data=datadummy, family=binomial(link="probit"))
summary(model4)
model4prediction <- predict(model4, type = "response")
hist(model4prediction)
optCutOff4 <- optimalCutoff(datadummy$TARGET_FLAG, model4prediction) # 0.4797163
vif(model4)

### Stepwise model
model5 <- step(model1, direction="both")
summary(model5)
model5prediction <- predict(model5, type = "response")
hist(model5prediction)
optCutOff5 <- optimalCutoff(datadummy$TARGET_FLAG, model5prediction) # 0.5070527
vif(model5)

### Model for TARGET_AMT
datatargetamt <- cbind(data[,3], datadummy[,-1])
names(datatargetamt)[1] <- "TARGET_AMT"

targetamtmodel <- lm(TARGET_AMT~., data=datatargetamt)
summary(targetamtmodel)

targetamtmodel2 <- step(targetamtmodel, direction="both")
summary(targetamtmodel2)

##################################################
### Model selection
matrix(data=c(
  AIC(model1),
  AIC(model2),
  AIC(model3),
  AIC(model4),
  AIC(model5),
  BIC(model1),
  BIC(model2),
  BIC(model3),
  BIC(model4),
  BIC(model5),
  (-2*logLik(model1, REML = TRUE)),
  (-2*logLik(model2, REML = TRUE)),
  (-2*logLik(model3, REML = TRUE)),
  (-2*logLik(model4, REML = TRUE)),
  (-2*logLik(model5, REML = TRUE)),
  ks_stat(actuals=datadummy$TARGET_FLAG, predictedScores=model1prediction),
  ks_stat(actuals=datadummy$TARGET_FLAG, predictedScores=model2prediction),
  ks_stat(actuals=datadummy$TARGET_FLAG, predictedScores=model3prediction),
  ks_stat(actuals=datadummy$TARGET_FLAG, predictedScores=model4prediction),
  ks_stat(actuals=datadummy$TARGET_FLAG, predictedScores=model5prediction)
), ncol=4, byrow=FALSE, 
dimnames=list(c("model1","model2","model3","model4","model5"),c("AIC","BIC","Log Likelihood Deviance","KS")))
# model5 does better on every metric except the ks metric

# Area under the curve
plotROC(datadummy$TARGET_FLAG, model1prediction) #0.8137
plotROC(datadummy$TARGET_FLAG, model2prediction) #0.804
plotROC(datadummy$TARGET_FLAG, model3prediction) #0.8136
plotROC(datadummy$TARGET_FLAG, model4prediction) #0.8039
plotROC(datadummy$TARGET_FLAG, model5prediction) #0.8132

# Concordance
matrix(data=c(
Concordance(datadummy$TARGET_FLAG, model1prediction),
Concordance(datadummy$TARGET_FLAG, model2prediction),
Concordance(datadummy$TARGET_FLAG, model3prediction),
Concordance(datadummy$TARGET_FLAG, model4prediction),
Concordance(datadummy$TARGET_FLAG, model5prediction)
), ncol=4, byrow=TRUE,
dimnames=list(c("model1","model2","model3","model4","model5"),c("Concordance","Discordance","Tied","Pairs")))

# Misclassification error
misClassError(datadummy$TARGET_FLAG, model5prediction, threshold = optCutOff1)
misClassError(datadummy$TARGET_FLAG, model5prediction, threshold = optCutOff2)
misClassError(datadummy$TARGET_FLAG, model5prediction, threshold = optCutOff3)
misClassError(datadummy$TARGET_FLAG, model5prediction, threshold = optCutOff4)
misClassError(datadummy$TARGET_FLAG, model5prediction, threshold = optCutOff5)
# lowest error on model5, but all basically the same

# Sensitivity
sensitivity(datadummy$TARGET_FLAG, model5prediction, threshold = optCutOff1)
sensitivity(datadummy$TARGET_FLAG, model5prediction, threshold = optCutOff2)
sensitivity(datadummy$TARGET_FLAG, model5prediction, threshold = optCutOff3)
sensitivity(datadummy$TARGET_FLAG, model5prediction, threshold = optCutOff4)
sensitivity(datadummy$TARGET_FLAG, model5prediction, threshold = optCutOff5)

# Specificity
specificity(datadummy$TARGET_FLAG, model5prediction, threshold = optCutOff1)
specificity(datadummy$TARGET_FLAG, model5prediction, threshold = optCutOff2)
specificity(datadummy$TARGET_FLAG, model5prediction, threshold = optCutOff3)
specificity(datadummy$TARGET_FLAG, model5prediction, threshold = optCutOff4)
specificity(datadummy$TARGET_FLAG, model5prediction, threshold = optCutOff5)
