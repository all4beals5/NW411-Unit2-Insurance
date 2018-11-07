##################################################
### Unit 2 Assignment - Insurance
### MSDS 411-DL-56
### Brandi Beals

##################################################
### Load packages
library(openxlsx)

##################################################
### Set working directory & read data
setwd("C:/Users/Brara/Dropbox/Masters in Predictive Analytics/411-DL-56/W-unit 2 weeks 4 to 6/Unit 2 - Insurance/4 Homework")
# work setwd("C:/Users/bbeals/Dropbox (Personal)/Masters in Predictive Analytics/411-DL-56/W-unit 2 weeks 4 to 6/Unit 2 - Insurance/4 Homework")
ins=read.csv("logit_insurance_test.csv",header=T)

### Convert data types
ins$BLUEBOOK <- as.numeric(gsub("[^0-9.]", "", ins$BLUEBOOK))
ins$INCOME <- as.numeric(gsub("[^0-9.]", "", ins$INCOME))
ins$HOME_VAL <- as.numeric(gsub("[^0-9.]", "", ins$HOME_VAL))
ins$OLDCLAIM <- as.numeric(gsub("[^0-9.]", "", ins$OLDCLAIM))

ins$PRIVATE_USE <- ifelse(ins$CAR_USE=='Private',1,0)
ins$RED_CAR <- ifelse(ins$RED_CAR=='yes',1,0)
ins$FEMALE <- ifelse(ins$SEX=='z_F',1,0)
ins$MARRIED <- ifelse(ins$MSTATUS=='Yes',1,0)
ins$SINGLE_PARENT <- ifelse(ins$PARENT1=='Yes',1,0)
ins$URBAN <- ifelse(ins$URBANICITY=='Highly Urban/ Urban',1,0)
ins$REVOKED <- ifelse(ins$REVOKED=='Yes',1,0)

### Create indicator variables for NA values
ins$AGE_IMP <- ifelse(is.na(ins$AGE)==TRUE,1,0)
ins$YOJ_IMP <- ifelse(is.na(ins$YOJ)==TRUE,1,0)
ins$INCOME_IMP <- ifelse(is.na(ins$INCOME)==TRUE,1,0)
ins$HOME_VAL_IMP <- ifelse(is.na(ins$HOME_VAL)==TRUE,1,0)
ins$CAR_AGE_IMP <- ifelse(is.na(ins$CAR_AGE)==TRUE,1,0)

### Impute with mean
ins$AGE <- na.aggregate(ins$AGE, ins$EDUCATION, mean, na.rm = TRUE)
ins$YOJ <- na.aggregate(ins$YOJ, ins$JOB, mean, na.rm = TRUE)
ins$INCOME <- na.aggregate(ins$INCOME, c(ins$JOB,ins$EDUCATION), mean, na.rm = TRUE)
ins$HOME_VAL <- na.aggregate(ins$HOME_VAL, c(ins$JOB,ins$MSTATUS,ins$INCOME), mean, na.rm = TRUE)
ins$CAR_AGE[is.na(ins$CAR_AGE)] <- mean(ins$CAR_AGE, na.rm = TRUE)

### Create dummy variables
ins <- ins[,-c(9,11,12,16,26)]
ins <- dummy_cols(ins, remove_first_dummy=TRUE)
ins <- ins[,-c(10,11,15)]

names(ins)[names(ins)=="EDUCATION_z_High School"] <- "EDUCATION_High_School"
names(ins)[names(ins)=="EDUCATION_<High School"] <- "EDUCATION_Less_High_School"
names(ins)[names(ins)=="JOB_z_Blue Collar"] <- "JOB_Blue_Collar"
names(ins)[names(ins)=="JOB_Home Maker"] <- "JOB_Home_Maker"
names(ins)[names(ins)=="JOB_"] <- "JOB_Unknown"
names(ins)[names(ins)=="CAR_TYPE_z_SUV"] <- "CAR_TYPE_SUV"
names(ins)[names(ins)=="CAR_TYPE_Sports Car"] <- "CAR_TYPE_Sports_Car"
names(ins)[names(ins)=="CAR_TYPE_Panel Truck"] <- "CAR_TYPE_Panel_Truck"

### Create new fields
ins$HOME_OWNER <- ifelse(ins$HOME_VAL>0,1,0)

### Model scoring
# *** Assumes model5 is available as a global variable ****
# *** You must run the Beals Unit2 Analysis Code script ***

ins$P_TARGET_FLAG <- predict(model5, newdata = ins, type = "response")
ins$P_TARGET_AMT <- predict(targetamtmodel2, newdata = ins, type = "response")

### Subset output
prediction <- insurance[c("INDEX","P_TARGET_FLAG","P_TARGET_AMT")]

### Prediction output 
write.xlsx(prediction, file = "Brandi Beals Unit2 Predictions.xlsx", sheetName = "Predictions", col.names = TRUE)


