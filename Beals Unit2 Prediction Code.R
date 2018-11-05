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
insurance=read.csv("logit_insurance_test.csv",header=T)

### Convert data types
insurance$BLUEBOOK <- as.numeric(gsub("[^0-9.]", "", insurance$BLUEBOOK))
insurance$INCOME <- as.numeric(gsub("[^0-9.]", "", insurance$INCOME))
insurance$HOME_VAL <- as.numeric(gsub("[^0-9.]", "", insurance$HOME_VAL))
insurance$OLDCLAIM <- as.numeric(gsub("[^0-9.]", "", insurance$OLDCLAIM))

insurance$PRIVATE_USE <- ifelse(insurance$CAR_USE=='Private',1,0)
insurance$RED_CAR <- ifelse(insurance$RED_CAR=='yes',1,0)
insurance$FEMALE <- ifelse(insurance$SEX=='z_F',1,0)
insurance$MARRIED <- ifelse(insurance$MSTATUS=='Yes',1,0)
insurance$SINGLE_PARENT <- ifelse(insurance$PARENT1=='Yes',1,0)
insurance$URBAN <- ifelse(insurance$URBANICITY=='Highly Urban/ Urban',1,0)
insurance$REVOKED <- ifelse(insurance$REVOKED=='Yes',1,0)

### Create indicator variables for NA values
insurance$AGE_IMP <- ifelse(is.na(insurance$AGE)==TRUE,1,0)
insurance$YOJ_IMP <- ifelse(is.na(insurance$YOJ)==TRUE,1,0)
insurance$INCOME_IMP <- ifelse(is.na(insurance$INCOME)==TRUE,1,0)
insurance$HOME_VAL_IMP <- ifelse(is.na(insurance$HOME_VAL)==TRUE,1,0)
insurance$CAR_AGE_IMP <- ifelse(is.na(insurance$CAR_AGE)==TRUE,1,0)

### Impute with mean
insurance$AGE[is.na(insurance$AGE)==TRUE] <- 
insurance$YOJ[is.na(insurance$YOJ)==TRUE] <-
insurance$INCOME[is.na(insurance$INCOME)==TRUE] <-
insurance$HOME_VAL[is.na(insurance$HOME_VAL)==TRUE] <-
insurance$CAR_AGE[is.na(insurance$CAR_AGE)==TRUE] <-

insurance$AGE <- na.aggregate(insurance$AGE, insurance$EDUCATION, mean, na.rm = TRUE)
insurance$YOJ <- na.aggregate(insurance$YOJ, insurance$JOB, mean, na.rm = TRUE)
insurance$INCOME <- na.aggregate(insurance$INCOME, c(insurance$JOB,insurance$EDUCATION), mean, na.rm = TRUE)
insurance$HOME_VAL <- na.aggregate(insurance$HOME_VAL, c(insurance$JOB,insurance$MSTATUS,insurance$INCOME), mean, na.rm = TRUE)
insurance$CAR_AGE[is.na(insurance$CAR_AGE)] <- mean(insurance$CAR_AGE, na.rm = TRUE)

### Create new fields
insurance$HOME_OWNER <- ifelse(insurance$HOME_VAL>0,1,0)



### Model scoring
moneyball$P_TARGET_WINS <- 206.09627163 + 
  0.04180158 * moneyball$TEAM_BATTING_H -
  0.03462849 * moneyball$TEAM_BATTING_2B + 
  0.17849720 * moneyball$TEAM_BATTING_3B +
  0.07855182 * moneyball$TEAM_BATTING_HR +
  0.08272586 * moneyball$TEAM_BASERUN_SB -
  0.04521347 * moneyball$TEAM_BASERUN_CS -
  12.64165775 * moneyball$TEAM_PITCHING_H +
  15.23820204 * moneyball$TEAM_PITCHING_BB -
  10.31248690 * moneyball$TEAM_PITCHING_SO -
  26.59985393 * moneyball$TEAM_FIELDING_E -
  0.10039003 * moneyball$TEAM_FIELDING_DP -
  21.88531997 * moneyball$TEAM_BASERUN_SB_RATIO +
  0.03424967 * moneyball$TEAM_BATTING_WALK +
  7.74191090 * moneyball$TEAM_BATTING_SO_IMP +
  33.48713640 * moneyball$TEAM_BASERUN_SB_IMP +
  5.31395778 * moneyball$TEAM_BASERUN_CS_IMP +
  5.66188510 * moneyball$TEAM_BATTING_HBP_IMP

### Subset output
prediction <- insurance[c("INDEX","P_TARGET_FLAG","P_TARGET_AMT")]

### Prediction output 
write.xlsx(prediction, file = "Brandi Beals Unit2 Predictions.xlsx", sheetName = "Predictions", col.names = TRUE)
