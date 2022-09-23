################################################################################
################################################################################
##### Data Scientist Tests Lendrock
################################################################################
################################################################################

#### Load Libraries

library(tidyverse)
library(lubridate)
library(corrplot)
library(GGally)
library(visdat)
library(caret)



## Set Working directory

getwd()
setwd("../../Documents/Lendrock")


## Read Data

data <- readRDS(file = "loan_dataset.RDS")
str(data)
summary(data)


## Looking for Na's

sapply(data, function(x) sum(is.na(x)))

vis_miss(data, warn_large_data = FALSE)


## Analyze NA values

data[is.na(data)]

data[["Employment.Type"]][is.na(data[["Employment.Type"]])] <- "NA"

data_na <- data[data$Employment.Type == "NA",]

ggplot(data_na, aes(x = loan_default)) + 
  geom_bar()


## Datetime Types

# Create a new column Age with Date of Birth

data$Date.of.Birth <- with(data, dmy(Date.of.Birth))

data <- data %>% mutate(Age = trunc(as.numeric(difftime(Sys.Date(), Date.of.Birth, units = "days")) / 365.25))


# Create a column with Disbursal Date

data$DisbursalDate <- with(data, dmy(DisbursalDate))

date_end <- max(data$DisbursalDate)

data <- data %>% mutate(Days_Disbursal = trunc(as.numeric(difftime(date_end, DisbursalDate, units = "days"))))


# Create a column with Average Account Age

average_acct_months <- transform(strcapture('(\\d+)yrs (\\d+)mon', data$AVERAGE.ACCT.AGE, 
                               proto = list(year = integer(), month = integer())), 
                    months = (year * 12) + month)
data$Average_Acct_Months <- average_acct_months$months


# Create a column with Credit History Length

credit_history_months <- transform(strcapture('(\\d+)yrs (\\d+)mon', data$CREDIT.HISTORY.LENGTH, 
                               proto = list(year = integer(), month = integer())), 
                    months = (year * 12) + month)
data$Credit_History_Months <- credit_history_months$months




## Unique

unique(data$PERFORM_CNS.SCORE.DESCRIPTION)


# Create a new column combining all flags variables

data <- data %>% mutate(Flags = Aadhar_flag + PAN_flag + VoterID_flag + Driving_flag + Passport_flag)
data$Flags <- as.factor(data$Flags)
summary(data$Flags)


## Factor Types

data$loan_default <- as.factor(data$loan_default)
data$Employment.Type <- as.factor(data$Employment.Type)
data$MobileNo_Avl_Flag <- as.factor(data$MobileNo_Avl_Flag)
data$Aadhar_flag <- as.factor(data$Aadhar_flag)
data$PAN_flag <- as.factor(data$PAN_flag)
data$VoterID_flag <- as.factor(data$VoterID_flag)
data$Driving_flag <- as.factor(data$Driving_flag)
data$Passport_flag <- as.factor(data$Passport_flag)
data$State_ID <- as.factor(data$State_ID)
data$PERFORM_CNS.SCORE.DESCRIPTION <- as.factor(data$PERFORM_CNS.SCORE.DESCRIPTION)


## Class Imbalanced

data1 <- data %>% filter(loan_default != 0)
data2 <- data %>% filter(loan_default == 0)

proportion = nrow(data1)/nrow(data2)


## Get the same proportion of 0 as 1 randomly

set.seed(100) 
Index <- createDataPartition(data2$loan_default,      #output variable. createDataPartition creates proportional partitions
                                  p = proportion,      #split probability for training
                                  list = FALSE, #Avoid output as a list
                                  times = 1)    #only one partition

## Obtain training and Test sets

data3 = data2[Index,]

data_balanced <- rbind(data1, data3)


## Downsampling the Umbalanced Class

Class_0 <- which(data$loan_default == 0)
Class_1 <- which(data$loan_default == 1)
length(Class_0)
length(Class_1)

Class_0_Downsample <- sample(Class_0, length(Class_1))
data_downsample <- data[c(Class_0_Downsample, Class_1)]

data_downsample <- downSample(data, data$loan_default)
data_downsample$Class <- NULL

## Upsampling the umbalanced class

Class_1_Upsample <- sample(Class_1, length(Class_0), replace=TRUE)
data_upsample <- data[c(Class_0, Class_1_Upsample)]



################################################################################
###  EDA
################################################################################

## Column Names

col_names <- names(data)
col_names

col_names <- c("UniqueID", "disbursed_amount", "asset_cost", "ltv", "branch_id", "supplier_id", "manufacturer_id", "Current_pincode_ID",
               "Date.of.Birth", "Employment.Type", "DisbursalDate", "State_ID", "Employee_code_ID", "MobileNo_Avl_Flag", "Aadhar_flag",               "PAN_flag",
               "VoterID_flag", "Driving_flag", "Passport_flag", "PERFORM_CNS.SCORE", "PERFORM_CNS.SCORE.DESCRIPTION", "PRI.NO.OF.ACCTS", 
               "PRI.ACTIVE.ACCTS", "PRI.OVERDUE.ACCTS", "PRI.CURRENT.BALANCE", "PRI.SANCTIONED.AMOUNT", "PRI.DISBURSED.AMOUNT", 
               "SEC.NO.OF.ACCTS", "SEC.ACTIVE.ACCTS", "SEC.OVERDUE.ACCTS", "SEC.CURRENT.BALANCE", "SEC.SANCTIONED.AMOUNT", 
               "SEC.DISBURSED.AMOUNT", "PRIMARY.INSTAL.AMT", "SEC.INSTAL.AMT", "NEW.ACCTS.IN.LAST.SIX.MONTHS", 
               "DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS", "AVERAGE.ACCT.AGE", "CREDIT.HISTORY.LENGTH", "NO.OF_INQUIRIES", "loan_default", 
               "Flags")



#### Histogrmas

ggplot(data, aes(x = disbursed_amount, y = asset_cost, colour = loan_default)) +
  geom_point()

ggplot(data2, aes(x = PERFORM_CNS.SCORE, fill = loan_default)) + 
  geom_histogram(bins = 30)

ggplot(data, aes(x = loan_default, y = disbursed_amount)) +
  geom_boxplot()

ggplot(data, aes(x = Employment.Type)) + 
  geom_bar()

ggplot(data, aes(x = PERFORM_CNS.SCORE.DESCRIPTION, fill = loan_default, colour = loan_default)) + 
  geom_bar()


#### Densidad

ggplot(data, aes(x=Current_pincode_ID, fill=loan_default)) +
  geom_density(alpha=0.5)

summary(data$SEC.NO.OF.ACCTS)


data1 <- data_filt %>% filter(SEC.NO.OF.ACCTS < 5)
#data2 <- data %>% filter(Age>0)

ggplot(data1, aes(x=SEC.NO.OF.ACCTS, fill=loan_default)) +
  geom_density(alpha=0.5)


data_filt <- data %>% filter(PRI.DISBURSED.AMOUNT != 0)
data_filt2 <- data %>% filter(PRI.DISBURSED.AMOUNT == 0)

#### Barras

data %>%
  count(PERFORM_CNS.SCORE.DESCRIPTION, loan_default) %>%
  group_by(PERFORM_CNS.SCORE.DESCRIPTION) %>%
  mutate(lab = paste0(round(prop.table(n) * 100, 2), '%')) %>%
  ggplot(aes(PERFORM_CNS.SCORE.DESCRIPTION,n, fill=loan_default)) + 
  geom_col() + 
  geom_text(aes(label=lab),position='stack',vjust=1.5)


#### Boxplot

ggplot(data, aes(x=loan_default, y=PRI.SANCTIONED.AMOUNT)) +
  geom_boxplot()


## Proportion of 0 / 1 for each factor variable

data_prop <- data %>% 
  group_by(Current_pincode_ID) %>%
  mutate(Prop = 100 * mean(loan_default == 1))

ggplot(data_prop, aes(x = Prop)) + 
  geom_histogram(bins = 100)

summary(data_prop$Prop)


## Prop table

prop.table(table(data$loan_default))

data_prop <- data[c("Passport_flag", "loan_default")]

table(data_prop)

prop.table(table(data_prop))


school.data %>% 
  group_by(state) %>%
  mutate(pct.female = mean(gender == "Female"))


## Correlation

M <- cor(data[, c(22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 40, 41)])
corrplot(M, method = "circle")

M <- cor(data[,-c(9, 10, 11, 21, 38, 39)])
corrplot(M, method = "circle")


## ggpairs

ggpairs(data[,-c(9, 10, 11, 21, 38, 39)])
ggpairs(data[,c(22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 40, 41)])




################################################################################
###  Loan Default Classification Models
################################################################################


## Remove columns for Models

data_models <- data_downsample

data_models$Date.of.Birth <- NULL
data_models$DisbursalDate <- NULL
data_models$AVERAGE.ACCT.AGE <- NULL
data_models$CREDIT.HISTORY.LENGTH <- NULL
data_models$MobileNo_Avl_Flag <- NULL
data_models$UniqueID

summary(data_models)


## Change some columns to factor

data_models$branch_id <- as.factor(data_models$branch_id)
data_models$supplier_id <- as.factor(data_models$supplier_id)
data_models$manufacturer_id <- as.factor(data_models$manufacturer_id)
data_models$Current_pincode_ID <- as.factor(data_models$Current_pincode_ID)
data_models$Employee_code_ID <- as.factor(data_models$Employee_code_ID)
data_models$PERFORM_CNS.SCORE <- as.factor(data_models$PERFORM_CNS.SCORE)



## Divide data into Train and Test (80/20)

set.seed(100) 
trainIndex <- createDataPartition(data_models$loan_default,      #output variable. createDataPartition creates proportional partitions
                                  p = 0.8,      #split probability for training
                                  list = FALSE, #Avoid output as a list
                                  times = 1)    #only one partition

## Obtain training and Test sets

fTR = data_models[trainIndex,]
fTS = data_models[-trainIndex,]


ctrl_tune <- trainControl(method="repeatedcv",number=3,repeats=1)

# ctrl_tune <- trainControl(method = "none",
#                           number = 10,
#                           summaryFunction = defaultSummary,    #Performance summary for comparing models in hold-out samples.
#                           returnResamp = "final",              #Return final information about resampling
#                           savePredictions = TRUE)              #save predictions


########################
#### Logistic Regression
########################

set.seed(150)

## Training the model


log.fit = train(form = loan_default ~ .- (UniqueID + branch_id + supplier_id + Current_pincode_ID + Employee_code_ID + PERFORM_CNS.SCORE),
  #form = loan_default ~ disbursed_amount + asset_cost + ltv + Employment.Type + Aadhar_flag + VoterID_flag + Driving_flag + Passport_flag,
  #form = loan_default ~ . - (UniqueID + Date.of.Birth + DisbursalDate + MobileNo_Avl_Flag + PERFORM_CNS.SCORE.DESCRIPTION + AVERAGE.ACCT.AGE + CREDIT.HISTORY.LENGTH),
  #form = loan_default ~ disbursed_amount + asset_cost + ltv + Employment.Type + State_ID + PERFORM_CNS.SCORE.DESCRIPTION + PRI.NO.OF.ACCTS + PRI.ACTIVE.ACCTS + PRI.OVERDUE.ACCTS + PRI.CURRENT.BALANCE + DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS + NO.OF_INQUIRIES + Age + Days_Disbursal + Average_Acct_Months + Credit_History_Months,
  data = fTR,
  method="glm", 
  preProcess = c("center", "scale"), 
  trControl = ctrl_tune,
  metric = "Accuracy")

## See the results

log.fit

summary(log.fit)
log.fit$resample

boxplot(log.fit$resample$Accuracy)

fTS$Prediction_LOG = predict(log.fit, fTS)

confusionMatrix(fTS$Prediction_LOG,
                fTS$loan_default,
                positive = "1")

table(fTS$loan_default, fTS$Prediction_LOG)



####################
#### Decision Tree
####################

library(rpart)

set.seed(150) #For replication
#Train decision tree
#rpart contains 1 tuning parameter cp (Complexity parameter). Three options:
#  - Train with a fixed parameter: tuneGrid = data.frame(cp = 0.1),
#  - Try with a range of values specified in tuneGrid: tuneGrid = data.frame(cp = seq(0,0.4,0.05))),
#  - Caret chooses 10 values: tuneLength = 10,

#NOTE: Formula method could be used, but it will automatically create dummy variables. 
# Decision trees can work with categorical variables as theey are. Then, x and y arguments are used
tree.fit <- train(form = loan_default ~ .- (UniqueID + branch_id + supplier_id + Current_pincode_ID + Employee_code_ID + PERFORM_CNS.SCORE),
                  method = "rpart",   #Decision tree with cp as tuning parameter
                  data=fTR,
                  control = rpart.control(minsplit = 5,  # Minimum number of obs in node to keep cutting
                                          minbucket = 5), # Minimum number of obs in a terminal node
                  parms = list(split = "gini"),          # impuriry measure
                  #tuneGrid = data.frame(cp = 0.1), # TRY this: tuneGrid = data.frame(cp = 0.25),
                  #tuneLength = 10,
                  tuneGrid = data.frame(cp = seq(0,0.1,0.0005)),
                  trControl = ctrl_tune, 
                  metric = "Accuracy")
tree.fit #information about the resampling settings
ggplot(tree.fit) #plot the summary metric as a function of the tuning parameter
summary(tree.fit)  #information about the model trained
tree.fit$finalModel #Cuts performed and nodes. Also shows the number and percentage of cases in each node.






###############
#### XGBoost
###############

# set.seed(150)
# 
# ## Training the model
# XGB.fit = train(#form = loan_default ~ disbursed_amount,
#   #form = loan_default ~ disbursed_amount + asset_cost + ltv + Employment.Type + Aadhar_flag + VoterID_flag + Driving_flag + Passport_flag,
#   form = loan_default ~ .- UniqueID,
#   data = fTR,
#   method = "xgbLinear", #Random forest
#   #tuneGrid = data.frame(mtry = 3), # m parameter          
#   #tuneGrid = expand.grid(nrounds=2400, lambda=1, alpha=0, eta=c(0.01,0.001,0.0001)),
#   tuneLength = 4,
#   trControl = ctrl_tune, #Resampling settings 
#   metric = "Accuracy")    #Summary metrics
# 
# ## See the results
# 
# XGB.fit
# summary(XGB.fit)
# XGB.fit$resample
# 
# boxplot(XGB.fit$resample$Accuracy)
# 
# fTS$Prediction_XGB = predict(XGB.fit, fTS)
# 
# confusionMatrix(fTS$Prediction_XGB,
#                 fTS$loan_default,
#                 positive = "1")
# 
# table(fTS$loan_default, fTS$Prediction_XGB)

