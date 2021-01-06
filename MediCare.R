library(openxlsx)
library(sqldf)
library(dplyr)
library(lubridate)
library(caret)
library(tidyverse)
library(xgboost)


setwd("C:/Users/rlang/Downloads/")

B_data <- read.csv("beneficiary.csv")
# B_data <- B_data[!is.na(B_data$DOD),]

I_data <- read.csv("inpatients.csv")
O_data <- read.csv("outpatients.csv")
P_data <- read.csv("providers.csv")

## Outpatients Data Cleaning
O_data$AdmissionDt <- 0
O_data$DischargeDt <- 1
O_data$DiagnosisGroupCode <- NA
O_data$NumDays <- 0
O_data$NumDays_hosp <- 0
O_data$cost_per_day <- 0
O_data$AmtReimbursed[is.na(O_data$AmtReimbursed) == TRUE] <- mean(O_data$AmtReimbursed, na.rm=TRUE)
O_data <- O_data[O_data$AmtReimbursed != 0, ]
O_data$DeductibleAmt[is.na(O_data$DeductibleAmt) == TRUE] <- mean(O_data$DeductibleAmt, na.rm=TRUE)

## Inpatients Data Cleaning
I_data$NumDays <- as.numeric(as.Date(I_data$EndDt))-as.numeric(as.Date(I_data$StartDt))+1
I_data$NumDays_hosp <- as.numeric(as.Date(I_data$DischargeDt))-as.numeric(as.Date(I_data$AdmissionDt))+1
I_data$cost_per_day <- I_data$DeductibleAmt/I_data$NumDays_hosp
I_data$AmtReimbursed[is.na(I_data$AmtReimbursed) == TRUE] <- mean(I_data$AmtReimbursed, na.rm=TRUE)
I_data <- I_data[I_data$AmtReimbursed != 0, ]
I_data$DeductibleAmt[is.na(I_data$DeductibleAmt) == TRUE] <- mean(I_data$DeductibleAmt, na.rm=TRUE)

## Combine Outpatients and Inpatients
data <- rbind(I_data, O_data)

## Work on the categories
Diagnosis <- c("DiagnosisCode_1", "DiagnosisCode_2", "DiagnosisCode_3", 
               "DiagnosisCode_4", "DiagnosisCode_5", "DiagnosisCode_6", "DiagnosisCode_7",
               "DiagnosisCode_8", "DiagnosisCode_9", "DiagnosisCode_10")
Procedure <- c("ProcedureCode_1", "ProcedureCode_2", "ProcedureCode_3", 
               "ProcedureCode_4", "ProcedureCode_5", "ProcedureCode_6")
Physician <- c("AttendingPhysician", "OperatingPhysician", "OtherPhysician")

data$Count_Diagnosis <- rowSums(!is.na(data[Diagnosis]))
data$Count_Procedure <- rowSums(!is.na(data[Procedure]))
data$Count_Physician <- rowSums(!is.na(data[Physician]))

data$Total_Amt <- data$AmtReimbursed + data$DeductibleAmt
data$Deductible_Ratio <- data$DeductibleAmt/data$Total_Amt

data$Average_Amt_Diagnosis <- data$Total_Amt/data$Count_Diagnosis
data$Average_Amt_Procedure <- data$Total_Amt/data$Count_Procedure
data$Average_Amt_Physician <- data$Total_Amt/data$Count_Physician

data$Average_Amt_Diagnosis[is.infinite(data$Average_Amt_Diagnosis) == TRUE] <- 0
data$Average_Amt_Procedure[is.infinite(data$Average_Amt_Procedure) == TRUE] <- 0
data$Average_Amt_Physician[is.infinite(data$Average_Amt_Physician) == TRUE] <- 0

# Keep the frequency of DiagnosisGroupCode
DiagnosisGroupCode_Frequency <- count(data, "DiagnosisGroupCode")
data$DiagnosisGroupCode_Freq <- with(DiagnosisGroupCode_Frequency, 
                                     freq[match(data$DiagnosisGroupCode, DiagnosisGroupCode)])


## Add Beneficiary Data
data_with_B <- sqldf('select * from data join B_data on data.BID = B_data.BID')
colnames(data_with_B)
data_with_B <- data_with_B[, !duplicated(colnames(data_with_B))]

## Add Provider Data
data_all <- sqldf('select * from data_with_B join P_data on data_with_B.PID = P_data.PID')
colnames(data_all)
data_all <- data_all[, !duplicated(colnames(data_all))]

## Data Analytics for Beneficiary Data
data_all$Age <- 2009-year(data_all$DOB)
data_all$RenalDisease[data_all$RenalDisease == "Y"] <- "1"

data_all$Chronic_Alzheimer <- data_all$Chronic_Alzheimer-1
data_all$Chronic_Heartfailure <- data_all$Chronic_Heartfailure-1
data_all$Chronic_KidneyDisease <- data_all$Chronic_KidneyDisease-1
data_all$Chronic_Cancer <- data_all$Chronic_Cancer-1
data_all$Chronic_ObstrPulmonary <- data_all$Chronic_ObstrPulmonary-1
data_all$Chronic_Depression <- data_all$Chronic_Depression-1
data_all$Chronic_Diabetes <- data_all$Chronic_Diabetes-1
data_all$Chronic_IschemicHeart <- data_all$Chronic_IschemicHeart-1
data_all$Chronic_Osteoporasis <- data_all$Chronic_Osteoporasis-1
data_all$Chronic_rheumatoidarthritis <- data_all$Chronic_rheumatoidarthritis-1
data_all$Chronic_stroke <- data_all$Chronic_stroke-1

# Assign mean to NA values and Remove zeros in reimbursement
data_all$OutpatientAnnualReimbursementAmt[is.na(data_all$OutpatientAnnualReimbursementAmt) == TRUE] <- 
  mean(data_all$OutpatientAnnualReimbursementAmt, na.rm=TRUE)
data_all <- data_all[data_all$OutpatientAnnualReimbursementAmt != 0, ]

data_all$InpatientAnnualReimbursementAmt[is.na(data_all$InpatientAnnualReimbursementAmt) == TRUE] <- 
  mean(data_all$InpatientAnnualReimbursementAmt, na.rm=TRUE)
data_all <- data_all[data_all$InpatientAnnualReimbursementAmt != 0, ]

data_all$OutpatientAnnualDeductibleAmt[is.na(data_all$OutpatientAnnualDeductibleAmt) == TRUE] <- 
  mean(data_all$OutpatientAnnualDeductibleAmt, na.rm=TRUE)
data_all$InpatientAnnualDeductibleAmt[is.na(data_all$InpatientAnnualDeductibleAmt) == TRUE] <- 
  mean(data_all$InpatientAnnualDeductibleAmt, na.rm=TRUE)

# Create two ratios
data_all$Annual_Reim_in_to_out <- data_all$InpatientAnnualReimbursementAmt/data_all$OutpatientAnnualReimbursementAmt
data_all$Annual_Deduct_in_to_out <- data_all$InpatientAnnualDeductibleAmt/data_all$OutpatientAnnualDeductibleAmt
data_all$Annual_Deduct_in_to_out[is.infinite(data_all$Annual_Deduct_in_to_out) == TRUE] <- 0

colnames(data_all)

data_all$Fraud[data_all$Fraud == "Yes"] <- 1
data_all$Fraud[data_all$Fraud == "No"] <- 0
data_all$Fraud <- as.numeric(data_all$Fraud)

drop_column <- c("BID","CID","StartDt","EndDt","AttendingPhysician","OperatingPhysician","OtherPhysician","AdmissionDt",
                 "AdmitDiagnosisCode","DischargeDt","DiagnosisGroupCode","DiagnosisCode_1","DiagnosisCode_2",
                 "DiagnosisCode_3","DiagnosisCode_4","DiagnosisCode_5","DiagnosisCode_6","DiagnosisCode_7",
                 "DiagnosisCode_8","DiagnosisCode_9","DiagnosisCode_10","ProcedureCode_1","ProcedureCode_2",
                 "ProcedureCode_3","ProcedureCode_4","ProcedureCode_5","ProcedureCode_6","DOB","DOD")

df = data_all[,!(names(data_all) %in% drop_column)]
# Check NA values
colSums(is.na(df))

dataframe2 <- df %>% 
  group_by(PID) %>%
  summarise_all("mean")

dataframe2 <- subset(dataframe2, select = -c(PID))

# Proportion of fraud: 9.35305%
prop.table(table(df$Fraud))

# Visualize fraud with pie chart
labels <- c("no fraud", "fraud")
labels <- paste(labels, round(100 * prop.table(table(P_data$Fraud)),2),"%")
pie(table(P_data$Fraud), labels, col = c("dark green", "orange"), main = "Pie chart of Medical Insurance claims")

# When no detection model is used, then all transactions 
# in the transfers dataset are considered legitimate.
# To determine the corresponding confusion matrix
predictions <- factor(rep("No", times = nrow(P_data)), levels = c("No","Yes"))
confusionMatrix(data = predictions, reference = factor(P_data$Fraud))

# Compute cost of not detecting fraud
cost <- sum(data_all$AmtReimbursed[data_all$Fraud == "Yes"])
print(cost) #295,681,120 = $0.3B per year


### XGBoost Models

FraudLabels <- dataframe2$Fraud
dataframe <- subset(dataframe2, select = -c(Fraud))
# get the numb 70/30 training test split
numberOfTrainingSamples <- round(length(FraudLabels) * .7)

# training data
train_data <- dataframe[1:numberOfTrainingSamples,]
train_labels <- FraudLabels[1:numberOfTrainingSamples]

# testing data
test_data <- dataframe[-(1:numberOfTrainingSamples),]
test_labels <- FraudLabels[-(1:numberOfTrainingSamples)]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = as.matrix(train_data), label= train_labels)
dtest <- xgb.DMatrix(data = as.matrix(test_data), label= test_labels)

# get the number of negative & positive cases in our data
negative_cases <- sum(train_labels == FALSE)
postive_cases <- sum(train_labels == TRUE)

# train a model using our training data
model <- xgboost(data = dtrain, # the data           
                 max.depth = 2, # the maximum depth of each decision tree
                 nround = 50, # number of boosting rounds
                 early_stopping_rounds = 8, # if we dont see an improvement in this many rounds, stop
                 objective = "binary:logistic", # the objective function
                 scale_pos_weight = negative_cases/postive_cases) # control for imbalanced classes

# generate predictions for our held-out testing data
pred <- predict(model, dtest)

# get & print the classification error
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))


# get information on how important each feature is
importance_matrix <- xgb.importance(names(dataframe), model = model)

# and plot it!
xgb.plot.importance(importance_matrix)


model_expand <- xgb.dump(model, with.stats = T)
model_expand[1:10] #This statement prints top 10 nodes of the model

#  See whether the variable is actually important
test <- chisq.test(test_data$Average_Amt_Physician, test_labels)
print(test)


# Test MSE
residuals = test_labels - pred
RMSE = sqrt(mean(residuals^2))
cat('The root mean square error of the test data is ', round(RMSE,3),'\n')

test_mean = mean(test_labels)
# Calculate total sum of squares
tss =  sum((test_labels - test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)

# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')


# Compute AUC for predicting Class with the model
library(ROCR)
prob <- predict(model, newdata=dtest, type="response")
predictio <- prediction(prob, test_labels)
perf <- performance(predictio, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(predictio, measure = "auc")
auc <- auc@y.values[[1]]
auc

sum(test_labels)/length(test_labels)

quantile(prob, 0.9)

# Change y axis to count instead of density
ggplot(aes(x = prob)) + geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(prob)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")
qplot(as.factor(prob), geom="histogram")
