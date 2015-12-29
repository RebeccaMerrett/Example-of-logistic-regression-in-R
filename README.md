# Example of logistic regression in R
Set working directory path to whatever folder you like.<br>
setwd("C:\\Users\\Rebecca Merrett\\Documents\\r-workspace\\RebeccaSpace")<br>
getwd()<br>
AmericanCommunitySurveyNY10 <- read.table("C:\\Users\\Rebecca Merrett\\Documents\\r-workspace\\RebeccaSpace\\acs_ny_missing.csv", sep=",", header=TRUE)<br>
head(AmericanCommunitySurveyNY10)<br>
<i>#Want predict a categorical true/false type of response for logistic regression.</i><br>
<i>#So add income column to FamilyIncome being either true as earning more than $150k or false as earning less than $150k.</i><br>
AmericanCommunitySurveyNY10$Income <- with(AmericanCommunitySurveyNY10,FamilyIncome>=150000)<br>
head(AmericanCommunitySurveyNY10)<br>
<i>#Check missing values and fill in with the mean.</i><br>
<i>#Can also fill in with mode (most frequent value), or drop column if nearly all values missing.</i><br>
<i>#Or use other values in record to infer the missing value.</i><br>
<i>#For example, in Excel can look at occupation in a record with missing income value, with a table of income averages for different occupations to infer the missing values.</i><br>
<i>#Then can match or vlookup to insert the missing value from the occupation average income table, then set cells as being either above or below $150k.</i><br>
sapply(AmericanCommunitySurveyNY10,function(x) sum(is.na(x)))<br>
AmericanCommunitySurveyNY10$NumChildren[is.na(AmericanCommunitySurveyNY10$NumChildren)] <- mean(AmericanCommunitySurveyNY10$NumChildren, na.rm=TRUE)<br>
AmericanCommunitySurveyNY10$NumVehicles[is.na(AmericanCommunitySurveyNY10$NumVehicles)] <- mean(AmericanCommunitySurveyNY10$NumVehicles, na.rm=TRUE)<br>
<i>#Split data into train (70%) and test (30%), and build the model on the training set.</i><br>
AmericanCommunitySurveyNY10Split <- sort(sample(nrow(AmericanCommunitySurveyNY10),nrow(AmericanCommunitySurveyNY10)*.7))<br>
AmericanCommunitySurveyNY10Train <- AmericanCommunitySurveyNY10[AmericanCommunitySurveyNY10Split,]<br>
AmericanCommunitySurveyNY10Test <- AmericanCommunitySurveyNY10[-AmericanCommunitySurveyNY10Split,]<br>
IncomeAboveBelow150K <- glm(Income ~ HouseCosts + NumWorkers + OwnRent + NumBedrooms + FamilyType +
NumChildren, data=AmericanCommunitySurveyNY10Train, family=binomial(link="logit"))<br>
summary(IncomeAboveBelow150K)<br>
<i>#Get the predicted results on test set, and plot receiver operating characteristic at a threshold with a calculated area under the curve.</i><br>
<i>#ROC is true positive rate against false positive rate, with a high AUC meaning high precision (low false positive) and high recall (low false negative).</i><br>
<i>#Minus response columns Income.</i><br>
ModelPredictions <- predict(IncomeAboveBelow150K, newdata=AmericanCommunitySurveyNY10Test[,-2, -19], type="response")<br>
ModelPredictions <- ifelse(ModelPredictions>0.5, 1, 0)<br>
MisclassificationError <- mean(ModelPredictions != AmericanCommunitySurveyNY10Test$Income)<br>
print(paste("Accuracy",1-MisclassificationError))<br>
library(ROCR)<br>
p <- predict(IncomeAboveBelow150K, newdata=AmericanCommunitySurveyNY10Test[ , -2, -19], type="response")<br>
pred <- prediction(p, AmericanCommunitySurveyNY10Test$Income)<br>
ROC <- performance(pred, "tpr", "fpr")<br>
plot(ROC)<br>
AUC <- performance(pred, "auc")@y.values[[1]]<br>
AUC
