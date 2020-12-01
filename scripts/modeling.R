# SYST 568 Project
# Model Exploration. Last updated 12/1/2020


library(InformationValue)
library(ggplot2)
library(ROCR)

final_teams_salary = read.csv('./data/final_teams_salary.csv')

factor_cols = c('playoff_nextyear')
final_teams_salary[factor_cols] <- lapply(final_teams_salary[factor_cols] , factor)
final_teams_salary <- na.omit(final_teams_salary, cols="playoff_nextyear")

# train-test split
set.seed(12345)
train_index <- sample(1:nrow(final_teams_salary), 0.5 * nrow(final_teams_salary))
test_index <- setdiff(1:nrow(final_teams_salary), train_index)
train <- final_teams_salary[train_index, -15]
test <- final_teams_salary[test_index, -15]

outputs <- data.frame(model_name=character(),
                 accuracy=numeric(),
                 sensitivity=numeric(),
                 mcr=numeric(),
                 auc=numeric())

record_outputs <- function(model_name, model) {
  #TODO: Generalize
  pred <- plogis(predict(model, test))
  scores <- prediction(predictions=pred, labels=test$playoff_nextyear)
  perf <- performance(scores, "tpr", "fpr")
  
  #PLOT ROC CURVE
  plot(perf,
       main="ROC Curves",
       xlab="1 - Specificity: False Positive Rate",
       ylab="Sensitivity: True Positive Rate",
       col="darkblue",  lwd = 3)
  abline(0,1, lty = 300, col = "green",  lwd = 3)
  grid(col="aquamarine")
  
  ## Performance Metrics
  cm = confusionMatrix(test$playoff_nextyear, pred)
  accuracy = (cm[2,2] + cm[1,1])/nrow(test)
  sensitivity = cm[2,2] / (cm[2,2] + cm[2,1])
  mcr = (cm[2,1] + cm[1,2]) / nrow(test)
  
  # AREA UNDER THE CURVE
  auc <- performance(scores, "auc")
  auc <- as.numeric(auc@y.values)  ##AUC Value
  return(list(model_name, accuracy, sensitivity, mcr, auc))
}


#### Logistic Regression - without salary ##### 
## NOTE - currently not implemeneted, uses different train / test data
# 
# final_teams$playoff_nextyear <- as.factor(final_teams$playoff_nextyear)
# final_teams <- na.omit(final_teams, cols="playoff_nextyear")
# 
# # benchmark accuracy with all predictions as "N"
# pred = rep('N', nrow(final_teams))
# ConMatrix = table(pred, final_teams$playoff_nextyear)
# ConMatrix
# (ConMatrix[1, 1]) /  nrow(final_teams)
# # benchmark accuracy is 0.77
# 
# # logistic regression
# logit.model <- step(glm(
#   playoff_nextyear~., data=train, family=binomial), direction='backward')
# summary(logit.model)
# 
# # test using split
# logit.probs = predict(logit.model, test, type="response")
# logit.pred = rep('N', nrow(test))
# logit.pred[logit.probs >= 0.5] = 'Y'
# logit.ConMatrix = table(logit.pred, test$playoff_nextyear)
# logit.ConMatrix
# (logit.ConMatrix[1, 1] + logit.ConMatrix[2, 2]) /  nrow(test)
# # achieves an accuracy of 0.778, not great, barely better than random

####### Logit regression - with salary #########
# benchmark accuracy with all predictions as "N"

pred = rep('N', nrow(final_teams_salary))
ConMatrix = table(pred, final_teams_salary$playoff_nextyear)
ConMatrix
(ConMatrix[1, 1]) /  nrow(final_teams_salary)
# benchmark accuracy is 0.751

# logistic regression
logit.model <- step(glm(
  playoff_nextyear~., data=train, family=binomial), direction='backward')
summary(logit.model)

# test using split
logit.probs = predict(logit.model, test, type="response")
logit.pred = rep('N', nrow(test))
logit.pred[logit.probs >= 0.5] = 'Y'
logit.ConMatrix = table(logit.pred, test$playoff_nextyear)
logit.ConMatrix
(logit.ConMatrix[1, 1] + logit.ConMatrix[2, 2]) /  nrow(test)

#TODO: Generalize, currently need to specify row numbers for each addition
outputs[1,] <- record_outputs('Logit Regression', logit.model)

print(outputs)
# achieves an accuracy of 0.741, worse because less data, worse than random