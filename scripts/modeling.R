# SYST 568 Project
# Model Exploration. Last updated 12/1/2020

library(ggplot2)
library(ROCR)
library(randomForest)
library(caret)
library(InformationValue)
library(dplyr)

final_teams_salary = read.csv('./data/final_teams_salary.csv')

factor_cols = c('playoff_nextyear')
final_teams_salary[factor_cols] <- lapply(final_teams_salary[factor_cols] , factor)
final_teams_salary <- na.omit(final_teams_salary, cols="playoff_nextyear")

# train-test split by yearID
set.seed(12345)
years <- unique(final_teams_salary$yearID)
train_years <- sample(years, 0.5*length(years))
test_years <- setdiff(years, train_years)
train <- subset(final_teams_salary, yearID %in% train_years)
test <- subset(final_teams_salary, yearID %in% test_years)

outputs <- data.frame(model_name=character(),
                 accuracy=numeric(),
                 sensitivity=numeric(),
                 mcr=numeric(),
                 auc=numeric())

get_top8_predictions <- function(data, model) {
  data$playoff_prob = predict(logit.model, data, type="response")
  playoff_teams <- data %>%
    group_by(yearID) %>%
    slice_max(playoff_prob, n = 8, with_ties = FALSE)
  playoff_teams$playoff_pred = 'Y'
  pred <- merge(test, playoff_teams, all.x=TRUE)
  pred$playoff_pred[is.na(pred$playoff_pred)] <- 'N'
  return(pred$playoff_pred)
}

record_outputs <- function(model_name, pred, model) {
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


####### Logit regression #########
logit.model <- step(glm(
  playoff_nextyear~.-yearID, data=train, family=binomial), direction='backward')
summary(logit.model)

#TODO: Generalize, currently need to specify row numbers for each addition
logit_pred <- get_top8_predictions(test, logit.model)
outputs[1,] <- record_outputs('Logit Regression', logit_pred, logit.model)


###### Random Forest ########
rf.model <- randomForest(playoff_nextyear~.-yearID,
                         data = train)

rf_pred <- get_top8_predictions(test, rf.model)
outputs[2,] <- record_outputs('Random Forest', rf_pred, rf.model)


###### XGB ########

tc <- trainControl(method = "repeatedCV", number=15, repeats=1)
xgb.model <- train(playoff_nextyear~., data=train, method="xgbTree", trControl=tc)

xgb_pred <- get_top8_predictions(test, xgb.model)
outputs[3,] <- record_outputs('XGB', xgb_pred, rf.model)

print(outputs)
