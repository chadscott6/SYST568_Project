# SYST 568 Project
# Model Exploration. Last updated 12/1/2020

library(ggplot2)
library(ROCR)
library(randomForest)
library(caret)
library(dplyr)

final_teams_salary = read.csv('./data/final_teams_salary.csv')[,-1]

factor_cols = c('playoff_nextyear')
final_teams_salary[factor_cols] <- lapply(final_teams_salary[factor_cols] , factor)
final_teams_salary <- na.omit(final_teams_salary, cols="playoff_nextyear")

# get number of playoff teams per year
playoffs_year <- final_teams_salary %>%
  group_by(yearID) %>%
  count(playoff_nextyear) %>%
  filter(playoff_nextyear == 'Y')
playoffs_year <- playoffs_year[,c(1,3)]

# train-test split by yearID
set.seed(12345)
years <- unique(final_teams_salary$yearID)
train_years <- sample(years, 0.8*length(years))
test_years <- setdiff(years, train_years)
train <- subset(final_teams_salary, yearID %in% train_years)
test <- subset(final_teams_salary, yearID %in% test_years)

outputs <- data.frame(model_name=character(),
                 accuracy=numeric(),
                 precision=numeric(),
                 recall=numeric(),
                 f1score=numeric())

# NOTE: this does not work yet
get_division_predictions <- function(data, model) {
  data$playoff_prob = predict(logit.model, data, type="response")
  playoff_teams <- data %>%
    group_by(yearID,lgID,divID) %>%
    slice_max(playoff_prob, n = 1, with_ties = FALSE)
  playoff_teams$playoff_pred = 'Y'
  pred <- merge(test, playoff_teams, all.x=TRUE)
  pred$playoff_pred[is.na(pred$playoff_pred)] <- 'N'
  return(pred$playoff_pred)
}

get_top_predictions <- function(data, model) {
  total_pred = vector()
  for (year in unique(data$yearID)) {
    year_data = filter(data,  yearID==year)
    year_data$playoff_prob = predict(model, year_data, type="prob")[,2]
    playoffs_n <- filter(playoffs_year, yearID==year)$n
    playoff_teams <- year_data %>%
      slice_max(playoff_prob, n = playoffs_n, with_ties = FALSE)
    playoff_teams$playoff_pred = 'Y'
    pred <- merge(year_data, playoff_teams, all.x=TRUE)
    pred$playoff_pred[is.na(pred$playoff_pred)] <- 'N'
    total_pred = append(total_pred, pred$playoff_pred)
  }
  return(total_pred)
}

record_outputs <- function(model_name, pred, model) {

  ConMatrix = table(test$playoff_nextyear, pred)
  # calculate metrics
  accuracy = (ConMatrix[1, 1] + ConMatrix[2, 2]) /  sum(ConMatrix)
  precision = ConMatrix[2, 2] / (ConMatrix[2, 2] + ConMatrix[1, 2])
  recall = ConMatrix[2, 2] / (ConMatrix[2, 2] + ConMatrix[2, 1])
  f1score = 2*(precision*recall / (precision+recall))
  
  return(list(model_name, accuracy, precision, recall, f1score))
}


####### Logit regression #########

tc <- trainControl(method = "repeatedCV", number=10, repeats=2)
tg <- expand.grid(nIter=c(1,2,5,10))
logit.model <- train(playoff_nextyear~., data=train, method="LogitBoost", trControl=tc, tuneGrid=tg)

logit_pred <- get_top_predictions(test, logit.model)
outputs[1,] <- record_outputs('Logit Regression', logit_pred, logit.model)


###### Random Forest ########

tc <- trainControl(method = "repeatedCV", number=10, repeats=2)
tg <- expand.grid(mtry=c(15,20,25,30))
rf.model <- train(playoff_nextyear~., data=train, method="rf", trControl=tc, tuneGrid=tg)

rf_pred <- get_top_predictions(test, rf.model)
outputs[2,] <- record_outputs('Random Forest', rf_pred, rf.model)

# Visualization
# Variable importance plot (Mean Decrease in Gini Index)
# var_importance <- data_frame(variable=setdiff(colnames(train), "playoff_nextyear"),
#                              importance=as.vector(importance(rf.model)))
# var_importance <- arrange(var_importance, desc(importance))
# var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)
# 
# p <- ggplot(var_importance, aes(x=variable, weight=importance, fill=variable))
# p <- p + geom_bar() + ggtitle("Variable Importance from Random Forest Fit")
# p <- p + xlab("Statistic") + ylab("Variable Importance (Mean Decrease in Gini Index)")
# p <- p + scale_fill_discrete(name="Variable Name")
# p + theme(axis.text.x=element_blank(),
#           axis.text.y=element_text(size=12),
#           axis.title=element_text(size=16),
#           plot.title=element_text(size=18),
#           legend.title=element_text(size=16),
#           legend.text=element_text(size=12))

###### XGB ########

tc <- trainControl(method = "repeatedCV", number=10, repeats=1)
tg <- expand.grid(nrounds=c(50,100, 150),
                  max_depth=c(1,3,5,10),
                  eta=c(0.3,0.4),
                  gamma=c(0),
                  colsample_bytree=c(0.6,0.8),
                  min_child_weight=c(1),
                  subsample=c(.5,.75,1))
xgb.model <- train(playoff_nextyear~., data=train, method="xgbTree", trControl=tc, tuneGrid=tg)

xgb_pred <- get_top_predictions(test, xgb.model)
outputs[3,] <- record_outputs('XGB', xgb_pred, rf.model)

print(logit.model)
print(rf.model)
print(xgb.model)
print(outputs)




