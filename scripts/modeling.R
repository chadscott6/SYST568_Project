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
train_years <- sample(years, 0.75*length(years))
test_years <- setdiff(years, train_years)
train <- subset(final_teams_salary, yearID %in% train_years)
test <- subset(final_teams_salary, yearID %in% test_years)

outputs <- data.frame(model_name=character(),
                 accuracy=numeric(),
                 precision=numeric(),
                 recall=numeric(),
                 f1score=numeric())

playoff_div_year <- read.csv('../data/input/playoffs_year.csv')

get_div_predictions <- function(data, model) {
  total_pred = vector()
  for (year in unique(data$yearID)) {
    year_data = filter(data,  yearID==year)
    year_data$playoff_prob = predict(model, year_data, type="prob")[,2]
    playoff_div <- filter(playoff_div_year, yearID==year)$div
    playoff_wc <- filter(playoff_div_year, yearID==year)$wc
    playoff_teams <- year_data %>%
      group_by(lgID,divID) %>%
      slice_max(playoff_prob, n = playoff_div, with_ties = FALSE)
    playoff_teams$playoff_pred = 'Y'
    pred <- merge(year_data, playoff_teams, all.x=TRUE)
    if (playoff_wc > 0) {
      wc_teams <- filter(pred, is.na(pred$playoff_pred))
      wc_teams <- wc_teams %>%
        group_by(lgID) %>%
        slice_max(playoff_prob, n = playoff_wc, with_ties = FALSE)
      pred[pred$franchID %in% wc_teams$franchID,]$playoff_pred = 'Y'
    }
    pred$playoff_pred[is.na(pred$playoff_pred)] <- 'N'
    total_pred = append(total_pred, pred$playoff_pred)
  }
  return(total_pred)
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

tc <- trainControl(method = "repeatedCV", number=5)
#tg <- expand.grid(nIter=c(1,2,5,10))

logit.model <- train(playoff_nextyear~.-franchID -yearID -lgID - divID, data=train, method="plr", trControl=tc)#, tuneGrid=tg)

logit_pred <- get_div_predictions(test, logit.model)
outputs[1,] <- record_outputs('Logit Regression', logit_pred, logit.model)

## Logit Regression with fewer predictor variables ##
# logit.model1 removes variables with Prob over 0.5 from logit.model
logit.model1 <- train(playoff_nextyear~.-franchID -yearID -lgID - divID -X2B_G - BB_G - SB_G - HBP_G - SF_G - RA_G - ER_G - CG_G - 
                        SHO_G - SV_G - IPouts_G - HA_G - SOA_G - E_G - DP_G - Playoffs, data=train, method="plr", trControl=tc)#, tuneGrid=tg)

logit_pred1 <- get_div_predictions(test, logit.model1)
outputs[4,] <- record_outputs('Logit Regression 1', logit_pred1, logit.model1)

# compare logit models
print(logit.model1)
print(logit.model)
summary(logit.model1)
summary(logit.model)


###### Random Forest ########

tc <- trainControl(method = "repeatedCV", number=5, repeats=2)
tg <- expand.grid(mtry=c(15,20,25,30))

rf.model <- train(playoff_nextyear~.-franchID -yearID -lgID - divID, data=train, method="rf", trControl=tc, tuneGrid=tg)

rf_pred <- get_div_predictions(test, rf.model)
outputs[2,] <- record_outputs('Random Forest', rf_pred, rf.model)


###### XGB ########

tc <- trainControl(method = "repeatedCV", number=5, repeats=2)
tg <- expand.grid(nrounds=c(50,100, 150),
                  max_depth=c(1,3,5,10),
                  eta=c(0.3,0.4),
                  gamma=c(0),
                  colsample_bytree=c(0.6,0.8),
                  min_child_weight=c(1),
                  subsample=c(.5,.75,1))
xgb.model <- train(playoff_nextyear~.-franchID -yearID -lgID - divID, data=train, method="xgbTree", trControl=tc, tuneGrid=tg)

xgb_pred <- get_div_predictions(test, xgb.model)
outputs[3,] <- record_outputs('XGB', xgb_pred, xgb.model)

print(logit.model)
print(rf.model)
print(xgb.model)
print(outputs)

##### Exploring results ######
final <- data.frame(test)
final$pred <- rf_pred

final[1:5,]

# Visualization

var_importance <- varImp(rf.model, scale=FALSE)$importance
var_importance <- setDT(var_importance, keep.rownames='variable')
var_importance <- arrange(var_importance, desc(Overall))

p <- ggplot(var_importance, aes(x=variable, weight=Overall))
p <- p + geom_bar() + ggtitle("Variable Importance from Random Forest Fit")
p <- p + xlab("Variable") + ylab("Importance (average increase in accuracy)")
p + theme(axis.text.x=element_text(size=12, angle=90, vjust=0.5, hjust=1),
          axis.text.y=element_text(size=12),
          axis.title=element_text(size=16),
          plot.title=element_text(size=18))

var_importance <- varImp(xgb.model, scale=FALSE)$importance
var_importance <- setDT(var_importance, keep.rownames='variable')
var_importance <- arrange(var_importance, desc(Overall))

p <- ggplot(var_importance, aes(x=variable, weight=Overall))
p <- p + geom_bar() + ggtitle("Variable Importance from XGB Fit")
p <- p + xlab("Variable") + ylab("Importance (average increase in accuracy)")
p + theme(axis.text.x=element_text(size=12, angle=90, vjust=0.5, hjust=1),
          axis.text.y=element_text(size=12),
          axis.title=element_text(size=16),
          plot.title=element_text(size=18))

final %>% filter(yearID == max(test$yearID)) %>% select(yearID, franchID, playoff_nextyear, pred)
print(final)

table(final$playoff_nextyear, final$pred)
outputs

