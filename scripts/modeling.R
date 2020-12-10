# SYST 568 Project
# Model Exploration. Last updated 12/10/2020

library(ggplot2)
library(randomForest)
library(caret)
library(dplyr)
library(here)


get_div_predictions <- function(data, model) {
  playoff_div_year <- read.csv(here::here('data/input/playoffs_year.csv'))
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


### record results by year####################################
results_year <- function(model_name, ConMatrix) {
  
  #ConMatrix = table(test$playoff_nextyear, pred)
  # calculate metrics
  accuracy = (ConMatrix[1, 1] + ConMatrix[2, 2]) /  sum(ConMatrix)
  precision = ConMatrix[2, 2] / (ConMatrix[2, 2] + ConMatrix[1, 2])
  recall = ConMatrix[2, 2] / (ConMatrix[2, 2] + ConMatrix[2, 1])
  f1score = 2*(precision*recall / (precision+recall))
  
  return(list(model_name, accuracy, precision, recall, f1score))
}


fit_models <- function(final_teams_salary) {

  # final_teams_salary = read.csv('./data/final_teams_salary.csv')[,-1]
  
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

  
  ####### Logit regression #########
  
  tc <- trainControl(method = "repeatedCV", number=5, repeats=1)
  #tg <- expand.grid(nIter=c(1,2,5,10))
  
  
  ## Logit Regression with fewer predictor variables ##
  # logit.model1 removes variables with Prob over 0.5 from original model
  logit.model <- train(playoff_nextyear~.-franchID -yearID -lgID - divID -X2B_G - BB_G - SB_G - HBP_G - SF_G - RA_G - ER_G - CG_G - 
                         SHO_G - SV_G - IPouts_G - HA_G - SOA_G - E_G - DP_G - Playoffs, data=train, method="plr", trControl=tc)#, tuneGrid=tg)
  
  logit_pred <- get_div_predictions(test, logit.model)

  
  ###### Random Forest ########
  
  tc <- trainControl(method = "repeatedCV", number=5, repeats=2)
  tg <- expand.grid(mtry=c(10,15,20,25))
  
  rf.model <- train(playoff_nextyear~.-franchID -yearID -lgID - divID, data=train, method="rf", trControl=tc, tuneGrid=tg)
  
  rf_pred <- get_div_predictions(test, rf.model)

  
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

  
  results <- data.frame(model_name=character(),
                        accuracy=numeric(),
                        precision=numeric(),
                        recall=numeric(),
                        f1score=numeric())
  
  ## rf ##############################################################
  final_rf <- data.frame(test)
  final_rf$pred <- rf_pred
  final_rf <- data.table(final_rf)
  
  
  results[1,] <- results_year("rf all years", table(final_rf$playoff_nextyear, final_rf$pred))
  results[2,] <- results_year("rf < 1992",table(final_rf[yearID<1992,]$pred, final_rf[yearID<1992,]$playoff_nextyear))
  results[3,] <- results_year("rf 1993 - 2010",table(final_rf[yearID>1992 & yearID <= 2010,]$pred, final_rf[yearID>1992 & yearID <= 2010,]$playoff_nextyear))
  results[4,] <- results_year("rf 2011 - present",table(final_rf[yearID>=2011,]$pred, final_rf[yearID>=2011,]$playoff_nextyear))

  
  ## logit ##############################################################
  final_logit <- data.frame(test)
  final_logit$pred <- logit_pred
  final_logit <- data.table(final_logit)

  
  results[5,] <- results_year("logit all years", table(final_logit$playoff_nextyear, final_logit$pred))
  results[6,] <- results_year("logit < 1992",table(final_logit[yearID<1992,]$pred, final_logit[yearID<1992,]$playoff_nextyear))
  results[7,] <- results_year("logit 1993 - 2010",table(final_logit[yearID>1992 & yearID <= 2010,]$pred, final_logit[yearID>1992 & yearID <= 2010,]$playoff_nextyear))
  results[8,] <- results_year("logit 2011 - present",table(final_logit[yearID>=2011,]$pred, final_logit[yearID>=2011,]$playoff_nextyear))

  ## xgb ##############################################################
  final_xgb <- data.frame(test)
  final_xgb$pred <- xgb_pred
  final_xgb <- data.table(final_xgb)
  

  results[9,] <- results_year("xgb all years", table(final_xgb$playoff_nextyear, final_xgb$pred))
  results[10,] <- results_year("xgb < 1992",table(final_xgb[yearID<1992,]$pred, final_xgb[yearID<1992,]$playoff_nextyear))
  results[11,] <- results_year("xgb 1993 - 2010",table(final_xgb[yearID>1992 & yearID <= 2010,]$pred, final_xgb[yearID>1992 & yearID <= 2010,]$playoff_nextyear))
  results[12,] <- results_year("xgb 2011 - present",table(final_xgb[yearID>=2011,]$pred, final_xgb[yearID>=2011,]$playoff_nextyear))
  
  generate_vis(rf.model, xgb.model)
  return(results)
}


generate_vis <- function(rf.model, xgb.model) {
  
  var_importance <- varImp(rf.model, scale=FALSE)$importance
  var_importance <- setDT(var_importance, keep.rownames='variable')
  var_importance <- arrange(var_importance, desc(Overall))
  
  rf_p <- ggplot(var_importance, aes(x=variable, weight=Overall))
  rf_p <- rf_p + geom_bar() + ggtitle("Variable Importance from Random Forest Fit")
  rf_p <- rf_p + xlab("Variable") + ylab("Importance (average increase in accuracy)")
  rf_p + theme(axis.text.x=element_text(size=12, angle=90, vjust=0.5, hjust=1),
               axis.text.y=element_text(size=12),
               axis.title=element_text(size=16),
               plot.title=element_text(size=18))
  print(rf_p)
  
  var_importance <- varImp(xgb.model, scale=FALSE)$importance
  var_importance <- setDT(var_importance, keep.rownames='variable')
  var_importance <- arrange(var_importance, desc(Overall))
  
  xgb_p <- ggplot(var_importance, aes(x=variable, weight=Overall))
  xgb_p <- xgb_p + geom_bar() + ggtitle("Variable Importance from XGB Fit")
  xgb_p <- xgb_p + xlab("Variable") + ylab("Importance (average increase in accuracy)")
  xgb_p + theme(axis.text.x=element_text(size=12, angle=90, vjust=0.5, hjust=1),
                axis.text.y=element_text(size=12),
                axis.title=element_text(size=16),
                plot.title=element_text(size=18))
  print(xgb_p)
}

