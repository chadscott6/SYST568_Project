# SYST 568 Project
# Data Cleaning. Created by Chad Scott. Last updated 12/01/2020 by Jonathan Nelson.
  options(max.print = 10000)
# Install Lahman package
#  install.packages("Lahman")

# load libraries
  library(Lahman)
  library(data.table)
  library(dplyr)

# view all available data sets in the Lahman package  
  data( package = "Lahman")
  
# load in Teams data
  data("Teams")
  Teams <- data.table(Teams)
# explore Teams data
  str(Teams)
  head(Teams)  

# filter Teams to only include 1960 - 2019
  Teams <- Teams[yearID>1968,]
  summary(Teams)  
  head(Teams)

### Playoffs Variable ########################################################
# Teams that made the playoffs 
  #2012 - Present (not inlcuding 2020) 10 teams made the playoffs
  Teams[yearID==2019,]
  Teams[yearID==2019 & (DivWin=="Y" | WCWin == "Y"),]
  
  Teams[yearID==2012 & (DivWin=="Y" | WCWin == "Y"),1:40]
  # 1994 -2011 8 teams made the playoffs 1994 there was NO POST SEASON
  Teams[yearID==2011,1:40]
  Teams[yearID==2011 & (DivWin=="Y" | WCWin=="Y"),1:40]
  
  Teams[yearID==1995,1:40]
  Teams[yearID==1995 & (DivWin=="Y" | WCWin=="Y"),1:40]
  
  Teams[yearID==1994,]
  Teams[yearID==1994 & (DivWin=="Y" | WCWin=="Y"),1:40]
  
  # 1969 -1993 4 teams made the playoffs
  Teams[yearID==1993,]
  Teams[yearID==1993 & DivWin=="Y",]
  
  Teams[yearID==1969,1:40]
  Teams[yearID==1969 & DivWin=="Y",1:40]
  
# Creating playoffs variable
  Teams$Playoffs <- "N"
  Teams[yearID>1993 & (DivWin=="Y" | WCWin=="Y"), Playoffs:= "Y"]
  Teams[yearID<1994 & DivWin=="Y", Playoffs:="Y"]

# creating playoffs Lag variable
  setkey(Teams, teamID, yearID) # important for ordering
  Teams[,playoff_nextyear:=(shift(Playoffs, -1)),by=teamID]  
  Teams[teamID=='ATL',c(1:20,46:50)] 

  
### Team Salaries ############################################################
  data("Salaries")
  Salaries <- data.table(Salaries)
  str(Salaries)  
  head(Salaries)
  Salaries$salary <- as.numeric(Salaries$salary)
  Salaries[,TeamSalary:=sum(salary), by = c("yearID","teamID")]
  Salaries <- unique(Salaries[,c("yearID","teamID","TeamSalary")])
  Salaries[,TeamSalary:=TeamSalary/mean(TeamSalary), by = c("yearID")]
  Salaries
  
# merge Salaries with Teams data
  Teams_w_salary <- merge(Teams, Salaries,  by = c("yearID","teamID"), all.x = TRUE)
# filter to only years with team salary data
  Teams_w_salary <- Teams_w_salary[yearID>1984 & yearID<2017,]
  

### Features for modeling ##########################################
  
  
  ### Data without salary ### 
  final_teams <- Teams[,c(6,15,17:24,28,33:36,50)]
  final_teams$playoff_nextyear <- as.factor(final_teams$playoff_nextyear)
  final_teams <- na.omit(final_teams, cols="playoff_nextyear")
  # train-test split
  set.seed(12345)
  train_index <- sample(1:nrow(final_teams), 0.5 * nrow(final_teams))
  test_index <- setdiff(1:nrow(final_teams), train_index)
  train <- final_teams[train_index, -15]
  test <- final_teams[test_index, -15]
  
  # benchmark accuracy with all predictions as "N"
  pred = rep('N', nrow(final_teams))
  ConMatrix = table(pred, final_teams$playoff_nextyear)
  ConMatrix
  (ConMatrix[1, 1]) /  nrow(final_teams)
  # benchmark accuracy is 0.77
  
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
  # achieves an accuracy of 0.778, not great, barely better than random
  
### Data with salary ### 
  final_teams_salary <- Teams_w_salary[,c(6,15,17:24,28,33:36,50,51)]
  final_teams_salary$playoff_nextyear <- as.factor(final_teams_salary$playoff_nextyear)
  final_teams_salary <- na.omit(final_teams_salary, cols="playoff_nextyear")
  # train-test split
  set.seed(12345)
  train_index <- sample(1:nrow(final_teams_salary), 0.5 * nrow(final_teams_salary))
  test_index <- setdiff(1:nrow(final_teams_salary), train_index)
  train <- final_teams_salary[train_index, -15]
  test <- final_teams_salary[test_index, -15]
  
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
  # achieves an accuracy of 0.741, worse because less data, worse than random
  