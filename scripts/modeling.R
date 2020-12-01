# SYST 568 Project
# Model Exploration. Last updated 12/1/2020


library(InformationValue)

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


#### Logistic Regression - without salary ##### 
## NOTE - currently not implemeneted, uses different train / test data

final_teams$playoff_nextyear <- as.factor(final_teams$playoff_nextyear)
final_teams <- na.omit(final_teams, cols="playoff_nextyear")
# train-test split
# set.seed(12345)
# train_index <- sample(1:nrow(final_teams), 0.5 * nrow(final_teams))
# test_index <- setdiff(1:nrow(final_teams), train_index)
# train <- final_teams[train_index, -15]
# test <- final_teams[test_index, -15]

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
# achieves an accuracy of 0.741, worse because less data, worse than random