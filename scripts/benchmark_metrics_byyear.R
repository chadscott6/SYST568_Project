####### random by year #################

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


## function ########################################################
record_outputs <- function(model_name, ConMatrix) {
  
  #ConMatrix = table(test$playoff_nextyear, pred)
  # calculate metrics
  accuracy = (ConMatrix[1, 1] + ConMatrix[2, 2]) /  sum(ConMatrix)
  precision = ConMatrix[2, 2] / (ConMatrix[2, 2] + ConMatrix[1, 2])
  recall = ConMatrix[2, 2] / (ConMatrix[2, 2] + ConMatrix[2, 1])
  f1score = 2*(precision*recall / (precision+recall))
  
  return(list(model_name, accuracy, precision, recall, f1score))
}
results_random <- data.frame(model_name=character(),
                      accuracy=numeric(),
                      precision=numeric(),
                      recall=numeric(),
                      f1score=numeric())


# Determine benchmark metrics, randomizing playoff teams

# get number of playoff teams per year
playoffs_year <- final_teams_salary %>%
  group_by(yearID) %>%
  count(playoff_nextyear) %>%
  filter(playoff_nextyear == 'Y')
playoffs_year <- playoffs_year[,c(1,3)]
# get total number of teams per year
teams_year <- final_teams_salary %>%
  group_by(yearID) %>%
  count()
# get random results
total_random_pred = vector()
set.seed(1985)
for (year in unique(final_teams_salary$yearID)) {
  n <- filter(playoffs_year, yearID==year)$n
  teams <- filter(teams_year, yearID==year)$n
  set.seed(year)
  random_pred = sample(c(rep('N',teams-n),rep('Y',n)))
  total_random_pred = append(total_random_pred, random_pred)
}

# cbind with final_teams_salary
final_teams_salary <- cbind(final_teams_salary, total_random_pred)

final_teams_salary <- data.table(final_teams_salary)

table(final_teams_salary$playoff_nextyear, total_random_pred)
table(final_teams_salary$playoff_nextyear, final_teams_salary$total_random_pred)
table(final_teams_salary[yearID>2004,]$playoff_nextyear, final_teams_salary[yearID>2004,]$total_random_pred)

summary(final_teams_salary$yearID)

results_random[1,] <- record_outputs("random all years", table(final_teams_salary$playoff_nextyear, final_teams_salary$total_random_pred))
results_random
results_random[2,] <- record_outputs("random < 1992",table(final_teams_salary[yearID<1992,]$total_random_pred, final_teams_salary[yearID<1992,]$playoff_nextyear))
results_random
results_random[3,] <- record_outputs("random 1993 - 2010",table(final_teams_salary[yearID>1993 & yearID <= 2010,]$total_random_pred, final_teams_salary[yearID>1993 & yearID <= 2010,]$playoff_nextyear))
results_random
results_random[4,] <- record_outputs("random 2011 - present",table(final_teams_salary[yearID>2011,]$total_random_pred, final_teams_salary[yearID>2011,]$playoff_nextyear))
results_random

