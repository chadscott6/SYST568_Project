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
# produce confusion matrix
bm.ConMatrix = table(final_teams_salary$playoff_nextyear, total_random_pred)
print(bm.ConMatrix)
# calculate metrics
bm.accuracy = (bm.ConMatrix[1, 1] + bm.ConMatrix[2, 2]) /  sum(bm.ConMatrix)
bm.precision = bm.ConMatrix[2, 2] / (bm.ConMatrix[2, 2] + bm.ConMatrix[1, 2])
bm.recall = bm.ConMatrix[2, 2] / (bm.ConMatrix[2, 2] + bm.ConMatrix[2, 1])
bm.f1score = 2*(bm.precision*bm.recall / (bm.precision+bm.recall))
print(bm.accuracy)
print(bm.f1score)
# benchmark accuracy score is approx. 0.622
# benchmark f1 score is approx. 0.281 
