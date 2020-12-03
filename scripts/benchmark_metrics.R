# Determine benchmark metrics, randomizing playoff teams

# get number of playoff teams per year
playoffs_year <- final_teams_salary %>%
  group_by(yearID) %>%
  count(playoff_nextyear) %>%
  filter(playoff_nextyear == 'Y')
playoffs_year <- playoffs_year[,c(1,3)]
# get random results
total_true = vector()
total_pred = vector()
for (year in seq(1985,2018,1)) {
  if (year == 1993 ){
    next
  }
  n <- filter(playoffs_year, yearID==year)$n
  set.seed(1)
  random_true = sample(c(rep(0,30-n),rep(1,n)))
  set.seed(2)
  random_pred = sample(c(rep(0,30-n),rep(1,n)))
  total_true = append(total_true, random_true)
  total_pred = append(total_pred, random_pred)
}
# produce confusion matrix
bm.ConMatrix = table(total_true, total_pred)
print(bm.ConMatrix)
# calculate metrics
bm.accuracy = (bm.ConMatrix[1, 1] + bm.ConMatrix[2, 2]) /  sum(bm.ConMatrix)
bm.precision = bm.ConMatrix[2, 2] / (bm.ConMatrix[2, 2] + bm.ConMatrix[1, 2])
bm.recall = bm.ConMatrix[2, 2] / (bm.ConMatrix[2, 2] + bm.ConMatrix[2, 1])
bm.f1score = 2*(bm.precision*bm.recall / (bm.precision+bm.recall))
print(bm.accuracy)
print(bm.f1score)
# benchmark accuracy score is approx. 0.634
# benchmark f1 score is approx. 0.267
