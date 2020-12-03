# Determine benchmark metrics, randomizing playoff teams

# randomize two sets of playoff predictions
set.seed(1)
random_true = sample(c(rep(0,22*31),rep(1,8*31)))
set.seed(2)
random_pred = sample(c(rep(0,22*31),rep(1,8*31)))
# produce confusion matrix
bm.ConMatrix = table(random_true, random_pred)
print(bm.ConMatrix)
# calculate metrics
bm.accuracy = (bm.ConMatrix[1, 1] + bm.ConMatrix[2, 2]) /  sum(bm.ConMatrix)
bm.precision = bm.ConMatrix[2, 2] / (bm.ConMatrix[2, 2] + bm.ConMatrix[1, 2])
bm.recall = bm.ConMatrix[2, 2] / (bm.ConMatrix[2, 2] + bm.ConMatrix[2, 1])
bm.f1score = 2*(bm.precision*bm.recall / (bm.precision+bm.recall))
print(bm.accuracy)
print(bm.f1score)
# benchmark accuracy score is approx. 0.619
# benchmark f1 score is approx. 0.286

## Logit Scores ##
logit.ConMatrix = table(test$playoff_pred, test$playoff_nextyear)
logit.ConMatrix
accuracy = (logit.ConMatrix[1, 1] + logit.ConMatrix[2, 2]) /  sum(logit.ConMatrix)
precision = logit.ConMatrix[2, 2] / (logit.ConMatrix[2, 2] + logit.ConMatrix[1, 2])
recall = logit.ConMatrix[2, 2] / (logit.ConMatrix[2, 2] + logit.ConMatrix[2, 1])
f1score = 2*(precision*recall / (precision+recall))
print(accuracy)
print(f1score)
# accuracy score of 0.696, improvement of 0.076 over random
# f1 score of 0.451, improvement of 0.149 over random