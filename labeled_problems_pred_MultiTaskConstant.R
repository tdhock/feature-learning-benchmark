library(penaltyLearning)
library(directlabels)
library(data.table)
folds.dt <- fread("labeled_problems_folds.csv")
addMeta <- function(dt){
  dt[, set.name := sub("/.*", "", prob.dir)]
  dt[, problem := sub(".*/", "", prob.dir)]
  dt[folds.dt, on=list(set.name, problem)]
}
errors.dt <- addMeta(fread("labeled_problems_errors.csv"))
possible.dt <- fread("labeled_problems_possible_errors.csv")
possible.errors <- possible.dt[errors.dt, on=list(prob.dir)]
possible.errors[, possible.fn := possible.tp]
possible.errors[, min.log.lambda := min.log.penalty]
zero.dt <- errors.dt[penalty==0]
pred.dt <- addMeta(data.table(
  prob.dir=zero.dt$prob.dir,
  pred.log.lambda=NA_real_))
for(test.fold in 1:4){
  train.errors <- possible.errors[fold != test.fold]
  constant.dt <- train.errors[penalty==0, data.table(prob.dir, pred.log.lambda=0)]
  roc <- penaltyLearning::ROChange(train.errors, constant.dt, "prob.dir")
  best.pen <- roc$thresholds[threshold=="min.error", (min.thresh+max.thresh)/2]
  cat(sprintf(
    "fold=%d penalty=%f\n",
    test.fold, best.pen))
  test.dt <- errors.dt[fold == test.fold & penalty == 0]
  pred.dt[test.dt, pred.log.lambda := best.pen, on=list(prob.dir)]
}

fwrite(pred.dt[, .(prob.dir, pred.log.lambda)], file.path("labeled_problems_pred", "MultiTaskConstant.csv"))
