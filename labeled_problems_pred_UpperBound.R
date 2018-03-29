library(penaltyLearning)
library(directlabels)
library(data.table)
folds.dt <- fread("labeled_problems_folds.csv")
addMeta <- function(dt){
  dt[, set.name := sub("/.*", "", prob.dir)]
  dt[, problem := sub(".*/", "", prob.dir)]
  dt[folds.dt, on=list(set.name, problem)]
}
targets.dt <- addMeta(fread("labeled_problems_targets.csv"))
targets.dt[, pred.log.lambda := ifelse(
  min.log.penalty == -Inf, max.log.penalty-1, ifelse(
    max.log.penalty == Inf, min.log.penalty+1,
    (min.log.penalty+max.log.penalty)/2))]

fwrite(targets.dt[, .(prob.dir, pred.log.lambda)], file.path("labeled_problems_pred", "UpperBoundAcc.csv"))

## now compute target intervals based on the weighted error, in order
## to upper bound the AUC.
errors.dt <- addMeta(fread("labeled_problems_errors.csv"))
possible.dt <- addMeta(fread("labeled_problems_possible_errors.csv"))
fold.possible <- possible.dt[, data.table(
  fold.fp=sum(possible.fp),
  fold.fn=sum(possible.tp)
  ), by=list(set.name, fold)]
possible.errors <- fold.possible[errors.dt, on=list(set.name, fold)]
werr <- possible.errors[, data.table(
  prob.dir,
  min.log.lambda=min.log.penalty,
  max.log.lambda=max.log.penalty,
  errors=fp/fold.fp+fn/fold.fn)]
wtargets <- targetIntervals(werr, "prob.dir")
wtargets[, pred.log.lambda := ifelse(
  min.log.lambda == -Inf, max.log.lambda-1, ifelse(
    max.log.lambda == Inf, min.log.lambda+1,
    (min.log.lambda+max.log.lambda)/2))]
fwrite(wtargets[, .(prob.dir, pred.log.lambda)], file.path("labeled_problems_pred", "UpperBoundAUC.csv"))

## issue: this actually results in AUC which is sometimes lower than
## the BestConstant model... why? and how can we compute the best
## possible?
