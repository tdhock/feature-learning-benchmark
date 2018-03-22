library(penaltyLearning)
library(data.table)
features.dt <- fread("labeled_problems_features.csv")
dir.create("labeled_problems_pred", showWarnings=FALSE)
## TODO actually train constant model.
fwrite(features.dt[, .(prob.dir, pred.log.lambda=2)], "labeled_problems_pred/AIC.csv")
fwrite(features.dt[, .(prob.dir, pred.log.lambda=log.log.data)], "labeled_problems_pred/BIC.csv")

errors.dt <- fread("labeled_problems_errors.csv")
possible.dt <- fread("labeled_problems_possible_errors.csv")
possible.errors <- possible.dt[errors.dt, on=list(prob.dir)]
possible.errors[, possible.fn := possible.tp]
possible.errors[, min.log.lambda := min.log.penalty]

folds.dt <- data.table(set.name=dir("data"))[, {
  folds.csv <- file.path("data", set.name, "folds.csv")
  fread(folds.csv)
}, by=list(set.name)]

## for every labeled_problems_pred/MODEL.csv, compute
## labeled_problems_pred_error/MODEL.csv
pred.csv.vec <- Sys.glob(file.path("labeled_problems_pred", "*.csv"))
for(pred.csv in pred.csv.vec){
  model <- sub(".csv$", "", basename(pred.csv))
  pred.dt <- fread(pred.csv)
  pred.dt[, problem := sub(".*/", "", prob.dir)]
  pred.dt[, set.name := sub("/.*", "", prob.dir)]
  pred.folds <- folds.dt[pred.dt, on=list(set.name, problem)]
  auc.dt <- pred.folds[, {
    fold.pred.dt <- data.table(prob.dir, pred.log.lambda)
    roc <- ROChange(possible.errors, fold.pred.dt, "prob.dir")
    with(roc, data.table(errors[type=="min"], auc)
  }, by=list(set.name, fold)]
}
