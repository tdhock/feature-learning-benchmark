library(penaltyLearning)
library(directlabels)
library(data.table)
library(future);plan(multiprocess)
folds.dt <- fread("labeled_problems_folds.csv")
addMeta <- function(dt){
  dt[, set.name := sub("/.*", "", prob.dir)]
  dt[, problem := sub(".*/", "", prob.dir)]
  dt[folds.dt, on=list(set.name, problem)]
}
features.dt <- addMeta(fread("labeled_problems_features.csv"))
for(set.name in unique(features.dt$set.name)){
  features.dt[[set.name]] <- ifelse(features.dt$set.name==set.name, 1, 0)
}
targets.dt <- addMeta(fread("labeled_problems_targets.csv"))
pred.dt <- addMeta(data.table(
  prob.dir=targets.dt$prob.dir,
  pred.log.lambda=NA_real_))
not.features <- c("prob.dir", "set.name", "problem", "fold")
for(test.fold in 1:4){
  cat(sprintf("fold=%d\n", test.fold))
  train.features <- features.dt[fold != test.fold]
  train.targets <- targets.dt[fold != test.fold]
  train.feature.mat <- as.matrix(
    train.features[, !names(train.features) %in% not.features, with=FALSE])
  train.target.mat <- as.matrix(
    train.targets[, c("min.log.penalty", "max.log.penalty"), with=FALSE])
  set.seed(1)
  fit <- IntervalRegressionCV(train.feature.mat, train.target.mat, reg.type="1sd")
  print(fit$pred.feature.names)
  test.features <- features.dt[fold == test.fold]
  test.feature.mat <- as.matrix(
    test.features[, !names(test.features) %in% not.features, with=FALSE])
  ## replace NA entries by min of that column (since NA is generated
  ## by taking log of a negative number)
  to.rep <- is.na(test.feature.mat)
  min.mat <- matrix(
    suppressWarnings(apply(test.feature.mat, 2, min, na.rm=TRUE)),
    nrow(test.feature.mat),
    ncol(test.feature.mat),
    byrow=TRUE)
  test.feature.mat[to.rep] <- min.mat[to.rep]
  pred.dt[test.features, pred.log.lambda := as.numeric(predict(fit, test.feature.mat)), on=list(prob.dir)]
}

fwrite(pred.dt[, .(prob.dir, pred.log.lambda)], file.path("labeled_problems_pred", "MultiTaskIRCV.csv"))
