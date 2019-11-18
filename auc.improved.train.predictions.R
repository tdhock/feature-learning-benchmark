library(data.table)

train.dt <- fread("auc.improved.train.csv")
train.dt[, set := sub("/.*", "", prob.dir)]

weight.algo.vec <- c(
  "class.improved", "class.initial", "label.improved", "label.initial")

result.list <- list()
for(set.name in unique(train.dt$set)){
  print(set.name)
  set.labels <- train.dt[set.name, on=.(set)]
  inputs.csv <- paste("xzcat", file.path(
    "../neuroblastoma-data/data", set.name, "inputs.csv.xz"))
  set.feature.dt <- fread(cmd=inputs.csv)
  set.ids <- set.feature.dt$sequenceID
  eval.csv <- paste("xzcat", file.path(
    "../neuroblastoma-data/data", set.name, "evaluation.csv.xz"))
  set.eval.dt <- fread(cmd=eval.csv)
  set.feature.mat <- as.matrix(set.feature.dt[, -1, with=FALSE])
  rownames(set.feature.mat) <- set.feature.dt$sequenceID
  keep <- apply(is.finite(set.feature.mat), 2, all)
  finite.feature.mat <- set.feature.mat[, keep]
  for(fold in unique(set.labels$test.fold)){
    train.label.dt <- set.labels[J(fold), on=.(test.fold)]
    test.ids <- set.ids[!set.ids %in% train.label.dt$prob.dir]
    X.train <- finite.feature.mat[train.label.dt$prob.dir, ]
    for(weight.algo in weight.algo.vec){
      y.train <- train.label.dt[[weight.algo]]
      fit <- glmnet::cv.glmnet(X.train, y.train)
      X.test <- finite.feature.mat[test.ids, ]
      pred.mat <- predict(fit, X.test)
      pred.dt <- data.table(
        sequenceID=rownames(pred.mat), pred.log.lambda=as.numeric(pred.mat))
      roc.list <- penaltyLearning::ROChange(set.eval.dt, pred.dt, "sequenceID")
      result.list[[paste(set.name, fold, weight.algo)]] <-
        with(roc.list, data.table(
          set.name, fold, weight.algo, auc,
          thresholds[threshold=="predicted"],
          roc=list(list(roc))))
    }
  }
}

result <- do.call(rbind, result.list)

saveRDS(result, "auc.improved.train.predictions.rds")
