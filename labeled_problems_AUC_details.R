library(penaltyLearning)
library(data.table)

folds.dt <- fread("labeled_problems_folds.csv")
addMeta <- function(dt){
  dt[, set.name := sub("/.*", "", prob.dir)]
  dt[, problem := sub(".*/", "", prob.dir)]
  dt[folds.dt, on=list(set.name, problem)]
}

errors.dt <- fread("labeled_problems_errors.csv")
possible.dt <- addMeta(fread("labeled_problems_possible_errors.csv"))
possible.errors <- possible.dt[errors.dt, on=list(prob.dir)]
possible.errors[, possible.fn := possible.tp]
possible.errors[, min.log.lambda := min.log.penalty]

some.possible <- possible.dt[set.name=="H3K36me3_TDH_ENCODE" & fold==3]
some.totals <- some.possible[, data.table(
  fp=sum(possible.fp),
  fn=sum(possible.tp)
  )]
some.errors <- possible.errors[set.name=="H3K36me3_TDH_ENCODE" & fold==3]
some.errors[, wfp := fp/some.totals$fp]
some.errors[, wfn := fn/some.totals$fn]
some.errors[penalty==Inf, sum(wfn)]
some.errors[penalty==0, sum(wfp)]
some.errors[, werr := wfp+wfn]

some.targets <- some.errors[, {
  dt <- .SD[werr==min(werr)]
  e <- .SD[errors==min(errors)]
  ##browser(expr=prob.dir=="H3K36me3_TDH_ENCODE/samples/thyroid/ENCFF119YYL/problems/chr5:100000-17500000")
  data.table(
    fp.werr=dt[1, fp],
    fp.errors=e[1, fp],
    fn.werr=dt[1, fn],
    fn.errors=e[1, fn],
    min.werr=dt[1, min.log.penalty],
    min.errors=e[1, min.log.penalty],
    max.werr=dt[.N, max.log.penalty],
    max.errors=e[.N, max.log.penalty])
}, by=list(prob.dir)]
some.targets[max.werr<Inf, mean(max.werr-min.werr, na.rm=TRUE)]
(diff.targets <- some.targets[pred.werr != pred.errors])
some.errors[diff.targets, on=list(prob.dir, min.log.penalty < pred.werr, max.log.penalty > pred.werr)]
some.errors[diff.targets, on=list(prob.dir, min.log.penalty < pred.errors, max.log.penalty > pred.errors)]
some.errors[prob.dir=="H3K36me3_TDH_ENCODE/samples/thyroid/ENCFF119YYL/problems/chr5:100000-17500000"]
## verify that all have same fp.
some.targets[fp.werr != fp.errors]

for(pred.name in c("werr", "errors", "constant")){
  col.name <- paste0("pred.", pred.name)
  some.targets[[col.name]] <- if(pred.name=="constant"){
    8.69448787107607
  }else{
    err.dt <- some.errors[, data.table(
      prob.dir,
      min.log.lambda=min.log.penalty,
      max.log.lambda=max.log.penalty,
      errors=get(pred.name))]
    t.dt <- targetIntervals(err.dt, "prob.dir")
    t.dt[, ifelse(
      max.log.lambda==Inf,
      min.log.lambda+1,
      (min.log.lambda+max.log.lambda)/2)]
  }
}

## for every labeled_problems_pred/MODEL.csv, compute
## labeled_problems_pred_error/MODEL.csv
roc.dt.list <- list()
pred.dt.list <- list()
auc.dt.list <- list()
for(pred.name in c("werr", "errors", "constant")){
  col.name <- paste0("pred.", pred.name)
  some.targets[, pred.log.lambda := get(col.name)]
  roc <- ROChange(some.errors, some.targets, "prob.dir")
  roc.dt.list[[pred.name]] <- data.table(pred.name, roc$roc)
  pred.dt.list[[pred.name]] <- data.table(pred.name, roc$thresholds)
  auc.dt.list[[pred.name]] <- data.table(pred.name, auc=roc$auc)
}
roc.dt <- do.call(rbind, roc.dt.list)
pred.dt <- do.call(rbind, pred.dt.list)
(auc.dt <- do.call(rbind, auc.dt.list))

ggplot()+
  scale_fill_manual(values=c(predicted="white", min.error="black"))+
  geom_path(aes(
    FPR, TPR, color=pred.name),
    data=roc.dt)+
  geom_point(aes(
    FPR, TPR, color=pred.name, fill=threshold),
    shape=21,
    data=pred.dt[threshold=="predicted"])

## issues:

## 1. why is the ROC error curve here not the same as the one we
## computed in the other script? there was a problem with target
## interval computation. OK now.

## 2. why is the werr curve getting to 100% tpr before the errors
## curve? because werr re-weights the small number of fn's as more
## important.

pred.dt[threshold=="predicted"]
## 3. why is werr (4fp) better than errors (4fp+1fn) ?? Resolved, the
## problem was incorrect computation of target interval. Now both have
## 4err.

## Resolution. computing target intervals based on the weighted error
## yields the upper bound on AUC.
