library(penaltyLearning)
library(data.table)

folds.dt <- fread("labeled_problems_folds.csv")
addMeta <- function(dt){
  dt[, set.name := sub("/.*", "", prob.dir)]
  dt[, problem := sub(".*/", "", prob.dir)]
  dt[folds.dt, on=list(set.name, problem)]
}
errors.dt <- addMeta(fread("labeled_problems_errors.csv"))
possible.dt <- addMeta(fread("labeled_problems_possible_errors.csv"))

get.min.fp.fn <- function(pred.dt){
  pred.with.thresh <- thresh.dt[pred.dt, on=.(prob.dir), nomatch=0L]
  pred.with.thresh[, thresh := log.lambda - pred.log.lambda]
  thresh.ord <- pred.with.thresh[order(-thresh), .(
    prob.dir=c(NA, prob.dir),
    min.thresh=c(thresh, -Inf),
    max.thresh=c(Inf, thresh),
    diff.fp=c(NA, fp),
    diff.fn=c(NA, fn),
    fp = cumsum(c(sum(first.dt$fp), fp)),
    fn.nomin = cumsum(c(sum(first.dt$fn), fn)),
    change=c(0, fn+fp)
  )]
  thresh.ord[, fn := fn.nomin-min(fn.nomin)]
  thresh.ord[, min.fp.fn := ifelse(fp<fn, fp, fn)]
  thresh.ord[, width.thresh := max.thresh-min.thresh]
  thresh.ord
}
get.step <- function(step.size){
  stopifnot(
    is.numeric(step.size) &&
      length(step.size)==1 &&
      step.size>=0)
  step.pred <- pred.dt[, data.table(
    prob.dir,
    pred.log.lambda=pred.log.lambda+step.size*direction)]
  step.thresh <- get.min.fp.fn(step.pred)
  step.thresh[, area.under.min := ifelse(
    min.fp.fn==0, 0, width.thresh*min.fp.fn)]
  step.thresh[, sum(area.under.min)]
}

auc.improved.list <- list()

## compute derivative of Area under min(FP, FN).
fold.possible <- unique(folds.dt[, .(set.name, fold)])
i.possible <- 1:nrow(fold.possible)
N.possible <- paste(i.possible, "improved")
i.todo <- i.possible[!N.possible %in% names(auc.improved.list)]
biggest.step <- 0.1
for(i in seq_along(i.todo)){
  test.fold.i <- i.todo[[i]]
  cat(sprintf("%4d / %4d test folds TODO=%d\n", i, length(i.todo), test.fold.i))
  test.fold.info <- fold.possible[test.fold.i]
  test.fold.errors <- errors.dt[test.fold.info, on=.(set.name, fold)]
  test.fold.errors[, min.log.lambda := min.log.penalty]
  test.fold.errors[, max.log.lambda := max.log.penalty]
  test.fold.errors[, seg.i := cumsum(
    c(1, diff(fp)!=0 | diff(fn) != 0)), by=.(prob.dir)]
  possible.errors <- possible.dt[test.fold.errors, on=list(
    set.name, fold, prob.dir)]
  possible.errors[, possible.fn := possible.tp]
  test.fold.segs <- possible.errors[, .(
    min.log.lambda=min(min.log.lambda),
    max.log.lambda=max(max.log.lambda)
  ), by=.(prob.dir, seg.i, fp, fn, possible.fn, possible.fp)]
  test.fold.segs[, wfp := fp*possible.fn]
  test.fold.segs[, wfn := fn*possible.fp]
  test.fold.segs[, errors := wfp+wfn]#equal weight errors.
  test.fold.segs[, mid.log.lambda := (max.log.lambda+min.log.lambda)/2]
  pred.list <- list()
  target.list <- list(
    initial=test.fold.errors,
    weighted=test.fold.segs)
  for(pred.name in names(target.list)){
    target.err.dt <- target.list[[pred.name]]
    test.fold.targets <- penaltyLearning::targetIntervals(
      target.err.dt, "prob.dir")
    test.fold.targets[, width := max.log.lambda-min.log.lambda]
    target.pred <- test.fold.targets[order(width==Inf, -width), data.table(
      prob.dir,
      pred.log.lambda=ifelse(
        max.log.lambda==Inf, min.log.lambda+1, ifelse(
          min.log.lambda==-Inf, max.log.lambda-1,
          (min.log.lambda+max.log.lambda)/2)
      )
    )]
    target.pred[!is.finite(pred.log.lambda), pred.log.lambda := 0]
    pred.list[[pred.name]] <- target.pred
  }
  ## only need to compute once:
  thresh.dt <- test.fold.errors[order(-min.log.lambda), {
    fp.diff <- diff(fp)
    fn.diff <- diff(fn)
    any.change <- fp.diff != 0 | fn.diff != 0
    data.table(
      log.lambda=min.log.lambda[c(any.change, FALSE)],
      fp=as.numeric(fp.diff[any.change]),
      fn=as.numeric(fn.diff[any.change]))
  }, by=.(prob.dir)]
  ltab <- table(table(thresh.dt$log.lambda))
  stopifnot(length(ltab)==1)
  ## initialization:
  first.dt <- test.fold.errors[max.log.lambda==Inf]
  pred.dt <- data.table(pred.list$initial, direction=1)
  step.number <- 1
  max.step <- biggest.step
  improving <- TRUE
  prev.loss <- Inf
  while(improving){
    ## these depend on predictions:
    thresh.ord <- get.min.fp.fn(pred.dt)
    thresh.ord[, min.change := c(NA, diff(min.fp.fn))]
    prob.deriv <- thresh.ord[min.change!=0, .(
      deriv=-sum(min.change)
    ), by=.(prob.dir)]
    pred.dt[, direction := 0]
    pred.dt[prob.deriv, direction := -deriv, on=.(prob.dir)]
    ##print(pred.dt[direction != 0])
    step.size.vec <- seq(0, max.step, l=20)
    step.size.loss <- sapply(step.size.vec, get.step)
    yrange.big <- diff(range(step.size.loss))>1e-10
    if(FALSE){
      plot(
        step.size.vec, step.size.loss,
        main=paste("step", step.number))
    }
    loss.inc <- step.size.loss[1]<step.size.loss
    step.number <- step.number+1
    is.min <- step.size.loss==min(step.size.loss)
    best.i <- as.integer(median(which(is.min)))
    step.size.best <- step.size.vec[best.i]
    loss <- step.size.loss[best.i]
    improving <- if(loss < prev.loss && yrange.big){
      TRUE
    }else{
      FALSE
    }
    prev.loss <- loss
    if(any(loss.inc)){
      first.bigger <- which(loss.inc)[1]
      if(first.bigger==2){
        improving <- TRUE
      }
      max.step <- step.size.vec[first.bigger]
    }
    if(step.number %% 20 == 0){
      max.step <- biggest.step
    }
    pred.dt[, pred.log.lambda := pred.log.lambda+step.size.best*direction]
  }
  mid.pred <- test.fold.segs[pred.dt, .(
    prob.dir,
    improved.pred=pred.log.lambda,
    mid.log.lambda), on=.(
    prob.dir,
    min.log.lambda < pred.log.lambda,
    max.log.lambda > pred.log.lambda)]
  mid.pred[, pred.log.lambda := ifelse(
    is.finite(mid.log.lambda), improved.pred, mid.log.lambda)]
  pred.list$improved <- data.table(pred.dt)
  for(pred.name in names(pred.list)){
    pred <- pred.list[[pred.name]]
    L <- penaltyLearning::ROChange(possible.errors, pred, "prob.dir")
    cat(sprintf("%s %f\n", pred.name, L$auc))
    auc.improved.list[[paste(test.fold.i, pred.name)]] <- with(L, data.table(
      test.fold.i,
      test.fold.info,
      pred.name,
      roc=list(list(roc)),
      pred=list(list(pred[, .(prob.dir, pred.log.lambda)])),
      thresholds[threshold=="min.error"],
      auc))
  }
}
(auc.improved <- do.call(rbind, auc.improved.list))

auc.improved[, pred[[1]], by=list(test.fold.i, pred.name)]

saveRDS(auc.improved, "auc.improved.rds")
