library(data.table)
data.dir.vec <- Sys.glob("data/*")
for(data.dir in data.dir.vec){
  print(data.dir)
  labels.bed.vec <- Sys.glob(file.path(data.dir, "samples/*/*/problems/*/labels.bed"))
  lab.counts <- data.table(labels.bed=labels.bed.vec)[, {
    if(0 < file.size(labels.bed)){
      lab.dt <- fread(labels.bed)
      setnames(lab.dt, c("chrom", "labelStart", "labelEnd", "annotation"))
      lab.dt[, list(
        negative=sum(annotation != "peaks"),
        positive=sum(annotation != "noPeaks"),
        prob.labels=.N
        )]
    }
  }, by=list(labels.bed)]
  ## lab.counts <- fread(paste("wc -l", file.path(data.dir, "samples/*/*/problems/*/labels.bed"), "|head -n -1"))
  ## setnames(lab.counts, c("prob.labels", "labels.bed"))
  lab.counts[, prob.dir := basename(dirname(labels.bed))]
  lab.totals <- lab.counts[0 < prob.labels, list(
    total.labels=sum(prob.labels),
    total.negative=sum(negative),
    total.positive=sum(positive)
    ), by=list(prob.dir)]
  seed.folds <- data.table(seed=0:100)[, {
    i.vec <- if(seed==0){
      order(lab.totals$total.labels)
    }else if(seed==1){
      order(-lab.totals$total.labels)
    }else{
      set.seed(seed)
      sample(1:nrow(lab.totals))
    }
    perm <- lab.totals[i.vec]
    perm[, cum.labels := cumsum(total.labels)]
    perm[, cum.frac := cum.labels/max(cum.labels)]
    perm[, fold := ceiling(cum.frac * 4)]
    perm
  }, by=list(seed)]
  seed.counts <- seed.folds[, list(
    labels.in.fold=sum(total.labels),
    positive.in.fold=sum(total.positive),
    negative.in.fold=sum(total.negative)
    ), by=list(seed, fold)]
  seed.ranges <- seed.counts[, list(
    n.folds=.N,
    min=min(labels.in.fold),
    max=max(labels.in.fold)
    ), by=list(seed)]
  best.seed <- seed.ranges[n.folds==4][which.min(max-min), seed]
  fold.counts <- seed.counts[seed==best.seed]
  print(fold.counts)
  selected.folds <- seed.folds[seed==best.seed]
  print(selected.folds)
  stopifnot(nrow(fold.counts)==4)
  stopifnot(all(0 < fold.counts$total.negative))
  stopifnot(all(0 < fold.counts$total.positive))
  stopifnot(0 < fold.counts$labels.in.fold)
  selected.folds[, list(prob.dir, fold, labels=total.labels, possible.tp=total.positive, possible.fp=total.negative)]
  fwrite(selected.folds[, list(problem=prob.dir, fold)], file.path(data.dir, "folds.csv"))
}
