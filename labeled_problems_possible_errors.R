library(data.table)
library(PeakError)
targets.dt <- fread("labeled_problems_targets.csv")

error.dt <- targets.dt[, {
  labels.dt <- fread(file.path("data", prob.dir, "labels.bed"))
  setnames(labels.dt, c("chrom", "chromStart", "chromEnd", "annotation"))
  error.regions <- PeakErrorChrom(Peaks(), labels.dt)
  data.table(error.regions)[, data.table(
    possible.fp=sum(possible.fp),
    possible.tp=sum(possible.tp),
    labels=.N)]
}, by=list(prob.dir)]
fwrite(error.dt, "labeled_problems_possible_errors.csv")
