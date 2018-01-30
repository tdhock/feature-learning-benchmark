library(data.table)
features.dt <- fread("target.intervals.features.csv")

labels.dt <- features.dt[500:3500, {
  dt <- fread(file.path("data", prob.dir, "labels.bed"))
  setnames(dt, c("chrom", "chromStart", "chromEnd", "annotation"))
  dt
}, by=list(prob.dir)]
labels.dt[, set := sub("/.*", "", prob.dir)]
count.dt <- labels.dt[, list(count=.N), by=list(set, annotation)]
features.dt[, list(problems=.N), by=list(set=sub("/.*", "", prob.dir))]
dcast(count.dt, set ~ annotation)
