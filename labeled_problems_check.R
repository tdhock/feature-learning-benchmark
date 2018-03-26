library(data.table)
errors.dt <- fread("labeled_problems_errors.csv")
pattern <- paste0(
  "(?<chrom>[^:]+)",
  ":",
  "(?<problemStart>[0-9]+)",
  "-",
  "(?<problemEnd>[0-9]+)")

## some models don't have penalty=Inf! -> corrected.
extremes <- errors.dt[, list(n.Inf=sum(penalty==Inf), n.0=sum(penalty==0)), by=list(prob.dir)]
no.zero <- extremes[n.Inf != 1 | n.0 != 1]

## some models do not achieve the max possible errors, some are issues
## some are not.
possible.dt <- fread("labeled_problems_possible_errors.csv")
join.dt <- possible.dt[errors.dt, on=list(prob.dir)]
count.dt <- join.dt[, list(max.fp=sum(fp==possible.fp), max.fn=sum(fn==possible.tp)), by=list(prob.dir)]
bad <- count.dt[max.fn==0 | max.fp==0]
stats.dt.list <- list()
for(bad.i in 1:nrow(bad)){
  bad.row <- bad[bad.i]
  bad.models <- join.dt[bad.row, on=list(prob.dir)]
  prob.dir <- paste0("data/", bad.row$prob.dir)
  cat(sprintf("%4d / %4d %s\n", bad.i, nrow(bad), bad.row$prob.dir))
  prob.mat <- namedCapture::str_match_named(basename(prob.dir), pattern)
  fwrite(data.table(prob.mat), file.path(prob.dir, "problem.bed"))
  coverage.bedGraph.gz <- file.path(prob.dir, "coverage.bedGraph.gz")
  if(file.exists(coverage.bedGraph.gz)){
    system(paste("gunzip", coverage.bedGraph.gz))
  }
  target <- PeakSegPipeline::problem.target(prob.dir, 0.01)
  prob.stats <- rbind(
    bad.models[penalty==0, {
      list(
        what="fp", old=fp,
        new=target$models[penalty==0, fp],
        possible=possible.fp, penalty)
    }], #not necessarily bad.
    bad.models[penalty==0, {
      list(
        what="fn", old=fn,
        new=target$models[penalty==0, fn],
        possible=possible.tp, penalty)
    }], #not necessarily bad.
    bad.models[penalty==Inf, {
      list(
        what="fn", old=fn,
        new=target$models[penalty==Inf, fn],
        possible=possible.tp, penalty)
    }])
  stats.dt.list[[prob.dir]] <- data.table(prob.dir=bad.row$prob.dir, prob.stats)
}
stats.dt <- do.call(rbind, stats.dt.list)
