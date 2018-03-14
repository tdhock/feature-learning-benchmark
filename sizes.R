library(data.table)

du.dt <- fread("du -ks data/*/samples/*/*/problems/*/coverage.bedGraph.gz")
setnames(du.dt, c("kilobytes", "file"))
setkey(du.dt, kilobytes)
du.dt[, n.lines := NA_integer_]

for(coverage.i in 4956:4960){
  coverage.bedGraph.gz <- du.dt[coverage.i, file]
  cmd <- paste("zcat", coverage.bedGraph.gz, "|wc -l")
  du.dt[coverage.i, n.lines := as.integer(system(cmd, intern=TRUE))]
}
