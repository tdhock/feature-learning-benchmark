library(data.table)
features.dt <- fread("target.intervals.features.csv")

pre <- "http://hubs.hpc.mcgill.ca/~thocking/PeakSegFPOP-labels/"
i.vec <- 1:nrow(features.dt)
labels.list <- list()
for(i in i.vec){
  prob.dir <- features.dt[i, prob.dir]
  cat(sprintf("%4d / %4d %s\n", i, nrow(features.dt), prob.dir))
  data.dir <- file.path("data", prob.dir)
  dir.create(data.dir, showWarnings=FALSE, recursive=TRUE)
  coverage.bedGraph.gz <- file.path(data.dir, "coverage.bedGraph.gz")
  labels.bed <- file.path(data.dir, "labels.bed")
  if(!file.exists(labels.bed)){
    u <- paste0(pre, prob.dir, "/labels.bed")
    tryCatch({
      download.file(u, labels.bed)
    }, error=function(e){
      cat("NO LABELS!\n")
    })
  }
  labels.dt <- tryCatch({
    fread(labels.bed)
  }, error=function(e){
    NULL
  })
  if(is.null(labels.dt)){
    unlink(coverage.bedGraph.gz)
  }else{
    if(!file.exists(coverage.bedGraph.gz)){
      u <- paste0(pre, prob.dir, "/coverage.bedGraph.gz")
      tryCatch({
        download.file(u, coverage.bedGraph.gz)
      }, error=function(e){
        unlink(coverage.bedGraph.gz)
        stop("download failed")
      })
    }
    ##cov.dt <- fread(paste("zcat", coverage.bedGraph.gz))
  }
  labels.list[[paste(i)]] <- labels.dt
}
labels <- do.call(rbind, labels.list)
