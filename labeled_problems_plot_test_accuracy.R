library(data.table)
library(ggplot2)

error.csv.vec <- Sys.glob(file.path("labeled_problems_pred_error", "*.csv"))

error.list <- list()
for(error.csv in error.csv.vec){
  model.error.dt <- fread(error.csv)
  model <- sub(".csv", "", basename(error.csv))
  error.list[[error.csv]] <- data.table(model, model.error.dt)
}
error.dt <- do.call(rbind, error.list)

error.dt[, accuracy.percent := 100*(labels-errors)/labels]
both.metrics <- melt(error.dt, measure.vars=c("accuracy.percent", "auc"))
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(set.name ~ variable, scales="free")+
  geom_point(aes(
    value, model),
             data=both.metrics)+
  xlab("")

mean.dots <- both.metrics[, data.table(
  mean=mean(value),
  sd=sd(value),
  median=median(value),
  q25=quantile(value, 0.25),
  q75=quantile(value, 0.75)
  ), by=list(model, variable, set.name)]
best.per.set <- mean.dots[, .SD[which.max(mean)], by=list(set.name, variable)]
best.set.auc <- best.per.set[variable=="auc"][order(median)]
mean.dots[, set.fac := factor(set.name, best.set.auc$set.name)]
mean.dots[, rank := rank(median), by=list(set.name, variable)]
rank.means <- mean.dots[variable=="accuracy.percent", data.table(
  mean.rank=mean(rank)
  ), by=list(model)][order(mean.rank)]
mean.dots[, model.fac := factor(model, rank.means$model)]
mean.dots[, set.fac := factor(set.name, best.set.auc$set.name)]
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(set.fac ~ variable, scales="free")+
  geom_segment(aes(
    mean+sd, model.fac,
    xend=mean-sd, yend=model.fac),
               data=mean.dots)+
  geom_point(aes(
    mean, model.fac),
             shape=1,
             data=mean.dots)+
  xlab("")

gg <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(set.fac ~ variable, scales="free", labeller=function(df){
    if("set.fac" %in% names(df)){
      df$set.fac <- gsub("[-_]", "\n", paste(df$set.fac))
    }
    if("variable" %in% names(df)){
      df$variable <- paste(df$variable)
    }
    df
  })+
  geom_segment(aes(
    q25, model.fac,
    xend=q75, yend=model.fac),
               data=mean.dots)+
  geom_point(aes(
    median, model.fac),
             shape=1,
             data=mean.dots)+
  xlab("")+
  ylab("model")
png("labeled_problems_plot_test_accuracy.png", 700, 1300, res=100)
print(gg)
dev.off()

error.dt[, set.fac := factor(set.name, best.set.auc$set.name)]
auc.models <- dcast(error.dt, set.fac + fold ~ model, value.var="auc")
gg <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_wrap("set.fac")+
  geom_abline(slope=1, intercept=0, color="grey")+
  coord_equal()+
  geom_point(aes(
    BestConstant, UpperBoundAUC),
             data=auc.models)
print(gg)

auc.models[order(UpperBoundAUC-BestConstant)]

accuracy.models <- dcast(error.dt, set.fac + fold ~ model, value.var="accuracy.percent")
