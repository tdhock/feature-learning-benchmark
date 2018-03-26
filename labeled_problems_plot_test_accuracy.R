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

error.dt[, accuracy.percent := 100*(labels-errors)/errors]
both.metrics <- melt(error.dt, measure.vars=c("accuracy.percent", "auc"))
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(set.name ~ variable, scales="free")+
  geom_point(aes(
    value, model),
             data=both.metrics)

auc.models <- dcast(error.dt, set.name + fold ~ model, value.var="auc")
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_wrap("set.name")+
  geom_abline(slope=1, intercept=0, color="grey")+
  coord_equal()+
  geom_point(aes(
    AIC, BIC),
             data=auc.models)
  
