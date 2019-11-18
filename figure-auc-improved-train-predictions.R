library(data.table)
library(ggplot2)
auc.dt <- readRDS("auc.improved.train.predictions.rds")

auc.meta <- nc::capture_first_df(auc.dt, weight.algo=list(
  weight=".*",
  "[.]",
  algo=".*"))
auc.wide <- dcast(auc.meta, set.name + fold + weight ~ algo, value.var="auc")

gg <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  geom_abline(intercept=0, slope=1, color="grey")+
  geom_point(aes(
    initial, improved, color=weight),
    data=auc.wide)+
  facet_wrap("set.name")+
  coord_equal()

png("figure-auc-improved-train-predictions.png", 12, 7, units="in", res=100)
print(gg)
dev.off()
