library(ggplot2)
library(scatterplot3d)
library(reshape2)
library(xtable)

library(gdxrrw)
igdx("S:/24.1/")

tc <- rgdx.param("results/results_onec.gdx", "p_trade_creation")
td <- rgdx.param("results/results_onec.gdx", "p_trade_diversion")
colnames(td) <- c("region", "commodity", "scenario", "value")
colnames(tc) <- c("region", "commodity", "scenario", "value")
td$commodity <- NULL
tc$commodity <- NULL

mtd <- melt(td, id=c("region","scenario"))
mtc <- melt(tc, id=c("region","scenario"))
both  <- rbind(dcast(mtd, region~scenario, sum), dcast(mtc, region~scenario, sum))
both$measure  <- c(rep("trade diversion",2), rep("trade creation",2))
both <- both[c(5,1,2,3,4)]
colnames(both) <- c("measure","region","AVE representation", "sigmoid representation", "orth. cond. representation")

print(xtable(both, digits=1),include.rownames = FALSE)

