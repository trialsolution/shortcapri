library(ggplot2)
library(scatterplot3d)
library(reshape2)
library(xtable)

library(gdxrrw)
igdx("S:/24.1/")


td <- rgdx.param("results/SA_results_Rent.gdx","p_trade_diversion_tot")
r <- rgdx.param("results/SA_results_Rent.gdx", "p_qpr_tot")

colnames(td) <- c("step","region","commodity","scenario","value")
colnames(r) <- c("step","region","commodity","value")
mtd <- merge(td,r, by=c("step","commodity"), all.x=TRUE)
mtd$region.y <- NULL
mtd$commodity <- NULL
colnames(mtd) <- c("step","region","scenario","value","shadowrate")
mtd <- subset(mtd, region=="R1" & shadowrate<13)
mtd$step <- NULL
mtd_melt <- melt(mtd, id=c("region","scenario","shadowrate"))


print(xtable(dcast(mtd_melt, shadowrate~scenario, sum), digits=2,
             caption="Trade diversion in R1"), 
      include.rownames = FALSE)

