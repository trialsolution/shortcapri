library(ggplot2)
library(scatterplot3d)
library(reshape2)
library(xtable)

library(gdxrrw)
igdx("S:/24.1/")

welfare <- rgdx.param("results/results_onec.gdx", "p_welfareRes")
colnames(welfare) <- c("region","item","commodity","scenario","value")
welfare <- subset(welfare, commodity=="X1")

welfare$commodity <- NULL


mw <- melt(welfare, id=c("region","item","scenario"))
wtable <- dcast(mw, region+item~scenario, sum)

# calculate changes relative to baseline
wtable <- transform(wtable, AVE=(SIM_AVE/CAL-1)*100, 
                    sigmoid=(sim_sigm/CAL_sigm-1)*100, orth=(sim_orth/CAL_orth-1)*100)
wtable[sapply(wtable,is.na)] <- NA
wtable[sapply(wtable,is.infinite)] <- NA
wtable <- wtable[, c("region","item","AVE","sigmoid","orth")]
wtable <- wtable[ wtable$region!="R2" , ]


print(xtable(wtable, digits=1,
             caption="Welfare impacts (percentage change relative to baseline"),
      include.rownames = FALSE)

