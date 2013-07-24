# Impact on different ARmington elasticities on trade diversion


library(gdxrrw)
igdx("D:/util/GAMS24.0/")

td <- rgdx.param("SA_results_arm2.gdx","p_trade_diversion_relative_tot")
elas <- rgdx.param("SA_results_arm2.gdx","p_Armington_elas_tot")
tdbilat <- rgdx.param("SA_results_arm2.gdx","p_trade_diversion_bilat_tot")
frate <- rgdx.param("SA_results_arm2.gdx","p_trq_fillrate_tot")


# let's concentrate on one region
elasR1 <- subset(elas, i2=="R1")

# merge the two tables
tdt <- merge(td,elasR1,by.x=c("i","j"),by.y=c("i1","i3"))

#correct header
colnames(tdt) <- c("step", "commodity", "scenario", "td", "region", "arm", "elas")

# create two separate variables 'arm1elas' 'arm2elas'
tdt$arm1elas <- 0
tdt$arm1elas <- tdt$elas * (tdt$arm=="Arm1")


tdt$arm2elas <- 0
tdt$arm2elas <- tdt$elas * (tdt$arm=="ARM2")

mtdt  <- melt(tdt, id=c("step","commodity","scenario","region","arm"))
mtdt <- mtdt[mtdt$variable=="arm1elas" | mtdt$variable=="arm2elas", ]
scens <- dcast(mtdt, step+commodity+scenario+region ~ arm, sum)


mtdt  <- melt(tdt, id=c("step","commodity","scenario","region","arm"))
mtdt <- mtdt[mtdt$variable=="td", ]
diversions <- dcast(mtdt, step+commodity+scenario+region~arm, mean)
diversions <- diversions[, c("step","commodity","scenario","region","ARM2")]
colnames(diversions)[5]  <- "td"

tdclean <- merge(scens,diversions)


library(ggplot2)

# plotting trade diverison vs. arm2 elasticities
p <- ggplot(subset(tdclean,Arm1==2 & commodity=="X1"), aes(x=ARM2,y=td,color=scenario))
p+geom_line()

# plotting trade diverison vs. arm1 elasticities
q <- ggplot(subset(tdclean,ARM2==6 & commodity=="X1"), aes(x=Arm1,y=td,color=scenario))
q+geom_line()



# impact on bilateral trade flows
# show that the trade diversion in the TRQ case is driven by the change in imports of region R2
# the other trade direction keeps stable due to the TRQ (fill rate sticks to 1)
elasR1 <- elasR1[,c(-2)]

colnames(tdbilat) <- c("step", "importer", "exporter", "commodity", "scenario", "td")
scens <- scens[, c(-4)]
tdbilat <- merge(scens,tdbilat)

# changes in imports under TRQ protection
p <- ggplot(subset(tdbilat, commodity=="X1" & Arm1==2 & importer=="R1" & exporter=="R3"), aes(x=ARM2,y=td,color=scenario))
p+geom_line()

# changes in imports with no TRQ protection
p <- ggplot(subset(tdbilat, commodity=="X1" & Arm1==2 & importer=="R2" & exporter=="R3"), aes(x=ARM2,y=td,color=scenario))
p+geom_line()




# impact on TRQ fill rate

colnames(frate) <- c("step","importer","exporter","commodity","scenario","fillrate")
frate  <- merge(scens,frate)


# by the orthogonality conditions the fill rate sticks to exactly one
# (except when the Arm1 elasticity > Arm2 elasticity)
# with the sigmoid representation tiny changes can be observed (small underfill)
# but basically TRQ remains filled in all 'realistic' scenarios
p <- ggplot(subset(frate, commodity=="X1" & Arm1==2), aes(x=ARM2, y=fillrate, color=scenario))
p+geom_line()
p <- ggplot(subset(frate, commodity=="X2" & Arm1==5), aes(x=ARM2, y=fillrate, color=scenario))
p+geom_line()



# impact on trade creation (here the change of imort shares in total demand)

colnames(tcreate) <- c("step","region","commodity","scenario","tc")
tclean <- merge(scens,tcreate)


# in region R1 that has a trq the AVE representation underestimates trade creation
p <- ggplot(subset(tcreate,commodity=="X1" & region=="R1" & Arm1==3), aes(x=ARM2,y=tc,color=scenario))
p+geom_line()

# in region R2 that has no trq the AVE representation overestimates trade creation
p <- ggplot(subset(tcreate,commodity=="X1" & region=="R2" & Arm1==3), aes(x=ARM2,y=tc,color=scenario))
p+geom_line()


# keeping Arm2 fixed...
p <- ggplot(subset(tcreate,commodity=="X1" & region=="R1" & ARM2==6), aes(x=Arm1,y=tc,color=scenario))
p+geom_line()

p <- ggplot(subset(tcreate,commodity=="X1" & region=="R2" & ARM2==6), aes(x=Arm1,y=tc,color=scenario))
p+geom_line()


