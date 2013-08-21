#' @title Three dimensional scatterplot with respect to Armington elasticities
#' 
#' 
#' 
#' 
#' @description
#' \code{plot3dres} creates a three dimensional scatterplot from the results of the sensitivity analysis
#' 
#' @param gdxfile gdx file to be loaded
#' @param balanceitem name of the balance item to be graphed
#' @param region the first region
#' @param secondregion name of the second region for those items which have two regional dimensions, e.g. trade flow
#' @param zlab measure of the third axis
#' 
#' @examples
#' plot3dres(gdxfile="results/SA_results_arm2_oR1.gdx",balanceitem="Exports",region="R1")
#' plot3dres(gdxfile="results/SA_results_arm2_oR1.gdx",balanceitem="trade",region="R1",secondregion="R3")


plot3dres <- function(gdxfile,balanceitem,region="R1",secondregion="empty",commodity="X1",
                      relative=FALSE,baselinegdx="empty",
                      zlab="measure"){
  
  p_results_tot  <-  rgdx.param(gdxfile, "p_results_tot")
  
  if (secondregion != "empty") {
  subres <- subset(p_results_tot, i4==balanceitem & i2==region & i3==secondregion)
  } else {
  subres <- subset(p_results_tot, i4==balanceitem & i2==region)
  }
  
  colnames(subres) <- c("step", "region", "dim3", "item", "commodity", 
                        "scenario", "value")
  
  # build a scens table
  elas <- rgdx.param(gdxfile, "p_Armington_elas_tot")
  scens <- rbind(data.frame(elas, scenario="SIM_AVE"), 
                 data.frame(elas, scenario="sim_orth"),  
                 data.frame(elas, scenario="sim_sigm"))
  colnames(scens) <- c("step", "region", "commodity", "elas", "value", "scenario")
  scens$arm1elas <- 0
  scens$arm1elas  <- scens$value * (scens$elas=="Arm1")
  scens$arm2elas <- 0
  scens$arm2elas  <- scens$value * (scens$elas=="ARM2")
  mscens <- melt(scens, id=c("step","region","commodity","elas","scenario"))
  mscens <- mscens[mscens$variable!="value",]
  scens <- dcast(mscens, step+commodity+scenario+region ~ elas, sum)

  
  # calculate baseline values if in 'relative' mode
  if (relative) {
    if (baselinegdx=="empty") { stop("no gdx for the baseline defined") }
    baseval <- rgdx.param(baselinegdx, "p_results")
    colnames(baseval) <- c("region1","region2","item","commodity","scenario","value")
    if (secondregion == "empty") {
      baseval <- subset(baseval, region1==region & item==balanceitem & scenario=="CAL")$value
    } else {
      baseval <- subset(baseval, region1==region & region2==secondregion & item==balanceitem & scenario=="CAL")$value
    }
  }
   
  
  subres <- merge(subres,scens, by=c("step","region","commodity","scenario"),all.y=TRUE)  
  
  subres$pcolor[subres$scenario=="SIM_AVE"] <- "red"
  subres$pcolor[subres$scenario=="sim_orth"] <- "green"
  subres$pcolor[subres$scenario=="sim_sigm"] <- "blue"

  
  if (relative) {
    subres$value <- subres$value / baseval
  }
    
  with(subres, {
  
  
  s3d <- scatterplot3d(Arm1, ARM2, value, color=pcolor, type="h", 
                       lty.hplot=2, pch=19,
                       xlab="Arm1 elasticities", ylab="Arm2 elasticities", 
                       zlab=zlab)
  
  s3d.coords <- s3d$xyz.convert(Arm1, ARM2, value)
  
  legend("topleft", inset=.05,      
         bty="n", #cex=.5,          
         c("AVE representation", "Sigmoid representation", 
           "Orth. cond. representation"), fill=c("red", "blue", "green"))
    })
}

