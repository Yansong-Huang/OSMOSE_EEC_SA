
# Read OSMOSE indicators --------------------------------------------------

sim10p_sp0 = readRDS("paper_results/indicators_results/2.indicators_processed/sim10p_sp0.rds")
sim10p_sp1 = readRDS("paper_results/indicators_results/2.indicators_processed/sim10p_sp1.rds")
sim10p_sp2 = readRDS("paper_results/indicators_results/2.indicators_processed/sim10p_sp2.rds")
sim10p_sp3 = readRDS("paper_results/indicators_results/2.indicators_processed/sim10p_sp3.rds")
sim10p_sp4 = readRDS("paper_results/indicators_results/2.indicators_processed/sim10p_sp4.rds")
sim10p_sp5 = readRDS("paper_results/indicators_results/2.indicators_processed/sim10p_sp5.rds")
sim10p_sp6 = readRDS("paper_results/indicators_results/2.indicators_processed/sim10p_sp6.rds")
sim10p_sp7 = readRDS("paper_results/indicators_results/2.indicators_processed/sim10p_sp7.rds")
sim10p_sp8 = readRDS("paper_results/indicators_results/2.indicators_processed/sim10p_sp8.rds")

sim20p_sp0 = readRDS("paper_results/indicators_results/2.indicators_processed/sim20p_sp0.rds")
sim20p_sp1 = readRDS("paper_results/indicators_results/2.indicators_processed/sim20p_sp1.rds")
sim20p_sp2 = readRDS("paper_results/indicators_results/2.indicators_processed/sim20p_sp2.rds")
sim20p_sp3 = readRDS("paper_results/indicators_results/2.indicators_processed/sim20p_sp3.rds")
sim20p_sp4 = readRDS("paper_results/indicators_results/2.indicators_processed/sim20p_sp4.rds")
sim20p_sp5 = readRDS("paper_results/indicators_results/2.indicators_processed/sim20p_sp5.rds")
sim20p_sp6 = readRDS("paper_results/indicators_results/2.indicators_processed/sim20p_sp6.rds")
sim20p_sp7 = readRDS("paper_results/indicators_results/2.indicators_processed/sim20p_sp7.rds")
sim20p_sp8 = readRDS("paper_results/indicators_results/2.indicators_processed/sim20p_sp8.rds")

sim30p_sp0 = readRDS("paper_results/indicators_results/2.indicators_processed/sim30p_sp0.rds")
sim30p_sp1 = readRDS("paper_results/indicators_results/2.indicators_processed/sim30p_sp1.rds")
sim30p_sp2 = readRDS("paper_results/indicators_results/2.indicators_processed/sim30p_sp2.rds")
sim30p_sp3 = readRDS("paper_results/indicators_results/2.indicators_processed/sim30p_sp3.rds")
sim30p_sp4 = readRDS("paper_results/indicators_results/2.indicators_processed/sim30p_sp4.rds")
sim30p_sp5 = readRDS("paper_results/indicators_results/2.indicators_processed/sim30p_sp5.rds")
sim30p_sp6 = readRDS("paper_results/indicators_results/2.indicators_processed/sim30p_sp6.rds")
sim30p_sp7 = readRDS("paper_results/indicators_results/2.indicators_processed/sim30p_sp7.rds")
sim30p_sp8 = readRDS("paper_results/indicators_results/2.indicators_processed/sim30p_sp8.rds")


# Figure 4A ---------------------------------------------------------------
# Indicator before and after the El Nino event

.meanList = function(x){
  x = apply(x, 2, mean, na.rm = TRUE)
  return(x)
}

# indexation related to El Nino
.filterNino = function(x, beforeNino = TRUE){
  
  if(isTRUE(beforeNino)){temp = c(1:60)} else {temp = c(61:204)}
  x = x[temp,]
  
  return(x)
}


#Scenario beforeNino
biomass_rc_sp0_10p_before = do.call(rbind, lapply(lapply(sim10p_sp0$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp1_10p_before = do.call(rbind, lapply(lapply(sim10p_sp1$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp2_10p_before = do.call(rbind, lapply(lapply(sim10p_sp2$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp3_10p_before = do.call(rbind, lapply(lapply(sim10p_sp3$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp4_10p_before = do.call(rbind, lapply(lapply(sim10p_sp4$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp5_10p_before = do.call(rbind, lapply(lapply(sim10p_sp5$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp6_10p_before = do.call(rbind, lapply(lapply(sim10p_sp6$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp7_10p_before = do.call(rbind, lapply(lapply(sim10p_sp7$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp8_10p_before = do.call(rbind, lapply(lapply(sim10p_sp8$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))

biomass_rc_sp0_20p_before = do.call(rbind, lapply(lapply(sim20p_sp0$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp1_20p_before = do.call(rbind, lapply(lapply(sim20p_sp1$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp2_20p_before = do.call(rbind, lapply(lapply(sim20p_sp2$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp3_20p_before = do.call(rbind, lapply(lapply(sim20p_sp3$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp4_20p_before = do.call(rbind, lapply(lapply(sim20p_sp4$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp5_20p_before = do.call(rbind, lapply(lapply(sim20p_sp5$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp6_20p_before = do.call(rbind, lapply(lapply(sim20p_sp6$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp7_20p_before = do.call(rbind, lapply(lapply(sim20p_sp7$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp8_20p_before = do.call(rbind, lapply(lapply(sim20p_sp8$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))

biomass_rc_sp0_30p_before = do.call(rbind, lapply(lapply(sim30p_sp0$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp1_30p_before = do.call(rbind, lapply(lapply(sim30p_sp1$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp2_30p_before = do.call(rbind, lapply(lapply(sim30p_sp2$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp3_30p_before = do.call(rbind, lapply(lapply(sim30p_sp3$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp4_30p_before = do.call(rbind, lapply(lapply(sim30p_sp4$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp5_30p_before = do.call(rbind, lapply(lapply(sim30p_sp5$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp6_30p_before = do.call(rbind, lapply(lapply(sim30p_sp6$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp7_30p_before = do.call(rbind, lapply(lapply(sim30p_sp7$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))
biomass_rc_sp8_30p_before = do.call(rbind, lapply(lapply(sim30p_sp8$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = TRUE) , FUN = .meanList))


#Scenario afterNino
biomass_rc_sp0_10p_after = do.call(rbind, lapply(lapply(sim10p_sp0$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp1_10p_after = do.call(rbind, lapply(lapply(sim10p_sp1$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp2_10p_after = do.call(rbind, lapply(lapply(sim10p_sp2$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp3_10p_after = do.call(rbind, lapply(lapply(sim10p_sp3$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp4_10p_after = do.call(rbind, lapply(lapply(sim10p_sp4$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp5_10p_after = do.call(rbind, lapply(lapply(sim10p_sp5$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp6_10p_after = do.call(rbind, lapply(lapply(sim10p_sp6$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp7_10p_after = do.call(rbind, lapply(lapply(sim10p_sp7$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp8_10p_after = do.call(rbind, lapply(lapply(sim10p_sp8$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))

biomass_rc_sp0_20p_after = do.call(rbind, lapply(lapply(sim20p_sp0$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp1_20p_after = do.call(rbind, lapply(lapply(sim20p_sp1$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp2_20p_after = do.call(rbind, lapply(lapply(sim20p_sp2$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp3_20p_after = do.call(rbind, lapply(lapply(sim20p_sp3$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp4_20p_after = do.call(rbind, lapply(lapply(sim20p_sp4$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp5_20p_after = do.call(rbind, lapply(lapply(sim20p_sp5$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp6_20p_after = do.call(rbind, lapply(lapply(sim20p_sp6$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp7_20p_after = do.call(rbind, lapply(lapply(sim20p_sp7$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp8_20p_after = do.call(rbind, lapply(lapply(sim20p_sp8$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))

biomass_rc_sp0_30p_after = do.call(rbind, lapply(lapply(sim30p_sp0$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp1_30p_after = do.call(rbind, lapply(lapply(sim30p_sp1$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp2_30p_after = do.call(rbind, lapply(lapply(sim30p_sp2$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp3_30p_after = do.call(rbind, lapply(lapply(sim30p_sp3$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp4_30p_after = do.call(rbind, lapply(lapply(sim30p_sp4$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp5_30p_after = do.call(rbind, lapply(lapply(sim30p_sp5$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp6_30p_after = do.call(rbind, lapply(lapply(sim30p_sp6$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp7_30p_after = do.call(rbind, lapply(lapply(sim30p_sp7$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))
biomass_rc_sp8_30p_after = do.call(rbind, lapply(lapply(sim30p_sp8$relativeChange$biomass_sp, FUN = .filterNino, beforeNino = FALSE) , FUN = .meanList))


figure4A_rc = function(sim1_sp0, sim1_sp1, sim1_sp2, sim1_sp3, sim1_sp4, sim1_sp5, sim1_sp6, sim1_sp7, sim1_sp8,
                       sim2_sp0, sim2_sp1, sim2_sp2, sim2_sp3, sim2_sp4, sim2_sp5, sim2_sp6, sim2_sp7, sim2_sp8,
                       sp, titlePlot, lim = NULL, axis1 = TRUE, axis2 = TRUE, speciesName, ...){
  
  if(is.null(lim)){lim = c(-100,100)} else { lim = lim }
  
  distance = (18:1) + 0.17*c(-0.7,0.7)
  
  boxplot(sim1_sp0[,sp]*100, at = distance[1] , outline = FALSE, axes = FALSE, ylim = lim, col = "turquoise1", xlim = c(0.5,18.5), horizontal = TRUE, width = 1.5)
  boxplot(sim2_sp0[,sp]*100, at = distance[2] , outline = FALSE, axes = FALSE, add = TRUE, col = "sienna1", horizontal = TRUE   , width = 1.5)
  boxplot(sim1_sp1[,sp]*100, at = distance[3] , outline = FALSE, axes = FALSE, add = TRUE, col = "turquoise1", horizontal = TRUE, width = 1.5)
  boxplot(sim2_sp1[,sp]*100, at = distance[4] , outline = FALSE, axes = FALSE, add = TRUE, col = "sienna1", horizontal = TRUE   , width = 1.5)
  boxplot(sim1_sp2[,sp]*100, at = distance[5] , outline = FALSE, axes = FALSE, add = TRUE, col = "turquoise1", horizontal = TRUE, width = 1.5)
  boxplot(sim2_sp2[,sp]*100, at = distance[6] , outline = FALSE, axes = FALSE, add = TRUE, col = "sienna1", horizontal = TRUE   , width = 1.5)
  boxplot(sim1_sp3[,sp]*100, at = distance[7] , outline = FALSE, axes = FALSE, add = TRUE, col = "turquoise1", horizontal = TRUE, width = 1.5)
  boxplot(sim2_sp3[,sp]*100, at = distance[8] , outline = FALSE, axes = FALSE, add = TRUE, col = "sienna1", horizontal = TRUE   , width = 1.5)
  boxplot(sim1_sp4[,sp]*100, at = distance[9] , outline = FALSE, axes = FALSE, add = TRUE, col = "turquoise1", horizontal = TRUE, width = 1.5)
  boxplot(sim2_sp4[,sp]*100, at = distance[10], outline = FALSE, axes = FALSE, add = TRUE, col = "sienna1", horizontal = TRUE   , width = 1.5)
  boxplot(sim1_sp5[,sp]*100, at = distance[11], outline = FALSE, axes = FALSE, add = TRUE, col = "turquoise1", horizontal = TRUE, width = 1.5)
  boxplot(sim2_sp5[,sp]*100, at = distance[12], outline = FALSE, axes = FALSE, add = TRUE, col = "sienna1", horizontal = TRUE   , width = 1.5)
  boxplot(sim1_sp6[,sp]*100, at = distance[13], outline = FALSE, axes = FALSE, add = TRUE, col = "turquoise1", horizontal = TRUE, width = 1.5)
  boxplot(sim2_sp6[,sp]*100, at = distance[14], outline = FALSE, axes = FALSE, add = TRUE, col = "sienna1", horizontal = TRUE   , width = 1.5)
  boxplot(sim1_sp7[,sp]*100, at = distance[15], outline = FALSE, axes = FALSE, add = TRUE, col = "turquoise1", horizontal = TRUE, width = 1.5)
  boxplot(sim2_sp7[,sp]*100, at = distance[16], outline = FALSE, axes = FALSE, add = TRUE, col = "sienna1", horizontal = TRUE   , width = 1.5)
  boxplot(sim1_sp8[,sp]*100, at = distance[17], outline = FALSE, axes = FALSE, add = TRUE, col = "turquoise1", horizontal = TRUE, width = 1.5)
  boxplot(sim2_sp8[,sp]*100, at = distance[18], outline = FALSE, axes = FALSE, add = TRUE, col = "sienna1", horizontal = TRUE   , width = 1.5)
  
  abline(v = 0,  col = "red", lty = 2)
  box(...)
  
  if(isTRUE(axis2)){
    axis(2, at = seq(from = 1.5, by = 2, length.out = 9), labels = speciesName, cex.axis = 0.8, las = 2)
  } else {
    axis(2, at = seq(from = 1.5, by = 2, length.out = 9), labels = FALSE, cex.axis = 0.8)
  }
  
  if(isTRUE(axis1)){
    axis1Default = axTicks(1)
    axis(1, at = axis1Default, labels = paste0(axis1Default, "%"), cex.axis = 0.8)}
  
  mtext(text = titlePlot, side = 3, cex = 1, adj = 0.05, cex.axis = 0.8) 
  
  return(invisible())
}

speciesComplete  = rev(c("ANCHOVY", "HAKE", "SARDINE", "JACK MACKEREL", "CHUB MACKEREL",
                         "MESOPELAGICS", "MUNIDA" , "HUMBOLDT SQUID", "EUPHAUSIIDS"))

speciesAbbre     = rev(c("AN", "HA", "SA", "JM", "CM", "ME", "MU", "HS", "EU"))

coloursBox       = c("black", "blue", "green4", "blueviolet", "coral")



# 10P ---------------------------------------------------------------------

png(filename = "paper_results/plots/4.figure4A_10p.png", width = 1300, height = 1000, pointsize = 16)
par(mfrow = c(3, 3))
par(cex   = 1)
par(mar   = c(2.5,3.5,1,0), oma = c(4,4,1,0.8))
figure4A_rc(sim1_sp0 = biomass_rc_sp0_10p_before, sim1_sp1 = biomass_rc_sp1_10p_before, sim1_sp2 = biomass_rc_sp2_10p_before, sim1_sp3 = biomass_rc_sp3_10p_before, sim1_sp4 = biomass_rc_sp4_10p_before, sim1_sp5 = biomass_rc_sp5_10p_before, sim1_sp6 = biomass_rc_sp6_10p_before, sim1_sp7 = biomass_rc_sp7_10p_before, sim1_sp8 = biomass_rc_sp8_10p_before,
            sim2_sp0 = biomass_rc_sp0_10p_after , sim2_sp1 = biomass_rc_sp1_10p_after , sim2_sp2 = biomass_rc_sp2_10p_after , sim2_sp3 = biomass_rc_sp3_10p_after , sim2_sp4 = biomass_rc_sp4_10p_after , sim2_sp5 = biomass_rc_sp5_10p_after , sim2_sp6 = biomass_rc_sp6_10p_after , sim2_sp7 = biomass_rc_sp7_10p_after , sim2_sp8 = biomass_rc_sp8_10p_after ,
            sp = 1, titlePlot = "UNCERTAINTY ON ANCHOVY"       , lim = c(-100,100), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_10p_before, sim1_sp1 = biomass_rc_sp1_10p_before, sim1_sp2 = biomass_rc_sp2_10p_before, sim1_sp3 = biomass_rc_sp3_10p_before, sim1_sp4 = biomass_rc_sp4_10p_before, sim1_sp5 = biomass_rc_sp5_10p_before, sim1_sp6 = biomass_rc_sp6_10p_before, sim1_sp7 = biomass_rc_sp7_10p_before, sim1_sp8 = biomass_rc_sp8_10p_before,
            sim2_sp0 = biomass_rc_sp0_10p_after , sim2_sp1 = biomass_rc_sp1_10p_after , sim2_sp2 = biomass_rc_sp2_10p_after , sim2_sp3 = biomass_rc_sp3_10p_after , sim2_sp4 = biomass_rc_sp4_10p_after , sim2_sp5 = biomass_rc_sp5_10p_after , sim2_sp6 = biomass_rc_sp6_10p_after , sim2_sp7 = biomass_rc_sp7_10p_after , sim2_sp8 = biomass_rc_sp8_10p_after ,
            sp = 2, titlePlot = "UNCERTAINTY ON HAKE"          , lim = c(-350,350), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_10p_before, sim1_sp1 = biomass_rc_sp1_10p_before, sim1_sp2 = biomass_rc_sp2_10p_before, sim1_sp3 = biomass_rc_sp3_10p_before, sim1_sp4 = biomass_rc_sp4_10p_before, sim1_sp5 = biomass_rc_sp5_10p_before, sim1_sp6 = biomass_rc_sp6_10p_before, sim1_sp7 = biomass_rc_sp7_10p_before, sim1_sp8 = biomass_rc_sp8_10p_before,
            sim2_sp0 = biomass_rc_sp0_10p_after , sim2_sp1 = biomass_rc_sp1_10p_after , sim2_sp2 = biomass_rc_sp2_10p_after , sim2_sp3 = biomass_rc_sp3_10p_after , sim2_sp4 = biomass_rc_sp4_10p_after , sim2_sp5 = biomass_rc_sp5_10p_after , sim2_sp6 = biomass_rc_sp6_10p_after , sim2_sp7 = biomass_rc_sp7_10p_after , sim2_sp8 = biomass_rc_sp8_10p_after ,
            sp = 3, titlePlot = "UNCERTAINTY ON SARDINE"       , lim = c(-2500,2500), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_10p_before, sim1_sp1 = biomass_rc_sp1_10p_before, sim1_sp2 = biomass_rc_sp2_10p_before, sim1_sp3 = biomass_rc_sp3_10p_before, sim1_sp4 = biomass_rc_sp4_10p_before, sim1_sp5 = biomass_rc_sp5_10p_before, sim1_sp6 = biomass_rc_sp6_10p_before, sim1_sp7 = biomass_rc_sp7_10p_before, sim1_sp8 = biomass_rc_sp8_10p_before,
            sim2_sp0 = biomass_rc_sp0_10p_after , sim2_sp1 = biomass_rc_sp1_10p_after , sim2_sp2 = biomass_rc_sp2_10p_after , sim2_sp3 = biomass_rc_sp3_10p_after , sim2_sp4 = biomass_rc_sp4_10p_after , sim2_sp5 = biomass_rc_sp5_10p_after , sim2_sp6 = biomass_rc_sp6_10p_after , sim2_sp7 = biomass_rc_sp7_10p_after , sim2_sp8 = biomass_rc_sp8_10p_after ,
            sp = 4, titlePlot = "UNCERTAINTY ON JACK MACKEREL" , lim = c(-180,180), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_10p_before, sim1_sp1 = biomass_rc_sp1_10p_before, sim1_sp2 = biomass_rc_sp2_10p_before, sim1_sp3 = biomass_rc_sp3_10p_before, sim1_sp4 = biomass_rc_sp4_10p_before, sim1_sp5 = biomass_rc_sp5_10p_before, sim1_sp6 = biomass_rc_sp6_10p_before, sim1_sp7 = biomass_rc_sp7_10p_before, sim1_sp8 = biomass_rc_sp8_10p_before,
            sim2_sp0 = biomass_rc_sp0_10p_after , sim2_sp1 = biomass_rc_sp1_10p_after , sim2_sp2 = biomass_rc_sp2_10p_after , sim2_sp3 = biomass_rc_sp3_10p_after , sim2_sp4 = biomass_rc_sp4_10p_after , sim2_sp5 = biomass_rc_sp5_10p_after , sim2_sp6 = biomass_rc_sp6_10p_after , sim2_sp7 = biomass_rc_sp7_10p_after , sim2_sp8 = biomass_rc_sp8_10p_after ,
            sp = 5, titlePlot = "UNCERTAINTY ON CHUB MACKEREL" , lim = c(-200,200), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_10p_before, sim1_sp1 = biomass_rc_sp1_10p_before, sim1_sp2 = biomass_rc_sp2_10p_before, sim1_sp3 = biomass_rc_sp3_10p_before, sim1_sp4 = biomass_rc_sp4_10p_before, sim1_sp5 = biomass_rc_sp5_10p_before, sim1_sp6 = biomass_rc_sp6_10p_before, sim1_sp7 = biomass_rc_sp7_10p_before, sim1_sp8 = biomass_rc_sp8_10p_before,
            sim2_sp0 = biomass_rc_sp0_10p_after , sim2_sp1 = biomass_rc_sp1_10p_after , sim2_sp2 = biomass_rc_sp2_10p_after , sim2_sp3 = biomass_rc_sp3_10p_after , sim2_sp4 = biomass_rc_sp4_10p_after , sim2_sp5 = biomass_rc_sp5_10p_after , sim2_sp6 = biomass_rc_sp6_10p_after , sim2_sp7 = biomass_rc_sp7_10p_after , sim2_sp8 = biomass_rc_sp8_10p_after ,
            sp = 6, titlePlot = "UNCERTAINTY ON MESOPELAGICS"  , lim = c(-400,400), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_10p_before, sim1_sp1 = biomass_rc_sp1_10p_before, sim1_sp2 = biomass_rc_sp2_10p_before, sim1_sp3 = biomass_rc_sp3_10p_before, sim1_sp4 = biomass_rc_sp4_10p_before, sim1_sp5 = biomass_rc_sp5_10p_before, sim1_sp6 = biomass_rc_sp6_10p_before, sim1_sp7 = biomass_rc_sp7_10p_before, sim1_sp8 = biomass_rc_sp8_10p_before,
            sim2_sp0 = biomass_rc_sp0_10p_after , sim2_sp1 = biomass_rc_sp1_10p_after , sim2_sp2 = biomass_rc_sp2_10p_after , sim2_sp3 = biomass_rc_sp3_10p_after , sim2_sp4 = biomass_rc_sp4_10p_after , sim2_sp5 = biomass_rc_sp5_10p_after , sim2_sp6 = biomass_rc_sp6_10p_after , sim2_sp7 = biomass_rc_sp7_10p_after , sim2_sp8 = biomass_rc_sp8_10p_after ,
            sp = 7, titlePlot = "UNCERTAINTY ON MUNIDA"        , lim = c(-100,100), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_10p_before, sim1_sp1 = biomass_rc_sp1_10p_before, sim1_sp2 = biomass_rc_sp2_10p_before, sim1_sp3 = biomass_rc_sp3_10p_before, sim1_sp4 = biomass_rc_sp4_10p_before, sim1_sp5 = biomass_rc_sp5_10p_before, sim1_sp6 = biomass_rc_sp6_10p_before, sim1_sp7 = biomass_rc_sp7_10p_before, sim1_sp8 = biomass_rc_sp8_10p_before,
            sim2_sp0 = biomass_rc_sp0_10p_after , sim2_sp1 = biomass_rc_sp1_10p_after , sim2_sp2 = biomass_rc_sp2_10p_after , sim2_sp3 = biomass_rc_sp3_10p_after , sim2_sp4 = biomass_rc_sp4_10p_after , sim2_sp5 = biomass_rc_sp5_10p_after , sim2_sp6 = biomass_rc_sp6_10p_after , sim2_sp7 = biomass_rc_sp7_10p_after , sim2_sp8 = biomass_rc_sp8_10p_after ,
            sp = 8, titlePlot = "UNCERTAINTY ON HUMBOLDT SQUID", lim = c(-550,550), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_10p_before, sim1_sp1 = biomass_rc_sp1_10p_before, sim1_sp2 = biomass_rc_sp2_10p_before, sim1_sp3 = biomass_rc_sp3_10p_before, sim1_sp4 = biomass_rc_sp4_10p_before, sim1_sp5 = biomass_rc_sp5_10p_before, sim1_sp6 = biomass_rc_sp6_10p_before, sim1_sp7 = biomass_rc_sp7_10p_before, sim1_sp8 = biomass_rc_sp8_10p_before,
            sim2_sp0 = biomass_rc_sp0_10p_after , sim2_sp1 = biomass_rc_sp1_10p_after , sim2_sp2 = biomass_rc_sp2_10p_after , sim2_sp3 = biomass_rc_sp3_10p_after , sim2_sp4 = biomass_rc_sp4_10p_after , sim2_sp5 = biomass_rc_sp5_10p_after , sim2_sp6 = biomass_rc_sp6_10p_after , sim2_sp7 = biomass_rc_sp7_10p_after , sim2_sp8 = biomass_rc_sp8_10p_after ,
            sp = 9, titlePlot = "UNCERTAINTY ON EUPHAUSIIDS"   , lim = c(-80,80),   col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

mtext(text = "RELATIVE CHANGE IN BIOMASS" ,
      side = 1, cex = 1.3  , line = 1, adj = 0.5, outer = TRUE)

mtext(text = "SCENARIO 10%" ,
      side = 1, cex = 1.3  , line = 2.5, adj = 0.5, outer = TRUE)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", cex = 0.8)

legend("bottomright", legend = c("BEFORE EL NIÑO", "AFTER EL NIÑO"), bty = "n",
       fill = c("turquoise1", "sienna1"), border = "black", cex = 1, xpd = TRUE)

dev.off()


# 20P ---------------------------------------------------------------------

png(filename = "paper_results/plots/4.figure4A_20.png", width = 1300, height = 1000, pointsize = 16)
par(mfrow = c(3, 3))
par(cex   = 1)
par(mar   = c(2.5,3.5,1,0), oma = c(4,4,1,0.8))
figure4A_rc(sim1_sp0 = biomass_rc_sp0_20p_before, sim1_sp1 = biomass_rc_sp1_20p_before, sim1_sp2 = biomass_rc_sp2_20p_before, sim1_sp3 = biomass_rc_sp3_20p_before, sim1_sp4 = biomass_rc_sp4_20p_before, sim1_sp5 = biomass_rc_sp5_20p_before, sim1_sp6 = biomass_rc_sp6_20p_before, sim1_sp7 = biomass_rc_sp7_20p_before, sim1_sp8 = biomass_rc_sp8_20p_before,
            sim2_sp0 = biomass_rc_sp0_20p_after , sim2_sp1 = biomass_rc_sp1_20p_after , sim2_sp2 = biomass_rc_sp2_20p_after , sim2_sp3 = biomass_rc_sp3_20p_after , sim2_sp4 = biomass_rc_sp4_20p_after , sim2_sp5 = biomass_rc_sp5_20p_after , sim2_sp6 = biomass_rc_sp6_20p_after , sim2_sp7 = biomass_rc_sp7_20p_after , sim2_sp8 = biomass_rc_sp8_20p_after ,
            sp = 1, titlePlot = "UNCERTAINTY ON ANCHOVY"       , lim = c(-200,200), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_20p_before, sim1_sp1 = biomass_rc_sp1_20p_before, sim1_sp2 = biomass_rc_sp2_20p_before, sim1_sp3 = biomass_rc_sp3_20p_before, sim1_sp4 = biomass_rc_sp4_20p_before, sim1_sp5 = biomass_rc_sp5_20p_before, sim1_sp6 = biomass_rc_sp6_20p_before, sim1_sp7 = biomass_rc_sp7_20p_before, sim1_sp8 = biomass_rc_sp8_20p_before,
            sim2_sp0 = biomass_rc_sp0_20p_after , sim2_sp1 = biomass_rc_sp1_20p_after , sim2_sp2 = biomass_rc_sp2_20p_after , sim2_sp3 = biomass_rc_sp3_20p_after , sim2_sp4 = biomass_rc_sp4_20p_after , sim2_sp5 = biomass_rc_sp5_20p_after , sim2_sp6 = biomass_rc_sp6_20p_after , sim2_sp7 = biomass_rc_sp7_20p_after , sim2_sp8 = biomass_rc_sp8_20p_after ,
            sp = 2, titlePlot = "UNCERTAINTY ON HAKE"          , lim = c(-1000,1000), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_20p_before, sim1_sp1 = biomass_rc_sp1_20p_before, sim1_sp2 = biomass_rc_sp2_20p_before, sim1_sp3 = biomass_rc_sp3_20p_before, sim1_sp4 = biomass_rc_sp4_20p_before, sim1_sp5 = biomass_rc_sp5_20p_before, sim1_sp6 = biomass_rc_sp6_20p_before, sim1_sp7 = biomass_rc_sp7_20p_before, sim1_sp8 = biomass_rc_sp8_20p_before,
            sim2_sp0 = biomass_rc_sp0_20p_after , sim2_sp1 = biomass_rc_sp1_20p_after , sim2_sp2 = biomass_rc_sp2_20p_after , sim2_sp3 = biomass_rc_sp3_20p_after , sim2_sp4 = biomass_rc_sp4_20p_after , sim2_sp5 = biomass_rc_sp5_20p_after , sim2_sp6 = biomass_rc_sp6_20p_after , sim2_sp7 = biomass_rc_sp7_20p_after , sim2_sp8 = biomass_rc_sp8_20p_after ,
            sp = 3, titlePlot = "UNCERTAINTY ON SARDINE"       , lim = c(-12000,12000), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_20p_before, sim1_sp1 = biomass_rc_sp1_20p_before, sim1_sp2 = biomass_rc_sp2_20p_before, sim1_sp3 = biomass_rc_sp3_20p_before, sim1_sp4 = biomass_rc_sp4_20p_before, sim1_sp5 = biomass_rc_sp5_20p_before, sim1_sp6 = biomass_rc_sp6_20p_before, sim1_sp7 = biomass_rc_sp7_20p_before, sim1_sp8 = biomass_rc_sp8_20p_before,
            sim2_sp0 = biomass_rc_sp0_20p_after , sim2_sp1 = biomass_rc_sp1_20p_after , sim2_sp2 = biomass_rc_sp2_20p_after , sim2_sp3 = biomass_rc_sp3_20p_after , sim2_sp4 = biomass_rc_sp4_20p_after , sim2_sp5 = biomass_rc_sp5_20p_after , sim2_sp6 = biomass_rc_sp6_20p_after , sim2_sp7 = biomass_rc_sp7_20p_after , sim2_sp8 = biomass_rc_sp8_20p_after ,
            sp = 4, titlePlot = "UNCERTAINTY ON JACK MACKEREL" , lim = c(-200,200), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_20p_before, sim1_sp1 = biomass_rc_sp1_20p_before, sim1_sp2 = biomass_rc_sp2_20p_before, sim1_sp3 = biomass_rc_sp3_20p_before, sim1_sp4 = biomass_rc_sp4_20p_before, sim1_sp5 = biomass_rc_sp5_20p_before, sim1_sp6 = biomass_rc_sp6_20p_before, sim1_sp7 = biomass_rc_sp7_20p_before, sim1_sp8 = biomass_rc_sp8_20p_before,
            sim2_sp0 = biomass_rc_sp0_20p_after , sim2_sp1 = biomass_rc_sp1_20p_after , sim2_sp2 = biomass_rc_sp2_20p_after , sim2_sp3 = biomass_rc_sp3_20p_after , sim2_sp4 = biomass_rc_sp4_20p_after , sim2_sp5 = biomass_rc_sp5_20p_after , sim2_sp6 = biomass_rc_sp6_20p_after , sim2_sp7 = biomass_rc_sp7_20p_after , sim2_sp8 = biomass_rc_sp8_20p_after ,
            sp = 5, titlePlot = "UNCERTAINTY ON CHUB MACKEREL" , lim = c(-200,200), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_20p_before, sim1_sp1 = biomass_rc_sp1_20p_before, sim1_sp2 = biomass_rc_sp2_20p_before, sim1_sp3 = biomass_rc_sp3_20p_before, sim1_sp4 = biomass_rc_sp4_20p_before, sim1_sp5 = biomass_rc_sp5_20p_before, sim1_sp6 = biomass_rc_sp6_20p_before, sim1_sp7 = biomass_rc_sp7_20p_before, sim1_sp8 = biomass_rc_sp8_20p_before,
            sim2_sp0 = biomass_rc_sp0_20p_after , sim2_sp1 = biomass_rc_sp1_20p_after , sim2_sp2 = biomass_rc_sp2_20p_after , sim2_sp3 = biomass_rc_sp3_20p_after , sim2_sp4 = biomass_rc_sp4_20p_after , sim2_sp5 = biomass_rc_sp5_20p_after , sim2_sp6 = biomass_rc_sp6_20p_after , sim2_sp7 = biomass_rc_sp7_20p_after , sim2_sp8 = biomass_rc_sp8_20p_after ,
            sp = 6, titlePlot = "UNCERTAINTY ON MESOPELAGICS"  , lim = c(-200,200), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_20p_before, sim1_sp1 = biomass_rc_sp1_20p_before, sim1_sp2 = biomass_rc_sp2_20p_before, sim1_sp3 = biomass_rc_sp3_20p_before, sim1_sp4 = biomass_rc_sp4_20p_before, sim1_sp5 = biomass_rc_sp5_20p_before, sim1_sp6 = biomass_rc_sp6_20p_before, sim1_sp7 = biomass_rc_sp7_20p_before, sim1_sp8 = biomass_rc_sp8_20p_before,
            sim2_sp0 = biomass_rc_sp0_20p_after , sim2_sp1 = biomass_rc_sp1_20p_after , sim2_sp2 = biomass_rc_sp2_20p_after , sim2_sp3 = biomass_rc_sp3_20p_after , sim2_sp4 = biomass_rc_sp4_20p_after , sim2_sp5 = biomass_rc_sp5_20p_after , sim2_sp6 = biomass_rc_sp6_20p_after , sim2_sp7 = biomass_rc_sp7_20p_after , sim2_sp8 = biomass_rc_sp8_20p_after ,
            sp = 7, titlePlot = "UNCERTAINTY ON MUNIDA"        , lim = c(-100,100), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_20p_before, sim1_sp1 = biomass_rc_sp1_20p_before, sim1_sp2 = biomass_rc_sp2_20p_before, sim1_sp3 = biomass_rc_sp3_20p_before, sim1_sp4 = biomass_rc_sp4_20p_before, sim1_sp5 = biomass_rc_sp5_20p_before, sim1_sp6 = biomass_rc_sp6_20p_before, sim1_sp7 = biomass_rc_sp7_20p_before, sim1_sp8 = biomass_rc_sp8_20p_before,
            sim2_sp0 = biomass_rc_sp0_20p_after , sim2_sp1 = biomass_rc_sp1_20p_after , sim2_sp2 = biomass_rc_sp2_20p_after , sim2_sp3 = biomass_rc_sp3_20p_after , sim2_sp4 = biomass_rc_sp4_20p_after , sim2_sp5 = biomass_rc_sp5_20p_after , sim2_sp6 = biomass_rc_sp6_20p_after , sim2_sp7 = biomass_rc_sp7_20p_after , sim2_sp8 = biomass_rc_sp8_20p_after ,
            sp = 8, titlePlot = "UNCERTAINTY ON HUMBOLDT SQUID", lim = c(-1000,1000), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_20p_before, sim1_sp1 = biomass_rc_sp1_20p_before, sim1_sp2 = biomass_rc_sp2_20p_before, sim1_sp3 = biomass_rc_sp3_20p_before, sim1_sp4 = biomass_rc_sp4_20p_before, sim1_sp5 = biomass_rc_sp5_20p_before, sim1_sp6 = biomass_rc_sp6_20p_before, sim1_sp7 = biomass_rc_sp7_20p_before, sim1_sp8 = biomass_rc_sp8_20p_before,
            sim2_sp0 = biomass_rc_sp0_20p_after , sim2_sp1 = biomass_rc_sp1_20p_after , sim2_sp2 = biomass_rc_sp2_20p_after , sim2_sp3 = biomass_rc_sp3_20p_after , sim2_sp4 = biomass_rc_sp4_20p_after , sim2_sp5 = biomass_rc_sp5_20p_after , sim2_sp6 = biomass_rc_sp6_20p_after , sim2_sp7 = biomass_rc_sp7_20p_after , sim2_sp8 = biomass_rc_sp8_20p_after ,
            sp = 9, titlePlot = "UNCERTAINTY ON EUPHAUSIIDS"   , lim = c(-70,70),   col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

mtext(text = "RELATIVE CHANGE IN BIOMASS" ,
      side = 1, cex = 1.3  , line = 1, adj = 0.5, outer = TRUE)

mtext(text = "SCENARIO 20%" ,
      side = 1, cex = 1.3  , line = 2.5, adj = 0.5, outer = TRUE)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", cex = 0.8)

legend("bottomright", legend = c("BEFORE EL NIÑO", "AFTER EL NIÑO"), bty = "n",
       fill = c("turquoise1", "sienna1"), border = "black", cex = 1, xpd = TRUE)

dev.off()


# 30P ---------------------------------------------------------------------

png(filename = "paper_results/plots/4.figure4A_30.png", width = 1300, height = 1000, pointsize = 16)
par(mfrow = c(3, 3))
par(cex   = 1)
par(mar   = c(2.5,3.5,1,0), oma = c(4,4,1,0.8))
figure4A_rc(sim1_sp0 = biomass_rc_sp0_30p_before, sim1_sp1 = biomass_rc_sp1_30p_before, sim1_sp2 = biomass_rc_sp2_30p_before, sim1_sp3 = biomass_rc_sp3_30p_before, sim1_sp4 = biomass_rc_sp4_30p_before, sim1_sp5 = biomass_rc_sp5_30p_before, sim1_sp6 = biomass_rc_sp6_30p_before, sim1_sp7 = biomass_rc_sp7_30p_before, sim1_sp8 = biomass_rc_sp8_30p_before,
            sim2_sp0 = biomass_rc_sp0_30p_after , sim2_sp1 = biomass_rc_sp1_30p_after , sim2_sp2 = biomass_rc_sp2_30p_after , sim2_sp3 = biomass_rc_sp3_30p_after , sim2_sp4 = biomass_rc_sp4_30p_after , sim2_sp5 = biomass_rc_sp5_30p_after , sim2_sp6 = biomass_rc_sp6_30p_after , sim2_sp7 = biomass_rc_sp7_30p_after , sim2_sp8 = biomass_rc_sp8_30p_after ,
            sp = 1, titlePlot = "UNCERTAINTY ON ANCHOVY"       , lim = c(-300,300), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_30p_before, sim1_sp1 = biomass_rc_sp1_30p_before, sim1_sp2 = biomass_rc_sp2_30p_before, sim1_sp3 = biomass_rc_sp3_30p_before, sim1_sp4 = biomass_rc_sp4_30p_before, sim1_sp5 = biomass_rc_sp5_30p_before, sim1_sp6 = biomass_rc_sp6_30p_before, sim1_sp7 = biomass_rc_sp7_30p_before, sim1_sp8 = biomass_rc_sp8_30p_before,
            sim2_sp0 = biomass_rc_sp0_30p_after , sim2_sp1 = biomass_rc_sp1_30p_after , sim2_sp2 = biomass_rc_sp2_30p_after , sim2_sp3 = biomass_rc_sp3_30p_after , sim2_sp4 = biomass_rc_sp4_30p_after , sim2_sp5 = biomass_rc_sp5_30p_after , sim2_sp6 = biomass_rc_sp6_30p_after , sim2_sp7 = biomass_rc_sp7_30p_after , sim2_sp8 = biomass_rc_sp8_30p_after ,
            sp = 2, titlePlot = "UNCERTAINTY ON HAKE"          , lim = c(-1000,1000), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_30p_before, sim1_sp1 = biomass_rc_sp1_30p_before, sim1_sp2 = biomass_rc_sp2_30p_before, sim1_sp3 = biomass_rc_sp3_30p_before, sim1_sp4 = biomass_rc_sp4_30p_before, sim1_sp5 = biomass_rc_sp5_30p_before, sim1_sp6 = biomass_rc_sp6_30p_before, sim1_sp7 = biomass_rc_sp7_30p_before, sim1_sp8 = biomass_rc_sp8_30p_before,
            sim2_sp0 = biomass_rc_sp0_30p_after , sim2_sp1 = biomass_rc_sp1_30p_after , sim2_sp2 = biomass_rc_sp2_30p_after , sim2_sp3 = biomass_rc_sp3_30p_after , sim2_sp4 = biomass_rc_sp4_30p_after , sim2_sp5 = biomass_rc_sp5_30p_after , sim2_sp6 = biomass_rc_sp6_30p_after , sim2_sp7 = biomass_rc_sp7_30p_after , sim2_sp8 = biomass_rc_sp8_30p_after ,
            sp = 3, titlePlot = "UNCERTAINTY ON SARDINE"       , lim = c(-22000,22000), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_30p_before, sim1_sp1 = biomass_rc_sp1_30p_before, sim1_sp2 = biomass_rc_sp2_30p_before, sim1_sp3 = biomass_rc_sp3_30p_before, sim1_sp4 = biomass_rc_sp4_30p_before, sim1_sp5 = biomass_rc_sp5_30p_before, sim1_sp6 = biomass_rc_sp6_30p_before, sim1_sp7 = biomass_rc_sp7_30p_before, sim1_sp8 = biomass_rc_sp8_30p_before,
            sim2_sp0 = biomass_rc_sp0_30p_after , sim2_sp1 = biomass_rc_sp1_30p_after , sim2_sp2 = biomass_rc_sp2_30p_after , sim2_sp3 = biomass_rc_sp3_30p_after , sim2_sp4 = biomass_rc_sp4_30p_after , sim2_sp5 = biomass_rc_sp5_30p_after , sim2_sp6 = biomass_rc_sp6_30p_after , sim2_sp7 = biomass_rc_sp7_30p_after , sim2_sp8 = biomass_rc_sp8_30p_after ,
            sp = 4, titlePlot = "UNCERTAINTY ON JACK MACKEREL" , lim = c(-250,250), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_30p_before, sim1_sp1 = biomass_rc_sp1_30p_before, sim1_sp2 = biomass_rc_sp2_30p_before, sim1_sp3 = biomass_rc_sp3_30p_before, sim1_sp4 = biomass_rc_sp4_30p_before, sim1_sp5 = biomass_rc_sp5_30p_before, sim1_sp6 = biomass_rc_sp6_30p_before, sim1_sp7 = biomass_rc_sp7_30p_before, sim1_sp8 = biomass_rc_sp8_30p_before,
            sim2_sp0 = biomass_rc_sp0_30p_after , sim2_sp1 = biomass_rc_sp1_30p_after , sim2_sp2 = biomass_rc_sp2_30p_after , sim2_sp3 = biomass_rc_sp3_30p_after , sim2_sp4 = biomass_rc_sp4_30p_after , sim2_sp5 = biomass_rc_sp5_30p_after , sim2_sp6 = biomass_rc_sp6_30p_after , sim2_sp7 = biomass_rc_sp7_30p_after , sim2_sp8 = biomass_rc_sp8_30p_after ,
            sp = 5, titlePlot = "UNCERTAINTY ON CHUB MACKEREL" , lim = c(-250,250), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_30p_before, sim1_sp1 = biomass_rc_sp1_30p_before, sim1_sp2 = biomass_rc_sp2_30p_before, sim1_sp3 = biomass_rc_sp3_30p_before, sim1_sp4 = biomass_rc_sp4_30p_before, sim1_sp5 = biomass_rc_sp5_30p_before, sim1_sp6 = biomass_rc_sp6_30p_before, sim1_sp7 = biomass_rc_sp7_30p_before, sim1_sp8 = biomass_rc_sp8_30p_before,
            sim2_sp0 = biomass_rc_sp0_30p_after , sim2_sp1 = biomass_rc_sp1_30p_after , sim2_sp2 = biomass_rc_sp2_30p_after , sim2_sp3 = biomass_rc_sp3_30p_after , sim2_sp4 = biomass_rc_sp4_30p_after , sim2_sp5 = biomass_rc_sp5_30p_after , sim2_sp6 = biomass_rc_sp6_30p_after , sim2_sp7 = biomass_rc_sp7_30p_after , sim2_sp8 = biomass_rc_sp8_30p_after ,
            sp = 6, titlePlot = "UNCERTAINTY ON MESOPELAGICS"  , lim = c(-400,400), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_30p_before, sim1_sp1 = biomass_rc_sp1_30p_before, sim1_sp2 = biomass_rc_sp2_30p_before, sim1_sp3 = biomass_rc_sp3_30p_before, sim1_sp4 = biomass_rc_sp4_30p_before, sim1_sp5 = biomass_rc_sp5_30p_before, sim1_sp6 = biomass_rc_sp6_30p_before, sim1_sp7 = biomass_rc_sp7_30p_before, sim1_sp8 = biomass_rc_sp8_30p_before,
            sim2_sp0 = biomass_rc_sp0_30p_after , sim2_sp1 = biomass_rc_sp1_30p_after , sim2_sp2 = biomass_rc_sp2_30p_after , sim2_sp3 = biomass_rc_sp3_30p_after , sim2_sp4 = biomass_rc_sp4_30p_after , sim2_sp5 = biomass_rc_sp5_30p_after , sim2_sp6 = biomass_rc_sp6_30p_after , sim2_sp7 = biomass_rc_sp7_30p_after , sim2_sp8 = biomass_rc_sp8_30p_after ,
            sp = 7, titlePlot = "UNCERTAINTY ON MUNIDA"        , lim = c(-200,200), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_30p_before, sim1_sp1 = biomass_rc_sp1_30p_before, sim1_sp2 = biomass_rc_sp2_30p_before, sim1_sp3 = biomass_rc_sp3_30p_before, sim1_sp4 = biomass_rc_sp4_30p_before, sim1_sp5 = biomass_rc_sp5_30p_before, sim1_sp6 = biomass_rc_sp6_30p_before, sim1_sp7 = biomass_rc_sp7_30p_before, sim1_sp8 = biomass_rc_sp8_30p_before,
            sim2_sp0 = biomass_rc_sp0_30p_after , sim2_sp1 = biomass_rc_sp1_30p_after , sim2_sp2 = biomass_rc_sp2_30p_after , sim2_sp3 = biomass_rc_sp3_30p_after , sim2_sp4 = biomass_rc_sp4_30p_after , sim2_sp5 = biomass_rc_sp5_30p_after , sim2_sp6 = biomass_rc_sp6_30p_after , sim2_sp7 = biomass_rc_sp7_30p_after , sim2_sp8 = biomass_rc_sp8_30p_after ,
            sp = 8, titlePlot = "UNCERTAINTY ON HUMBOLDT SQUID", lim = c(-2500,2500), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4A_rc(sim1_sp0 = biomass_rc_sp0_30p_before, sim1_sp1 = biomass_rc_sp1_30p_before, sim1_sp2 = biomass_rc_sp2_30p_before, sim1_sp3 = biomass_rc_sp3_30p_before, sim1_sp4 = biomass_rc_sp4_30p_before, sim1_sp5 = biomass_rc_sp5_30p_before, sim1_sp6 = biomass_rc_sp6_30p_before, sim1_sp7 = biomass_rc_sp7_30p_before, sim1_sp8 = biomass_rc_sp8_30p_before,
            sim2_sp0 = biomass_rc_sp0_30p_after , sim2_sp1 = biomass_rc_sp1_30p_after , sim2_sp2 = biomass_rc_sp2_30p_after , sim2_sp3 = biomass_rc_sp3_30p_after , sim2_sp4 = biomass_rc_sp4_30p_after , sim2_sp5 = biomass_rc_sp5_30p_after , sim2_sp6 = biomass_rc_sp6_30p_after , sim2_sp7 = biomass_rc_sp7_30p_after , sim2_sp8 = biomass_rc_sp8_30p_after ,
            sp = 9, titlePlot = "UNCERTAINTY ON EUPHAUSIIDS"   , lim = c(-120,120),   col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

mtext(text = "RELATIVE CHANGE IN BIOMASS" ,
      side = 1, cex = 1.3  , line = 1, adj = 0.5, outer = TRUE)

mtext(text = "SCENARIO 30%" ,
      side = 1, cex = 1.3  , line = 2.5, adj = 0.5, outer = TRUE)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", cex = 0.8)

legend("bottomright", legend = c("BEFORE EL NIÑO", "AFTER EL NIÑO"), bty = "n",
       fill = c("turquoise1", "sienna1"), border = "black", cex = 1, xpd = TRUE)

dev.off()


# Figure 4B ---------------------------------------------------------------

figure4B_rc = function(sim_sp0, sim_sp1, sim_sp2, sim_sp3, sim_sp4, sim_sp5, sim_sp6, sim_sp7, sim_sp8,
                       ind, var, titlePlot, lim = NULL, axis1 = TRUE, axis2 = TRUE, cex.point = 1, speciesName, ...){
  
  tempBefore = c(1:60)
  tempAfter  = c(61:204)
  
  #before
  toPlot_sp0_before = apply(sim_sp0[[ind]][[var]][tempBefore, ], MARGIN = 2, mean, na.rm = TRUE)
  toPlot_sp1_before = apply(sim_sp1[[ind]][[var]][tempBefore, ], MARGIN = 2, mean, na.rm = TRUE)
  toPlot_sp2_before = apply(sim_sp2[[ind]][[var]][tempBefore, ], MARGIN = 2, mean, na.rm = TRUE)
  toPlot_sp3_before = apply(sim_sp3[[ind]][[var]][tempBefore, ], MARGIN = 2, mean, na.rm = TRUE)
  toPlot_sp4_before = apply(sim_sp4[[ind]][[var]][tempBefore, ], MARGIN = 2, mean, na.rm = TRUE)
  toPlot_sp5_before = apply(sim_sp5[[ind]][[var]][tempBefore, ], MARGIN = 2, mean, na.rm = TRUE)
  toPlot_sp6_before = apply(sim_sp6[[ind]][[var]][tempBefore, ], MARGIN = 2, mean, na.rm = TRUE)
  toPlot_sp7_before = apply(sim_sp7[[ind]][[var]][tempBefore, ], MARGIN = 2, mean, na.rm = TRUE)
  toPlot_sp8_before = apply(sim_sp8[[ind]][[var]][tempBefore, ], MARGIN = 2, mean, na.rm = TRUE)
  
  #after
  toPlot_sp0_after = apply(sim_sp0[[ind]][[var]][tempAfter, ], MARGIN = 2, mean, na.rm = TRUE)
  toPlot_sp1_after = apply(sim_sp1[[ind]][[var]][tempAfter, ], MARGIN = 2, mean, na.rm = TRUE)
  toPlot_sp2_after = apply(sim_sp2[[ind]][[var]][tempAfter, ], MARGIN = 2, mean, na.rm = TRUE)
  toPlot_sp3_after = apply(sim_sp3[[ind]][[var]][tempAfter, ], MARGIN = 2, mean, na.rm = TRUE)
  toPlot_sp4_after = apply(sim_sp4[[ind]][[var]][tempAfter, ], MARGIN = 2, mean, na.rm = TRUE)
  toPlot_sp5_after = apply(sim_sp5[[ind]][[var]][tempAfter, ], MARGIN = 2, mean, na.rm = TRUE)
  toPlot_sp6_after = apply(sim_sp6[[ind]][[var]][tempAfter, ], MARGIN = 2, mean, na.rm = TRUE)
  toPlot_sp7_after = apply(sim_sp7[[ind]][[var]][tempAfter, ], MARGIN = 2, mean, na.rm = TRUE)
  toPlot_sp8_after = apply(sim_sp8[[ind]][[var]][tempAfter, ], MARGIN = 2, mean, na.rm = TRUE)
  
  
  if(is.null(lim)){lim = c(-100,100)} else { lim = lim }
  
  distance = (18:1) + 0.17*c(-0.7,0.7)
  
  boxplot(toPlot_sp0_before*100, at = distance[1] , outline = FALSE, axes = FALSE, ylim = lim, col = "turquoise1", xlim = c(0.5,18.5), horizontal = TRUE, width = 1.5)
  boxplot(toPlot_sp0_after*100 , at = distance[2] , outline = FALSE, axes = FALSE, add = TRUE, col = "sienna1", horizontal = TRUE   , width = 1.5)
  boxplot(toPlot_sp1_before*100, at = distance[3] , outline = FALSE, axes = FALSE, add = TRUE, col = "turquoise1", horizontal = TRUE, width = 1.5)
  boxplot(toPlot_sp1_after*100 , at = distance[4] , outline = FALSE, axes = FALSE, add = TRUE, col = "sienna1", horizontal = TRUE   , width = 1.5)
  boxplot(toPlot_sp2_before*100, at = distance[5] , outline = FALSE, axes = FALSE, add = TRUE, col = "turquoise1", horizontal = TRUE, width = 1.5)
  boxplot(toPlot_sp2_after*100 , at = distance[6] , outline = FALSE, axes = FALSE, add = TRUE, col = "sienna1", horizontal = TRUE   , width = 1.5)
  boxplot(toPlot_sp3_before*100, at = distance[7] , outline = FALSE, axes = FALSE, add = TRUE, col = "turquoise1", horizontal = TRUE, width = 1.5)
  boxplot(toPlot_sp3_after*100 , at = distance[8] , outline = FALSE, axes = FALSE, add = TRUE, col = "sienna1", horizontal = TRUE   , width = 1.5)
  boxplot(toPlot_sp4_before*100, at = distance[9] , outline = FALSE, axes = FALSE, add = TRUE, col = "turquoise1", horizontal = TRUE, width = 1.5)
  boxplot(toPlot_sp4_after*100 , at = distance[10], outline = FALSE, axes = FALSE, add = TRUE, col = "sienna1", horizontal = TRUE   , width = 1.5)
  boxplot(toPlot_sp5_before*100, at = distance[11], outline = FALSE, axes = FALSE, add = TRUE, col = "turquoise1", horizontal = TRUE, width = 1.5)
  boxplot(toPlot_sp5_after*100 , at = distance[12], outline = FALSE, axes = FALSE, add = TRUE, col = "sienna1", horizontal = TRUE   , width = 1.5)
  boxplot(toPlot_sp6_before*100, at = distance[13], outline = FALSE, axes = FALSE, add = TRUE, col = "turquoise1", horizontal = TRUE, width = 1.5)
  boxplot(toPlot_sp6_after*100 , at = distance[14], outline = FALSE, axes = FALSE, add = TRUE, col = "sienna1", horizontal = TRUE   , width = 1.5)
  boxplot(toPlot_sp7_before*100, at = distance[15], outline = FALSE, axes = FALSE, add = TRUE, col = "turquoise1", horizontal = TRUE, width = 1.5)
  boxplot(toPlot_sp7_after*100 , at = distance[16], outline = FALSE, axes = FALSE, add = TRUE, col = "sienna1", horizontal = TRUE   , width = 1.5)
  boxplot(toPlot_sp8_before*100, at = distance[17], outline = FALSE, axes = FALSE, add = TRUE, col = "turquoise1", horizontal = TRUE, width = 1.5)
  boxplot(toPlot_sp8_after*100 , at = distance[18], outline = FALSE, axes = FALSE, add = TRUE, col = "sienna1", horizontal = TRUE   , width = 1.5)
  
  abline(v = 0,  col = "red", lty = 2)
  box(...)
  
  if(isTRUE(axis2)){
    axis(2, at = seq(from = 1.5, by = 2, length.out = 9), labels = speciesName, cex.axis = 0.7, las = 2)
  } else {
    axis(2, at = seq(from = 1.5, by = 2, length.out = 9), labels = FALSE)
  }
  
  if(isTRUE(axis1)){
    axis1Default = axTicks(1)
    axis(1, at = axis1Default, labels = paste0(axis1Default, "%"), cex.axis = 0.85)} 
  
  mtext(text = titlePlot, side = 3, cex = 0.7, adj = 0.05)
  
  return(invisible())
  
}

speciesComplete  = rev(c("ANCHOVY", "HAKE", "SARDINE", "JACK MACKEREL", "CHUB MACKEREL",
                         "MESOPELAGICS", "MUNIDA" , "HUMBOLDT SQUID", "EUPHAUSIIDS"))

speciesAbbre     = rev(c("AN", "HA", "SA", "JM", "CM", "ME", "MU", "HS", "EU"))

coloursBox  = c("black", "blue", "green4", "blueviolet", "coral")


# 10P ---------------------------------------------------------------------

png(filename = "paper_results/plots/4.figure4B_10p.png", width = 1300, height = 1400, pointsize = 24)
par(mfrow = c(3, 2))
par(cex   = 1)
par(mar   = c(2.5,2.5,1,0), oma = c(3,4,1,0.5))

figure4B_rc(sim_sp0 = sim10p_sp0, sim_sp1 = sim10p_sp1, sim_sp2 = sim10p_sp2, sim_sp3 = sim10p_sp3,
            sim_sp4 = sim10p_sp4, sim_sp5 = sim10p_sp5, sim_sp6 = sim10p_sp6, sim_sp7 = sim10p_sp7, sim_sp8 = sim10p_sp8,
            ind = "relativeChange", var = "mti", titlePlot = "UNCERTAINTY ON MARINE TROPHIC INDEX" ,
            lim = c(-4, 4), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure4B_rc(sim_sp0 = sim10p_sp0, sim_sp1 = sim10p_sp1, sim_sp2 = sim10p_sp2, sim_sp3 = sim10p_sp3,
            sim_sp4 = sim10p_sp4, sim_sp5 = sim10p_sp5, sim_sp6 = sim10p_sp6, sim_sp7 = sim10p_sp7, sim_sp8 = sim10p_sp8,
            ind = "relativeChange", var = "meanTL", titlePlot = "UNCERTAINTY ON MEAN TROPHIC LEVEL" ,
            lim = c(-10, 10), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4B_rc(sim_sp0 = sim10p_sp0, sim_sp1 = sim10p_sp1, sim_sp2 = sim10p_sp2, sim_sp3 = sim10p_sp3,
            sim_sp4 = sim10p_sp4, sim_sp5 = sim10p_sp5, sim_sp6 = sim10p_sp6, sim_sp7 = sim10p_sp7, sim_sp8 = sim10p_sp8,
            ind = "relativeChange", var = "meanLength", titlePlot = "UNCERTAINTY ON MEAN LENGTH" ,
            lim = c(-30, 30), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure4B_rc(sim_sp0 = sim10p_sp0, sim_sp1 = sim10p_sp1, sim_sp2 = sim10p_sp2, sim_sp3 = sim10p_sp3,
            sim_sp4 = sim10p_sp4, sim_sp5 = sim10p_sp5, sim_sp6 = sim10p_sp6, sim_sp7 = sim10p_sp7, sim_sp8 = sim10p_sp8,
            ind = "relativeChange", var = "meanLifespan", titlePlot = "UNCERTAINTY ON MEAN LIFESPAN" ,
            lim = c(-38, 38), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4B_rc(sim_sp0 = sim10p_sp0, sim_sp1 = sim10p_sp1, sim_sp2 = sim10p_sp2, sim_sp3 = sim10p_sp3,
            sim_sp4 = sim10p_sp4, sim_sp5 = sim10p_sp5, sim_sp6 = sim10p_sp6, sim_sp7 = sim10p_sp7, sim_sp8 = sim10p_sp8,
            ind = "relativeChange", var = "biomassOverYield", titlePlot = "UNCERTAINTY ON BIOMASS OVER YIELD" ,
            lim = c(-300, 300), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure4B_rc(sim_sp0 = sim10p_sp0, sim_sp1 = sim10p_sp1, sim_sp2 = sim10p_sp2, sim_sp3 = sim10p_sp3,
            sim_sp4 = sim10p_sp4, sim_sp5 = sim10p_sp5, sim_sp6 = sim10p_sp6, sim_sp7 = sim10p_sp7, sim_sp8 = sim10p_sp8,
            ind = "relativeChange", var = "lfi40", titlePlot = "UNCERTAINTY ON LFI40" ,
            lim = c(-300, 300), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

mtext(text = "RELATIVE CHANGE IN SCENARIO 10%" , side = 1, cex = 1.5  , line = 1.2, adj = 0.5, outer = TRUE)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", cex = 0.8)

legend("bottomright", legend = c("BEFORE EL NIÑO", "AFTER EL NIÑO"), bty = "n",
       fill = c("turquoise1", "sienna1"), border = "black", cex = 0.8, xpd = TRUE)

dev.off()

# 20P ---------------------------------------------------------------------

png(filename = "paper_results/plots/4.figure4B_20p.png", width = 1300, height = 1400, pointsize = 24)
par(mfrow = c(3, 2))
par(cex   = 1)
par(mar   = c(2.5,2.5,1,0.5), oma = c(3,4,1,0.5))

figure4B_rc(sim_sp0 = sim20p_sp0, sim_sp1 = sim20p_sp1, sim_sp2 = sim20p_sp2, sim_sp3 = sim20p_sp3,
            sim_sp4 = sim20p_sp4, sim_sp5 = sim20p_sp5, sim_sp6 = sim20p_sp6, sim_sp7 = sim20p_sp7, sim_sp8 = sim20p_sp8,
            ind = "relativeChange", var = "mti", titlePlot = "UNCERTAINTY ON MARINE TROPHIC INDEX" ,
            lim = c(-4, 4), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure4B_rc(sim_sp0 = sim20p_sp0, sim_sp1 = sim20p_sp1, sim_sp2 = sim20p_sp2, sim_sp3 = sim20p_sp3,
            sim_sp4 = sim20p_sp4, sim_sp5 = sim20p_sp5, sim_sp6 = sim20p_sp6, sim_sp7 = sim20p_sp7, sim_sp8 = sim20p_sp8,
            ind = "relativeChange", var = "meanTL", titlePlot = "UNCERTAINTY ON MEAN TROPHIC LEVEL" ,
            lim = c(-10, 10), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4B_rc(sim_sp0 = sim20p_sp0, sim_sp1 = sim20p_sp1, sim_sp2 = sim20p_sp2, sim_sp3 = sim20p_sp3,
            sim_sp4 = sim20p_sp4, sim_sp5 = sim20p_sp5, sim_sp6 = sim20p_sp6, sim_sp7 = sim20p_sp7, sim_sp8 = sim20p_sp8,
            ind = "relativeChange", var = "meanLength", titlePlot = "UNCERTAINTY ON MEAN LENGTH" ,
            lim = c(-30, 30), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure4B_rc(sim_sp0 = sim20p_sp0, sim_sp1 = sim20p_sp1, sim_sp2 = sim20p_sp2, sim_sp3 = sim20p_sp3,
            sim_sp4 = sim20p_sp4, sim_sp5 = sim20p_sp5, sim_sp6 = sim20p_sp6, sim_sp7 = sim20p_sp7, sim_sp8 = sim20p_sp8,
            ind = "relativeChange", var = "meanLifespan", titlePlot = "UNCERTAINTY ON MEAN LIFESPAN" ,
            lim = c(-38, 38), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4B_rc(sim_sp0 = sim20p_sp0, sim_sp1 = sim20p_sp1, sim_sp2 = sim20p_sp2, sim_sp3 = sim20p_sp3,
            sim_sp4 = sim20p_sp4, sim_sp5 = sim20p_sp5, sim_sp6 = sim20p_sp6, sim_sp7 = sim20p_sp7, sim_sp8 = sim20p_sp8,
            ind = "relativeChange", var = "biomassOverYield", titlePlot = "UNCERTAINTY ON BIOMASS OVER YIELD" ,
            lim = c(-320, 320), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure4B_rc(sim_sp0 = sim20p_sp0, sim_sp1 = sim20p_sp1, sim_sp2 = sim20p_sp2, sim_sp3 = sim20p_sp3,
            sim_sp4 = sim20p_sp4, sim_sp5 = sim20p_sp5, sim_sp6 = sim20p_sp6, sim_sp7 = sim20p_sp7, sim_sp8 = sim20p_sp8,
            ind = "relativeChange", var = "lfi40", titlePlot = "UNCERTAINTY ON LFI40" ,
            lim = c(-380, 380), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

mtext(text = "RELATIVE CHANGE IN SCENARIO 20%" ,
      side = 1, cex = 1.5  , line = 1.2, adj = 0.5, outer = TRUE)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", cex = 0.8)

legend("bottomright", legend = c("BEFORE EL NIÑO", "AFTER EL NIÑO"), bty = "n",
       fill = c("turquoise1", "sienna1"), border = "black", cex = 0.8, xpd = TRUE)

dev.off()

# 30P ---------------------------------------------------------------------

png(filename = "paper_results/plots/4.figure4B_30p.png", width = 1300, height = 1400, pointsize = 24)
par(mfrow = c(3, 2))
par(cex   = 1)
par(mar   = c(2.5,2.5,1,0.5), oma = c(3,4,1,0.5))

figure4B_rc(sim_sp0 = sim30p_sp0, sim_sp1 = sim30p_sp1, sim_sp2 = sim30p_sp2, sim_sp3 = sim30p_sp3,
            sim_sp4 = sim30p_sp4, sim_sp5 = sim30p_sp5, sim_sp6 = sim30p_sp6, sim_sp7 = sim30p_sp7, sim_sp8 = sim30p_sp8,
            ind = "relativeChange", var = "mti", titlePlot = "UNCERTAINTY ON MARINE TROPHIC INDEX" ,
            lim = c(-10, 10), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure4B_rc(sim_sp0 = sim30p_sp0, sim_sp1 = sim30p_sp1, sim_sp2 = sim30p_sp2, sim_sp3 = sim30p_sp3,
            sim_sp4 = sim30p_sp4, sim_sp5 = sim30p_sp5, sim_sp6 = sim30p_sp6, sim_sp7 = sim30p_sp7, sim_sp8 = sim30p_sp8,
            ind = "relativeChange", var = "meanTL", titlePlot = "UNCERTAINTY ON MEAN TROPHIC LEVEL" ,
            lim = c(-10, 10), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4B_rc(sim_sp0 = sim30p_sp0, sim_sp1 = sim30p_sp1, sim_sp2 = sim30p_sp2, sim_sp3 = sim30p_sp3,
            sim_sp4 = sim30p_sp4, sim_sp5 = sim30p_sp5, sim_sp6 = sim30p_sp6, sim_sp7 = sim30p_sp7, sim_sp8 = sim30p_sp8,
            ind = "relativeChange", var = "meanLength", titlePlot = "UNCERTAINTY ON MEAN LENGTH" ,
            lim = c(-50, 50), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure4B_rc(sim_sp0 = sim30p_sp0, sim_sp1 = sim30p_sp1, sim_sp2 = sim30p_sp2, sim_sp3 = sim30p_sp3,
            sim_sp4 = sim30p_sp4, sim_sp5 = sim30p_sp5, sim_sp6 = sim30p_sp6, sim_sp7 = sim30p_sp7, sim_sp8 = sim30p_sp8,
            ind = "relativeChange", var = "meanLifespan", titlePlot = "UNCERTAINTY ON MEAN LIFESPAN" ,
            lim = c(-50, 50), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure4B_rc(sim_sp0 = sim30p_sp0, sim_sp1 = sim30p_sp1, sim_sp2 = sim30p_sp2, sim_sp3 = sim30p_sp3,
            sim_sp4 = sim30p_sp4, sim_sp5 = sim30p_sp5, sim_sp6 = sim30p_sp6, sim_sp7 = sim30p_sp7, sim_sp8 = sim30p_sp8,
            ind = "relativeChange", var = "biomassOverYield", titlePlot = "UNCERTAINTY ON BIOMASS OVER YIELD" ,
            lim = c(-500, 500), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure4B_rc(sim_sp0 = sim30p_sp0, sim_sp1 = sim30p_sp1, sim_sp2 = sim30p_sp2, sim_sp3 = sim30p_sp3,
            sim_sp4 = sim30p_sp4, sim_sp5 = sim30p_sp5, sim_sp6 = sim30p_sp6, sim_sp7 = sim30p_sp7, sim_sp8 = sim30p_sp8,
            ind = "relativeChange", var = "lfi40", titlePlot = "UNCERTAINTY ON LFI40" ,
            lim = c(-600, 600), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

mtext(text = "RELATIVE CHANGE IN SCENARIO 30%" ,
      side = 1, cex = 1.5  , line = 1.2, adj = 0.5, outer = TRUE)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", cex = 0.8)

legend("bottomright", legend = c("BEFORE EL NIÑO", "AFTER EL NIÑO"), bty = "n",
       fill = c("turquoise1", "sienna1"), border = "black", cex = 0.8, xpd = TRUE)

dev.off()
