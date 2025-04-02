

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


# Figure1A ----------------------------------------------------------------
#Boxplot of biomass by species in one panel - using relative change

.meanList = function(x){
  x = apply(x, 2, mean, na.rm = TRUE)
  return(x)
}

#Scenario 1 - 10P
biomass_rc_sp0_10p = do.call(rbind, lapply(sim10p_sp0$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp1_10p = do.call(rbind, lapply(sim10p_sp1$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp2_10p = do.call(rbind, lapply(sim10p_sp2$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp3_10p = do.call(rbind, lapply(sim10p_sp3$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp4_10p = do.call(rbind, lapply(sim10p_sp4$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp5_10p = do.call(rbind, lapply(sim10p_sp5$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp6_10p = do.call(rbind, lapply(sim10p_sp6$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp7_10p = do.call(rbind, lapply(sim10p_sp7$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp8_10p = do.call(rbind, lapply(sim10p_sp8$relativeChange$biomass_sp , FUN = .meanList))

#Scenario 2 - 20P
biomass_rc_sp0_20p = do.call(rbind, lapply(sim20p_sp0$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp1_20p = do.call(rbind, lapply(sim20p_sp1$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp2_20p = do.call(rbind, lapply(sim20p_sp2$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp3_20p = do.call(rbind, lapply(sim20p_sp3$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp4_20p = do.call(rbind, lapply(sim20p_sp4$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp5_20p = do.call(rbind, lapply(sim20p_sp5$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp6_20p = do.call(rbind, lapply(sim20p_sp6$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp7_20p = do.call(rbind, lapply(sim20p_sp7$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp8_20p = do.call(rbind, lapply(sim20p_sp8$relativeChange$biomass_sp , FUN = .meanList))

#Scenario 3 - 30P
biomass_rc_sp0_30p = do.call(rbind, lapply(sim30p_sp0$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp1_30p = do.call(rbind, lapply(sim30p_sp1$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp2_30p = do.call(rbind, lapply(sim30p_sp2$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp3_30p = do.call(rbind, lapply(sim30p_sp3$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp4_30p = do.call(rbind, lapply(sim30p_sp4$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp5_30p = do.call(rbind, lapply(sim30p_sp5$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp6_30p = do.call(rbind, lapply(sim30p_sp6$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp7_30p = do.call(rbind, lapply(sim30p_sp7$relativeChange$biomass_sp , FUN = .meanList))
biomass_rc_sp8_30p = do.call(rbind, lapply(sim30p_sp8$relativeChange$biomass_sp , FUN = .meanList))


figure1_rc = function(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
                      sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
                      sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
                      sp, titlePlot, lim = NULL, axis1 = TRUE, axis2 = TRUE, speciesName, ...){
  
  if(is.null(lim)){lim = c(-100,100)} else { lim = lim }
  
  distance = (27:1) + 0.17*c(-0.7, 0, 0.7)
  
  boxplot(sim10p_sp0[,sp]*100, at = distance[1] , outline = FALSE, axes = FALSE, ylim = lim, col = "white" , xlim = c(0.5,27.5), horizontal = TRUE, width = 1.5)
  boxplot(sim20p_sp0[,sp]*100, at = distance[2] , outline = FALSE, axes = FALSE, add = TRUE, col = "gray77", horizontal = TRUE, width = 1.5)
  boxplot(sim30p_sp0[,sp]*100, at = distance[3] , outline = FALSE, axes = FALSE, add = TRUE, col = "gray30", horizontal = TRUE, width = 1.5)
  
  boxplot(sim10p_sp1[,sp]*100, at = distance[4] , outline = FALSE, axes = FALSE, add = TRUE, col = "white" , horizontal = TRUE , width = 1.5)
  boxplot(sim20p_sp1[,sp]*100, at = distance[5] , outline = FALSE, axes = FALSE, add = TRUE, col = "gray77", horizontal = TRUE, width = 1.5)
  boxplot(sim30p_sp1[,sp]*100, at = distance[6] , outline = FALSE, axes = FALSE, add = TRUE, col = "gray30", horizontal = TRUE, width = 1.5)
  
  boxplot(sim10p_sp2[,sp]*100, at = distance[7] , outline = FALSE, axes = FALSE, add = TRUE, col = "white" , horizontal = TRUE , width = 1.5)
  boxplot(sim20p_sp2[,sp]*100, at = distance[8] , outline = FALSE, axes = FALSE, add = TRUE, col = "gray77", horizontal = TRUE, width = 1.5)
  boxplot(sim30p_sp2[,sp]*100, at = distance[9] , outline = FALSE, axes = FALSE, add = TRUE, col = "gray30", horizontal = TRUE, width = 1.5)
  
  boxplot(sim10p_sp3[,sp]*100, at = distance[10], outline = FALSE, axes = FALSE, add = TRUE, col = "white" , horizontal = TRUE , width = 1.5)
  boxplot(sim20p_sp3[,sp]*100, at = distance[11], outline = FALSE, axes = FALSE, add = TRUE, col = "gray77", horizontal = TRUE, width = 1.5)
  boxplot(sim30p_sp3[,sp]*100, at = distance[12], outline = FALSE, axes = FALSE, add = TRUE, col = "gray30", horizontal = TRUE, width = 1.5)
  
  boxplot(sim10p_sp4[,sp]*100, at = distance[13], outline = FALSE, axes = FALSE, add = TRUE, col = "white" , horizontal = TRUE , width = 1.5)
  boxplot(sim20p_sp4[,sp]*100, at = distance[14], outline = FALSE, axes = FALSE, add = TRUE, col = "gray77", horizontal = TRUE, width = 1.5)
  boxplot(sim30p_sp4[,sp]*100, at = distance[15], outline = FALSE, axes = FALSE, add = TRUE, col = "gray30", horizontal = TRUE, width = 1.5)
  
  boxplot(sim10p_sp5[,sp]*100, at = distance[16], outline = FALSE, axes = FALSE, add = TRUE, col = "white" , horizontal = TRUE , width = 1.5)
  boxplot(sim20p_sp5[,sp]*100, at = distance[17], outline = FALSE, axes = FALSE, add = TRUE, col = "gray77", horizontal = TRUE, width = 1.5)
  boxplot(sim30p_sp5[,sp]*100, at = distance[18], outline = FALSE, axes = FALSE, add = TRUE, col = "gray30", horizontal = TRUE, width = 1.5)
  
  boxplot(sim10p_sp6[,sp]*100, at = distance[19], outline = FALSE, axes = FALSE, add = TRUE, col = "white" , horizontal = TRUE , width = 1.5)
  boxplot(sim20p_sp6[,sp]*100, at = distance[20], outline = FALSE, axes = FALSE, add = TRUE, col = "gray77", horizontal = TRUE, width = 1.5)
  boxplot(sim30p_sp6[,sp]*100, at = distance[21], outline = FALSE, axes = FALSE, add = TRUE, col = "gray30", horizontal = TRUE, width = 1.5)
  
  boxplot(sim10p_sp7[,sp]*100, at = distance[22], outline = FALSE, axes = FALSE, add = TRUE, col = "white" , horizontal = TRUE , width = 1.5)
  boxplot(sim20p_sp7[,sp]*100, at = distance[23], outline = FALSE, axes = FALSE, add = TRUE, col = "gray77", horizontal = TRUE, width = 1.5)
  boxplot(sim30p_sp7[,sp]*100, at = distance[24], outline = FALSE, axes = FALSE, add = TRUE, col = "gray30", horizontal = TRUE, width = 1.5)
  
  boxplot(sim10p_sp8[,sp]*100, at = distance[25], outline = FALSE, axes = FALSE, add = TRUE, col = "white" , horizontal = TRUE , width = 1.5)
  boxplot(sim20p_sp8[,sp]*100, at = distance[26], outline = FALSE, axes = FALSE, add = TRUE, col = "gray77", horizontal = TRUE, width = 1.5)
  boxplot(sim30p_sp8[,sp]*100, at = distance[27], outline = FALSE, axes = FALSE, add = TRUE, col = "gray30", horizontal = TRUE, width = 1.5)
  
  abline(v = 0,  col = "red", lty = 2)
  box(...)
  
  if(isTRUE(axis2)){
    axis(2, at = seq(from = 2, by = 3, length.out = 9), labels = speciesName, cex.axis = 0.8, las = 2)
  } else {
    axis(2, at = seq(from = 2, by = 3, length.out = 9), labels = FALSE, cex.axis = 0.8) 
  }
  
  if(isTRUE(axis1)){
    axis1Default = axTicks(1)
    axis(1, at = axis1Default, labels = paste0(axis1Default, "%"), cex.axis = 0.8)} 
  
  mtext(text = titlePlot, side = 3, cex = 0.8, adj = 0.05)
  
  return(invisible())
}

speciesComplete  = rev(c("ANCHOVY", "HAKE", "SARDINE", "JACK MACKEREL", "CHUB MACKEREL",
                         "MESOPELAGICS", "MUNIDA" , "HUMBOLDT SQUID", "EUPHAUSIIDS"))

speciesAbbre     = rev(c("AN", "HA", "SA", "JM", "CM", "ME", "MU", "HS", "EU"))

coloursBox       = c("black", "blue", "green4", "blueviolet", "coral")


png(filename = "paper_results/plots/1.figure1A.png", width = 1200, height = 1000, pointsize = 17)
par(mfrow = c(3, 3))
par(cex   = 1)
par(mar   = c(2.5,3.5,1.1,0), oma = c(4,4,1.1,0.8))
figure1_rc(sim10p_sp0 = biomass_rc_sp0_10p, sim10p_sp1 = biomass_rc_sp1_10p, sim10p_sp2 = biomass_rc_sp2_10p, sim10p_sp3 = biomass_rc_sp3_10p, sim10p_sp4 = biomass_rc_sp4_10p, sim10p_sp5 = biomass_rc_sp5_10p, sim10p_sp6 = biomass_rc_sp6_10p, sim10p_sp7 = biomass_rc_sp7_10p, sim10p_sp8 = biomass_rc_sp8_10p,
           sim20p_sp0 = biomass_rc_sp0_20p, sim20p_sp1 = biomass_rc_sp1_20p, sim20p_sp2 = biomass_rc_sp2_20p, sim20p_sp3 = biomass_rc_sp3_20p, sim20p_sp4 = biomass_rc_sp4_20p, sim20p_sp5 = biomass_rc_sp5_20p, sim20p_sp6 = biomass_rc_sp6_20p, sim20p_sp7 = biomass_rc_sp7_20p, sim20p_sp8 = biomass_rc_sp8_20p,
           sim30p_sp0 = biomass_rc_sp0_30p, sim30p_sp1 = biomass_rc_sp1_30p, sim30p_sp2 = biomass_rc_sp2_30p, sim30p_sp3 = biomass_rc_sp3_30p, sim30p_sp4 = biomass_rc_sp4_30p, sim30p_sp5 = biomass_rc_sp5_30p, sim30p_sp6 = biomass_rc_sp6_30p, sim30p_sp7 = biomass_rc_sp7_30p, sim30p_sp8 = biomass_rc_sp8_30p,
           sp = 1, titlePlot = "UNCERTAINTY ON ANCHOVY"       , lim = c(-200,200), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure1_rc(sim10p_sp0 = biomass_rc_sp0_10p, sim10p_sp1 = biomass_rc_sp1_10p, sim10p_sp2 = biomass_rc_sp2_10p, sim10p_sp3 = biomass_rc_sp3_10p, sim10p_sp4 = biomass_rc_sp4_10p, sim10p_sp5 = biomass_rc_sp5_10p, sim10p_sp6 = biomass_rc_sp6_10p, sim10p_sp7 = biomass_rc_sp7_10p, sim10p_sp8 = biomass_rc_sp8_10p,
           sim20p_sp0 = biomass_rc_sp0_20p, sim20p_sp1 = biomass_rc_sp1_20p, sim20p_sp2 = biomass_rc_sp2_20p, sim20p_sp3 = biomass_rc_sp3_20p, sim20p_sp4 = biomass_rc_sp4_20p, sim20p_sp5 = biomass_rc_sp5_20p, sim20p_sp6 = biomass_rc_sp6_20p, sim20p_sp7 = biomass_rc_sp7_20p, sim20p_sp8 = biomass_rc_sp8_20p,
           sim30p_sp0 = biomass_rc_sp0_30p, sim30p_sp1 = biomass_rc_sp1_30p, sim30p_sp2 = biomass_rc_sp2_30p, sim30p_sp3 = biomass_rc_sp3_30p, sim30p_sp4 = biomass_rc_sp4_30p, sim30p_sp5 = biomass_rc_sp5_30p, sim30p_sp6 = biomass_rc_sp6_30p, sim30p_sp7 = biomass_rc_sp7_30p, sim30p_sp8 = biomass_rc_sp8_30p,
           sp = 2, titlePlot = "UNCERTAINTY ON HAKE"          , lim = c(-750,750), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure1_rc(sim10p_sp0 = biomass_rc_sp0_10p, sim10p_sp1 = biomass_rc_sp1_10p, sim10p_sp2 = biomass_rc_sp2_10p, sim10p_sp3 = biomass_rc_sp3_10p, sim10p_sp4 = biomass_rc_sp4_10p, sim10p_sp5 = biomass_rc_sp5_10p, sim10p_sp6 = biomass_rc_sp6_10p, sim10p_sp7 = biomass_rc_sp7_10p, sim10p_sp8 = biomass_rc_sp8_10p,
           sim20p_sp0 = biomass_rc_sp0_20p, sim20p_sp1 = biomass_rc_sp1_20p, sim20p_sp2 = biomass_rc_sp2_20p, sim20p_sp3 = biomass_rc_sp3_20p, sim20p_sp4 = biomass_rc_sp4_20p, sim20p_sp5 = biomass_rc_sp5_20p, sim20p_sp6 = biomass_rc_sp6_20p, sim20p_sp7 = biomass_rc_sp7_20p, sim20p_sp8 = biomass_rc_sp8_20p,
           sim30p_sp0 = biomass_rc_sp0_30p, sim30p_sp1 = biomass_rc_sp1_30p, sim30p_sp2 = biomass_rc_sp2_30p, sim30p_sp3 = biomass_rc_sp3_30p, sim30p_sp4 = biomass_rc_sp4_30p, sim30p_sp5 = biomass_rc_sp5_30p, sim30p_sp6 = biomass_rc_sp6_30p, sim30p_sp7 = biomass_rc_sp7_30p, sim30p_sp8 = biomass_rc_sp8_30p,
           sp = 3, titlePlot = "UNCERTAINTY ON SARDINE"       , lim = c(-15000,15000), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure1_rc(sim10p_sp0 = biomass_rc_sp0_10p, sim10p_sp1 = biomass_rc_sp1_10p, sim10p_sp2 = biomass_rc_sp2_10p, sim10p_sp3 = biomass_rc_sp3_10p, sim10p_sp4 = biomass_rc_sp4_10p, sim10p_sp5 = biomass_rc_sp5_10p, sim10p_sp6 = biomass_rc_sp6_10p, sim10p_sp7 = biomass_rc_sp7_10p, sim10p_sp8 = biomass_rc_sp8_10p,
           sim20p_sp0 = biomass_rc_sp0_20p, sim20p_sp1 = biomass_rc_sp1_20p, sim20p_sp2 = biomass_rc_sp2_20p, sim20p_sp3 = biomass_rc_sp3_20p, sim20p_sp4 = biomass_rc_sp4_20p, sim20p_sp5 = biomass_rc_sp5_20p, sim20p_sp6 = biomass_rc_sp6_20p, sim20p_sp7 = biomass_rc_sp7_20p, sim20p_sp8 = biomass_rc_sp8_20p,
           sim30p_sp0 = biomass_rc_sp0_30p, sim30p_sp1 = biomass_rc_sp1_30p, sim30p_sp2 = biomass_rc_sp2_30p, sim30p_sp3 = biomass_rc_sp3_30p, sim30p_sp4 = biomass_rc_sp4_30p, sim30p_sp5 = biomass_rc_sp5_30p, sim30p_sp6 = biomass_rc_sp6_30p, sim30p_sp7 = biomass_rc_sp7_30p, sim30p_sp8 = biomass_rc_sp8_30p,
           sp = 4, titlePlot = "UNCERTAINTY ON JACK MACKEREL" , lim = c(-200,200), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure1_rc(sim10p_sp0 = biomass_rc_sp0_10p, sim10p_sp1 = biomass_rc_sp1_10p, sim10p_sp2 = biomass_rc_sp2_10p, sim10p_sp3 = biomass_rc_sp3_10p, sim10p_sp4 = biomass_rc_sp4_10p, sim10p_sp5 = biomass_rc_sp5_10p, sim10p_sp6 = biomass_rc_sp6_10p, sim10p_sp7 = biomass_rc_sp7_10p, sim10p_sp8 = biomass_rc_sp8_10p,
           sim20p_sp0 = biomass_rc_sp0_20p, sim20p_sp1 = biomass_rc_sp1_20p, sim20p_sp2 = biomass_rc_sp2_20p, sim20p_sp3 = biomass_rc_sp3_20p, sim20p_sp4 = biomass_rc_sp4_20p, sim20p_sp5 = biomass_rc_sp5_20p, sim20p_sp6 = biomass_rc_sp6_20p, sim20p_sp7 = biomass_rc_sp7_20p, sim20p_sp8 = biomass_rc_sp8_20p,
           sim30p_sp0 = biomass_rc_sp0_30p, sim30p_sp1 = biomass_rc_sp1_30p, sim30p_sp2 = biomass_rc_sp2_30p, sim30p_sp3 = biomass_rc_sp3_30p, sim30p_sp4 = biomass_rc_sp4_30p, sim30p_sp5 = biomass_rc_sp5_30p, sim30p_sp6 = biomass_rc_sp6_30p, sim30p_sp7 = biomass_rc_sp7_30p, sim30p_sp8 = biomass_rc_sp8_30p,
           sp = 5, titlePlot = "UNCERTAINTY ON CHUB MACKEREL" , lim = c(-200,200), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure1_rc(sim10p_sp0 = biomass_rc_sp0_10p, sim10p_sp1 = biomass_rc_sp1_10p, sim10p_sp2 = biomass_rc_sp2_10p, sim10p_sp3 = biomass_rc_sp3_10p, sim10p_sp4 = biomass_rc_sp4_10p, sim10p_sp5 = biomass_rc_sp5_10p, sim10p_sp6 = biomass_rc_sp6_10p, sim10p_sp7 = biomass_rc_sp7_10p, sim10p_sp8 = biomass_rc_sp8_10p,
           sim20p_sp0 = biomass_rc_sp0_20p, sim20p_sp1 = biomass_rc_sp1_20p, sim20p_sp2 = biomass_rc_sp2_20p, sim20p_sp3 = biomass_rc_sp3_20p, sim20p_sp4 = biomass_rc_sp4_20p, sim20p_sp5 = biomass_rc_sp5_20p, sim20p_sp6 = biomass_rc_sp6_20p, sim20p_sp7 = biomass_rc_sp7_20p, sim20p_sp8 = biomass_rc_sp8_20p,
           sim30p_sp0 = biomass_rc_sp0_30p, sim30p_sp1 = biomass_rc_sp1_30p, sim30p_sp2 = biomass_rc_sp2_30p, sim30p_sp3 = biomass_rc_sp3_30p, sim30p_sp4 = biomass_rc_sp4_30p, sim30p_sp5 = biomass_rc_sp5_30p, sim30p_sp6 = biomass_rc_sp6_30p, sim30p_sp7 = biomass_rc_sp7_30p, sim30p_sp8 = biomass_rc_sp8_30p,
           sp = 6, titlePlot = "UNCERTAINTY ON MESOPELAGICS"  , lim = c(-250,250), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure1_rc(sim10p_sp0 = biomass_rc_sp0_10p, sim10p_sp1 = biomass_rc_sp1_10p, sim10p_sp2 = biomass_rc_sp2_10p, sim10p_sp3 = biomass_rc_sp3_10p, sim10p_sp4 = biomass_rc_sp4_10p, sim10p_sp5 = biomass_rc_sp5_10p, sim10p_sp6 = biomass_rc_sp6_10p, sim10p_sp7 = biomass_rc_sp7_10p, sim10p_sp8 = biomass_rc_sp8_10p,
           sim20p_sp0 = biomass_rc_sp0_20p, sim20p_sp1 = biomass_rc_sp1_20p, sim20p_sp2 = biomass_rc_sp2_20p, sim20p_sp3 = biomass_rc_sp3_20p, sim20p_sp4 = biomass_rc_sp4_20p, sim20p_sp5 = biomass_rc_sp5_20p, sim20p_sp6 = biomass_rc_sp6_20p, sim20p_sp7 = biomass_rc_sp7_20p, sim20p_sp8 = biomass_rc_sp8_20p,
           sim30p_sp0 = biomass_rc_sp0_30p, sim30p_sp1 = biomass_rc_sp1_30p, sim30p_sp2 = biomass_rc_sp2_30p, sim30p_sp3 = biomass_rc_sp3_30p, sim30p_sp4 = biomass_rc_sp4_30p, sim30p_sp5 = biomass_rc_sp5_30p, sim30p_sp6 = biomass_rc_sp6_30p, sim30p_sp7 = biomass_rc_sp7_30p, sim30p_sp8 = biomass_rc_sp8_30p,
           sp = 7, titlePlot = "UNCERTAINTY ON MUNIDA"        , lim = c(-200,200), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure1_rc(sim10p_sp0 = biomass_rc_sp0_10p, sim10p_sp1 = biomass_rc_sp1_10p, sim10p_sp2 = biomass_rc_sp2_10p, sim10p_sp3 = biomass_rc_sp3_10p, sim10p_sp4 = biomass_rc_sp4_10p, sim10p_sp5 = biomass_rc_sp5_10p, sim10p_sp6 = biomass_rc_sp6_10p, sim10p_sp7 = biomass_rc_sp7_10p, sim10p_sp8 = biomass_rc_sp8_10p,
           sim20p_sp0 = biomass_rc_sp0_20p, sim20p_sp1 = biomass_rc_sp1_20p, sim20p_sp2 = biomass_rc_sp2_20p, sim20p_sp3 = biomass_rc_sp3_20p, sim20p_sp4 = biomass_rc_sp4_20p, sim20p_sp5 = biomass_rc_sp5_20p, sim20p_sp6 = biomass_rc_sp6_20p, sim20p_sp7 = biomass_rc_sp7_20p, sim20p_sp8 = biomass_rc_sp8_20p,
           sim30p_sp0 = biomass_rc_sp0_30p, sim30p_sp1 = biomass_rc_sp1_30p, sim30p_sp2 = biomass_rc_sp2_30p, sim30p_sp3 = biomass_rc_sp3_30p, sim30p_sp4 = biomass_rc_sp4_30p, sim30p_sp5 = biomass_rc_sp5_30p, sim30p_sp6 = biomass_rc_sp6_30p, sim30p_sp7 = biomass_rc_sp7_30p, sim30p_sp8 = biomass_rc_sp8_30p,
           sp = 8, titlePlot = "UNCERTAINTY ON HUMBOLDT SQUID", lim = c(-2200,2200), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure1_rc(sim10p_sp0 = biomass_rc_sp0_10p, sim10p_sp1 = biomass_rc_sp1_10p, sim10p_sp2 = biomass_rc_sp2_10p, sim10p_sp3 = biomass_rc_sp3_10p, sim10p_sp4 = biomass_rc_sp4_10p, sim10p_sp5 = biomass_rc_sp5_10p, sim10p_sp6 = biomass_rc_sp6_10p, sim10p_sp7 = biomass_rc_sp7_10p, sim10p_sp8 = biomass_rc_sp8_10p,
           sim20p_sp0 = biomass_rc_sp0_20p, sim20p_sp1 = biomass_rc_sp1_20p, sim20p_sp2 = biomass_rc_sp2_20p, sim20p_sp3 = biomass_rc_sp3_20p, sim20p_sp4 = biomass_rc_sp4_20p, sim20p_sp5 = biomass_rc_sp5_20p, sim20p_sp6 = biomass_rc_sp6_20p, sim20p_sp7 = biomass_rc_sp7_20p, sim20p_sp8 = biomass_rc_sp8_20p,
           sim30p_sp0 = biomass_rc_sp0_30p, sim30p_sp1 = biomass_rc_sp1_30p, sim30p_sp2 = biomass_rc_sp2_30p, sim30p_sp3 = biomass_rc_sp3_30p, sim30p_sp4 = biomass_rc_sp4_30p, sim30p_sp5 = biomass_rc_sp5_30p, sim30p_sp6 = biomass_rc_sp6_30p, sim30p_sp7 = biomass_rc_sp7_30p, sim30p_sp8 = biomass_rc_sp8_30p,
           sp = 9, titlePlot = "UNCERTAINTY ON EUPHAUSIIDS"   , lim = c(-120,120),   col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

mtext(text = "RELATIVE CHANGE IN BIOMASS" , side = 1, cex = 1.5  , line = 1.2, adj = 0.5, outer = TRUE)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", cex = 0.8)

legend("bottomright", legend = c("SCENARIO 10%", "SCENARIO 20%", "SCENARIO 30%"), bty = "n",
       fill = c("white", "gray77", "gray30"), border = "black", cex = 0.9, xpd = TRUE)

dev.off()

# Figure1B ----------------------------------------------------------------
#Boxplot of biomass by species in one panel - using coefficient of variation

#Scenario 1 - 10P
biomass_cv_sp0_10p = apply(sim10p_sp0$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp1_10p = apply(sim10p_sp1$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp2_10p = apply(sim10p_sp2$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp3_10p = apply(sim10p_sp3$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp4_10p = apply(sim10p_sp4$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp5_10p = apply(sim10p_sp5$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp6_10p = apply(sim10p_sp6$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp7_10p = apply(sim10p_sp7$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp8_10p = apply(sim10p_sp8$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100

#Scenario 2 - 20P
biomass_cv_sp0_20p = apply(sim20p_sp0$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp1_20p = apply(sim20p_sp1$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp2_20p = apply(sim20p_sp2$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp3_20p = apply(sim20p_sp3$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp4_20p = apply(sim20p_sp4$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp5_20p = apply(sim20p_sp5$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp6_20p = apply(sim20p_sp6$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp7_20p = apply(sim20p_sp7$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp8_20p = apply(sim20p_sp8$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100

#Scenario 2 - 30P
biomass_cv_sp0_30p = apply(sim30p_sp0$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp1_30p = apply(sim30p_sp1$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp2_30p = apply(sim30p_sp2$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp3_30p = apply(sim30p_sp3$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp4_30p = apply(sim30p_sp4$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp5_30p = apply(sim30p_sp5$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp6_30p = apply(sim30p_sp6$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp7_30p = apply(sim30p_sp7$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100
biomass_cv_sp8_30p = apply(sim30p_sp8$coefficientVariation$biomass_sp , 2, mean, na.rm = TRUE)*100

speciesComplete  = rev(c("ANCHOVY", "HAKE", "SARDINE", "JACK MACKEREL", "CHUB MACKEREL",
                         "MESOPELAGICS", "MUNIDA" , "HUMBOLDT SQUID", "EUPHAUSIIDS"))

speciesAbbre     = rev(c("AN", "HA", "SA", "JM", "CM", "ME", "MU", "HS", "EU"))

figure1_cv = function(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
                      sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
                      sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
                      sp, xlim = NULL, titlePlot, axis1 = TRUE, axis2 = TRUE, cex.point = 1, speciesName, ...){
  
  if(is.null(xlim)){xlim = c(-100,100)} else { xlim = xlim }
  
  distance = (27:1) + 0.17*c(-0.7,0,0.7)
  
  plot(1, type="n", xlab="", ylab="", xlim = xlim, ylim = c(0.5,27.5), axes = FALSE)
  abline(v = 0, col = "red", lty = 2)
  
  abline(v = 10, col = "blue", lty = 3, lwd = 0.8)
  abline(v = 20, col = "blue", lty = 3, lwd = 0.8)
  abline(v = 30, col = "blue", lty = 3, lwd = 0.8)
  
  box(...)
  
  if(isTRUE(axis2)){
    axis(2, at = seq(from = 2, by = 3, length.out = 9), labels = speciesName, cex.axis = 0.7, las = 2)
  } else {
    axis(2, at = seq(from = 2, by = 3, length.out = 9), labels = FALSE)
  }
  
  if(isTRUE(axis1)){
    axis1Default = axTicks(1)
    axis(1, at = axis1Default, labels = paste0(axis1Default, "%"))}
  
  mtext(text = titlePlot, side = 3,cex = 0.7, adj = 0.05)
  
  #segments
  segments(0, distance[1 ], sim10p_sp0[sp], distance[1 ], col = "black", lty = 2)
  segments(0, distance[2 ], sim20p_sp0[sp], distance[2 ], col = "black", lty = 2)
  segments(0, distance[3 ], sim30p_sp0[sp], distance[3 ], col = "black", lty = 2)
  segments(0, distance[4 ], sim10p_sp1[sp], distance[4 ], col = "black", lty = 2)
  segments(0, distance[5 ], sim20p_sp1[sp], distance[5 ], col = "black", lty = 2)
  segments(0, distance[6 ], sim30p_sp1[sp], distance[6 ], col = "black", lty = 2)
  segments(0, distance[7 ], sim10p_sp2[sp], distance[7 ], col = "black", lty = 2)
  segments(0, distance[8 ], sim20p_sp2[sp], distance[8 ], col = "black", lty = 2)
  segments(0, distance[9 ], sim30p_sp2[sp], distance[9 ], col = "black", lty = 2)
  segments(0, distance[10], sim10p_sp3[sp], distance[10], col = "black", lty = 2)
  segments(0, distance[11], sim20p_sp3[sp], distance[11], col = "black", lty = 2)
  segments(0, distance[12], sim30p_sp3[sp], distance[12], col = "black", lty = 2)
  segments(0, distance[13], sim10p_sp4[sp], distance[13], col = "black", lty = 2)
  segments(0, distance[14], sim20p_sp4[sp], distance[14], col = "black", lty = 2)
  segments(0, distance[15], sim30p_sp4[sp], distance[15], col = "black", lty = 2)
  segments(0, distance[16], sim10p_sp5[sp], distance[16], col = "black", lty = 2)
  segments(0, distance[17], sim20p_sp5[sp], distance[17], col = "black", lty = 2)
  segments(0, distance[18], sim30p_sp5[sp], distance[18], col = "black", lty = 2)
  segments(0, distance[19], sim10p_sp6[sp], distance[19], col = "black", lty = 2)
  segments(0, distance[20], sim20p_sp6[sp], distance[20], col = "black", lty = 2)
  segments(0, distance[21], sim30p_sp6[sp], distance[21], col = "black", lty = 2)
  segments(0, distance[22], sim10p_sp7[sp], distance[22], col = "black", lty = 2)
  segments(0, distance[23], sim20p_sp7[sp], distance[23], col = "black", lty = 2)
  segments(0, distance[24], sim30p_sp7[sp], distance[24], col = "black", lty = 2)
  segments(0, distance[25], sim10p_sp8[sp], distance[25], col = "black", lty = 2)
  segments(0, distance[26], sim20p_sp8[sp], distance[26], col = "black", lty = 2)
  segments(0, distance[27], sim30p_sp8[sp], distance[27], col = "black", lty = 2)
  
  points(x = sim10p_sp0[sp], y = distance[1 ], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "white" , col = "black")
  points(x = sim20p_sp0[sp], y = distance[2 ], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray77", col = "black")
  points(x = sim30p_sp0[sp], y = distance[3 ], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray30", col = "black")
  points(x = sim10p_sp1[sp], y = distance[4 ], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "white" , col = "black")
  points(x = sim20p_sp1[sp], y = distance[5 ], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray77", col = "black")
  points(x = sim30p_sp1[sp], y = distance[6 ], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray30", col = "black")
  points(x = sim10p_sp2[sp], y = distance[7 ], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "white" , col = "black")
  points(x = sim20p_sp2[sp], y = distance[8 ], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray77", col = "black")
  points(x = sim30p_sp2[sp], y = distance[9 ], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray30", col = "black")
  points(x = sim10p_sp3[sp], y = distance[10], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "white" , col = "black")
  points(x = sim20p_sp3[sp], y = distance[11], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray77", col = "black")
  points(x = sim30p_sp3[sp], y = distance[12], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray30", col = "black")
  points(x = sim10p_sp4[sp], y = distance[13], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "white" , col = "black")
  points(x = sim20p_sp4[sp], y = distance[14], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray77", col = "black")
  points(x = sim30p_sp4[sp], y = distance[15], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray30", col = "black")
  points(x = sim10p_sp5[sp], y = distance[16], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "white" , col = "black")
  points(x = sim20p_sp5[sp], y = distance[17], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray77", col = "black")
  points(x = sim30p_sp5[sp], y = distance[18], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray30", col = "black")
  points(x = sim10p_sp6[sp], y = distance[19], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "white" , col = "black")
  points(x = sim20p_sp6[sp], y = distance[20], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray77", col = "black")
  points(x = sim30p_sp6[sp], y = distance[21], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray30", col = "black")
  points(x = sim10p_sp7[sp], y = distance[22], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "white" , col = "black")
  points(x = sim20p_sp7[sp], y = distance[23], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray77", col = "black")
  points(x = sim30p_sp7[sp], y = distance[24], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray30", col = "black")
  points(x = sim10p_sp8[sp], y = distance[25], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "white" , col = "black")
  points(x = sim20p_sp8[sp], y = distance[26], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray77", col = "black")
  points(x = sim30p_sp8[sp], y = distance[27], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray30", col = "black")
  
  return(invisible())
  
}

png(filename = "paper_results/plots/1.figure1B.png", width = 1200, height = 900, pointsize = 16)
par(mfrow = c(3, 3))
par(cex   = 1)
par(mar   = c(2.5,2.5,1,0), oma = c(3,4,1,0.5))
figure1_cv(sim10p_sp0 = biomass_cv_sp0_10p, sim10p_sp1 = biomass_cv_sp1_10p, sim10p_sp2 = biomass_cv_sp2_10p, sim10p_sp3 = biomass_cv_sp3_10p, sim10p_sp4 = biomass_cv_sp4_10p, sim10p_sp5 = biomass_cv_sp5_10p, sim10p_sp6 = biomass_cv_sp6_10p, sim10p_sp7 = biomass_cv_sp7_10p, sim10p_sp8 = biomass_cv_sp8_10p,
           sim20p_sp0 = biomass_cv_sp0_20p, sim20p_sp1 = biomass_cv_sp1_20p, sim20p_sp2 = biomass_cv_sp2_20p, sim20p_sp3 = biomass_cv_sp3_20p, sim20p_sp4 = biomass_cv_sp4_20p, sim20p_sp5 = biomass_cv_sp5_20p, sim20p_sp6 = biomass_cv_sp6_20p, sim20p_sp7 = biomass_cv_sp7_20p, sim20p_sp8 = biomass_cv_sp8_20p,
           sim30p_sp0 = biomass_cv_sp0_30p, sim30p_sp1 = biomass_cv_sp1_30p, sim30p_sp2 = biomass_cv_sp2_30p, sim30p_sp3 = biomass_cv_sp3_30p, sim30p_sp4 = biomass_cv_sp4_30p, sim30p_sp5 = biomass_cv_sp5_30p, sim30p_sp6 = biomass_cv_sp6_30p, sim30p_sp7 = biomass_cv_sp7_30p, sim30p_sp8 = biomass_cv_sp8_30p,
           sp = 1, titlePlot = "UNCERTAINTY ON ANCHOVY"       , xlim = c(-0.1,100), lwd = 2, speciesName = speciesComplete)

figure1_cv(sim10p_sp0 = biomass_cv_sp0_10p, sim10p_sp1 = biomass_cv_sp1_10p, sim10p_sp2 = biomass_cv_sp2_10p, sim10p_sp3 = biomass_cv_sp3_10p, sim10p_sp4 = biomass_cv_sp4_10p, sim10p_sp5 = biomass_cv_sp5_10p, sim10p_sp6 = biomass_cv_sp6_10p, sim10p_sp7 = biomass_cv_sp7_10p, sim10p_sp8 = biomass_cv_sp8_10p,
           sim20p_sp0 = biomass_cv_sp0_20p, sim20p_sp1 = biomass_cv_sp1_20p, sim20p_sp2 = biomass_cv_sp2_20p, sim20p_sp3 = biomass_cv_sp3_20p, sim20p_sp4 = biomass_cv_sp4_20p, sim20p_sp5 = biomass_cv_sp5_20p, sim20p_sp6 = biomass_cv_sp6_20p, sim20p_sp7 = biomass_cv_sp7_20p, sim20p_sp8 = biomass_cv_sp8_20p,
           sim30p_sp0 = biomass_cv_sp0_30p, sim30p_sp1 = biomass_cv_sp1_30p, sim30p_sp2 = biomass_cv_sp2_30p, sim30p_sp3 = biomass_cv_sp3_30p, sim30p_sp4 = biomass_cv_sp4_30p, sim30p_sp5 = biomass_cv_sp5_30p, sim30p_sp6 = biomass_cv_sp6_30p, sim30p_sp7 = biomass_cv_sp7_30p, sim30p_sp8 = biomass_cv_sp8_30p,
           sp = 2, titlePlot = "UNCERTAINTY ON HAKE"          , xlim = c(-0.1,250), lwd = 2, speciesName = speciesAbbre)

figure1_cv(sim10p_sp0 = biomass_cv_sp0_10p, sim10p_sp1 = biomass_cv_sp1_10p, sim10p_sp2 = biomass_cv_sp2_10p, sim10p_sp3 = biomass_cv_sp3_10p, sim10p_sp4 = biomass_cv_sp4_10p, sim10p_sp5 = biomass_cv_sp5_10p, sim10p_sp6 = biomass_cv_sp6_10p, sim10p_sp7 = biomass_cv_sp7_10p, sim10p_sp8 = biomass_cv_sp8_10p,
           sim20p_sp0 = biomass_cv_sp0_20p, sim20p_sp1 = biomass_cv_sp1_20p, sim20p_sp2 = biomass_cv_sp2_20p, sim20p_sp3 = biomass_cv_sp3_20p, sim20p_sp4 = biomass_cv_sp4_20p, sim20p_sp5 = biomass_cv_sp5_20p, sim20p_sp6 = biomass_cv_sp6_20p, sim20p_sp7 = biomass_cv_sp7_20p, sim20p_sp8 = biomass_cv_sp8_20p,
           sim30p_sp0 = biomass_cv_sp0_30p, sim30p_sp1 = biomass_cv_sp1_30p, sim30p_sp2 = biomass_cv_sp2_30p, sim30p_sp3 = biomass_cv_sp3_30p, sim30p_sp4 = biomass_cv_sp4_30p, sim30p_sp5 = biomass_cv_sp5_30p, sim30p_sp6 = biomass_cv_sp6_30p, sim30p_sp7 = biomass_cv_sp7_30p, sim30p_sp8 = biomass_cv_sp8_30p,
           sp = 3, titlePlot = "UNCERTAINTY ON SARDINE"       , xlim = c(-0.1,120), lwd = 2, speciesName = speciesAbbre)

figure1_cv(sim10p_sp0 = biomass_cv_sp0_10p, sim10p_sp1 = biomass_cv_sp1_10p, sim10p_sp2 = biomass_cv_sp2_10p, sim10p_sp3 = biomass_cv_sp3_10p, sim10p_sp4 = biomass_cv_sp4_10p, sim10p_sp5 = biomass_cv_sp5_10p, sim10p_sp6 = biomass_cv_sp6_10p, sim10p_sp7 = biomass_cv_sp7_10p, sim10p_sp8 = biomass_cv_sp8_10p,
           sim20p_sp0 = biomass_cv_sp0_20p, sim20p_sp1 = biomass_cv_sp1_20p, sim20p_sp2 = biomass_cv_sp2_20p, sim20p_sp3 = biomass_cv_sp3_20p, sim20p_sp4 = biomass_cv_sp4_20p, sim20p_sp5 = biomass_cv_sp5_20p, sim20p_sp6 = biomass_cv_sp6_20p, sim20p_sp7 = biomass_cv_sp7_20p, sim20p_sp8 = biomass_cv_sp8_20p,
           sim30p_sp0 = biomass_cv_sp0_30p, sim30p_sp1 = biomass_cv_sp1_30p, sim30p_sp2 = biomass_cv_sp2_30p, sim30p_sp3 = biomass_cv_sp3_30p, sim30p_sp4 = biomass_cv_sp4_30p, sim30p_sp5 = biomass_cv_sp5_30p, sim30p_sp6 = biomass_cv_sp6_30p, sim30p_sp7 = biomass_cv_sp7_30p, sim30p_sp8 = biomass_cv_sp8_30p,
           sp = 4, titlePlot = "UNCERTAINTY ON JACK MACKEREL" , xlim = c(-0.1,100), lwd = 2, speciesName = speciesComplete)

figure1_cv(sim10p_sp0 = biomass_cv_sp0_10p, sim10p_sp1 = biomass_cv_sp1_10p, sim10p_sp2 = biomass_cv_sp2_10p, sim10p_sp3 = biomass_cv_sp3_10p, sim10p_sp4 = biomass_cv_sp4_10p, sim10p_sp5 = biomass_cv_sp5_10p, sim10p_sp6 = biomass_cv_sp6_10p, sim10p_sp7 = biomass_cv_sp7_10p, sim10p_sp8 = biomass_cv_sp8_10p,
           sim20p_sp0 = biomass_cv_sp0_20p, sim20p_sp1 = biomass_cv_sp1_20p, sim20p_sp2 = biomass_cv_sp2_20p, sim20p_sp3 = biomass_cv_sp3_20p, sim20p_sp4 = biomass_cv_sp4_20p, sim20p_sp5 = biomass_cv_sp5_20p, sim20p_sp6 = biomass_cv_sp6_20p, sim20p_sp7 = biomass_cv_sp7_20p, sim20p_sp8 = biomass_cv_sp8_20p,
           sim30p_sp0 = biomass_cv_sp0_30p, sim30p_sp1 = biomass_cv_sp1_30p, sim30p_sp2 = biomass_cv_sp2_30p, sim30p_sp3 = biomass_cv_sp3_30p, sim30p_sp4 = biomass_cv_sp4_30p, sim30p_sp5 = biomass_cv_sp5_30p, sim30p_sp6 = biomass_cv_sp6_30p, sim30p_sp7 = biomass_cv_sp7_30p, sim30p_sp8 = biomass_cv_sp8_30p,
           sp = 5, titlePlot = "UNCERTAINTY ON CHUB MACKEREL" , xlim = c(-0.1,110), lwd = 2, speciesName = speciesAbbre)

figure1_cv(sim10p_sp0 = biomass_cv_sp0_10p, sim10p_sp1 = biomass_cv_sp1_10p, sim10p_sp2 = biomass_cv_sp2_10p, sim10p_sp3 = biomass_cv_sp3_10p, sim10p_sp4 = biomass_cv_sp4_10p, sim10p_sp5 = biomass_cv_sp5_10p, sim10p_sp6 = biomass_cv_sp6_10p, sim10p_sp7 = biomass_cv_sp7_10p, sim10p_sp8 = biomass_cv_sp8_10p,
           sim20p_sp0 = biomass_cv_sp0_20p, sim20p_sp1 = biomass_cv_sp1_20p, sim20p_sp2 = biomass_cv_sp2_20p, sim20p_sp3 = biomass_cv_sp3_20p, sim20p_sp4 = biomass_cv_sp4_20p, sim20p_sp5 = biomass_cv_sp5_20p, sim20p_sp6 = biomass_cv_sp6_20p, sim20p_sp7 = biomass_cv_sp7_20p, sim20p_sp8 = biomass_cv_sp8_20p,
           sim30p_sp0 = biomass_cv_sp0_30p, sim30p_sp1 = biomass_cv_sp1_30p, sim30p_sp2 = biomass_cv_sp2_30p, sim30p_sp3 = biomass_cv_sp3_30p, sim30p_sp4 = biomass_cv_sp4_30p, sim30p_sp5 = biomass_cv_sp5_30p, sim30p_sp6 = biomass_cv_sp6_30p, sim30p_sp7 = biomass_cv_sp7_30p, sim30p_sp8 = biomass_cv_sp8_30p,
           sp = 6, titlePlot = "UNCERTAINTY ON MESOPELAGICS"  , xlim = c(-0.1,100), lwd = 2, speciesName = speciesAbbre)

figure1_cv(sim10p_sp0 = biomass_cv_sp0_10p, sim10p_sp1 = biomass_cv_sp1_10p, sim10p_sp2 = biomass_cv_sp2_10p, sim10p_sp3 = biomass_cv_sp3_10p, sim10p_sp4 = biomass_cv_sp4_10p, sim10p_sp5 = biomass_cv_sp5_10p, sim10p_sp6 = biomass_cv_sp6_10p, sim10p_sp7 = biomass_cv_sp7_10p, sim10p_sp8 = biomass_cv_sp8_10p,
           sim20p_sp0 = biomass_cv_sp0_20p, sim20p_sp1 = biomass_cv_sp1_20p, sim20p_sp2 = biomass_cv_sp2_20p, sim20p_sp3 = biomass_cv_sp3_20p, sim20p_sp4 = biomass_cv_sp4_20p, sim20p_sp5 = biomass_cv_sp5_20p, sim20p_sp6 = biomass_cv_sp6_20p, sim20p_sp7 = biomass_cv_sp7_20p, sim20p_sp8 = biomass_cv_sp8_20p,
           sim30p_sp0 = biomass_cv_sp0_30p, sim30p_sp1 = biomass_cv_sp1_30p, sim30p_sp2 = biomass_cv_sp2_30p, sim30p_sp3 = biomass_cv_sp3_30p, sim30p_sp4 = biomass_cv_sp4_30p, sim30p_sp5 = biomass_cv_sp5_30p, sim30p_sp6 = biomass_cv_sp6_30p, sim30p_sp7 = biomass_cv_sp7_30p, sim30p_sp8 = biomass_cv_sp8_30p,
           sp = 7, titlePlot = "UNCERTAINTY ON MUNIDA"        , xlim = c(-0.1,100), lwd = 2, speciesName = speciesComplete)

figure1_cv(sim10p_sp0 = biomass_cv_sp0_10p, sim10p_sp1 = biomass_cv_sp1_10p, sim10p_sp2 = biomass_cv_sp2_10p, sim10p_sp3 = biomass_cv_sp3_10p, sim10p_sp4 = biomass_cv_sp4_10p, sim10p_sp5 = biomass_cv_sp5_10p, sim10p_sp6 = biomass_cv_sp6_10p, sim10p_sp7 = biomass_cv_sp7_10p, sim10p_sp8 = biomass_cv_sp8_10p,
           sim20p_sp0 = biomass_cv_sp0_20p, sim20p_sp1 = biomass_cv_sp1_20p, sim20p_sp2 = biomass_cv_sp2_20p, sim20p_sp3 = biomass_cv_sp3_20p, sim20p_sp4 = biomass_cv_sp4_20p, sim20p_sp5 = biomass_cv_sp5_20p, sim20p_sp6 = biomass_cv_sp6_20p, sim20p_sp7 = biomass_cv_sp7_20p, sim20p_sp8 = biomass_cv_sp8_20p,
           sim30p_sp0 = biomass_cv_sp0_30p, sim30p_sp1 = biomass_cv_sp1_30p, sim30p_sp2 = biomass_cv_sp2_30p, sim30p_sp3 = biomass_cv_sp3_30p, sim30p_sp4 = biomass_cv_sp4_30p, sim30p_sp5 = biomass_cv_sp5_30p, sim30p_sp6 = biomass_cv_sp6_30p, sim30p_sp7 = biomass_cv_sp7_30p, sim30p_sp8 = biomass_cv_sp8_30p,
           sp = 8, titlePlot = "UNCERTAINTY ON HUMBOLDT SQUID", xlim = c(-0.1,200), lwd = 2, speciesName = speciesAbbre)

figure1_cv(sim10p_sp0 = biomass_cv_sp0_10p, sim10p_sp1 = biomass_cv_sp1_10p, sim10p_sp2 = biomass_cv_sp2_10p, sim10p_sp3 = biomass_cv_sp3_10p, sim10p_sp4 = biomass_cv_sp4_10p, sim10p_sp5 = biomass_cv_sp5_10p, sim10p_sp6 = biomass_cv_sp6_10p, sim10p_sp7 = biomass_cv_sp7_10p, sim10p_sp8 = biomass_cv_sp8_10p,
           sim20p_sp0 = biomass_cv_sp0_20p, sim20p_sp1 = biomass_cv_sp1_20p, sim20p_sp2 = biomass_cv_sp2_20p, sim20p_sp3 = biomass_cv_sp3_20p, sim20p_sp4 = biomass_cv_sp4_20p, sim20p_sp5 = biomass_cv_sp5_20p, sim20p_sp6 = biomass_cv_sp6_20p, sim20p_sp7 = biomass_cv_sp7_20p, sim20p_sp8 = biomass_cv_sp8_20p,
           sim30p_sp0 = biomass_cv_sp0_30p, sim30p_sp1 = biomass_cv_sp1_30p, sim30p_sp2 = biomass_cv_sp2_30p, sim30p_sp3 = biomass_cv_sp3_30p, sim30p_sp4 = biomass_cv_sp4_30p, sim30p_sp5 = biomass_cv_sp5_30p, sim30p_sp6 = biomass_cv_sp6_30p, sim30p_sp7 = biomass_cv_sp7_30p, sim30p_sp8 = biomass_cv_sp8_30p,
           sp = 9, titlePlot = "UNCERTAINTY ON EUPHAUSIIDS"   , xlim = c(-0.1,300), lwd = 2, speciesName = speciesAbbre)

mtext(text = "COEFFICIENT OF VARIATION OF BIOMASS" , side = 1, cex = 1.5  , line = 1.2, adj = 0.5, outer = TRUE)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", cex = 0.8)

legend("bottomright", legend = c("SCENARIO 10%", "SCENARIO 20%", "SCENARIO 30%"), bty = "n",
       fill = c("white", "gray77", "gray30"), border = "black", cex = 0.9, xpd = TRUE)


dev.off()
