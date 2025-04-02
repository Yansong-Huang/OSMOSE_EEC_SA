

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


# Figure2A ----------------------------------------------------------------
# Boxplot of ecosystem indicators in one panel - using relative change

figure2_rc = function(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
                      sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
                      sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
                      ind, var, titlePlot, lim = NULL, axis1 = TRUE, axis2 = TRUE, cex.point = 1, speciesName, ...){
  
  if(var != "slopeSizeSpectrum"){
    
    toPlot_sim10p_sp0 = apply(sim10p_sp0[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim10p_sp1 = apply(sim10p_sp1[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim10p_sp2 = apply(sim10p_sp2[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim10p_sp3 = apply(sim10p_sp3[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim10p_sp4 = apply(sim10p_sp4[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim10p_sp5 = apply(sim10p_sp5[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim10p_sp6 = apply(sim10p_sp6[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim10p_sp7 = apply(sim10p_sp7[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim10p_sp8 = apply(sim10p_sp8[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    
    toPlot_sim20p_sp0 = apply(sim20p_sp0[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim20p_sp1 = apply(sim20p_sp1[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim20p_sp2 = apply(sim20p_sp2[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim20p_sp3 = apply(sim20p_sp3[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim20p_sp4 = apply(sim20p_sp4[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim20p_sp5 = apply(sim20p_sp5[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim20p_sp6 = apply(sim20p_sp6[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim20p_sp7 = apply(sim20p_sp7[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim20p_sp8 = apply(sim20p_sp8[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    
    toPlot_sim30p_sp0 = apply(sim30p_sp0[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim30p_sp1 = apply(sim30p_sp1[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim30p_sp2 = apply(sim30p_sp2[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim30p_sp3 = apply(sim30p_sp3[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim30p_sp4 = apply(sim30p_sp4[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim30p_sp5 = apply(sim30p_sp5[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim30p_sp6 = apply(sim30p_sp6[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim30p_sp7 = apply(sim30p_sp7[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    toPlot_sim30p_sp8 = apply(sim30p_sp8[[ind]][[var]], MARGIN = 2, mean, na.rm = TRUE)
    
  } else {
    
    toPlot_sim10p_sp0 = sim10p_sp0[[ind]][[var]]
    toPlot_sim10p_sp1 = sim10p_sp1[[ind]][[var]]
    toPlot_sim10p_sp2 = sim10p_sp2[[ind]][[var]]
    toPlot_sim10p_sp3 = sim10p_sp3[[ind]][[var]]
    toPlot_sim10p_sp4 = sim10p_sp4[[ind]][[var]]
    toPlot_sim10p_sp5 = sim10p_sp5[[ind]][[var]]
    toPlot_sim10p_sp6 = sim10p_sp6[[ind]][[var]]
    toPlot_sim10p_sp7 = sim10p_sp7[[ind]][[var]]
    toPlot_sim10p_sp8 = sim10p_sp8[[ind]][[var]]
    
    toPlot_sim20p_sp0 = sim20p_sp0[[ind]][[var]]
    toPlot_sim20p_sp1 = sim20p_sp1[[ind]][[var]]
    toPlot_sim20p_sp2 = sim20p_sp2[[ind]][[var]]
    toPlot_sim20p_sp3 = sim20p_sp3[[ind]][[var]]
    toPlot_sim20p_sp4 = sim20p_sp4[[ind]][[var]]
    toPlot_sim20p_sp5 = sim20p_sp5[[ind]][[var]]
    toPlot_sim20p_sp6 = sim20p_sp6[[ind]][[var]]
    toPlot_sim20p_sp7 = sim20p_sp7[[ind]][[var]]
    toPlot_sim20p_sp8 = sim20p_sp8[[ind]][[var]]
    
    toPlot_sim30p_sp0 = sim30p_sp0[[ind]][[var]]
    toPlot_sim30p_sp1 = sim30p_sp1[[ind]][[var]]
    toPlot_sim30p_sp2 = sim30p_sp2[[ind]][[var]]
    toPlot_sim30p_sp3 = sim30p_sp3[[ind]][[var]]
    toPlot_sim30p_sp4 = sim30p_sp4[[ind]][[var]]
    toPlot_sim30p_sp5 = sim30p_sp5[[ind]][[var]]
    toPlot_sim30p_sp6 = sim30p_sp6[[ind]][[var]]
    toPlot_sim30p_sp7 = sim30p_sp7[[ind]][[var]]
    toPlot_sim30p_sp8 = sim30p_sp8[[ind]][[var]]
  }
  
  if(is.null(lim)){lim = c(-100,100)} else { lim = lim }
  
  distance = (27:1) + 0.17*c(-0.7, 0, 0.7)
  
  boxplot(toPlot_sim10p_sp0*100, at = distance[1] , outline = FALSE, axes = FALSE, ylim = lim, col = "white", xlim = c(0.5,27.5), horizontal = TRUE)
  boxplot(toPlot_sim20p_sp0*100, at = distance[2] , outline = FALSE, axes = FALSE, add = TRUE, col = "gray77", horizontal = TRUE)
  boxplot(toPlot_sim30p_sp0*100, at = distance[3] , outline = FALSE, axes = FALSE, add = TRUE, col = "gray30", horizontal = TRUE)
  
  boxplot(toPlot_sim10p_sp1*100, at = distance[4] , outline = FALSE, axes = FALSE, add = TRUE, col = "white", horizontal = TRUE)
  boxplot(toPlot_sim20p_sp1*100, at = distance[5] , outline = FALSE, axes = FALSE, add = TRUE, col = "gray77", horizontal = TRUE)
  boxplot(toPlot_sim30p_sp1*100, at = distance[6] , outline = FALSE, axes = FALSE, add = TRUE, col = "gray30", horizontal = TRUE)
  
  boxplot(toPlot_sim10p_sp2*100, at = distance[7] , outline = FALSE, axes = FALSE, add = TRUE, col = "white", horizontal = TRUE)
  boxplot(toPlot_sim20p_sp2*100, at = distance[8] , outline = FALSE, axes = FALSE, add = TRUE, col = "gray77", horizontal = TRUE)
  boxplot(toPlot_sim30p_sp2*100, at = distance[9] , outline = FALSE, axes = FALSE, add = TRUE, col = "gray30", horizontal = TRUE)
  
  boxplot(toPlot_sim10p_sp3*100, at = distance[10], outline = FALSE, axes = FALSE, add = TRUE, col = "white", horizontal = TRUE)
  boxplot(toPlot_sim20p_sp3*100, at = distance[11], outline = FALSE, axes = FALSE, add = TRUE, col = "gray77", horizontal = TRUE)
  boxplot(toPlot_sim30p_sp3*100, at = distance[12], outline = FALSE, axes = FALSE, add = TRUE, col = "gray30", horizontal = TRUE)
  
  boxplot(toPlot_sim10p_sp4*100, at = distance[13], outline = FALSE, axes = FALSE, add = TRUE, col = "white", horizontal = TRUE)
  boxplot(toPlot_sim20p_sp4*100, at = distance[14], outline = FALSE, axes = FALSE, add = TRUE, col = "gray77", horizontal = TRUE)
  boxplot(toPlot_sim30p_sp4*100, at = distance[15], outline = FALSE, axes = FALSE, add = TRUE, col = "gray30", horizontal = TRUE)
  
  boxplot(toPlot_sim10p_sp5*100, at = distance[16], outline = FALSE, axes = FALSE, add = TRUE, col = "white", horizontal = TRUE)
  boxplot(toPlot_sim20p_sp5*100, at = distance[17], outline = FALSE, axes = FALSE, add = TRUE, col = "gray77", horizontal = TRUE)
  boxplot(toPlot_sim30p_sp5*100, at = distance[18], outline = FALSE, axes = FALSE, add = TRUE, col = "gray30", horizontal = TRUE)
  
  boxplot(toPlot_sim10p_sp6*100, at = distance[19], outline = FALSE, axes = FALSE, add = TRUE, col = "white", horizontal = TRUE)
  boxplot(toPlot_sim20p_sp6*100, at = distance[20], outline = FALSE, axes = FALSE, add = TRUE, col = "gray77", horizontal = TRUE)
  boxplot(toPlot_sim30p_sp6*100, at = distance[21], outline = FALSE, axes = FALSE, add = TRUE, col = "gray30", horizontal = TRUE)
  
  boxplot(toPlot_sim10p_sp7*100, at = distance[22], outline = FALSE, axes = FALSE, add = TRUE, col = "white", horizontal = TRUE)
  boxplot(toPlot_sim20p_sp7*100, at = distance[23], outline = FALSE, axes = FALSE, add = TRUE, col = "gray77", horizontal = TRUE)
  boxplot(toPlot_sim30p_sp7*100, at = distance[24], outline = FALSE, axes = FALSE, add = TRUE, col = "gray30", horizontal = TRUE)
  
  boxplot(toPlot_sim10p_sp8*100, at = distance[25], outline = FALSE, axes = FALSE, add = TRUE, col = "white", horizontal = TRUE)
  boxplot(toPlot_sim20p_sp8*100, at = distance[26], outline = FALSE, axes = FALSE, add = TRUE, col = "gray77", horizontal = TRUE)
  boxplot(toPlot_sim30p_sp8*100, at = distance[27], outline = FALSE, axes = FALSE, add = TRUE, col = "gray30", horizontal = TRUE)
  
  abline(v = 0,  col = "red", lty = 2)
  box(...)
  
  if(isTRUE(axis2)){
    axis(2, at = seq(from = 2, by = 3, length.out = 9), labels = speciesName, cex.axis = 0.8, las = 2)
  } else {
    axis(2, at = seq(from = 2, by = 3, length.out = 9), labels = FALSE)
  }
  
  if(isTRUE(axis1)){
    axis1Default = axTicks(1)
    axis(1, at = axis1Default, labels = paste0(axis1Default, "%"))} 
  
  mtext(text = titlePlot, side = 3, cex = 0.8, adj = 0.05)
  
  return(invisible())
  
}

speciesComplete  = rev(c("ANCHOVY", "HAKE", "SARDINE", "JACK MACKEREL", "CHUB MACKEREL",
                         "MESOPELAGICS", "MUNIDA" , "HUMBOLDT SQUID", "EUPHAUSIIDS"))

speciesAbbre     = rev(c("AN", "HA", "SA", "JM", "CM", "ME", "MU", "HS", "EU"))

coloursBox  = c("black", "blue", "green4", "blueviolet", "coral")


png(filename = "paper_results/plots/2.figure2A.png", width = 1200, height = 900, pointsize = 16)
par(mfrow = c(3, 3))
par(cex   = 1)
par(mar   = c(2.5,2.5,1,0), oma = c(3,4,1,0.5))

figure2_rc(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
           sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
           sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
           ind = "relativeChange", var = "meanLength", titlePlot = "UNCERTAINTY ON MEAN LENGTH" ,
           lim = c(-50, 50), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure2_rc(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
           sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
           sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
           ind = "relativeChange", var = "meanTL", titlePlot = "UNCERTAINTY ON MEAN TROPHIC LEVEL" ,
           lim = c(-10, 10), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure2_rc(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
           sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
           sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
           ind = "relativeChange", var = "meanLifespan", titlePlot = "UNCERTAINTY ON MEAN LIFESPAN" ,
           lim = c(-40, 40), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure2_rc(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
           sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
           sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
           ind = "relativeChange", var = "biomassOverYield", titlePlot = "UNCERTAINTY ON BIOMASS OVER YIELD" ,
           lim = c(-500, 500), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure2_rc(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
           sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
           sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
           ind = "relativeChange", var = "mti", titlePlot = "UNCERTAINTY ON MARINE TROPHIC INDEX" ,
           lim = c(-5, 5), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure2_rc(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
           sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
           sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
           ind = "relativeChange", var = "slopeSizeSpectrum", titlePlot = "UNCERTAINTY ON SLOPE SIZES SPECTRUM" ,
           lim = c(-20, 20), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure2_rc(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
           sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
           sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
           ind = "relativeChange", var = "lfi20", titlePlot = "UNCERTAINTY ON LFI20" ,
           lim = c(-350, 350), col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure2_rc(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
           sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
           sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
           ind = "relativeChange", var = "lfi30", titlePlot = "UNCERTAINTY ON LFI30" ,
           lim = c(-650, 650), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure2_rc(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
           sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
           sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
           ind = "relativeChange", var = "lfi40", titlePlot = "UNCERTAINTY ON LFI40" ,
           lim = c(-550, 550), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

mtext(text = "RELATIVE CHANGE (%)" , side = 1, cex = 1.5  , line = 1.2, adj = 0.5, outer = TRUE)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", cex = 0.8)

legend("bottomright", legend = c("SCENARIO 10%", "SCENARIO 20%", "SCENARIO 30%"), bty = "n",
       fill = c("white", "gray77", "gray30"), border = "black", cex = 1, xpd = TRUE)

dev.off()


# Figure2B ----------------------------------------------------------------
# Boxplot of ecosystem indicators in one panel - using coefficient of variation

figure2_cv = function(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
                      sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
                      sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
                      ind, var, titlePlot, xlim = NULL, axis1 = TRUE, axis2 = TRUE, cex.point = 1, speciesName, ...){
  
  toPlot_sim10p_sp0 = mean(sim10p_sp0[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim10p_sp1 = mean(sim10p_sp1[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim10p_sp2 = mean(sim10p_sp2[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim10p_sp3 = mean(sim10p_sp3[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim10p_sp4 = mean(sim10p_sp4[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim10p_sp5 = mean(sim10p_sp5[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim10p_sp6 = mean(sim10p_sp6[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim10p_sp7 = mean(sim10p_sp7[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim10p_sp8 = mean(sim10p_sp8[[ind]][[var]], na.rm = TRUE)*100
  
  toPlot_sim20p_sp0 = mean(sim20p_sp0[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim20p_sp1 = mean(sim20p_sp1[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim20p_sp2 = mean(sim20p_sp2[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim20p_sp3 = mean(sim20p_sp3[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim20p_sp4 = mean(sim20p_sp4[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim20p_sp5 = mean(sim20p_sp5[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim20p_sp6 = mean(sim20p_sp6[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim20p_sp7 = mean(sim20p_sp7[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim20p_sp8 = mean(sim20p_sp8[[ind]][[var]], na.rm = TRUE)*100
  
  toPlot_sim30p_sp0 = mean(sim30p_sp0[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim30p_sp1 = mean(sim30p_sp1[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim30p_sp2 = mean(sim30p_sp2[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim30p_sp3 = mean(sim30p_sp3[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim30p_sp4 = mean(sim30p_sp4[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim30p_sp5 = mean(sim30p_sp5[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim30p_sp6 = mean(sim30p_sp6[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim30p_sp7 = mean(sim30p_sp7[[ind]][[var]], na.rm = TRUE)*100
  toPlot_sim30p_sp8 = mean(sim30p_sp8[[ind]][[var]], na.rm = TRUE)*100
  
  if(is.null(xlim)){xlim = c(-100,100)} else { xlim = xlim }
  
  distance = (27:1) + 0.17*c(-0.7,0,0.7)
  
  plot(1, type="n", xlab="", ylab="", xlim = xlim, ylim = c(0.5,27.5), axes = FALSE)
  abline(v = 0, col = "red", lty = 2)
  
  #abline(v = 10, col = "blue", lty = 3, lwd = 0.8)
  #abline(v = 20, col = "blue", lty = 3, lwd = 0.8)
  #abline(v = 30, col = "blue", lty = 3, lwd = 0.8)
  
  box(...)
  
  if(isTRUE(axis2)){
    axis(2, at = seq(from = 2, by = 3, length.out = 9), labels = speciesName, cex.axis = 0.8, las = 2)
  } else {
    axis(2, at = seq(from = 2, by = 3, length.out = 9), labels = FALSE, cex.axis = 0.9)
  }
  
  if(isTRUE(axis1)){
    axis1Default = axTicks(1)
    axis(1, at = axis1Default, labels = paste0(axis1Default, "%"))} 
  
  mtext(text = titlePlot, side = 3,cex = 0.9, adj = 0.05)
  
  #segments
  segments(0, distance[1 ], toPlot_sim10p_sp0, distance[1 ], col = "black", lty = 2)
  segments(0, distance[2 ], toPlot_sim20p_sp0, distance[2 ], col = "black", lty = 2)
  segments(0, distance[3 ], toPlot_sim30p_sp0, distance[3 ], col = "black", lty = 2)
  segments(0, distance[4 ], toPlot_sim10p_sp1, distance[4 ], col = "black", lty = 2)
  segments(0, distance[5 ], toPlot_sim20p_sp1, distance[5 ], col = "black", lty = 2)
  segments(0, distance[6 ], toPlot_sim30p_sp1, distance[6 ], col = "black", lty = 2)
  segments(0, distance[7 ], toPlot_sim10p_sp2, distance[7 ], col = "black", lty = 2)
  segments(0, distance[8 ], toPlot_sim20p_sp2, distance[8 ], col = "black", lty = 2)
  segments(0, distance[9 ], toPlot_sim30p_sp2, distance[9 ], col = "black", lty = 2)
  segments(0, distance[10], toPlot_sim10p_sp3, distance[10], col = "black", lty = 2)
  segments(0, distance[11], toPlot_sim20p_sp3, distance[11], col = "black", lty = 2)
  segments(0, distance[12], toPlot_sim30p_sp3, distance[12], col = "black", lty = 2)
  segments(0, distance[13], toPlot_sim10p_sp4, distance[13], col = "black", lty = 2)
  segments(0, distance[14], toPlot_sim20p_sp4, distance[14], col = "black", lty = 2)
  segments(0, distance[15], toPlot_sim30p_sp4, distance[15], col = "black", lty = 2)
  segments(0, distance[16], toPlot_sim10p_sp5, distance[16], col = "black", lty = 2)
  segments(0, distance[17], toPlot_sim20p_sp5, distance[17], col = "black", lty = 2)
  segments(0, distance[18], toPlot_sim30p_sp5, distance[18], col = "black", lty = 2)
  segments(0, distance[19], toPlot_sim10p_sp6, distance[19], col = "black", lty = 2)
  segments(0, distance[20], toPlot_sim20p_sp6, distance[20], col = "black", lty = 2)
  segments(0, distance[21], toPlot_sim30p_sp6, distance[21], col = "black", lty = 2)
  segments(0, distance[22], toPlot_sim10p_sp7, distance[22], col = "black", lty = 2)
  segments(0, distance[23], toPlot_sim20p_sp7, distance[23], col = "black", lty = 2)
  segments(0, distance[24], toPlot_sim30p_sp7, distance[24], col = "black", lty = 2)
  segments(0, distance[25], toPlot_sim10p_sp8, distance[25], col = "black", lty = 2)
  segments(0, distance[26], toPlot_sim20p_sp8, distance[26], col = "black", lty = 2)
  segments(0, distance[27], toPlot_sim30p_sp8, distance[27], col = "black", lty = 2)
  
  points(x = toPlot_sim10p_sp0, y = distance[1 ], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "white" , col = "black")
  points(x = toPlot_sim20p_sp0, y = distance[2 ], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray77", col = "black")
  points(x = toPlot_sim30p_sp0, y = distance[3 ], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray30", col = "black")
  points(x = toPlot_sim10p_sp1, y = distance[4 ], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "white" , col = "black")
  points(x = toPlot_sim20p_sp1, y = distance[5 ], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray77", col = "black")
  points(x = toPlot_sim30p_sp1, y = distance[6 ], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray30", col = "black")
  points(x = toPlot_sim10p_sp2, y = distance[7 ], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "white" , col = "black")
  points(x = toPlot_sim20p_sp2, y = distance[8 ], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray77", col = "black")
  points(x = toPlot_sim30p_sp2, y = distance[9 ], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray30", col = "black")
  points(x = toPlot_sim10p_sp3, y = distance[10], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "white" , col = "black")
  points(x = toPlot_sim20p_sp3, y = distance[11], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray77", col = "black")
  points(x = toPlot_sim30p_sp3, y = distance[12], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray30", col = "black")
  points(x = toPlot_sim10p_sp4, y = distance[13], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "white" , col = "black")
  points(x = toPlot_sim20p_sp4, y = distance[14], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray77", col = "black")
  points(x = toPlot_sim30p_sp4, y = distance[15], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray30", col = "black")
  points(x = toPlot_sim10p_sp5, y = distance[16], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "white" , col = "black")
  points(x = toPlot_sim20p_sp5, y = distance[17], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray77", col = "black")
  points(x = toPlot_sim30p_sp5, y = distance[18], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray30", col = "black")
  points(x = toPlot_sim10p_sp6, y = distance[19], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "white" , col = "black")
  points(x = toPlot_sim20p_sp6, y = distance[20], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray77", col = "black")
  points(x = toPlot_sim30p_sp6, y = distance[21], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray30", col = "black")
  points(x = toPlot_sim10p_sp7, y = distance[22], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "white" , col = "black")
  points(x = toPlot_sim20p_sp7, y = distance[23], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray77", col = "black")
  points(x = toPlot_sim30p_sp7, y = distance[24], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray30", col = "black")
  points(x = toPlot_sim10p_sp8, y = distance[25], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "white" , col = "black")
  points(x = toPlot_sim20p_sp8, y = distance[26], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray77", col = "black")
  points(x = toPlot_sim30p_sp8, y = distance[27], type = "p", pch = 21, lwd = 0.9, cex = cex.point, bg = "gray30", col = "black")
  
  return(invisible())
  
}

speciesComplete  = rev(c("ANCHOVY", "HAKE", "SARDINE", "JACK MACKEREL", "CHUB MACKEREL",
                         "MESOPELAGICS", "MUNIDA" , "HUMBOLDT SQUID", "EUPHAUSIIDS"))

speciesAbbre     = rev(c("AN", "HA", "SA", "JM", "CM", "ME", "MU", "HS", "EU"))

coloursBox  = c("black", "blue", "green4", "blueviolet", "coral")


png(filename = "paper_results/plots/2.figure2B.png", width = 1200, height = 1000, pointsize = 16)
par(mfrow = c(3, 3))
par(cex   = 1)
par(mar   = c(2.2,2.5,1.2,0), oma = c(3,5,1,0.5))

figure2_cv(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
           sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
           sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
           ind = "coefficientVariation", var = "meanTL", titlePlot = "UNCERTAINTY ON MEAN TROPHIC LEVEL" ,
           xlim = c(0, 5)        , col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure2_cv(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
           sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
           sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
           ind = "coefficientVariation", var = "mti", titlePlot = "UNCERTAINTY ON MARINE TROPHIC INDEX" ,
           xlim = c(0,5)         , col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure2_cv(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
           sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
           sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
           ind = "coefficientVariation", var = "slopeSizeSpectrum", titlePlot = "UNCERTAINTY ON SLOPE SIZES SPECTRUM" ,
           xlim = c(0,-10), col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure2_cv(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
           sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
           sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
           ind = "coefficientVariation", var = "meanLength", titlePlot = "UNCERTAINTY ON MEAN LENGTH" ,
           xlim = c(0, 40)        , col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure2_cv(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
           sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
           sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
           ind = "coefficientVariation", var = "meanLifespan", titlePlot = "UNCERTAINTY ON MEAN LIFESPAN" ,
           xlim = c(0, 40)        , col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure2_cv(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
           sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
           sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
           ind = "coefficientVariation", var = "biomassOverYield", titlePlot = "UNCERTAINTY ON BIOMASS OVER YIELD" ,
           xlim = c(0, 400)           , col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure2_cv(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
           sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
           sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
           ind = "coefficientVariation", var = "lfi20", titlePlot = "UNCERTAINTY ON LFI20" , 
           xlim = c(0,100)           , col = coloursBox[1], lwd = 2, speciesName = speciesComplete)

figure2_cv(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
           sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
           sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
           ind = "coefficientVariation", var = "lfi30", titlePlot = "UNCERTAINTY ON LFI30" ,
           xlim = c(0,100)           , col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

figure2_cv(sim10p_sp0, sim10p_sp1, sim10p_sp2, sim10p_sp3, sim10p_sp4, sim10p_sp5, sim10p_sp6, sim10p_sp7, sim10p_sp8,
           sim20p_sp0, sim20p_sp1, sim20p_sp2, sim20p_sp3, sim20p_sp4, sim20p_sp5, sim20p_sp6, sim20p_sp7, sim20p_sp8,
           sim30p_sp0, sim30p_sp1, sim30p_sp2, sim30p_sp3, sim30p_sp4, sim30p_sp5, sim30p_sp6, sim30p_sp7, sim30p_sp8,
           ind = "coefficientVariation", var = "lfi40", titlePlot = "UNCERTAINTY ON LFI40" ,
           xlim = c(0,100)           , col = coloursBox[1], lwd = 2, speciesName = speciesAbbre)

mtext(text = "COEFFICIENT OF VARIATION (%)" , side = 1, cex = 1.5  , line = 1.5, adj = 0.5, outer = TRUE)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", cex = 0.8)

legend("bottomright", legend = c("SCENARIO 10%", "SCENARIO 20%", "SCENARIO 30%"), bty = "n",
       fill = c("white", "gray77", "gray30"), border = "black", cex = 0.9, xpd = TRUE)


dev.off()
