

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

# Figure ------------------------------------------------------------------

figure3 = function(sim, ind, titlePlot, axis1 = TRUE, axis2 = TRUE, ylim, nino = TRUE, ...){
  
  sim = sim[[ind]]*100
  sim = t(sim)
  quantiles_x = apply(sim, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  mean_x      = apply(sim, 2, mean, na.rm = TRUE) #median
  
  matplot(t(quantiles_x), type = "l", lty = 3, col = "gray", ylab = "", axes = FALSE, ylim = ylim)
  
  if(isTRUE(axis1)){
    axis(1, at = seq(from = 0, by = 12*3, length.out = 6),
         labels = seq(from = 1992, to = 2008, by = 3))}
  
  if(isTRUE(axis2)){
    axis2Default = axTicks(2)
    axis(2, at = axis2Default, labels = paste0(axis2Default, "%"), las = 2)} 
  
  box(...)
  
  x  = seq(from = 1, to = 204, by = 1)
  y1 = quantiles_x[2, ]
  y2 = quantiles_x[1, ]
  
  polygon(x = c(x, rev(x)), y = c(y2, rev(y1)), col = "gray", border = FALSE)
  lines(mean_x, col = "black", lwd = 2)
  
  if(isTRUE(nino)){
    rect(xleft = 60, xright = 84, ybottom = par("usr")[3], ytop = par("usr")[4], #ex:juin1997 - juin1998 (66-78)
         border = NA, col = adjustcolor("azure3", alpha = 0.20))
    
    abline(v = 60, lty = 3, lwd = 0.6, col = "black")
    abline(v = 84, lty = 3, lwd = 0.6, col = "black")
  }
  
  mtext(titlePlot, side = 3, line = -1.2, cex = 0.8, adj = 0.95)
  
  return(invisible())
}

# 20P ---------------------------------------------------------------------


#Ind 2 - LFI40 - 20P
png(filename = "paper_results/plots/3.Fig3_lfi40_20p.png", width = 1200, height = 800, pointsize = 15)
par(mfrow = c(3, 3))
par(cex   = 1)
par(mar   = c(1,3,0,0.9), oma = c(1.5,2.7,2.5,0.5))

figure3(sim = sim20p_sp0$relativeChange, ind = "lfi40", titlePlot = "ANCHOVY"       ,  ylim = c(-80,80)    , axis1 = FALSE, lwd = 2)
figure3(sim = sim20p_sp1$relativeChange, ind = "lfi40", titlePlot = "HAKE"          ,  ylim = c(-100,100)  , axis1 = FALSE, lwd = 2)
figure3(sim = sim20p_sp2$relativeChange, ind = "lfi40", titlePlot = "SARDINE"       ,  ylim = c(-120,120)  , axis1 = FALSE, lwd = 2)
figure3(sim = sim20p_sp3$relativeChange, ind = "lfi40", titlePlot = "JACK MACKEREL" ,  ylim = c(-300,300)  , axis1 = FALSE, lwd = 2)
figure3(sim = sim20p_sp4$relativeChange, ind = "lfi40", titlePlot = "CHUB MACKEREL" ,  ylim = c(-60,60)    , axis1 = FALSE, lwd = 2)
figure3(sim = sim20p_sp5$relativeChange, ind = "lfi40", titlePlot = "MESOPELAGICS"  ,  ylim = c(-120,120)  , axis1 = FALSE, lwd = 2)
figure3(sim = sim20p_sp6$relativeChange, ind = "lfi40", titlePlot = "MUNIDA"        ,  ylim = c(-30,30)    , lwd = 2)
figure3(sim = sim20p_sp7$relativeChange, ind = "lfi40", titlePlot = "HUMBOLDT SQUID",  ylim = c(-3500,3500), lwd = 2)
figure3(sim = sim20p_sp8$relativeChange, ind = "lfi40", titlePlot = "EUPHAUSIIDS"   ,  ylim = c(-200,200)  , lwd = 2)

mtext(text = "LFI40 IN SCENARIO 20%" , side = 3, line = 1, cex = 1.8, outer = TRUE)
mtext(text = "RELATIVE CHANGE (%)"   , side = 2, line = 0.9, cex = 1.5, outer = TRUE)

dev.off()

# 30P ---------------------------------------------------------------------

#Ind 2 - LFI40 - 30P
png(filename = "paper_results/plots/3.Fig3_lfi40_30p.png", width = 1200, height = 800, pointsize = 15)
par(mfrow = c(3, 3))
par(cex   = 1)
par(mar   = c(1,3,0,0.9), oma = c(1.5,2.7,2.5,0.5))

figure3(sim = sim30p_sp0$relativeChange, ind = "lfi40", titlePlot = "ANCHOVY"       ,  ylim = c(-120,120)  , axis1 = FALSE, lwd = 2)
figure3(sim = sim30p_sp1$relativeChange, ind = "lfi40", titlePlot = "HAKE"          ,  ylim = c(-120,120)  , axis1 = FALSE, lwd = 2)
figure3(sim = sim30p_sp2$relativeChange, ind = "lfi40", titlePlot = "SARDINE"       ,  ylim = c(-120,120)  , axis1 = FALSE, lwd = 2)
figure3(sim = sim30p_sp3$relativeChange, ind = "lfi40", titlePlot = "JACK MACKEREL" ,  ylim = c(-300,300)  , axis1 = FALSE, lwd = 2)
figure3(sim = sim30p_sp4$relativeChange, ind = "lfi40", titlePlot = "CHUB MACKEREL" ,  ylim = c(-60,60)    , axis1 = FALSE, lwd = 2)
figure3(sim = sim30p_sp5$relativeChange, ind = "lfi40", titlePlot = "MESOPELAGICS"  ,  ylim = c(-120,120)  , axis1 = FALSE, lwd = 2)
figure3(sim = sim30p_sp6$relativeChange, ind = "lfi40", titlePlot = "MUNIDA"        ,  ylim = c(-20,20)    , lwd = 2)
figure3(sim = sim30p_sp7$relativeChange, ind = "lfi40", titlePlot = "HUMBOLDT SQUID",  ylim = c(-3500,3500), lwd = 2)
figure3(sim = sim30p_sp8$relativeChange, ind = "lfi40", titlePlot = "EUPHAUSIIDS"   ,  ylim = c(-180,180)  , lwd = 2)

mtext(text = "LFI40 IN SCENARIO 30%" , side = 3, line = 1, cex = 1.8, outer = TRUE)
mtext(text = "RELATIVE CHANGE (%)"   , side = 2, line = 0.9, cex = 1.5, outer = TRUE)

dev.off()
