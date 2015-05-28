###############################################################################
###############################################################################
#-----------------------------------------------------------------------------#
####Author     : Kelli Faye Johnson
####Contact    : kellifayejohnson@gmail.com
####Lastupdate :
####Purpose    : Key for plot_tune.R
####Packages   :
####Inputs     :
####Outputs    :
####Remarks    : Character width = 80
#-----------------------------------------------------------------------------#
###############################################################################
###############################################################################

    num <- 4
    ylim1 <- c(0, 4)
    ylim2 <- c(0, 1.07)
    ylim3 <- c(0, 2)
    ylim4 <- c(0, 0.15)
    labelline <- 1
    labelcex <- 0.8
    titleline <- 2.5
    titlecex <- 1
    xlim <- c(0, 5.5)

jpeg("SC66aRMP_plot_tune.jpeg", res = 100, width = 900, height = 260)
par(oma = c(2, 2, 5, 2), las = 1, mar = rep(0.5, 4),
  tck = 0.05, mgp = c(3, 0.1, 0))
nf <- layout(matrix(c(1, 3, 6, 2, 5, 4, 8,
                      1, 3, 7, 2, 5, 4, 9), ncol = 7, nrow = 2, byrow = TRUE))

# 1 & 2
for (count in 1:2){
errbar(x = 0.9, y = 2, yplus = 3, yminus = 1, frame.plot = FALSE, ylim = ylim1,
  ylab = "", xaxt = "n", xlab = "", pch = "-", xlim = xlim)
if (count == 1) text(3, y = 3, "TC (0.4D1 + \n0.4D4 + 0.2D7)")
if (count == 2) text(3, y = 3, "TC (0.4R1 + \n0.4R4 + 0.2R7)")
text(3, y = 2, "Low 5%\n TC")
text(3, y = 1, "Bias = 0.5 \nLow 5% TC")
mtext(side = 3, "Total catch", line = labelline, cex = labelcex)
if (count == 1) mtext(side = 3, "Development", line = titleline, cex = titlecex)
if (count == 2) mtext(side = 3, "Rehabilitation", line = titleline, cex = titlecex)
}

# 3 & 4
for (count in 1:2) {
errbar(x = 1.2, y = 0.6, yplus = 0.9, yminus = 0.25,
  frame.plot = FALSE, ylim = ylim2,
  ylab = "", xaxt = "n", xlab = "-", pch = "-", xlim = xlim)
text(x = 3, y = 0.9, "Low 5% \nP fin")
text(x = 3, y = 0.6, "Low 5% \nP min")
text(x = 3, y = 0.25, "Bias = 1.5 \n Low 5% \nP min")
mtext(side = 3, "R-Risk", line = labelline, cex = labelcex)
}

# 5
errbar(x = 1.1, y = 0.75, yplus = 0.75,
  yminus = 0.25, frame.plot = FALSE, ylim = ylim2,
  ylab = "", xaxt = "n", xlab = "", pch = "", xlim = xlim)
text(x = 3, y = 0.75, "S1\nLow 5% \nP min")
text(x = 3, y = 0.25, "S1 Bias = 1.5\n Low 5% \nP min")
mtext(side = 3, "S-Risk", line = labelline, cex = labelcex)

# 6 & 7
for (count in 1:2){
plot(x = 3, y = 1, frame.plot = FALSE, ylim = ylim3, xlim = xlim,
  ylab = "", xaxt = "n", xlab = "", pch = "")
if (count == 1) text(x = 3, y = 1, "CC (0.4D1 + \n0.4D4 + 0.2D7)")
if (count == 2) text(x = 3, y = 1, "CC (0.4R1 + \n0.4R4 + 0.2R7)")
mtext(side = 3, "Cont catch", line = labelline, cex = labelcex)

# 8 & 9
plot(x = 3, y = 0.05, frame.plot = FALSE,
  ylim = ylim4, xlim = xlim,
  ylab = "", xaxt = "n", xlab = "", pch = "")
if (count == 1) text(x = 3, y = 0.05, "Mean AAV \n(D1, D4, D7)")
if (count == 2) text(x = 3, y = 0.05, "Mean AAV \n(R1, R4, R7)")
abline(h = ylim4[2])
mtext(side = 3, "AAV", line = labelline - 2.35, cex = labelcex)
}
dev.off()

