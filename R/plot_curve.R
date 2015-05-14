

plot_curve <- function(orig, alt, keep, out) {
  on.exit(dev.off())
  num <- dim(keep)[1]
  symb <- LETTERS[1:num]
  xlim <- c(-1, num + 2)
  limtc <- c(0, 2.5)
  limpf <- c(0, 1)
  limaa <- c(0, 0.8)
  little <- 0.6
  big <- 0.95

  orig <- data.frame(orig[match(keep$name, orig$trial), ],
    stringsAsFactors = FALSE)
  orig$AAV <- gsub("[[:punct:]]$|[[:space:]]", "", orig$AAV)

  alt <- data.frame(alt[match(keep$name, alt$trial), ],
    stringsAsFactors = FALSE)
  alt$AAV <- gsub("[[:punct:]]$|[[:space:]]", "", alt$AAV)

  # response curve a
  jpeg(paste0(out, ".jpeg"), res = 100, width = 600)
  par(mfrow = c(1, 7), las = 1, mar = rep(0.1, 4), oma = c(0.2, 3, 2.5, 0.2),
    tck = 0.05, mgp = c(3, 0.1, 0))
  errbar(x = 1:dim(orig)[1], y = orig[, 2], yplus = orig[, 4], frame.plot = FALSE,
    yminus = orig[, 3], xaxt = "n", ylim = limtc, pch = symb, xlim = xlim)
  mtext(side = 3, "Orig", line = -2, cex = little)
  errbar(x = 1:dim(alt)[1], y = alt[, 2], yplus = alt[, 4], frame.plot = FALSE,
    yminus = alt[, 3], xaxt = "n", ylim = limtc, pch = symb, xlim = xlim, yaxt = "n")
  axis(2, labels = FALSE)
  mtext(side = 3, "Alt", line = -2, cex = little)
  text(x = xlim[1], y = limtc[2] * 1.05, "Total catch", cex = big, xpd = NA)

  errbar(x = 1:dim(orig)[1], y = orig[, 6], yplus = orig[, 8], frame.plot = FALSE,
    yminus = orig[, 7], xaxt = "n", ylim = limpf, pch = symb, xlim = xlim)
    mtext(side = 3, "Orig", line = -2, cex = little)
  errbar(x = 1:dim(alt)[1], y = alt[, 6], yplus = alt[, 8], frame.plot = FALSE,
    yminus = alt[, 7], xaxt = "n", ylim = limpf, pch = symb, xlim = xlim, yaxt = "n")
  axis(2, labels = FALSE)
  mtext(side = 3, "Alt", line = -2, cex = little)
  text(x = xlim[1], y = limpf[2] * 1.05, "Final size", cex = big, xpd = NA)

  errbar(x = 1:dim(orig)[1], y = orig[, 12], yplus = orig[, 14], frame.plot = FALSE,
    yminus = orig[, 13], xaxt = "n", ylim = limpf, pch = symb, xlim = xlim)
  errbar(x = 1:dim(alt)[1], y = alt[, 12], yplus = alt[, 14], frame.plot = FALSE,
    yminus = alt[, 13], xaxt = "n", ylim = limpf, pch = symb, xlim = xlim, yaxt = "n")
  axis(2, labels = FALSE)
  mtext(side = 3, "Alt", line = -2, cex = little)
  text(x = xlim[1], y = limpf[2] * 1.05, "Lowest size", cex = big, xpd = NA)

  y <- as.numeric(orig[, 24])
  plot(x = 1:dim(orig)[1], y = y, ylim = limaa, xaxt = "n", pch = symb,
    frame.plot = FALSE, yaxt = "n", xlim = c(0, dim(orig)[1]))
  mtext(side = 3, "Orig", line = -2, cex = little)
  y <- as.numeric(as.character(alt[, 24]))
  points(x = 1:dim(alt)[1], y = y + limaa[2] / 2, ylim = limaa, xaxt = "n", pch = symb,
    yaxt = "n")
  axis(2, at = seq(0, limaa[2], length.out = 5),
    labels = c(0, limaa[2]/4, limaa[2]/2, limaa[2]/4, limaa[2]/2))
  mtext(side = 3, outer = FALSE, "AAV", line = -0.25)
  mtext(side = 3, "Alt", line = -20, cex = little)
  text(x = num/2, y = limaa[2] * 1.05, "Lowest size", cex = big, xpd = NA)
  abline(h = limaa[2] / 2, xpd = FALSE)
  dev.off()

}

###############################################################################
###############################################################################
#### Step
#### a
###############################################################################
###############################################################################
keep <- subset(sheet, curve %in% c("a", "all", "ab") & component == 1 & dt == 0 &
    !depl %in% c(0.60, 0.99))
keep <- keep[order(keep$depl), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig01F1"))

keep <- subset(sheet, curve %in% c("a", "all", "ab") & component == 2 & dt == 0 &
  !depl %in% c(0.60, 0.99))
keep <- keep[order(keep$depl), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig01F2"))

keep <- subset(sheet, curve %in% c("a", "all", "ab") & component == 1 & dt == 1 &
    !depl %in% c(0.60, 0.99))
keep <- keep[order(keep$depl), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig01M1"))

keep <- subset(sheet, curve %in% c("a", "all", "ab") & component == 2 & dt == 1 &
  !depl %in% c(0.60, 0.99))
keep <- keep[order(keep$depl), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig01M2"))

###############################################################################
###############################################################################
#### Step
#### b
###############################################################################
###############################################################################
keep <- subset(sheet, curve %in% c("b", "all", "ab") & component == 1 & dt == 0 &
  !depl %in% c(0.3))
keep <- keep[order(keep$depl), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig02F1"))

keep <- subset(sheet, curve %in% c("b", "all", "ab") & component == 2 & dt == 0 &
  !depl %in% c(0.3))
keep <- keep[order(keep$depl), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig02F2"))

keep <- subset(sheet, curve %in% c("b", "all", "ab") & component == 1 & dt == 1 &
  !depl %in% c(0.3))
keep <- keep[order(keep$depl), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig02M1"))

keep <- subset(sheet, curve %in% c("b", "all", "ab") & component == 2 & dt == 1 &
  !depl %in% c(0.3))
keep <- keep[order(keep$depl), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig02M2"))

###############################################################################
###############################################################################
#### Step
#### c
###############################################################################
###############################################################################
keep <- subset(sheet, curve %in% c("c", "all") & component == 1 & dt == 0 &
  depl %in% c(0.3))
ords <- order(keep$cerror)
keep <- keep[c(ords[-1], ords[1]), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig03F13"))

keep <- subset(sheet, curve %in% c("c", "all") & component == 1 & dt == 0 &
  depl %in% c(0.6))
ords <- order(keep$cerror)
keep <- keep[c(ords[-1], ords[1]), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig03F16"))

keep <- subset(sheet, curve %in% c("c", "all") & component == 1 & dt == 0 &
  depl %in% c(0.99))
ords <- order(keep$cerror)
keep <- keep[c(ords[-1], ords[1]), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig03F19"))

keep <- subset(sheet, curve %in% c("c", "all") & component == 2 & dt == 0 &
  depl %in% c(0.3))
ords <- order(keep$cerror)
keep <- keep[c(ords[-1], ords[1]), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig03F23"))

keep <- subset(sheet, curve %in% c("c", "all") & component == 2 & dt == 0 &
  depl %in% c(0.6))
ords <- order(keep$cerror)
keep <- keep[c(ords[-1], ords[1]), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig03F26"))

keep <- subset(sheet, curve %in% c("c", "all") & component == 2 & dt == 0 &
  depl %in% c(0.99))
ords <- order(keep$cerror)
keep <- keep[c(ords[-1], ords[1]), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig03F29"))

keep <- subset(sheet, curve %in% c("c", "all") & component == 1 & dt == 1 &
  depl %in% c(0.3))
ords <- order(keep$cerror)
keep <- keep[c(ords[-1], ords[1]), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig03M13"))

keep <- subset(sheet, curve %in% c("c", "all") & component == 1 & dt == 1 &
  depl %in% c(0.6))
ords <- order(keep$cerror)
keep <- keep[c(ords[-1], ords[1]), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig03M16"))

keep <- subset(sheet, curve %in% c("c", "all") & component == 1 & dt == 1 &
  depl %in% c(0.99))
ords <- order(keep$cerror)
keep <- keep[c(ords[-1], ords[1]), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig03M19"))

keep <- subset(sheet, curve %in% c("c", "all") & component == 2 & dt == 1 &
  depl %in% c(0.3))
ords <- order(keep$cerror)
keep <- keep[c(ords[-1], ords[1]), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig03M23"))

keep <- subset(sheet, curve %in% c("c", "all") & component == 2 & dt == 1 &
  depl %in% c(0.6))
ords <- order(keep$cerror)
keep <- keep[c(ords[-1], ords[1]), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig03M26"))

keep <- subset(sheet, curve %in% c("c", "all") & component == 2 & dt == 1 &
  depl %in% c(0.99))
ords <- order(keep$cerror)
keep <- keep[c(ords[-1], ords[1]), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig03M29"))

###############################################################################
###############################################################################
#### Step
#### c
###############################################################################
###############################################################################
keep <- subset(sheet, curve %in% c("d", "all") & component == 1 & dt == 0 &
  depl %in% c(0.3))
keep <- keep[order(keep$cerror), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig04F13"))

keep <- subset(sheet, curve %in% c("d", "all") & component == 1 & dt == 0 &
  depl %in% c(0.6))
keep <- keep[order(keep$cerror), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig04F16"))

keep <- subset(sheet, curve %in% c("d", "all") & component == 1 & dt == 0 &
  depl %in% c(0.99))
keep <- keep[order(keep$cerror), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig04F19"))

keep <- subset(sheet, curve %in% c("d", "all") & component == 2 & dt == 0 &
  depl %in% c(0.3))
keep <- keep[order(keep$cerror), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig04F23"))

keep <- subset(sheet, curve %in% c("d", "all") & component == 2 & dt == 0 &
  depl %in% c(0.6))
keep <- keep[order(keep$cerror), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig04F26"))

keep <- subset(sheet, curve %in% c("d", "all") & component == 2 & dt == 0 &
  depl %in% c(0.99))
keep <- keep[order(keep$cerror), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig04F29"))

keep <- subset(sheet, curve %in% c("d", "all") & component == 1 & dt == 1 &
  depl %in% c(0.3))
keep <- keep[order(keep$cerror), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig04M13"))

keep <- subset(sheet, curve %in% c("d", "all") & component == 1 & dt == 1 &
  depl %in% c(0.6))
keep <- keep[order(keep$cerror), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig04M16"))

keep <- subset(sheet, curve %in% c("d", "all") & component == 1 & dt == 1 &
  depl %in% c(0.99))
keep <- keep[order(keep$cerror), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig04M19"))

keep <- subset(sheet, curve %in% c("d", "all") & component == 2 & dt == 1 &
  depl %in% c(0.3))
keep <- keep[order(keep$cerror), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig04M23"))

keep <- subset(sheet, curve %in% c("d", "all") & component == 2 & dt == 1 &
  depl %in% c(0.6))
keep <- keep[order(keep$cerror), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig04M26"))

keep <- subset(sheet, curve %in% c("d", "all") & component == 2 & dt == 1 &
  depl %in% c(0.99))
keep <- keep[order(keep$cerror), ]
plot_curve(aep100, ps4100, keep, paste0(paper, "_Fig04M29"))