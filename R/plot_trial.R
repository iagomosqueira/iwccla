plot_trial <- function(data, get, out = NULL) {
  if (!is.null(out)) {
    jpeg(paste0(out, "_Fig_", get, ".jpeg"), res = 100, width = 1100, height = 600)
  }

  final <- unlist(lapply(
      with(subset(data, grepl(get, trial)), split(subset(data, grepl(get, trial)), F)),
    function(x) split(x, x$year)), recursive = FALSE)

    num <- 4
    ylim1 <- c(0, 7)
    ylim2 <- c(0, 1.07)
    ylim3 <- c(0, 3.5)
    ylim4 <- c(0, 0.2)
    labelline <- 1
    labelcex <- 0.8
    titleline <- 2.5
    titlecex <- 1
    xlim <- c(0, 5.5)

  par(oma = c(2, 5, 5, 0), las = 1, mar = rep(0.4, 4),
    tck = 0.05, mgp = c(3, 0.1, 0))
  nf <- layout(
    matrix(c(1, 5, 9,  13, 2, 6, 10, 14,
             1, 5, 9,  17, 2, 6, 10, 18,
             3, 7, 11, 15, 4, 8, 12, 16,
             3, 7, 11, 19, 4, 8, 12, 20),
    ncol = 8, nrow = 4, byrow = TRUE))

  # Some error checks
  test1 <- unlist(sapply(sapply(final, "[", "F"), unique,
    incomparables = c("NA", NA)))
  if (length(test1) != length(final)) stop("Did not subset F approp.")

  # First plot of TC
  for (ind in c(1, 3, 2, 4)) {
    errbar(x = 1:num, y = final[[ind]][, "tcMed"], cap = 0.1,
      yplus = final[[ind]][, "tc95"], yminus = final[[ind]][, "tc5"],
      xaxt = "n", xlab = "", ylab = "", frame.plot = FALSE, pch = "",
      xlim = xlim,
      ylim = ylim1 *
      ifelse(gsub("[A-Z][0-9]\\.", "", names(final))[ind] == 300, 2, 1))
    text(x = 1:num, y = final[[ind]][, "tcMed"], final[[ind]]$id)
    if (ind %in% c(3, 1)) {
      mtext(side = 3, "Total catch", line = labelline, cex = labelcex)
      mtext(side = 3, line = titleline, cex = titlecex,
        ifelse(final[[ind]]$F[1] == "F1", "1+", "Mature"))
    }
    if (ind == 1) {
      mtext(side = 2, las = 0, line = 1.5,
        paste(gsub("[A-Z][0-9]\\.", "", names(final))[ind], "years"))
    }
    if (ind == 2) {
      mtext(side = 2, las = 0, line = 1.5,
        paste(gsub("[A-Z][0-9]\\.", "", names(final))[ind], "years"))
    }
  }
  # Second plot of Depl
  for (ind in c(1, 3, 2, 4)) {
    errbar(x = 1:num, y = final[[ind]][, "pfMed"],
      yplus = final[[ind]][, "pf95"], yminus = final[[ind]][, "pf5"],
      xaxt = "n", xlab = "", ylab = "", frame.plot = FALSE, pch = "",
      xlim = xlim, ylim = ylim2, cap = 0.1)
    # Plot line for AEP
    if (get == "T1-D1" & ind == 3) {
      abline(h = 0.723, lwd = 0.5, lty = 2)
      legend("topleft", lty = 2, legend = "Tune (43 & 51)", bty = "n")
    }
    if (get == "T1-D1" & ind == 2) {
      abline(h = 0.681, lwd = 0.5, lty = 2)
      legend("topleft", lty = 2, legend = "Tune (73 & 54)", bty = "n")
    }

    text(x = 1:num, y = final[[ind]][, "pfMed"], final[[ind]]$id)
    if (ind %in% c(3, 1)) mtext(side = 3, "Final population", line = labelline, cex = labelcex)
  }
  # Third plot of lowest population size
  for (ind in c(1, 3, 2, 4)) {
    errbar(x = 1:num, y = final[[ind]][, "pl10"],
      yplus = final[[ind]][, "pl25"], yminus = final[[ind]][, "pl5"],
      xaxt = "n", xlab = "", ylab = "", frame.plot = FALSE, pch = "",
      xlim = xlim, ylim = ylim2, cap = 0.1)
    text(x = 1:num, y = final[[ind]][, "pl10"], final[[ind]]$id)
    if (ind %in% c(3, 1)) mtext(side = 3, "Lowest population", line = labelline, cex = labelcex)
  }
  # Fourth plot of lowest population size
  for (ind in c(1, 3, 2, 4)) {
    plot(x = 1:num, y = final[[ind]][, "cccMed"],
      xaxt = "n", xlab = "", ylab = "", frame.plot = FALSE, pch = "",
      xlim = xlim, ylim = ylim3)
    text(x = 1:num, y = final[[ind]][, "cccMed"], final[[ind]]$id)
    if (ind %in% c(3, 1)) mtext(side = 3, "CC", line = labelline, cex = labelcex)
  }
  # Fifth plot of lowest population size
  for (ind in c(1, 3, 2, 4)) {
    plot(x = 1:num, y = final[[ind]][, "AAV"],
      xaxt = "n", xlab = "", ylab = "", frame.plot = FALSE, pch = "",
      xlim = xlim, ylim = ylim4)
    text(x = 1:num, y = final[[ind]][, "AAV"], final[[ind]]$id)
    if (ind %in% c(3, 1)) mtext(side = 3, "AAV", line = labelline - 2.35, cex = labelcex)
    abline(h = ylim4[2])
  }
  mtext(side = 2, get, outer = TRUE, cex = titlecex, line = 2.5, las = 0)
  if (!is.null(out)) {
    dev.off()
  }
}