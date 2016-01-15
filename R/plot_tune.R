#' Create a response curve for the *Catch Limit Algorithm*
#'
#' @details
#'
#' @param plot1
#' @param plot2
#' @param plot3
#' @param plot4
#' @param set Should be \code{"D"} for development and \code{"R"} for
#'   rehabilitation plots.
#' @param out A character value to save the plot to, no extension necessary.
#'   If \code{NULL} then the plot will print to the screen rather than to the disk.
#' @return A plot is printed to the disk according the the file name specified in \code{out}
#'   or if out is \code{NULL} the plot is printed to the screen.
##' @seealso \code{\link{functionname}}
#' @author Kelli Faye Johnson
#' @export

plot_tune <- function(plot1, plot2, plot3, plot4, trialset, year = 100, out = NULL, ...) {
    num <- 4
    ylim1 <- c(0, ifelse(year == 100, 2.5, 5))
    ylim2 <- c(0, 1.07)
    ylim3 <- c(0, 2)
    ylim4 <- c(0, 0.11)
    labelline <- 1
    labelcex <- 0.8
    titleline <- 2.5
    titlecex <- 1
    xlim <- c(0, 5.5)

data1 <- getsection(plot1, plot2, plot3, plot4, set = "D", trials = trialset, ...)
data1 <- data1[!apply(data1, 1, function(x) all(x == "")), ]
data1 <- data1[, !colnames(data1) %in% c("L5PloS1", "L5PloS1B1.5")]
myrownames <- rownames(data1)
data1 <- apply(data1, 2, as.numeric)
rownames(data1) <- myrownames

data2 <- getsection(plot1, plot2, plot3, plot4, set = "R", trials = trialset, ...)
data2 <- data2[!apply(data2, 1, function(x) all(x == "")), ]
myrownames <- rownames(data2)
data2 <- apply(data2, 2, as.numeric)
rownames(data2) <- myrownames

  if (!is.null(out)) {
    jpeg(paste0(out, ".jpeg"), res = 80, width = 700, height = 200)
  }

par(oma = c(1, 5, 5, .5), las = 1, mar = rep(0.5, 4),
  tck = 0.05, mgp = c(3, 0.1, 0))
nf <- layout(matrix(c(1, 2, 3, 5, 6, 7, 8,
                      1, 2, 4, 5, 6, 7, 9), ncol = 7, nrow = 2, byrow = TRUE))

# Data 1
words <- ifelse(unique(trialset$D) == "F1", "1+", "Mature")
words <- paste(words, year, "years")
errbar(x = 1:num, y = data1[, "L5TC"], yplus = data1[, "MedTC"],
  yminus = data1[, "L5TCB.5"], frame.plot = FALSE, ylim = ylim1,
  ylab = "", xaxt = "n", xlab = "", pch = "-", xlim = xlim, xpd = TRUE)
text(1:num, y = data1[, "L5TC"], rownames(data1))
mtext(side = 3, "Total catch", line = labelline, cex = labelcex)
mtext(side = 3, "Development", line = titleline, cex = titlecex)
mtext(side = 2, words, line = 1.5, las = 0)
errbar(x = 1:num, y = data1[, "L5Plo"], yplus = data1[, "L5Pf"],
  yminus = data1[, "L5PloB1.5"], frame.plot = FALSE, ylim = ylim2,
  ylab = "", xaxt = "n", xlab = "", pch = "-", xlim = xlim)
text(x = 1:num, y = data1[, "L5Plo"], rownames(data1))
mtext(side = 3, "R-Risk", line = labelline, cex = labelcex)

plot(x = 1:num, y = data1[, "MedCC"], frame.plot = FALSE,
  ylim = ylim3, xlim = xlim,
  ylab = "", xaxt = "n", xlab = "", pch = "")
text(x = 1:num, y = data1[, "MedCC"], rownames(data1))
mtext(side = 3, "CC", line = labelline, cex = labelcex)

plot(x = 1:num, y = data1[, "AAV"], frame.plot = FALSE,
  ylim = ylim4, xlim = xlim,
  ylab = "", xaxt = "n", xlab = "", pch = "")
text(x = 1:num, y = data1[, "AAV"], rownames(data1))
abline(h = ylim4[2])
mtext(side = 3, "AAV", line = labelline - 2.35, cex = labelcex)

# Data 2
errbar(x = 1:num, y = data2[, "L5TC"], yplus = data2[, "MedTC"],
  yminus = data2[, "L5TCB.5"], frame.plot = FALSE, ylim = ylim1,
  ylab = "", xaxt = "n", xlab = "", pch = "-", xlim = xlim)
text(x = 1:num, y = data2[, "L5TC"], rownames(data1))
mtext(side = 3, "Total catch", line = labelline, cex = labelcex)
mtext(side = 3, "Rehabilitation", line = titleline, cex = titlecex)

errbar(x = 1:num, y = data2[, "L5PloS1"], yplus = data2[, "L5PloS1"],
  yminus = data2[, "L5PloS1B1.5"], frame.plot = FALSE, ylim = ylim2,
  ylab = "", xaxt = "n", xlab = "", pch = "-", xlim = xlim)
text(x = 1:num, y = data2[, "L5PloS1"], rownames(data1))
mtext(side = 3, "S-Risk", line = labelline, cex = labelcex)

errbar(x = 1:num, y = data2[, "L5Plo"], yplus = data2[, "L5Pf"],
  yminus = data2[, "L5PloB1.5"], frame.plot = FALSE, ylim = ylim2,
  ylab = "", xaxt = "n", xlab = "", pch = "-", xlim = xlim)
text(x = 1:num, y = data2[, "L5Plo"], rownames(data1))
mtext(side = 3, "R-Risk", line = labelline, cex = labelcex)

plot(x = 1:num, y = data2[, "MedCC"], frame.plot = FALSE,
  ylim = ylim3, xlim = xlim,
  ylab = "", xaxt = "n", xlab = "", pch = "")
text(x = 1:num, y = data2[, "MedCC"], rownames(data1))
mtext(side = 3, "CC", line = labelline, cex = labelcex)

plot(x = 1:num, y = data2[, "AAV"], frame.plot = FALSE,
  ylim = ylim4, xlim = xlim,
  ylab = "", xaxt = "n", xlab = "", pch = "")
text(x = 1:num, y = data2[, "AAV"], rownames(data1))
abline(h = ylim4[2])
mtext(side = 3, "AAV", line = labelline - 2.35, cex = labelcex)

if (!is.null(out)) dev.off()

}
