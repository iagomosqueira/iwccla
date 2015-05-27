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

plot_tune <- function(plot1, plot2, plot3, plot4, set, out = NULL) {
    num <- 4
    ylim1 <- c(0, 4)
    ylim2 <- c(0, 1.07)
    ylim3 <- c(0, 2)
    ylim4 <- c(0, 0.5)
    labelline <- 1
    labelcex <- 0.8
    titleline <- 2.5
    titlecex <- 1
    xlim <- c(0, 5.5)

data1 <- getsection(plot1, plot2, plot3, plot4, set = "D", trials = set)[, -1]
data1 <- data1[!apply(data1, 1, function(x) all(x == "")), ]
data1 <- data1[, !colnames(data1) %in% c("L5%PloS1", "L5%PloS1B1.5")]
data1 <- apply(data1, 2, as.numeric)

data2 <- getsection(plot1, plot2, plot3, plot4, set = "R", trials = set)[, -1]
data2 <- data2[!apply(data2, 1, function(x) all(x == "")), ]
data2 <- apply(data2, 2, as.numeric)

  if (!is.null(out)) {
    jpeg(paste0(out, ".jpeg"), res = 100, width = 900, height = 260)
  }

par(oma = c(2, 2, 5, 2), las = 1, mar = rep(0, 4))
nf <- layout(matrix(c(1, 2, 3, 5, 6, 7, 8,
                      1, 2, 4, 5, 6, 7, 9), ncol = 7, nrow = 2, byrow = TRUE))

# Data 1
errbar(x = 1:num, y = data1[, "L5%TC"], yplus = data1[, "MedTC"],
  yminus = data1[, "L5%TCBias0.5"], frame.plot = FALSE, ylim = ylim1,
  ylab = "", xaxt = "n", xlab = "", pch = LETTERS[1:num], xlim = xlim)
mtext(side = 3, "Total catch", line = labelline, cex = labelcex)
mtext(side = 3, "Development", line = titleline, cex = titlecex)

errbar(x = 1:num, y = data1[, "L5%Plo"], yplus = data1[, "L5%Pf"],
  yminus = data1[, "L5%PlowB1.5"], frame.plot = FALSE, ylim = ylim2,
  ylab = "", xaxt = "n", xlab = "", pch = LETTERS[1:num], xlim = xlim)
mtext(side = 3, "R-Risk", line = labelline, cex = labelcex)

plot(x = 1:num, y = data1[, "MedCC"], frame.plot = FALSE,
  ylim = ylim3, xlim = xlim,
  ylab = "", xaxt = "n", xlab = "", pch = LETTERS[1:num], )
mtext(side = 3, "Cont catch", line = labelline, cex = labelcex)

plot(x = 1:num, y = data1[, "AAV"], frame.plot = FALSE,
  ylim = ylim4, xlim = xlim,
  ylab = "", xaxt = "n", xlab = "", pch = LETTERS[1:num], )
abline(h = ylim4[2])
mtext(side = 3, "AAV", line = labelline - 2.35, cex = labelcex)

# Data 2
errbar(x = 1:num, y = data2[, "L5%TC"], yplus = data2[, "MedTC"],
  yminus = data2[, "L5%TCBias0.5"], frame.plot = FALSE, ylim = ylim1,
  ylab = "", xaxt = "n", xlab = "", pch = LETTERS[1:num], xlim = xlim)
mtext(side = 3, "Total catch", line = labelline, cex = labelcex)
mtext(side = 3, "Rehabilitation", line = titleline, cex = titlecex)

errbar(x = 1:num, y = data2[, "L5%PloS1"], yplus = data2[, "L5%PloS1"],
  yminus = data2[, "L5%PloS1B1.5"], frame.plot = FALSE, ylim = ylim2,
  ylab = "", xaxt = "n", xlab = "", pch = LETTERS[1:num], xlim = xlim)
mtext(side = 3, "S-Risk", line = labelline, cex = labelcex)

errbar(x = 1:num, y = data2[, "L5%Plo"], yplus = data2[, "L5%Pf"],
  yminus = data2[, "L5%PlowB1.5"], frame.plot = FALSE, ylim = ylim2,
  ylab = "", xaxt = "n", xlab = "", pch = LETTERS[1:num], xlim = xlim)
mtext(side = 3, "R-Risk", line = labelline, cex = labelcex)

plot(x = 1:num, y = data2[, "MedCC"], frame.plot = FALSE,
  ylim = ylim3, xlim = xlim,
  ylab = "", xaxt = "n", xlab = "", pch = LETTERS[1:num], )
mtext(side = 3, "Cont catch", line = labelline, cex = labelcex)

plot(x = 1:num, y = data2[, "AAV"], frame.plot = FALSE,
  ylim = ylim4, xlim = xlim,
  ylab = "", xaxt = "n", xlab = "", pch = LETTERS[1:num], )
abline(h = ylim4[2])
mtext(side = 3, "AAV", line = labelline - 2.35, cex = labelcex)

if (!is.null(out)) dev.off()

}
