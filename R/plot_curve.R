#' Create a response curve for the *Catch Limit Algorithm*
#'
#' @details
#'
#' @param orig A \code{data.frame} of \code{RESOUT.RRR}
#' @param alt A \code{data.frame} of \code{RESOUT.RRR}
#' @param keep A \code{data.frame} of trials with specific names and attributes.
#' @param out A character value to save the plot to, no extension necessary.
#' @return Nothing is returned to the console, instead a plot is saved to the disk.
##' @seealso \code{\link{functionname}}
#' @author Kelli Faye Johnson
#' @export

plot_curve <- function(plot1, plot2, keep, out) {

  myplot <- function(orig, alt, little = 0.6, big = 0.95, label = "",
    limtc = c(0, 2.5), limpf = c(0, 1), limaa = c(0, 0.8)) {
    num <- dim(orig)[1]
    print(num)
    symb <- LETTERS[1:num]
    xlim <- c(-1, num + 2)

    errbar(x = 1:dim(orig)[1], y = orig[, 2], yplus = orig[, 4], frame.plot = FALSE,
      yminus = orig[, 3], xaxt = "n", ylim = limtc, pch = symb, xlim = xlim)
    axis(2, at = seq(limtc[1], limtc[2], by = .25)[seq(2,11,by = 2)], label = FALSE)
    mtext(side = 3, label, padj = 0, line = 1, cex = big * 0.8)
    mtext(side = 3, "Orig", line = -2, cex = little)
    errbar(x = 1:dim(alt)[1], y = alt[, 2], yplus = alt[, 4], frame.plot = FALSE,
      yminus = alt[, 3], xaxt = "n", ylim = limtc, pch = symb, xlim = xlim, yaxt = "n")
    axis(2, labels = FALSE)
    axis(2, at = seq(limtc[1], limtc[2], by = .25)[seq(2,11,by = 2)], label = FALSE)
    mtext(side = 3, "Alt", line = -2, cex = little)
    text(x = xlim[1], y = limtc[2] * 1.05, "Total catch", cex = big, xpd = NA)

    errbar(x = 1:dim(orig)[1], y = orig[, 6], yplus = orig[, 8], frame.plot = FALSE,
      yminus = orig[, 7], xaxt = "n", ylim = limpf, pch = symb, xlim = xlim)
      mtext(side = 3, "Orig", line = -2, cex = little)
    axis(2, at = seq(limpf[1], limpf[2], by = 0.1)[seq(2,11,by = 2)], label = FALSE)
    errbar(x = 1:dim(alt)[1], y = alt[, 6], yplus = alt[, 8], frame.plot = FALSE,
      yminus = alt[, 7], xaxt = "n", ylim = limpf, pch = symb, xlim = xlim, yaxt = "n")
    axis(2, labels = FALSE)
    axis(2, at = seq(limpf[1], limpf[2], by = 0.1)[seq(2,11,by = 2)], label = FALSE)
    mtext(side = 3, "Alt", line = -2, cex = little)
    text(x = xlim[1], y = limpf[2] * 1.05, "Final size", cex = big, xpd = NA)

    errbar(x = 1:dim(orig)[1], y = orig[, 12], yplus = orig[, 14], frame.plot = FALSE,
      yminus = orig[, 13], xaxt = "n", ylim = limpf, pch = symb, xlim = xlim)
      mtext(side = 3, "Orig", line = -2, cex = little)
    axis(2, at = seq(limpf[1], limpf[2], by = 0.1)[seq(2,11,by = 2)], label = FALSE)
    errbar(x = 1:dim(alt)[1], y = alt[, 12], yplus = alt[, 14], frame.plot = FALSE,
      yminus = alt[, 13], xaxt = "n", ylim = limpf, pch = symb, xlim = xlim, yaxt = "n")
    axis(2, labels = FALSE)
    axis(2, at = seq(limpf[1], limpf[2], by = 0.1)[seq(2,11,by = 2)], label = FALSE)
    mtext(side = 3, "Alt", line = -2, cex = little)
    text(x = xlim[1], y = limpf[2] * 1.05, "Lowest size", cex = big, xpd = NA)

    y <- as.numeric(orig[, 24])
    plot(x = 1:dim(alt)[1], y = y + limaa[2] / 2, ylim = limaa, xaxt = "n", pch = symb,
      frame.plot = FALSE, yaxt = "n", xlim = c(0, dim(orig)[1]))
    y <- as.numeric(as.character(alt[, 24]))
    points(x = 1:dim(orig)[1], y = y, ylim = limaa, xaxt = "n", pch = symb,
      yaxt = "n")
    mtext(side = 3, "Orig", line = -2, cex = little)
    axis(2, at = seq(0, limaa[2], length.out = 9),
      labels = c(0, seq(limaa[2]/8, limaa[2]/2, length.out = 4),
        seq(limaa[2]/8, limaa[2]/2, length.out = 4)))
    mtext(side = 3, "Alt", line = -20, cex = little)
    text(x = num/2, y = limaa[2] * 1.05, "AAV", cex = big, xpd = NA)
    abline(h = limaa[2] / 2, xpd = FALSE)
  }

  keepa <- keep[grepl("^F", keep$name), ]
  plota <- data.frame(plot1[match(keepa$name, plot1$trial), ],
    stringsAsFactors = FALSE)
  plota$AAV <- gsub("[[:punct:]]$|[[:space:]]", "", plota$AAV)
  plotb <- data.frame(plot2[match(keepa$name, plot2$trial), ],
    stringsAsFactors = FALSE)
  plotb$AAV <- gsub("[[:punct:]]$|[[:space:]]", "", plotb$AAV)

  keepb <- keep[grepl("^M", keep$name), ]
  plotc <- data.frame(plot1[match(keepb$name, plot1$trial), ],
    stringsAsFactors = FALSE)
  plotc$AAV <- gsub("[[:punct:]]$|[[:space:]]", "", plotc$AAV)
  plotd <- data.frame(plot2[match(keepb$name, plot2$trial), ],
    stringsAsFactors = FALSE)
  plotd$AAV <- gsub("[[:punct:]]$|[[:space:]]", "", plotd$AAV)

  mars <- rep(0.1, 4)
  jpeg(paste0(out, ".jpeg"), res = 100, width = 1100)
  par(mfrow = c(1, 15), las = 1, mar = mars, oma = c(0.2, 3, 2.5, 0.2),
    tck = 0.05, mgp = c(3, 0.1, 0))
  myplot(plota, plotb, label = "Fecundity")
  plot(0, 0, type = "n", frame.plot = FALSE, xaxt = "n", yaxt = "n")
  myplot(plotc, plotd, label = "Natural Morality")
  dev.off()

}
