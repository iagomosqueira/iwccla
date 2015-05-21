#' Create a response curve for the *Catch Limit Algorithm*
#'
#' @details
#'
#' @param orig A \code{data.frame} of \code{RESOUT.RRR}
#' @param alt A \code{data.frame} of \code{RESOUT.RRR}
#' @param set A \code{data.frame} of trials with specific names and attributes.
#' @param out A character value to save the plot to, no extension necessary.
#' @return Nothing is returned to the console, instead a plot is saved to the disk.
##' @seealso \code{\link{functionname}}
#' @author Kelli Faye Johnson
#' @export

plot_curve <- function(plot1, plot2, plot3, plot4, set, out, part = 1) {

  myplot <- function(orig, alt, little = 0.6, big = 0.95, label = "",
    limtc = c(0, 2.5), limpf = c(0, 1), limaa = c(0, 0.8)) {
    num <- dim(orig)[1]
    numa <- dim(alt)[1]
    symb <- LETTERS[1:num]
    xlim <- c(-1, num + 2)
    hdmlt <- 1.08

    axis1 <- seq(limtc[1], limtc[2], by = 0.25)
    axis1 <- axis1[seq(2, length(axis1), by = 2)]
    axis2 <- seq(limpf[1], limpf[2], by = 0.1)
    axis2 <- axis2[seq(2, length(axis2), by = 2)]

    littlelabtxt <- c("Orig", "Alt")
    if (part == 2) littlelabtxt <- c("0.01", "0.04")
    littlelab <- -1.1

    errbar(x = 1:num, y = orig[, 2], yplus = orig[, 4], frame.plot = FALSE,
      yminus = orig[, 3], xaxt = "n", ylim = limtc, pch = symb, xlim = xlim)
    axis(2, at = axis1, label = FALSE)
    mtext(side = 3, label, padj = 0, line = 1.1, cex = big * 0.8)
    mtext(side = 3, littlelabtxt[1], littlelab, cex = little)
    errbar(x = 1:numa, y = alt[, 2], yplus = alt[, 4], frame.plot = FALSE,
      yminus = alt[, 3], xaxt = "n", ylim = limtc, pch = symb, xlim = xlim, yaxt = "n")
    axis(2, labels = FALSE); axis(2, axis1, label = FALSE)
    mtext(side = 3, littlelabtxt[2], littlelab, cex = little)
    text(x = xlim[1], y = limtc[2] * hdmlt, "Total catch", cex = big, xpd = NA)

    errbar(x = 1:num, y = orig[, 6], yplus = orig[, 8], frame.plot = FALSE,
      yminus = orig[, 7], xaxt = "n", ylim = limpf, pch = symb, xlim = xlim)
      mtext(side = 3, littlelabtxt[1], littlelab, cex = little)
    axis(2, at = axis2, label = FALSE)
    errbar(x = 1:numa, y = alt[, 6], yplus = alt[, 8], frame.plot = FALSE,
      yminus = alt[, 7], xaxt = "n", ylim = limpf, pch = symb, xlim = xlim, yaxt = "n")
    axis(2, labels = FALSE); axis(2, at = axis2, label = FALSE)
    mtext(side = 3, littlelabtxt[2], littlelab, cex = little)
    text(x = xlim[1], y = limpf[2] * hdmlt, "Final size", cex = big, xpd = NA)

    errbar(x = 1:num, y = orig[, 12], yplus = orig[, 14], frame.plot = FALSE,
      yminus = orig[, 13], xaxt = "n", ylim = limpf, pch = symb, xlim = xlim)
      mtext(side = 3, littlelabtxt[1], littlelab, cex = little)
    axis(2, at = axis2, label = FALSE)
    errbar(x = 1:numa, y = alt[, 12], yplus = alt[, 14], frame.plot = FALSE,
      yminus = alt[, 13], xaxt = "n", ylim = limpf, pch = symb, xlim = xlim, yaxt = "n")
    axis(2, labels = FALSE); axis(2, at = axis2, label = FALSE)
    mtext(side = 3, littlelabtxt[2], littlelab, cex = little)
    text(x = xlim[1], y = limpf[2] * hdmlt, "Lowest size", cex = big, xpd = NA)

    y <- as.numeric(orig[, 24])
    plot(x = 1:numa, y = y + limaa[2] / 2, ylim = limaa, xaxt = "n", pch = symb,
      frame.plot = FALSE, yaxt = "n", xlim = c(0, num))
    y <- as.numeric(as.character(alt[, 24]))
    points(x = 1:num, y = y, ylim = limaa, xaxt = "n", pch = symb, yaxt = "n")
    mtext(side = 3, littlelabtxt[1], littlelab, cex = little)
    axis(2, at = seq(0, limaa[2], length.out = 9),
      labels = c(0, seq(limaa[2]/8, limaa[2]/2, length.out = 4),
        seq(limaa[2]/8, limaa[2]/2, length.out = 4)))
    mtext(side = 3, littlelabtxt[2], line = littlelab - 10, cex = little)
    text(x = num/2, y = limaa[2] * hdmlt, "AAV", cex = big, xpd = NA)
    abline(h = limaa[2] / 2, xpd = FALSE)
  }
  mars <- c(1.0, 0.1, 1.0, 0.1)
  if (part == 1) {
    jpeg(paste0(out, ".jpeg"), res = 100, width = 1100, height = 600)
    par(mfrow = c(2, 15), las = 1, mar = mars, oma = c(0.2, 3, 2.5, 0.2),
      tck = 0.05, mgp = c(3, 0.1, 0))
    seta <- set[grepl("^F", set$name), ]
    plota <- data.frame(plot1[match(seta$name, plot1$trial), ],
      stringsAsFactors = FALSE)
    plota$AAV <- gsub("[[:punct:]]$|[[:space:]]", "", plota$AAV)
    plotb <- data.frame(plot2[match(seta$name, plot2$trial), ],
      stringsAsFactors = FALSE)
    plotb$AAV <- gsub("[[:punct:]]$|[[:space:]]", "", plotb$AAV)

    setb <- set[grepl("^M", set$name), ]
    plotc <- data.frame(plot1[match(setb$name, plot1$trial), ],
      stringsAsFactors = FALSE)
    plotc$AAV <- gsub("[[:punct:]]$|[[:space:]]", "", plotc$AAV)
    plotd <- data.frame(plot2[match(setb$name, plot2$trial), ],
      stringsAsFactors = FALSE)
    plotd$AAV <- gsub("[[:punct:]]$|[[:space:]]", "", plotd$AAV)

    myplot(plota, plotb, label = "Fecundity")
    plot(0, 0, type = "n", frame.plot = FALSE, xaxt = "n", yaxt = "n")
    myplot(plotc, plotd, label = "Natural Morality")

    seta <- set[grepl("^F", set$name), ]
    plota <- data.frame(plot3[match(seta$name, plot3$trial), ],
      stringsAsFactors = FALSE)
    plota$AAV <- gsub("[[:punct:]]$|[[:space:]]", "", plota$AAV)
    plotb <- data.frame(plot4[match(seta$name, plot4$trial), ],
      stringsAsFactors = FALSE)
    plotb$AAV <- gsub("[[:punct:]]$|[[:space:]]", "", plotb$AAV)

    setb <- set[grepl("^M", set$name), ]
    plotc <- data.frame(plot3[match(setb$name, plot3$trial), ],
      stringsAsFactors = FALSE)
    plotc$AAV <- gsub("[[:punct:]]$|[[:space:]]", "", plotc$AAV)
    plotd <- data.frame(plot4[match(setb$name, plot4$trial), ],
      stringsAsFactors = FALSE)
    plotd$AAV <- gsub("[[:punct:]]$|[[:space:]]", "", plotd$AAV)

    myplot(plota, plotb, label = "", limtc = c(0, 7.5))

    myplot(plotc, plotd, label = "", limtc = c(0, 7.5))

    dev.off()
  }

  if (part == 2) {
    jpeg(paste0(out, ".jpeg"), res = 100, width = 1100, height = 300)
    par(mfrow = c(1, 15), las = 1, mar = mars, oma = c(0.2, 3, 2.5, 0.2),
      tck = 0.05, mgp = c(3, 0.1, 0))
    all <- rbind(plot1, plot2, plot3, plot4)
    all$AAV <- gsub("[[:punct:]]$|[[:space:]]", "", all$AAV)

    plota <- subset(all, grepl("^F1", trial) & trial %in% keep$name)
    plotb <- subset(all, grepl("^F2", trial) & trial %in% keep$name)
    myplot(plota, plotb, label = "", limtc = c(0, 7.5), label = "Fecundity")

    plotc <- subset(all, grepl("^M1", trial) & trial %in% keep$name)
    plotd <- subset(all, grepl("^M2", trial) & trial %in% keep$name)
    plot(0, 0, type = "n", frame.plot = FALSE, xaxt = "n", yaxt = "n")
    myplot(plotc, plotd, label = "", limtc = c(0, 7.5), label = "Natural Mortality")

    dev.off()
  }


}
