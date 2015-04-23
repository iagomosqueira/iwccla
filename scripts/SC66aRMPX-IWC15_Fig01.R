###############################################################################
###############################################################################
#-----------------------------------------------------------------------------#
####Author     : Kelli Faye Johnson
####Contact    : kellifayejohnson@gmail.com
####Lastupdate :
####Purpose    :
####Packages   :
####Inputs     :
####Outputs    :
####Remarks    : Character width = 80
#-----------------------------------------------------------------------------#
###############################################################################
###############################################################################
getz <- function(data, myvar) {
  z <- reshape(data[, c("A", "z", myvar)],
         idvar = "A", timevar = "z", direction = "wide")
  z <- z[, -1]
  return(as.matrix(z))
}

###############################################################################
###############################################################################
#### Step
####
###############################################################################
###############################################################################
dir.xy <- file.path("input")
f1 <- read.table(file.path(dir.xy, "XYallmor_L0.3_R0.02.txt"), header = FALSE)
f2 <- read.table(file.path(dir.xy, "XYmatmor_L0.3_R0.02.txt"), header = FALSE)

colnames(f1) <- c("OPTF", "A", "z", "MSYR", "MSYL")
colnames(f2) <- colnames(f1)

x <- unique(f1$A[order(f1$A)])
y <- unique(f1$z[order(f1$z)])

###############################################################################
###############################################################################
#### Step
####
###############################################################################
###############################################################################
labcex <- 0.8
par(mfrow = c(2, 2), oma = c(4,4,3,3), mar = c(0,0,0,0), las = 1)
contour(x, y, getz(f2, "MSYR"), xlab = "", xaxt = "n",
  labcex = labcex)
mtext(side = 3, expression(italic(MSYR)), line = 0.1)
mtext(side = 2, expression(italic(z)), outer = TRUE, line = 3)
contour(x, y, getz(f2, "MSYL"), axes = FALSE, labcex = labcex)
box()
mtext(side = 3, expression(italic(MSYL)), line = 0.1)
mtext(side = 4, text = "mature", las = 0, line = 0.1)
contour(x, y, getz(f1, "MSYR"), labcex = labcex)
contour(x, y, getz(f1, "MSYL"), ylab = "", yaxt = "n",
  labcex = labcex)
mtext(side = 4, text = "1+", las = 0, line = 0.1)
mtext(side = 1, expression(italic(A)), outer = TRUE, line = 3)
