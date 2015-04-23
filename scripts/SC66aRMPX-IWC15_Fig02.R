###############################################################################
###############################################################################
#-----------------------------------------------------------------------------#
####Author     : Kelli Faye Johnson
####Contact    : kellifayejohnson@gmail.com
####Lastupdate : 2015-04-16
####Purpose    : SC66aRMPX-IWC15 Figure 2
####Packages   : akima
####Inputs     :
####Outputs    :
####Remarks    : Character width = 80
#-----------------------------------------------------------------------------#
###############################################################################
###############################################################################

###############################################################################
###############################################################################
#### Step
####
###############################################################################
###############################################################################
my.dim <- 200

###############################################################################
###############################################################################
#### Step
####
###############################################################################
###############################################################################
# install akima
if (!"akima" %in% installed.packages()[, 1]) {
  install.packages("akima", repos = "http://cran.us.r-project.org")
}
library(akima, quietly = TRUE)

# Read in the data
myfiles <- dir("input", pattern = "XY", full.names = TRUE)
if (grepl("all", myfiles[1])) {
  myfiles <- rev(myfiles)
}

xyz <- do.call("cbind",
  mapply(read.table, myfiles, MoreArgs = list(header = FALSE)))
colnames(xyz) <- c(paste0("mat.", c("OPTF", "A", "z", "MSYR", "MSYL")),
 paste0("all.", c("OPTF", "A", "z", "MSYR", "MSYL")))

# Double check that grid was the same
all(xyz[, "mat.A"] == xyz[, "all.A"])
all(xyz[, "mat.z"] == xyz[, "all.z"])

###############################################################################
###############################################################################
#### Step
####
###############################################################################
###############################################################################
par(mfrow = c(1, 2), las = 1, mar = c(4, 4.5, 1, 0.5))

xyzinterp <- interp(xyz[, "mat.MSYR"], xyz[, "mat.MSYL"],
  xyz[, "mat.MSYR"] / xyz[, "all.MSYR"],
  duplicate = "median",
  xo = seq(min(xyz[, "mat.MSYR"]), max(xyz[, "mat.MSYR"]), length = my.dim),
  yo = seq(min(xyz[, "mat.MSYL"]), max(xyz[, "mat.MSYL"]), length = my.dim))

image(xyzinterp, xlab = expression(italic(MSYR[mat])),
  ylab = expression(italic(MSYL[mat])), las = 1,
  col = grey(seq(0.1, 0.95, length = 20)))
contour(xyzinterp, add = TRUE, labcex = 0.8, nlevels = 19)

xyzinterp <- interp(xyz[, "mat.A"], xyz[, "mat.z"],
  xyz[, "mat.MSYR"] / xyz[, "all.MSYR"],
  duplicate = "median",
  xo = unique(xyz[, "mat.A"]),
  yo = unique(xyz[, "mat.z"]))

image(xyzinterp, xlab = expression(italic(A)),
  ylab = expression(italic(z)), las = 1,
  col = grey(seq(0.1, 0.95, length = 20)))
contour(xyzinterp, add = TRUE, labcex = 0.8, nlevels = 10)

###############################################################################
###############################################################################
#### Step
#### End of file
###############################################################################
###############################################################################
