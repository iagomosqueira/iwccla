###############################################################################
###############################################################################
#-----------------------------------------------------------------------------#
####Author     : Kelli Faye Johnson
####Contact    : kellifayejohnson@gmail.com
####Lastupdate : 2015-05-22
####Purpose    : Results for SC66aRMP10
####Packages   :
####Inputs     :
####Outputs    :
####Remarks    : Character width = 80
#-----------------------------------------------------------------------------#
###############################################################################
###############################################################################

###############################################################################
###############################################################################
#### Step
#### Variable inputs
###############################################################################
###############################################################################
paper <- "SC66aRMPWP08"

dir.home <- "c:/iwccla"
dir.rs <- "c:/users/kelli/dropbox/iwccla"
dir.wk <- file.path(dir.home, "ms")

sheet <- read.csv(file.path(dir.home, "Trials_KFJ_tune.csv"), header = TRUE)
sheet <- sheet[order(sheet$dt, sheet$component, sheet$T, sheet$msyr), ]

###############################################################################
###############################################################################
#### Step
#### Attach libraries and set directory
###############################################################################
###############################################################################
library(Hmisc, quietly = TRUE) # plot error bars

setwd(dir.wk)
source(file.path("..", "R", "readRRR2.R"))

###############################################################################
###############################################################################
#### Step
#### Read in results
###############################################################################
###############################################################################
org100 <- readRRR2(file.path(dir.rs, "orig_100",    "RESOUT2.RRR"))
org300 <- readRRR2(file.path(dir.rs, "orig_300",    "RESOUT2.RRR"))
prb100 <- readRRR2(file.path(dir.rs, "prob.7_100",  "RESOUT2.RRR"))
prb300 <- readRRR2(file.path(dir.rs, "prob.7_300",  "RESOUT2.RRR"))
ps1100 <- readRRR2(file.path(dir.rs, "pslope1_100", "RESOUT2.RRR"))
ps1300 <- readRRR2(file.path(dir.rs, "pslope1_300", "RESOUT2.RRR"))
ps4100 <- readRRR2(file.path(dir.rs, "pslope4_100", "RESOUT2.RRR"))
ps4300 <- readRRR2(file.path(dir.rs, "pslope4_300", "RESOUT2.RRR"))

###############################################################################
###############################################################################
#### Step
#### Zeh plots for each trial
###############################################################################
###############################################################################
data <- rbind(
      data.frame("id" = "43", "year" = 100, org100),
      data.frame("id" = "51", "year" = 100, ps1100),
      data.frame("id" = "54", "year" = 100, ps4100),
      data.frame("id" = "43", "year" = 300, org300),
      data.frame("id" = "51", "year" = 300, ps1300),
      data.frame("id" = "54", "year" = 300, ps4300))
data <- data[data$trial %in% sheet$name, ]
data$F <- sapply(strsplit(data$trial, "-"), "[", 1)

get <- paste0("F1-T1-", c("D", "R", "S"), "1")
final <- subset(data, trial %in% get)
# Make a list with one element per unique F and year
final <- unlist(lapply(split(final, final$trial),
  function(x) split(x, x$year)), recursive = FALSE)

# Set up repeated items for the plot
num <- length(unique(final[[1]]$id))
xlim <- c(0, num + 1.5)
ylim1 <- c(0, 5)
ylim2 <- c(0, 1.07)
labelline <- 0.5; labelcex <- 0.8
titleline <- 2.5;  titlecex <-1.0
years <- c(100, 300)

# Start the plot
jpeg(paste0(paper, "_Fig_01.jpeg"), res = 300, width = 2300, height = 2700)

par(oma = c(2, 5, 5, 0), las = 1, mar = rep(0, 4),
  tck = 0.05, mgp = c(3, 0.1, 0))

nf <- layout(matrix(c(1, 7, 13, 4, 10, 16,
                      1, 7, 13, 4, 10, 16,
                      2, 8, 14, 5, 11, 17,
                      2, 8, 14, 5, 11, 17,
                      3, 9, 15, 6, 12, 18,
                      3, 9, 15, 6, 12, 18), ncol = 6, byrow = TRUE))
plotorder <- apply(expand.grid(get, years), 1, paste, collapse = ".")
plotorderid <- match(plotorder, names(final))

  # First plot of TC
  for (ind in plotorderid) {
    this <- strsplit(names(final[ind]), "\\.")[[1]]
    thistrial <- this[1]
    thisyr <- this[2]
    errbar(x = 1:num, y = final[[ind]][, "tcMed"], cap = 0.1,
      yplus = final[[ind]][, "tc95"], yminus = final[[ind]][, "tc5"],
      xaxt = "n", xlab = "", ylab = "", frame.plot = FALSE, pch = "",
      xlim = xlim,
      ylim = ylim1 * ifelse(thisyr == 300, 2, 1))
    text(x = 1:num, y = final[[ind]][, "tcMed"], final[[ind]]$id)
    if (ind %in% plotorderid[1:3]) {
      mtext(side = 2, line = 1.5, cex = titlecex, thistrial, las = 0)
    }
    if (ind == plotorderid[1]) {
      mtext(side = 3, "Total catch", line = labelline, cex = labelcex)
      mtext(side = 3, las = 0, line = titleline, paste(thisyr, "years"), adj = 0)
    }
    if (ind == plotorderid[num+1]) {
      mtext(side = 3, "Total catch", line = labelline, cex = labelcex)
      mtext(side = 3, las = 0, line = titleline, paste(thisyr, "years"), adj = 0)
    }
  }
  # Second plot of Depl
  for (ind in plotorderid) {
    this <- strsplit(names(final[ind]), "\\.")[[1]]
    thistrial <- this[1]
    thisyr <- this[2]
    errbar(x = 1:num, y = final[[ind]][, "pfMed"],
      yplus = final[[ind]][, "pf95"], yminus = final[[ind]][, "pf5"],
      xaxt = "n", xlab = "", ylab = "", frame.plot = FALSE, pch = "",
      xlim = xlim, ylim = ylim2, cap = 0.1)
    # # Plot line for AEP
    # if (thistrial == "F1-T1-D1" & ind %in% plotorder[1:num]) {
    #   abline(h = 0.723, lwd = 0.5, lty = 2)
    #   legend("topleft", lty = 2, legend = "Tune (43 & 51)", bty = "n")
    # }
    text(x = 1:num, y = final[[ind]][, "pfMed"], final[[ind]]$id)
    if (ind %in% plotorderid[c(1, num+1)]) {
      mtext(side = 3, "Final population", line = labelline, cex = labelcex)
    }
  }
  # Third plot of lowest population size
  for (ind in plotorderid) {
    errbar(x = 1:num, y = final[[ind]][, "pl10"],
      yplus = final[[ind]][, "pl25"], yminus = final[[ind]][, "pl5"],
      xaxt = "n", xlab = "", ylab = "", frame.plot = FALSE, pch = "",
      xlim = xlim, ylim = ylim2, cap = 0.1)
    text(x = 1:num, y = final[[ind]][, "pl10"], final[[ind]]$id)
    if (ind %in% plotorderid[c(1, num+1)]) {
      mtext(side = 3, "Lowest population", line = labelline, cex = labelcex)
    }
  }

    dev.off()

###############################################################################
###############################################################################
#### Step
#### End of file
###############################################################################
###############################################################################
