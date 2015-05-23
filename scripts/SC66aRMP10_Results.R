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
paper <- "SC66aRMP10"

dir.home <- "c:/iwccla"
dir.rs <- "c:/users/kelli/dropbox/iwccla"
dir.wk <- file.path(dir.home, "ms")

width.msyl <- c(11, rep(8, 9))
width.resout <- c(15, rep(7, 4), rep(c(9, 7, 7), 6), 9)

fn.100 <- c("orig_100")
fn.300 <- c("orig_300")
fn.dat <- file.path("..", "lib", "COPY.dat")
fn.pars <- dir(file.path("..", "lib"), pattern = "PAR", full.names = TRUE)
fn.basetrials <- file.path(dir.home, "Trials_KFJ_base.csv")

# Work with TC, PF, ML, AAV
cols.resout <- c("trial", rep("TC", 3), "tC", rep("PF", 3), "Pf1", "PF1", "PF2",
  rep("PL", 3), rep("PL1", 3), rep("MF", 3), rep("MF1", 3), "AAV")#,
  #rep("RCC", 3), "RRR", "DEPL")

###############################################################################
###############################################################################
#### Step
#### Attach libraries and set directory
###############################################################################
###############################################################################
library(Hmisc) # plot error bars
library(xtable)


setwd(dir.wk)
source(file.path("..", "R", "plot_curve.R"))
source(file.path("..", "R", "readRRR.R"))
sheet <- read.csv(fn.basetrials, header = TRUE)

###############################################################################
###############################################################################
#### Step
#### Read in a dat file
###############################################################################
###############################################################################
datfile <- rbind(
  read.fwf(fn.dat, widths = c(37, 7, 15), n = 24, skip = 3),
  read.fwf(fn.dat, widths = c(37, 7, 15), n = 4, skip = 28),
  data.frame(V1 = read.fwf(fn.dat, widths = 60, n = 1, skip = 32), V2 = "", V3 = ""),
  read.fwf(fn.dat, widths = c(37, 7, 15), n = 4, skip = 33),
  read.fwf(fn.dat, widths = c(37, 7, 15), n = 9, skip = 37))
# Format the integers to have zero decimal places
datfile[, 3] <- format(datfile[, 3], format = "f", drop0trailing = TRUE)

###############################################################################
###############################################################################
#### Step
#### Table of dat file
###############################################################################
###############################################################################
mylabel <- "datfile"
print(xtable(datfile,
  caption.placement = "top"),
  caption.placement = "top",
  include.rownames = FALSE,
  include.colnames = FALSE,
  hline.after = c(0, NROW(datfile)),
  file = paste0(paper, "_", mylabel, ".tex"))

###############################################################################
###############################################################################
#### Step
#### Read in the par files
###############################################################################
###############################################################################
data.pars <- list()
for (fn in seq_along(fn.pars)) {
  data.pars[[fn]] <- read.fwf(fn.pars[fn], widths = c(30, 6), n = 15)
}
data.pars <- data.frame("parname" = data.pars[[1]][, 1],
  do.call(cbind, sapply(data.pars, "[", 2))[, -1])
colnames(data.pars) <- c("parameter", "alt1", "alt2", "original")

###############################################################################
###############################################################################
#### Step
#### Table of par values
###############################################################################
###############################################################################
mylabel <- "parfile"
print(xtable(data.pars,
  caption.placement = "top"),
  caption.placement = "top",
  include.rownames = FALSE,
  include.colnames = TRUE,
  hline.after = c(0, 1, NROW(data.pars)),
  file = paste0(paper, "_", mylabel, ".tex"))

###############################################################################
###############################################################################
#### Step
#### Read in results
###############################################################################
###############################################################################
aep300 <- readRRR(file.path(dir.rs, "orig_300", "RESOUT.RRR"),    width.resout)
aep100 <- readRRR(file.path(dir.rs, "orig_100", "RESOUT.RRR"),    width.resout)
ps4300 <- readRRR(file.path(dir.rs, "pslope4_300", "RESOUT.RRR"), width.resout)
ps4100 <- readRRR(file.path(dir.rs, "pslope4_100", "RESOUT.RRR"), width.resout)

###############################################################################
###############################################################################
#### Step
#### Table on summary statistics for the 6 base-case trials for the four groups
###############################################################################
###############################################################################
keep <- subset(sheet, T == "T1" & depl %in% c(0.3, 0.60, 0.99))
keep <- keep[order(keep$dt, keep$component, keep$msyr), ]

mycols <- c(1:8, 12:14, 24)
blank <- aep100[1, mycols]; blank[, ] <- "-"

data.table <- rbind(
  aep100[match(keep$name, aep100$trial), mycols, drop = TRUE], blank,
  ps4100[match(keep$name, ps4100$trial), mycols, drop = TRUE], blank,
  aep300[match(keep$name, aep300$trial), mycols, drop = TRUE], blank,
  ps4300[match(keep$name, ps4300$trial), mycols, drop = TRUE])

# Working with
# Total catch: median, 5%, 96%, mean
# Terminal population: median, 5%, 96%
# Lowest population: a, b, c
# AAV
toprow <- t(data.frame(top = c("Trial", "", "Total catch", "", "",
  "Final population size", "", "", "Lowest population size", "", "", "AAV"),
  second = c("", "Med", "5\\%", "96\\%", "Mean", "Med", "5\\%", "96\\%",
  "", "", "", ""), stringsAsFactors = FALSE))
colnames(toprow) <- colnames(data.table)
data.table <- rbind(toprow, data.table)

mylabel <- "totalcatch"
print(xtable(data.table,
  digits = 3),
  include.rownames = FALSE,
  include.colnames = FALSE,
  sanitize.text.function = function(x){x},
  file = paste0(paper, "_", mylabel, ".tex"))

system(paste("pandoc -f latex -t docx", paste0(paper, "_", mylabel, ".tex"),
  "-o", paste0(paper, "_", mylabel, ".docx")))

###############################################################################
###############################################################################
#### Step Response curves
#### a
###############################################################################
###############################################################################
keep <- subset(sheet, curve %in% c("a", "all", "ab") & component == 1 &
    !depl %in% c(0.60, 0.99))
keep <- keep[order(keep$depl), ]
plot_curve(aep100, ps4100, aep300, ps4300, keep, paste0(paper, "_Fig01_1"))

keep <- subset(sheet, curve %in% c("a", "all", "ab") & component == 2 &
  !depl %in% c(0.60, 0.99))
keep <- keep[order(keep$depl), ]
plot_curve(aep100, ps4100, aep300, ps4300, keep, paste0(paper, "_Fig01_2"))

###############################################################################
###############################################################################
#### Step
#### b
###############################################################################
###############################################################################
keep <- subset(sheet, curve %in% c("b", "all", "ab") & component == 1 &
  !depl %in% c(0.3))
keep <- keep[order(keep$depl), ]
plot_curve(aep100, ps4100, aep300, ps4300, keep, paste0(paper, "_Fig02_1"))

keep <- subset(sheet, curve %in% c("b", "all", "ab") & component == 2 &
  !depl %in% c(0.3))
keep <- keep[order(keep$depl), ]
plot_curve(aep100, ps4100, aep300, ps4300, keep, paste0(paper, "_Fig02_2"))

###############################################################################
###############################################################################
#### Step
#### c
###############################################################################
###############################################################################
keep <- subset(sheet, curve %in% c("c", "all") & component == 1 &
  depl %in% c(0.3))
ords <- order(keep$cerror)
keep <- keep[c(ords[-c(1:2)], ords[1:2]), ]
plot_curve(aep100, ps4100, aep300, ps4300, keep, paste0(paper, "_Fig03_13"))

keep <- subset(sheet, curve %in% c("c", "all") & component == 1 &
  depl %in% c(0.6))
ords <- order(keep$cerror)
keep <- keep[c(ords[-c(1:2)], ords[1:2]), ]
plot_curve(aep100, ps4100, aep300, ps4300, keep, paste0(paper, "_Fig03_16"))

keep <- subset(sheet, curve %in% c("c", "all") & component == 1 &
  depl %in% c(0.99))
ords <- order(keep$cerror)
keep <- keep[c(ords[-c(1:2)], ords[1:2]), ]
plot_curve(aep100, ps4100, aep300, ps4300, keep, paste0(paper, "_Fig03_19"))

keep <- subset(sheet, curve %in% c("c", "all") & component == 2 &
  depl %in% c(0.3))
ords <- order(keep$cerror)
keep <- keep[c(ords[-c(1:2)], ords[1:2]), ]
plot_curve(aep100, ps4100, aep300, ps4300, keep, paste0(paper, "_Fig03_23"))

keep <- subset(sheet, curve %in% c("c", "all") & component == 2 &
  depl %in% c(0.6))
ords <- order(keep$cerror)
keep <- keep[c(ords[-c(1:2)], ords[1:2]), ]
plot_curve(aep100, ps4100, aep300, ps4300, keep, paste0(paper, "_Fig03_26"))

keep <- subset(sheet, curve %in% c("c", "all") & component == 2 &
  depl %in% c(0.99))
ords <- order(keep$cerror)
keep <- keep[c(ords[-c(1:2)], ords[1:2]), ]
plot_curve(aep100, ps4100, aep300, ps4300, keep, paste0(paper, "_Fig03_29"))

###############################################################################
###############################################################################
#### Step
#### d
###############################################################################
###############################################################################
keep <- subset(sheet, curve %in% c("d", "all") & component == 1 &
  depl %in% c(0.3))
keep <- keep[order(keep$survbias), ]
plot_curve(aep100, ps4100, aep300, ps4300, keep, paste0(paper, "_Fig04_13"))

keep <- subset(sheet, curve %in% c("d", "all") & component == 1 &
  depl %in% c(0.6))
keep <- keep[order(keep$survbias), ]
plot_curve(aep100, ps4100, aep300, ps4300, keep, paste0(paper, "_Fig04_16"))

keep <- subset(sheet, curve %in% c("d", "all") & component == 1 &
  depl %in% c(0.99))
keep <- keep[order(keep$survbias), ]
plot_curve(aep100, ps4100, aep300, ps4300, keep, paste0(paper, "_Fig04_19"))

keep <- subset(sheet, curve %in% c("d", "all") & component == 2 &
  depl %in% c(0.3))
keep <- keep[order(keep$survbias), ]
plot_curve(aep100, ps4100, aep300, ps4300, keep, paste0(paper, "_Fig04_23"))

keep <- subset(sheet, curve %in% c("d", "all") & component == 2 &
  depl %in% c(0.6))
keep <- keep[order(keep$survbias), ]
plot_curve(aep100, ps4100, aep300, ps4300, keep, paste0(paper, "_Fig04_26"))

keep <- subset(sheet, curve %in% c("d", "all") & component == 2 &
  depl %in% c(0.99))
keep <- keep[order(keep$survbias), ]
plot_curve(aep100, ps4100, aep300, ps4300, keep, paste0(paper, "_Fig04_29"))

###############################################################################
###############################################################################
#### Step
#### Part 2
###############################################################################
###############################################################################
keep <- subset(sheet, survbias == 0.5 & depl == 0.3 & biasopt == 0)
keep <- keep[order(keep$survbias), ]
plot_curve(aep100, aep300, ps4100, ps4300, keep,
  paste0(paper, "test"), part = 2)

###############################################################################
###############################################################################
#### Step
#### End of file
###############################################################################
###############################################################################
