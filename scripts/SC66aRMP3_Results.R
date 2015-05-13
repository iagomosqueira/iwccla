###############################################################################
###############################################################################
#-----------------------------------------------------------------------------#
####Author     : Kelli Faye Johnson
####Contact    : kellifayejohnson@gmail.com
####Lastupdate : 2015-05-10
####Purpose    : Results for SC66aRMP3
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
paper <- "SC66aRMP3"

dir.home <- "c:/iwccla"
dir.rs <- "c:/users/kelli/dropbox/iwccla"

width.msyl <- c(11, rep(8, 9))
width.resout <- c(15, rep(7, 4), rep(c(9, 7, 7), 6), 9)

fn.100 <- c("orig_100")
fn.300 <- c("basetrials")
fn.dat <- file.path("..", "lib", "COPY.dat")
fn.pars <- dir(file.path("..", "lib"), pattern = "PAR", full.names = TRUE)
fn.basetrials <- file.path(dir.home, "Trials_KFJ_base.csv")

cols.msyl <- c()
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
#plot error bars
library(Hmisc)
library(xtable)

dir.wk <- file.path(dir.home, "ms")
setwd(dir.wk)

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
#### Read in results
###############################################################################
###############################################################################
aep300 <- read.fwf(file.path(dir.rs, "orig_300", "RESOUT.RRR"), width.resout, as.is = FALSE)
aep100 <- read.fwf(file.path(dir.rs, "orig_100", "RESOUT.RRR"), width.resout, as.is = FALSE)
# ps4300 <- read.fwf(file.path(dir.rs, "pslope4_300", "RESOUT.RRR"), width.resout, as.is = FALSE)
ps4100 <- read.fwf(file.path(dir.rs, "pslope4_100", "RESOUT.RRR"), width.resout, as.is = FALSE)
ps9100 <- read.fwf(file.path(dir.rs, "pslope9_100", "RESOUT.RRR"), width.resout, as.is = FALSE)
colnames(aep300) <- colnames(aep100) <- cols.resout

# FAKE
ps9300 <- ps4300 <- ps9100
ps9300[, ] <- ps4300[, ] <- NA

colnames(ps4300) <- colnames(ps4100) <- cols.resout
colnames(ps9300) <- colnames(ps9100) <- cols.resout

###############################################################################
###############################################################################
#### Step
#### Base-case trial tables
###############################################################################
###############################################################################

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
#### Table on summary statistics for the 6 base-case trials for the four groups
###############################################################################
###############################################################################
myrows <- 1:24
mycols <- c(1:8, 12:14, 24)

data.table <- rbind(
  aep300[myrows, mycols],
  aep100[myrows, mycols],
  ps4300[myrows, mycols],
  ps4100[myrows, mycols],
  ps9300[myrows, mycols],
  ps9100[myrows, mycols])

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
#### Step
#### Response curve
###############################################################################
###############################################################################
limtc <- c(0, 2.5)
limpf <- c(0, 1)

sheet <- read.csv(fn.basetrials, header = TRUE)
keep <- subset(sheet, curve %in% c("a", "all", "ab") & component == 1 & dt == 0 &
  msyr == 0.01 & !depl %in% c(0.60, 0.99))
keep <- keep[order(keep$depl), ]
plotme <- aep100[rownames(keep), ]

head(plotme)
# response curve a
par(mfrow = c(1, 5), las = 1, mar = rep(0, 4), oma = rep(5, 4),
  tck = 0.05, mgp = c(3, 0.1, 0))
errbar(x = 1:dim(plotme)[1], y = plotme[, 2], yplus = plotme[, 4], yminus = plotme[, 3],
  xaxt = "n", ylim = limtc)
errbar(x = 1:dim(plotme)[1], y = plotme[, 6], yplus = plotme[, 8], yminus = plotme[, 7],
  xaxt = "n", ylim = limpf)
errbar(x = 1:dim(plotme)[1], y = plotme[, 6], yplus = plotme[, 8], yminus = plotme[, 7],
  xaxt = "n", ylim = limpf)



with(plotme, errbar(x,mean,max,min))
grid(nx=NA, ny=NULL)


