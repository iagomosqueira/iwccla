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

width.msyl <- c(11, rep(8, 9))
width.resout <- c(15, rep(7, 4), rep(c(9, 7, 7), 6), 9)

fn.100 <- c("recreation")
fn.300 <- c("basetrials")
fn.dat <- file.path("..", "lib", "COPY.dat")
fn.pars <- dir(file.path("..", "lib"), pattern = "PAR", full.names = TRUE)
cols.msyl <- c()
# Work with TC, PF, ML, AAV
cols.resout <- c("trial", rep("TC", 3), "tC", rep("PF", 3), "Pf1", "PF1", "PF2",
  rep("PL", 3), rep("PL1", 3), rep("MF", 3), rep("MF1", 3), "AAV")#,
  #rep("RCC", 3), "RRR", "DEPL")

###############################################################################
###############################################################################
#### Step
#### Attach libraries
###############################################################################
###############################################################################
library(xtable)

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
aep300 <- read.fwf(file.path("basetrials", "MSYL.RRR"))

aepm300 <- read.fwf("c:/users/kelli/dropbox/MSYL.RRR", width.msyl)
aepr300 <- read.fwf("c:/users/kelli/dropbox/RESOUT.RRR", width.resout)
colnames(aepr300) <- cols.resout

args(read.fwf)

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
myrows <- 1:12
mycols <- 1:5
data.table <- droplevels(aepr300[myrows, mycols])
colnames(data.table) <- LETTERS[mycols]
data.table[, 1] <- sapply(strsplit(as.character(data.table[, 1]), "-"),
  function(x) {
    temp <- x[2:3]
    gsub("[[:space:]]+", "", paste(temp, collapse = "-"))
})

data.table <- merge(data.table[1:6, ], data.table[7:12, ], by = "A",
  suffixes = c(".x", ".y"))
data.table <- rbind(
  c("", "\\underline{median}",
    "", "\\underline{5th\\%ile}",
    "", "\\underline{96th\\%ile}",
    "", "\\underline{mean}", ""),
  c("trial", rep(c("F", "M"), 4)),
  data.table[, order(colnames(data.table))])

mylabel <- "totalcatch"
print(xtable(data.table,
  digits = 3),
  include.rownames = FALSE,
  include.colnames = FALSE,
  hline.after = c(0, 2, NROW(data.table)),
  sanitize.text.function = function(x){x},
  file = paste0(paper, "_", mylabel, ".tex"))

