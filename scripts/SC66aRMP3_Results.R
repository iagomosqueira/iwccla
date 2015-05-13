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
ps4300 <- read.fwf(file.path(dir.rs, "pslope4_300", "RESOUT.RRR"), width.resout, as.is = FALSE)
ps4100 <- read.fwf(file.path(dir.rs, "pslope4_100", "RESOUT.RRR"), width.resout, as.is = FALSE)
colnames(aep300) <- colnames(aep100) <- cols.resout
colnames(ps4300) <- colnames(ps4100) <- cols.resout

aep300$trial <- gsub("^[[:space:]]+|[[:space:]]+$", "", aep300$trial)
aep100$trial <- gsub("^[[:space:]]+|[[:space:]]+$", "", aep100$trial)
ps4300$trial <- gsub("^[[:space:]]+|[[:space:]]+$", "", ps4300$trial)
ps4100$trial <- gsub("^[[:space:]]+|[[:space:]]+$", "", ps4100$trial)

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
mycols <- c(1:8, 12:14, 24)

data.table <- rbind(
  aep300[myrows, mycols],
  aep100[myrows, mycols],
  ps4300[myrows, mycols],
  ps4100[myrows, mycols])

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
limaa <- c(0, 0.4)

sheet <- read.csv(fn.basetrials, header = TRUE)
keep <- subset(sheet, curve %in% c("a", "all", "ab") & component == 1 & dt == 0 &
  msyr == 0.01 & !depl %in% c(0.60, 0.99))
keep <- keep[order(keep$depl), ]
orig <- data.frame(aep100[match(keep$name, aep100$trial), ],
  stringsAsFactors = FALSE)
orig$AAV <- gsub("[[:punct:]]", "", orig$AAV)
alt <- data.frame(ps4100[match(keep$name, ps4100$trial), ],
  stringsAsFactors = FALSE)
alt$AAV <- gsub("[[:punct:]]", "", alt$AAV)
symb <- LETTERS[1:dim(orig)[1]]


# response curve a
jpeg(paste0(paper, "_Fig01.jpeg"), quality = 100)
par(mfrow = c(1, 7), las = 1, mar = rep(0.1, 4), oma = c(0.2, 3, 0.2, 0.2),
  tck = 0.05, mgp = c(3, 0.1, 0))
errbar(x = 1:dim(orig)[1], y = orig[, 2], yplus = orig[, 4], frame.plot = FALSE,
  yminus = orig[, 3], xaxt = "n", ylim = limtc, pch = symb, xlim = xlim)
mtext(side = 3, outer = FALSE, "Total catch", line = -1, adj = 1)
legend(x = -2, y = limtc[2], bty = "n", "Orig")
errbar(x = 1:dim(alt)[1], y = alt[, 2], yplus = alt[, 4], frame.plot = FALSE,
  yminus = alt[, 3], xaxt = "n", ylim = limtc, pch = symb, xlim = xlim, yaxt = "n")
axis(2, labels = FALSE)
legend(x = -2, y = limtc[2], bty = "n", "Alt")

errbar(x = 1:dim(orig)[1], y = orig[, 6], yplus = orig[, 8], frame.plot = FALSE,
  yminus = orig[, 7], xaxt = "n", ylim = limpf, pch = symb, xlim = xlim)
mtext(side = 3, outer = FALSE, "Final size", line = -1, adj = 1)
legend(x = -2, y = limpf[2], bty = "n", "Orig")
errbar(x = 1:dim(alt)[1], y = alt[, 6], yplus = alt[, 8], frame.plot = FALSE,
  yminus = alt[, 7], xaxt = "n", ylim = limpf, pch = symb, xlim = xlim, yaxt = "n")
axis(2, labels = FALSE)
legend(x = -2, y = limpf[2], bty = "n", "Alt")

errbar(x = 1:dim(orig)[1], y = orig[, 12], yplus = orig[, 14], frame.plot = FALSE,
  yminus = orig[, 13], xaxt = "n", ylim = limpf, pch = symb, xlim = xlim)
mtext(side = 3, outer = FALSE, "Lowest size", line = -1, adj = 1)
legend(x = -2, y = limpf[2], bty = "n", "Orig")
errbar(x = 1:dim(alt)[1], y = alt[, 12], yplus = alt[, 14], frame.plot = FALSE,
  yminus = alt[, 13], xaxt = "n", ylim = limpf, pch = symb, xlim = xlim, yaxt = "n")
axis(2, labels = FALSE)
legend(x = -2, y = limpf[2], bty = "n", "Alt")

y <- as.numeric(orig[, 24])
plot(x = 1:dim(orig)[1], y = y, ylim = limaa, xaxt = "n", pch = symb,
  frame.plot = FALSE, yaxt = "n")
legend(x = 0, y = limaa[2] / 2, bty = "n", "Orig")
y <- as.numeric(as.character(alt[, 24]))
points(x = 1:dim(alt)[1], y = y + 0.2, ylim = limaa, xaxt = "n", pch = symb,
  yaxt = "n")
axis(2, at = seq(0, limaa[2], length.out = 5),
  labels = c(0, limaa[2]/4, limaa[2]/2, limaa[2]/4, limaa[2]/2))
mtext(side = 3, outer = FALSE, "AAV", line = -1)
legend(x = 0, y = limaa[2], bty = "n", "Alt")
dev.off()

