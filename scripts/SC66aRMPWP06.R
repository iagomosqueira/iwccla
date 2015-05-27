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
paper <- "SC66aRMPWP06"

dir.home <- "c:/iwccla"
dir.rs <- "c:/users/kelli/dropbox/iwccla"
dir.wk <- file.path(dir.home, "ms")

width.resout <- c(15, rep(7, 4), rep(c(9, 7, 7), 6), 9)

fn.dat <- file.path(dir.home, "lib", "COPY.dat")
fn.pars <- dir(file.path(dir.home, "lib"), pattern = "PAR", full.names = TRUE)

sheet <- read.csv(file.path(dir.home, "Trials_KFJ_tune.csv"), header = TRUE)
sheet <- sheet[order(sheet$dt, sheet$component, sheet$T, sheet$msyr), ]

###############################################################################
###############################################################################
#### Step
#### Attach libraries and set directory
###############################################################################
###############################################################################
library(Hmisc, quietly = TRUE) # plot error bars
library(xtable, quietly = TRUE)

setwd(dir.wk)
source(file.path("..", "R", "readRRR2.R"))
source(file.path("..", "R", "plot_tune.R"))

###############################################################################
###############################################################################
#### Step
#### Table of par values
###############################################################################
###############################################################################
data.pars <- list()
# rearrange the par files
fn.pars <- fn.pars[-grep("N.PAR|9.3443", fn.pars)]
fn.pars <- rev(fn.pars)

for (fn in seq_along(fn.pars)) {
  data.pars[[fn]] <- read.fwf(fn.pars[fn], widths = c(30, 6), n = 15)
}

data.pars <- data.frame("parname" = data.pars[[1]][, 1],
  do.call(cbind, sapply(data.pars, "[", 2)))
colnames(data.pars) <- c("parameter", "current", "Norway",
  "Norway-0.723", "Orig-0.681")
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
#### Table with results from all trials
#### org100 and ps1100 are tuned to 0.723
#### 4-3        5-1
#### ps4300 and prb300 are tuned to 0.681
#### 5-4        7-3
###############################################################################
###############################################################################

###############################################################################
###############################################################################
#### Step
#### Table of all results
###############################################################################
###############################################################################
# Do a separate table for 100 and 300 year

tblkp <- subset(sheet, T == "T1" & depl %in% c(0.3, 0.60, 0.99))
tblkp <- tblkp[order(tblkp$dt, tblkp$component, tblkp$msyr), ]

mycols <- -grep("depl|msyr|ccc[0-9]", colnames(org100))
blank <- org100[1, mycols]; blank[, ] <- "-"

data.table <- rbind(
  org100[match(tblkp$name, org100$trial), mycols, drop = TRUE], blank,
  ps1100[match(tblkp$name, ps1100$trial), mycols, drop = TRUE], blank,
  ps4100[match(tblkp$name, ps4100$trial), mycols, drop = TRUE], blank,
  prb100[match(tblkp$name, prb100$trial), mycols, drop = TRUE], blank
  )
write.csv(data.table, file.path(paste0(paper, "_Table_all.csv")))

###############################################################################
###############################################################################
#### Step
#### Base trials
###############################################################################
###############################################################################
resultsbase <- rbind(
cbind(org100[org100$trial == "F1-T1-D1", mycols], org100[org100$trial == "F2-T1-D1", mycols][, -1]),
cbind(ps1100[ps1100$trial == "F1-T1-D1", mycols], ps1100[ps1100$trial == "F2-T1-D1", mycols][, -1]),
cbind(ps4100[ps4100$trial == "F1-T1-D1", mycols], ps4100[ps4100$trial == "F2-T1-D1", mycols][, -1]),
cbind(prb100[prb100$trial == "F1-T1-D1", mycols], prb100[prb100$trial == "F2-T1-D1", mycols][, -1]),
cbind(org300[org300$trial == "F1-T1-D1", mycols], org300[org300$trial == "F2-T1-D1", mycols][, -1]),
cbind(ps1300[ps1300$trial == "F1-T1-D1", mycols], ps1300[ps1300$trial == "F2-T1-D1", mycols][, -1]),
cbind(ps4300[ps4300$trial == "F1-T1-D1", mycols], ps4300[ps4300$trial == "F2-T1-D1", mycols][, -1]),
cbind(prb300[prb300$trial == "F1-T1-D1", mycols], prb300[prb300$trial == "F2-T1-D1", mycols][, -1]))
write.csv(resultsbase, file.path(paste0(paper, "_Table_base.csv")),
  row.names = FALSE)

###############################################################################
###############################################################################
#### Step
#### Table on summary statistics for the 6 base-case trials for the four groups
###############################################################################
###############################################################################

gettrial <- function(data, set = "D", trials) {
  a <- subset(data, data$trial %in% trials$name)
  low <- paste0("T2-", set, "1")
  high <- paste0("T3-", set, "1")
  d <- a[grepl(paste0("T1-", set), a$trial), ]
  d <- d[order(d$trial), ]
  beg <- c(
    "MedTC" = sum(d$tcMed * c(0.4, 0.4, 0.2)),
    "MedCC" = sum(d$cccMed * c(0.4, 0.4, 0.2)),
    "L5%TC" = d[grepl("T1-[A-Z]1", d$trial), "tc5"],
    "L5%TCBias0.5" = a[grepl(low, a$trial), "tc5"],
    "L5%Pf" = d[grepl("T1-[A-Z]1", d$trial), "pf5"],
    "L5%Plo" = d[grepl("T1-[A-Z]1", d$trial), "pl5"])
  if (length(beg) != 6) {
    crap <- c("MedTC" = NA, "MedCC" = NA, "L5%TC" = NA,
      "L5%TCBias0.5" = NA, "L5%Pf" = NA, "L5%Plo" = NA)
    crap[match(names(beg), names(crap))] <- beg
    beg <- crap
  }
  mid <- c("L5%PloS1" = "-", "L5%PloS1B1.5" = "-")
  if (set == "R") {
    mid <- c(
      "L5%PloS1" = a[grepl("T1-S1", a$trial), "pf5"],
      "L5%PloS1B1.5" = a[grep(high, a$trial), "pf5"])
    if (length(mid) != 2) {
      crap <- c("L5%PloS1" = NA, "L5%PloS1B1.5" = NA)
      crap[match(names(mid), names(crap))] <- mid
      mid <- crap
    }
  }
  end <- c(
    "L5%PlowB1.5" = a[grepl(high, a$trial), "pl5"],
    "AAV" = mean(d$AAV)
    )
  if (length(end) != 2) {
    crap <- c("L5%PlowB1.5" = NA, "AAV" = NA)
    crap[match(names(end), names(crap))] <- end
    end <- crap
  }
  values <- c(beg, mid, end)
  return(values)
}


getsection <- function(vara, varb, varc, vard, ...) {
  temp <- rbind(gettrial(vara, ...), gettrial(varb, ...),
    gettrial(varc, ...), gettrial(vard, ...))
  temp <- rbind(rep("", NCOL(temp)), temp[1:2, ],
    rep("", NCOL(temp)), temp[3:4, ])
  cbind("name" = c("Pf=0.723", "orig", "Norway", "Pf=0.681", "orig", "Norway"),
    temp)
}

# What do I need
# keep1[!keep1$name %in% org100$trial, ]
# keep2[!keep2$name %in% org100$trial, ]
# keep1[!keep1$name %in% prb100$trial, ]
# keep2[!keep2$name %in% prb100$trial, ]
# keep1[!keep1$name %in% ps1100$trial, ]
# keep2[!keep2$name %in% ps1100$trial, ]
# keep1[!keep1$name %in% ps4100$trial, ]
# keep2[!keep2$name %in% ps4100$trial, ]


# keep1[!keep1$name %in% org300$trial, ]
# keep2[!keep2$name %in% org300$trial, ]
keep1[!keep1$name %in% prb300$trial, ]
keep2[!keep2$name %in% prb300$trial, ]
# keep1[!keep1$name %in% ps1300$trial, ]
# keep2[!keep2$name %in% ps1300$trial, ]
# keep1[!keep1$name %in% ps4300$trial, ]
keep2[!keep2$name %in% ps4300$trial, ]

###############################################################################
###############################################################################
#### Step
#### Create the tables
###############################################################################
###############################################################################
keep1 <- subset(sheet, dt == 0 & component == 1)
keep2 <- subset(sheet, dt == 0 & component == 2)
yr100 <- rbind(cbind(
  getsection(org100, ps4100, ps1100, prb100, set = "D", trials = keep1),
  getsection(org100, ps4100, ps1100, prb100, set = "D", trials = keep2)[, -1]),
  cbind(
  getsection(org100, ps4100, ps1100, prb100, set = "R", trials = keep1),
  getsection(org100, ps4100, ps1100, prb100, set = "R", trials = keep2)[, -1]))
write.csv(yr100, paste0(paper, "_Table_", substitute(yr100), ".csv"),
  row.names = FALSE)

yr300 <- rbind(cbind(
  getsection(org300, ps4300, ps1300, prb300, set = "D", trials = keep1),
  getsection(org300, ps4300, ps1300, prb300, set = "D", trials = keep2)[, -1]),
  cbind(
  getsection(org300, ps4300, ps1300, prb300, set = "R", trials = keep1),
  getsection(org300, ps4300, ps1300, prb300, set = "R", trials = keep2)[, -1]))
write.csv(yr300, paste0(paper, "_Table_", substitute(yr300), ".csv"),
  row.names = FALSE)

###############################################################################
###############################################################################
#### Step Response curves
#### a
###############################################################################
###############################################################################
plot_tune(org100, ps1100, prb100, ps4100, set = keep1, out = paste0(paper, "_Fig01_100"))
plot_tune(org300, ps1300, prb300, ps4300, set = keep1, out = paste0(paper, "_Fig01_300"))
plot_tune(org100, ps1100, prb100, ps4100, set = keep2, out = paste0(paper, "_Fig02_100"))
plot_tune(org300, ps1300, prb300, ps4300, set = keep2, out = paste0(paper, "_Fig02_300"))

###############################################################################
###############################################################################
#### Step
#### End of file
###############################################################################
###############################################################################
