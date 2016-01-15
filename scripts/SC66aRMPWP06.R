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
source(file.path("..", "scripts", "SC66aRMP_plottunekey.R")

###############################################################################
###############################################################################
#### Step
#### Table on summary statistics for the 6 base-case trials for the four groups
###############################################################################
###############################################################################
gettrial <- function(data, set = "D", trials = subset(sheet, component == 1)) {
  a <- subset(data, data$trial %in% trials$name)
  low  <- paste0("T2-", set, "1")
  high <- paste0("T3-", set, "1")
  d <- a[grepl(paste0("T1-", set), a$trial), ]
  d <- d[order(d$trial), ]
  if (dim(d)[1] != 3) {
    shouldbe <- c(0.01, 0.04, 0.07)
    need <- c(shouldbe %in% d$msyr)
    fake <- d[rep(1, 3), ]
    fake[] <- NA
    fake$msyr <- shouldbe
    fake$trial <- paste0(gsub("-[A-Z]1-[A-Z][0-1]", "", d$trial[1]), "-T1-",
      set, c(1, 4, 7))
    fake[need, ] <- d
    d <- fake
  }

  insert <- function(x) {
    if (length(x) > 0) {
      return(x)
    } else {return(NA)}
  }
  beg <- rep(NA, 6)
  names(beg) <- c("MedTC", "MedCC", "L5TC", "L5TCB.5", "L5Pf", "L5Plo")
  beg[1] <- insert(sum(d$tcMed * c(0.4, 0.4, 0.2)))
  beg[2] <- insert(sum(d$cccMed * c(0.4, 0.4, 0.2)))
  beg[3] <- insert(d[grepl("T1-[A-Z]1", d$trial), "tc5"])
  beg[4] <- insert(a[grepl(low, a$trial), "tc5"])
  beg[5] <- insert(d[grepl("T1-[A-Z]1", d$trial), "pf5"])
  beg[6] <- insert(d[grepl("T1-[A-Z]1", d$trial), "pl5"])

  mid <- c("L5PloS1" = "-", "L5PloS1B1.5" = "-")
  if (set == "R") {
    mid[1] <- insert(a[grepl("T1-S1", a$trial), "pl5"])
    mid[2] <- insert(a[grep("T3-S1", a$trial), "pl5"])
  }

  end <- rep(NA, 2)
  names(end) <- c("L5PloB1.5", "AAV")
  end[1] <- insert(a[grepl(high, a$trial), "pl5"])
  end[2] <- insert(mean(d$AAV))

  values <- c(beg, mid, end)
  return(values)
}

getsection <- function(vara, varb, varc, vard, names, ...) {
  temp <- rbind(
    gettrial(vara, ...),
    gettrial(varb, ...),
    gettrial(varc, ...),
    gettrial(vard, ...))
  rownames(temp) <- names
  return(temp)
}

###############################################################################
###############################################################################
#### Step
#### Table of par values
###############################################################################
###############################################################################
data.pars <- list()
# rearrange the par files
fn.pars <- fn.pars[-grep("N.PAR|9.3443", fn.pars)]
fn.pars <- fn.pars[c(4, 1:3)]

for (fn in seq_along(fn.pars)) {
  data.pars[[fn]] <- read.fwf(fn.pars[fn], widths = c(30, 6), n = 15)
}

data.pars <- data.frame("parname" = data.pars[[1]][, 1],
  do.call(cbind, sapply(data.pars, "[", 2)))
colnames(data.pars) <- c("parameter", "4-3", "7-3", "5-1", "5-4")
data.pars$parameter <- as.character(data.pars$parameter)
data.pars <- rbind(data.pars,
  c("F2-T1-D1 (100 years)", 0.723, NA, 0.723, NA),
  c("F1-T1-D1 (300 years)", NA, 0.681, NA, 0.681))

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
#### Summary of missing trials
###############################################################################
###############################################################################
# What do I need
# sheet[!sheet$name %in% org100$trial, ]
# sheet[!sheet$name %in% prb100$trial, ]
# sheet[!sheet$name %in% ps1100$trial, ]
# sheet[!sheet$name %in% ps4100$trial, ]
# sheet[!sheet$name %in% org300$trial, ]
# sheet[!sheet$name %in% prb300$trial, ]
# sheet[!sheet$name %in% ps1300$trial, ]
# sheet[!sheet$name %in% ps4300$trial, ]

prb300 <- rbind(prb300, prb300[1, ])
prb300[1, ] <- NA
prb300[1, "trial"] <- c("F1-T1-R7")

###############################################################################
###############################################################################
#### Step
#### Table with results from all trials
#### org100 and ps1100 are tuned to 0.723
#### 43         51
#### ps4300 and prb300 are tuned to 0.681
#### 54         73
###############################################################################
###############################################################################

###############################################################################
###############################################################################
#### Step
#### Table of all results
###############################################################################
###############################################################################
# Do a separate table for 100 and 300 year
total  <- subset(sheet, component == 1)
mature <- subset(sheet, component == 2)

mycols <- colnames(org100)[!grepl("depl|msyr|ccc[0-9]", colnames(org100))]
mynas <- rep(NA, dim(total)[1] - 1)
all100 <- rbind(
  cbind("CLA" = c("43", mynas), org100[match(total$name, org100$trial), mycols, drop = TRUE], org100[match(mature$name, org100$trial), mycols[-1], drop = TRUE]),
  cbind("CLA" = c("73", mynas), prb100[match(total$name, prb100$trial), mycols, drop = TRUE], prb100[match(mature$name, prb100$trial), mycols[-1], drop = TRUE]),
  cbind("CLA" = c("51", mynas), ps1100[match(total$name, ps1100$trial), mycols, drop = TRUE], ps1100[match(mature$name, ps1100$trial), mycols[-1], drop = TRUE]),
  cbind("CLA" = c("54", mynas), ps4100[match(total$name, ps4100$trial), mycols, drop = TRUE], ps4100[match(mature$name, ps4100$trial), mycols[-1], drop = TRUE])
  )
all100$trial <- gsub("F1-", "", all100$trial)
write.csv(all100, file.path(paste0(paper, "_Table_all100.csv")), row.names = FALSE)

all300 <- rbind(
  cbind("CLA" = c("43", mynas), org300[match(total$name, org300$trial), mycols, drop = TRUE], org300[match(mature$name, org300$trial), mycols[-1], drop = TRUE]),
  cbind("CLA" = c("73", mynas), prb300[match(total$name, prb300$trial), mycols, drop = TRUE], prb300[match(mature$name, prb300$trial), mycols[-1], drop = TRUE]),
  cbind("CLA" = c("51", mynas), ps1300[match(total$name, ps1300$trial), mycols, drop = TRUE], ps1300[match(mature$name, ps1300$trial), mycols[-1], drop = TRUE]),
  cbind("CLA" = c("54", mynas), ps4300[match(total$name, ps4300$trial), mycols, drop = TRUE], ps4300[match(mature$name, ps4300$trial), mycols[-1], drop = TRUE])
  )
all300$trial <- gsub("F1-", "", all300$trial)
write.csv(all300, file.path(paste0(paper, "_Table_all300.csv")), row.names = FALSE)

###############################################################################
###############################################################################
#### Step
#### Base trials
###############################################################################
###############################################################################
resultsbase <- rbind(
cbind("CLA" = "43", org100[org100$trial == "F1-T1-D1", mycols], org100[org100$trial == "F2-T1-D1", mycols[-1]]),
cbind("CLA" = "73", prb100[prb100$trial == "F1-T1-D1", mycols], prb100[prb100$trial == "F2-T1-D1", mycols[-1]]),
cbind("CLA" = "51", ps1100[ps1100$trial == "F1-T1-D1", mycols], ps1100[ps1100$trial == "F2-T1-D1", mycols[-1]]),
cbind("CLA" = "54", ps4100[ps4100$trial == "F1-T1-D1", mycols], ps4100[ps4100$trial == "F2-T1-D1", mycols[-1]]),
cbind("CLA" = "43", org300[org300$trial == "F1-T1-D1", mycols], org300[org300$trial == "F2-T1-D1", mycols[-1]]),
cbind("CLA" = "73", prb300[prb300$trial == "F1-T1-D1", mycols], prb300[prb300$trial == "F2-T1-D1", mycols[-1]]),
cbind("CLA" = "51", ps1300[ps1300$trial == "F1-T1-D1", mycols], ps1300[ps1300$trial == "F2-T1-D1", mycols[-1]]),
cbind("CLA" = "54", ps4300[ps4300$trial == "F1-T1-D1", mycols], ps4300[ps4300$trial == "F2-T1-D1", mycols[-1]])
)
resultsbase$trial <- gsub("F1-", "", resultsbase$trial)
write.csv(resultsbase, file.path(paste0(paper, "_Table_base.csv")), row.names = FALSE)

###############################################################################
###############################################################################
#### Step
#### Create the tables
###############################################################################
###############################################################################
keep1 <- subset(sheet, dt == 0 & component == 1)
keep2 <- subset(sheet, dt == 0 & component == 2)
comparison <- rbind(
  cbind(getsection(org100, prb100, ps1100, ps4100, names = c("43", "73", "51", "54"), set = "D", trials = keep1), getsection(org100, prb100, ps1100, ps4100, names = c("43", "73", "51", "54"), set = "D", trials = keep2)),
  cbind(getsection(org100, prb100, ps1100, ps4100, names = c("43", "73", "51", "54"), set = "R", trials = keep1), getsection(org100, prb100, ps1100, ps4100, names = c("43", "73", "51", "54"), set = "R", trials = keep2)),
  cbind(getsection(org300, prb300, ps1300, ps4300, names = c("43", "73", "51", "54"), set = "D", trials = keep1), getsection(org300, prb300, ps1300, ps4300, names = c("43", "43", "51", "54"), set = "D", trials = keep2)),
  cbind(getsection(org300, prb300, ps1300, ps4300, names = c("43", "73", "51", "54"), set = "R", trials = keep1), getsection(org300, prb300, ps1300, ps4300, names = c("43", "43", "51", "54"), set = "R", trials = keep2)))
write.csv(comparison, paste0(paper, "_Table_comparison", ".csv"))

###############################################################################
###############################################################################
#### Step Response curves
#### Tuning plot
###############################################################################
###############################################################################
plot_tune(org100, prb100, ps1100, ps4100, names = c("43", "73", "51", "54"), trialset = keep1, out = paste0(paper, "_Fig01_100"))
plot_tune(org100, prb100, ps1100, ps4100, names = c("43", "73", "51", "54"), trialset = keep2, out = paste0(paper, "_Fig02_100"))
plot_tune(org300, prb300, ps1300, ps4300, names = c("43", "73", "51", "54"), trialset = keep1, out = paste0(paper, "_Fig01_300"), year = 300)
plot_tune(org300, prb300, ps1300, ps4300, names = c("43", "73", "51", "54"), trialset = keep2, out = paste0(paper, "_Fig02_300"), year = 300)

###############################################################################
###############################################################################
#### Step
#### Zeh plots for each trial
###############################################################################
###############################################################################
data <- rbind(
      data.frame("id" = "43", "year" = 100, org100),
      data.frame("id" = "73", "year" = 100, prb100),
      data.frame("id" = "51", "year" = 100, ps1100),
      data.frame("id" = "54", "year" = 100, ps4100),
      data.frame("id" = "43", "year" = 300, org300),
      data.frame("id" = "73", "year" = 300, prb300),
      data.frame("id" = "51", "year" = 300, ps1300),
      data.frame("id" = "54", "year" = 300, ps4300))
data <- data[data$trial %in% sheet$name, ]
data$F <- sapply(strsplit(data$trial, "-"), "[", 1)

for(use in unique(gsub("F[0-9]-", "", sheet$name))) {
  plot_trial(data, use, out = paper)
}

###############################################################################
###############################################################################
#### Step
#### Create AEP's plot for MSYR=1(1+) and MSYR4 and MSYR7 (mat)
###############################################################################
###############################################################################
# rbind(subset(data, F == "F1" & msyr == 0.01 & depl == 0.99),
#       subset(data, F == "F2" & msyr != 0.01 & depl == 0.99))
c147 <- sheet[sheet$dt == 0, ]
c147a <- c147[c147$component == 1 & c147$msyr == 0.01, ]
c147b <- c147[c147$component == 2 & c147$msyr != 0.01, ]
c147 <- rbind(c147a, c147b)

comparison147 <- rbind(
  getsection(org100, prb100, ps1100, ps4100, names = c("43", "73", "51", "54"), set = "D", trials = c147),
  getsection(org100, prb100, ps1100, ps4100, names = c("43", "73", "51", "54"), set = "R", trials = c147),
  getsection(org300, prb300, ps1300, ps4300, names = c("43", "73", "51", "54"), set = "D", trials = c147),
  getsection(org300, prb300, ps1300, ps4300, names = c("43", "73", "51", "54"), set = "R", trials = c147))
write.csv(comparison147, paste0(paper, "_Table_comparison_147", ".csv"))

###############################################################################
###############################################################################
#### Step
#### End of file
###############################################################################
###############################################################################
