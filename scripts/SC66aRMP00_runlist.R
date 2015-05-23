###############################################################################
###############################################################################
#-----------------------------------------------------------------------------#
####Author     : Kelli Faye Johnson
####Contact    : kellifayejohnson@gmail.com
####Lastupdate : 2015-05-22
####Purpose    : SC66aRMP run summary
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
dir.home <- "c:/iwccla"
setwd(dir.home)

dir.rs <- "c:/users/kelli/dropbox/iwccla"

width.resout <- c(15, rep(7, 4), rep(c(9, 7, 7), 6), 9)

fn.basetrials <- file.path(dir.home, )

###############################################################################
###############################################################################
#### Step
#### Attach libraries and set directory
###############################################################################
###############################################################################
source(file.path("R", "readRRR.R"))
sheet <- read.csv("Trials_KFJ_base.csv", header = TRUE)

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
#### Table of which runs are complete
###############################################################################
###############################################################################
test <- sheet[, -which(colnames(sheet) %in% c("oldname", "nyear", "ntrial"))]
test$PF.o100 <- aep100[match(test$name, aep100$trial), "PF"]
test$PF.o300 <- aep300[match(test$name, aep300$trial), "PF"]
test$PF.p100 <- ps4100[match(test$name, ps4100$trial), "PF"]
test$PF.p300 <- ps4300[match(test$name, ps4300$trial), "PF"]

test$o100 <- ifelse(is.na(test$PF.o100), FALSE, TRUE)
test$o300 <- ifelse(is.na(test$PF.o300), FALSE, TRUE)
test$p100 <- ifelse(is.na(test$PF.p100), FALSE, TRUE)
test$p300 <- ifelse(is.na(test$PF.p300), FALSE, TRUE)

# Turn off timevarying for 300 years
test[which(test$istep > 0), c("o300", "p300")] <- "DoNotDo"
test[which(test$msyryr != "NULL" | test$kyear != "NULL"), ]$o300 <- "DoNotDo"
test[which(test$msyryr != "NULL" | test$kyear != "NULL"), ]$p300 <- "DoNotDo"

# Define bad runs
test$o300[test$name == "M2-T1-cB4"] <- "bad"
test[test$name == "M2-T6cD-S4", c("o100", "o300", "p100", "p300")] <- "bad"
test[test$name == "M2-T6cA-D4", "p300"] <- "bad"

test[test$name == "M1-T10A-R1", c("o100", "p100", "o300", "p300")] <- "bad"
test[test$name == "M2-T5-S4", c("o100", "p100", "o300", "p300")] <- "bad"
test[test$name == "M2-T10A-S4", c("o300", "p300")] <- "bad_didnotneed"

#low convergence
test[test$name == "M2-T10A-D4", "o300"] <- "badunitnumberbutranwlow"
test[test$name == "M2-T10A-R4", "o300"] <- "QuotaFailedToReachRequiredAccuracy"
test[test$name == "M2-T10A-R4", "p300"] <- "ExtremelyHighAValueTooManyDecimals"
test[test$name == "M2-T10A-D4", "p300"] <- "ExtremelyHighAValueTooManyDecimals"
test[test$name == "M2-T12B-D4", "o100"] <- "low"
test[test$name == "M2-T12A-D4", "o100"] <- "low"
test[test$name == "M2-T19-D4",  "o100"] <- "low"
test[test$name == "M1-T20-D1",  "o100"] <- "low"
test[test$name == "M2-T12B-D4", "p100"] <- "low"
test[test$name == "M2-T12A-D4", "p100"] <- "low"
test[test$name == "M2-T19-D4", "p100"] <- "low"
test[test$name == "M1-T20-D1", "p100"] <- "low"
test[test$name == "M1-T20-D1", "p300"] <- "low"

# Currently running
test$o100[c(344:349, 354:360)] <- "Kelli"

test$p100[c(344:360)] <- "Kelli"

test[test$o100 != TRUE, c("name", "PF.o100", "o100")]
test[test$p100 != TRUE, c("name", "PF.p100", "p100")]
test[!test$o300 %in% c(TRUE, "DoNotDo"), c("name", "PF.o300", "o300")]
test[!test$p300 %in% c(TRUE, "DoNotDo"), c("name", "PF.p300", "p300")]

write.csv(test, "c:/users/kelli/dropbox/ForKelli/runlist.csv", row.names = FALSE)

###############################################################################
###############################################################################
#### Step
#### End of file
###############################################################################
###############################################################################
