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
dir.home <- "c:/iwccla"
dir.rs <- "c:/users/kelli/dropbox/iwccla"
width.resout <- c(15, rep(7, 4), rep(c(9, 7, 7), 6), 9)

setwd(dir.home)

fn.sheet <- "Trials_KFJ_base.csv"
fn.norway <- file.path("norway", "AldrinHuseby_2007_Table.csv")

paper <- "SC66aRMPWP03"

###############################################################################
###############################################################################
#### Step
#### Attach libraries and set directory
###############################################################################
###############################################################################
source(file.path("R", "readRRR.R"))
sheet <- read.csv(fn.sheet, header = TRUE)
norway <- read.csv(fn.norway, header = TRUE, stringsAsFactors = FALSE)

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
####
###############################################################################
###############################################################################
norway$T <- sapply(strsplit(norway$Trial, "-"), "[", 1)
norway$R <- sapply(strsplit(norway$Trial, "-"), "[", 2)

###############################################################################
###############################################################################
#### Step
#### Table 2
###############################################################################
###############################################################################
data.o <- merge(subset(norway, CLA == "original"),
  subset(sheet, component == 1 & dt == 0), by.x = c("T", "R", "Depl", "MSYR"),
  by.y = c("T", "DRS", "depl", "msyr"), all.x = TRUE)

comparerowO <- match(data.o$name, aep100$trial)

oldvsnewO <- data.frame("trial" = data.o$name,
"TCMed" = data.o$Cmed   - as.numeric(as.character(aep100[comparerowO, 2])),
"TC5"   = data.o$C5     - as.numeric(as.character(aep100[comparerowO, 3])),
"TC95"  = data.o$C95    - as.numeric(as.character(aep100[comparerowO, 4])),
"TCMean"= data.o$Cmean  - as.numeric(as.character(aep100[comparerowO, 5])),
"PFMed" = data.o$PFMed  - as.numeric(as.character(aep100[comparerowO, 6])),
"PF5"   = data.o$PF5    - as.numeric(as.character(aep100[comparerowO, 7])),
"PF95"  = data.o$PF95   - as.numeric(as.character(aep100[comparerowO, 8])),
"PL5"   = data.o$Pmin5  - as.numeric(as.character(aep100[comparerowO, 12])),
"AAV"   = data.o$AAV    - as.numeric(as.character(aep100[comparerowO, 24]))
)
oldvsnewO <- oldvsnewO[!is.na(oldvsnewO$TCMed), ]
write.csv(oldvsnewO, file.path("ms", paste0(paper, "Table02.csv")),
  row.names = FALSE)

###############################################################################
###############################################################################
#### Step
#### Table 3
###############################################################################
###############################################################################
data.a <- merge(subset(norway, CLA == "alternate"),
  subset(sheet, component == 1 & dt == 0), by.x = c("T", "R", "Depl", "MSYR"),
  by.y = c("T", "DRS", "depl", "msyr"), all.x = TRUE)

comparerowA <- match(data.a$name, ps4100$trial)

oldvsnewA <- data.frame("trial" = data.a$name,
"TCMed" = data.a$Cmed   - as.numeric(as.character(ps4100[comparerowA, 2])),
"TC5"   = data.a$C5     - as.numeric(as.character(ps4100[comparerowA, 3])),
"TC95"  = data.a$C95    - as.numeric(as.character(ps4100[comparerowA, 4])),
"TCMean"= data.a$Cmean  - as.numeric(as.character(ps4100[comparerowA, 5])),
"PFMed" = data.a$PFMed  - as.numeric(as.character(ps4100[comparerowA, 6])),
"PF5"   = data.a$PF5    - as.numeric(as.character(ps4100[comparerowA, 7])),
"PF95"  = data.a$PF95   - as.numeric(as.character(ps4100[comparerowA, 8])),
"PL5"   = data.a$Pmin5  - as.numeric(as.character(ps4100[comparerowA, 12])),
"AAV"   = data.a$AAV    - as.numeric(as.character(ps4100[comparerowA, 24]))
)
oldvsnewA <- oldvsnewA[!is.na(oldvsnewA$TCMed), ]
write.csv(oldvsnewA, file.path("ms", paste0(paper, "Table03.csv")),
  row.names = FALSE)

###############################################################################
###############################################################################
#### Step
#### End of file
###############################################################################
###############################################################################
