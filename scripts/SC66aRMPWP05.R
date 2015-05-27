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

base <- "c:/iwccla"
dir.rs <- "c:/users/kelli/dropbox/iwccla"
paper <- "SC66aRMPWPxx"
setwd(base)

verbose <- FALSE

ignore <- mapply(source, dir("R", full.names = TRUE),
  MoreArgs = list(verbose = verbose))


width.resout <- c(15, rep(7, 4), rep(c(9, 7, 7), 6), 9)


###############################################################################
###############################################################################
#### Step
#### Read in the data
###############################################################################
###############################################################################
aep100 <- readRRR(file.path(dir.rs, "orig_100", "RESOUT.RRR"),    width.resout)
ps4100 <- readRRR(file.path(dir.rs, "pslope4_100", "RESOUT.RRR"), width.resout)
aep300 <- readRRR(file.path(dir.rs, "orig_300", "RESOUT.RRR"),    width.resout)
ps4300 <- readRRR(file.path(dir.rs, "pslope4_300", "RESOUT.RRR"), width.resout)

aep100.a <- readRRR(file.path("211vs222", "orig100", "RESOUT.RRR"),    width.resout)
ps4100.a <- readRRR(file.path("211vs222", "pslope4100", "RESOUT.RRR"), width.resout)
aep300.a <- readRRR(file.path("211vs222", "orig300", "RESOUT.RRR"),    width.resout)
ps4300.a <- readRRR(file.path("211vs222", "pslope4300", "RESOUT.RRR"), width.resout)

compare <- dir(file.path("211vs222", "orig100"), pattern = "-")

###############################################################################
###############################################################################
#### Step
#### Build the table
###############################################################################
###############################################################################
keepcols <- c(1:8, 12:14, 24)
out <- rbind(
aep100[match(compare, aep100$trial), keepcols],
aep100.a[match(compare, aep100.a$trial), keepcols],
aep300[match(compare, aep300$trial), keepcols],
aep300.a[match(compare, aep300.a$trial), keepcols]#,
# ps4100[match(compare, ps4100$trial), keepcols],
# ps4100.a[match(compare, ps4100.a$trial), keepcols],
# ps4300[match(compare, ps4300$trial), keepcols],
# ps4300.a[match(compare, ps4300.a$trial), keepcols]
)

out <- cbind("component" = rep(rep(c("1+", "mat"), each = length(compare)), 2),
  out)

write.csv(out, file.path("ms", paste0(paper, "_table_compare.csv")),
  row.names = FALSE)
