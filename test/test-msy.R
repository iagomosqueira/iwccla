###############################################################################
###############################################################################
#-----------------------------------------------------------------------------#
####Author     : Kelli Faye Johnson
####Contact    : kellifayejohnson@gmail.com
####Lastupdate : 2015-02-08
####Purpose    :
####Packages   :
####Inputs     :
####Outputs    :
####Remarks    : Character width = 80
#-----------------------------------------------------------------------------#
###############################################################################
###############################################################################
dir.main <- "c:/iwccla"

testname <- "test-msy"
executable.in <- "Man-v14.for"
executable <- "a.exe"

###############################################################################
###############################################################################
#### Step
#### Do some directory work and load necessary packages
###############################################################################
###############################################################################
# Directory with master copy of all MANST programs
dir.lib <- file.path(dir.main, "lib")
# Directory where the tests will take place
dir.test <- file.path(dir.main, "test")

file.copy(dir.lib, dir.test, recursive = TRUE)
done <- mapply(source, dir(file.path(dir.main, "R"), full.names = TRUE))

# create the executable
setwd(file.path(dir.test, tail(unlist(strsplit(dir.lib, "/")), 1)))
system(paste("gfortran", executable.in))
if (length(dir(pattern = ".exe")) == 1) {
  timesince <- as.numeric(Sys.time() - file.info(executable)$mtime, units = "days")
  message("Created executable")
  if (timesince > 1) {
    message(paste(executable.in, "was compiled", timesince, "days ago.",
      "You should recompile."))
  }
}

dat.in <- "T1A-D1.dat" #"det.dat"
dat.out <- "COPY.dat"
dat.results <- ifelse(grepl("z", executable.in), "RES0", "RESTEST")

###############################################################################
###############################################################################
#### Step
#### Loop through values of MSYR and MSYL
###############################################################################
###############################################################################
res <- list()
my.l <- seq(0.6, 0.9, by = 0.3)
my.r <- seq(0.01, 0.07, by = 0.1) #by = 0.005)
for (l in seq_along(my.l)) {
  temp <- list()
  for (r in seq_along(my.r)) {
    msy.dat <- readLines(dat.in)
    msylvalue <-
      paste0("TRUE MSYL(1)                         MSYL     ", my.l[l])
    msy.dat[grep("TRUE MSYL",  msy.dat)] <- msylvalue
    msyrvalue <-
      paste0("TRUE MSY RATE(1)                     MSYR1    ", my.r[r], "0000")
    msy.dat[grep("TRUE MSY RATE", msy.dat)] <- msyrvalue
    writeLines(msy.dat, dat.out)
    system(executable)
    message(paste(my.l[l], ":", my.r[r]))
    flush.console()
    data <- readLines(dat.results)
    temp[[r]] <- get_results(data)
  }
  res[[l]] <- temp
}



msyl <- list()
for(num in seq_along(res$biomass)) {
  msyl[[num]] <- res$biomass[[num]][200, "ptrue"] / res$km
}
mean(sapply(msyl, mean))

msyr <- list()
for(num in seq_along(res$biomass)) {
  msyr[[num]] <- res$biomass[[num]][200, "catch"] / res$biomass[[num]][200, "ptrue"]
}
mean(sapply(msyr, mean))
