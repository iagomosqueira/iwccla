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

# Switches
# Turn off process error
deterministic <- FALSE
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
dir.create(file.path(dir.test, "results"))

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
my.l <- seq(0.4, 0.8, by = 0.1)
my.r <- seq(0.01, 0.07, by = 0.01)
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
    if (deterministic) {
      msy.dat[grep("ETA", msy.dat)] <-
        "PROCESS ERROR PARAMETER              ETA      0.00"
    }
    writeLines(msy.dat, dat.out)
    system(executable)
    message(paste(my.l[l], ":", my.r[r]))
    flush.console()
    data <- readLines(dat.results)
    file.copy(dat.results,
      file.path("..", "results", paste(dat.results, l, r, ".txt", sep = "_")),
      overwrite = TRUE)
    temp[[r]] <- get_results(data)
  }
  res[(length(res) + 1):(length(res) + length(temp))] <- temp
}

###############################################################################
###############################################################################
#### Step
#### Obtain values from result files
###############################################################################
###############################################################################
k1 <- sapply(res, "[", "k1")
msy <- apply(sapply(res, "[", "msy"), 2, mean)

plot(0, 0, xlim = range(my.r), ylim = c(0, 1), las = 1, ylab = "MSYL",
     xlab = "MSYR(true)")
for (ind in seq_along(msy)) {
  points(my.r, do.call("rbind", msy[[ind]])[, 1], xaxt = "n", las = 1,
         pch = ind)
}
legend("topright", legend = names(msy), bty = "n", pch = seq_along(msy))