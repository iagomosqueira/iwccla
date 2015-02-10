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
# Use mortality density dependence
mortality <- FALSE
# Load old results (instead of running all simulations)
loadold <- FALSE


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

dat.out <- "COPY.dat"
dat.results <- ifelse(grepl("z", executable.in), "RES0", "RESTEST")

###############################################################################
###############################################################################
#### Step
#### Loop through values of MSYR and MSYL
###############################################################################
###############################################################################
if (!loadold) {
  res <- list()
  counter <- 0
  my.l <- seq(0.4, 0.8, by = 0.1)
  my.r <- seq(0.01, 0.07, by = 0.005)
  for(mortality in c(FALSE, TRUE)) {
    dat.in <- ifelse(mortality, "T1A-D1.dat", "T1B-D1.dat")
    for (l in seq_along(my.l)) {
      temp <- list()
      for (r in seq_along(my.r)) {
        msy.dat <- readLines(dat.in)
        msylvalue <-
          paste0("TRUE MSYL(1)                         MSYL     ", my.l[l])
        msy.dat[grep("TRUE MSYL",  msy.dat)] <- msylvalue
        msyrvalue <-
          paste0("TRUE MSY RATE(1)                     MSYR1    ",
                 format(my.r[r], nsmall = 6))
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
          file.path("..", "results",
                    paste(dat.results, l, r, mortality, ".txt", sep = "_")),
          overwrite = TRUE)
        counter <- counter + 1
        res[[counter]] <- get_results(data)
      }
    }
  }
}
# Backup way to get results if you do not want to run all simulations
if (loadold){
  res <- lapply(dir("c:/iwccla/test/results", full.names = TRUE), function(x) {
    data <- readLines(x)
    get_results(data)
    })
}
###############################################################################
###############################################################################
#### Step
#### Obtain values from result files
###############################################################################
###############################################################################
t <- sapply(sapply(res, "[", "input"), "[", "DD")

fec <- res[which(t == "fecundity")]
mor <- res[which(t != "fecundity")]

###############################################################################
###############################################################################
#### Step
#### Plot the results
###############################################################################
###############################################################################
# Plot
my.cex <- 2

for (dat in 1:2) {
  if (dat == 1) data <- fec
  if (dat == 2) data <- mor

  k1 <- unlist(sapply(data, "[", "k1"))
  km <- unlist(sapply(data, "[", "km"))
  msy <- t(sapply(sapply(data, "[", "msy"), apply, 2, mean))
  r <- as.numeric(sapply(sapply(data, "[", "input"), "[", "MSYR"))
  l <- as.numeric(sapply(sapply(data, "[", "input"), "[", "MSYL"))

  png(file.path(dir.main, "ms", "figures", paste0("figure0", dat, ".png")),
      width = 500, height = 600)
  par(mfrow = c(2, 1), mar = c(0, 4, 0, 1), oma = c(4, 1, 3, 2))
  plot(r, msy[, 1], pch = l * 10, cex = my.cex,
       xlim = range(my.r), ylim = range(my.l), las = 1 ,
       ylab = "MSYL", xaxt = "n", type = "p")
  legend("topleft", bty = "n", legend = my.l, pch = my.l * 10,
         title = "MSYL(True)", horiz = TRUE, cex = 1.5, title.adj = 0)
  plot(r, msy[, 2], pch = l * 10,
       xlim = range(my.r), ylim = range(msy[, 2]),
       ylab = "MSYR",  las = 1, cex = my.cex)
  mtext(side = 1, "MSYR(true)", outer = TRUE, line = 2)
  mtext(side = 3, ifelse(dat == 1, "fecundity", "mortality"), outer = TRUE,
        line = 1)
  dev.off()

  png(file.path(dir.main, "ms", "figures", paste0("figure1", dat, ".png")),
      width = 500)
  plot(unlist(sapply(sapply(data, "[", "ptrueterminal"), mean)) / km,
       unlist(sapply(sapply(data, "[", "catchterminal"), mean)),
       las = 1, pch = l * 10, type = "p", cex = my.cex,
       xlab = "Mean depletion (400 trials)",
       ylab = "Catch in the terminal year")
  legend("topleft", legend = my.l, pch = my.l * 10, bty = "n",
         title = "MSYL(True)", horiz = TRUE, title.adj = 0)
  mtext(side = 3, ifelse(dat == 1, "fecundity", "mortalilty"), outer = TRUE,
        line = 1)
  dev.off()
}