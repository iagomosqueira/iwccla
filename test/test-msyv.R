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
executable.in <- "Man-v14z.for"
executable <- "a.exe"
dat.in <- "T1A-d1.dat"

# Switches
# Length of simulation
nyears <- 1999
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
dir.create(file.path(dir.test, "results"), showWarnings = FALSE)

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
  my.l <- seq(0.3, 0.8, by = 0.1)
  my.r <- seq(0.01, 0.06, by = 0.005)
  for(mortality in c(FALSE, TRUE)) {
    for (component in 1:2) {
        for (l in seq_along(my.l)) {
          temp <- list()
          for (r in seq_along(my.r)) {
            msy.dat <- readLines(dat.in)
            # Change the data file
            msy.dat[grep("DENSITY-DEPENDENCE TYPE", msy.dat)] <-
              paste0("DENSITY-DEPENDENCE TYPE              OPTDT     ",
                ifelse(mortality, 1, 0))
            msy.dat[grep("No. OF TRIALS", msy.dat)] <-
              "No. OF TRIALS                        NTRIAL    2"
            msy.dat[grep("No. OF YEARS IN SIMULATION", msy.dat)] <-
              paste0("No. OF YEARS IN SIMULATION           NYEAR  ", nyears)
            # Absolutely no difference if DEPL is started at my.l[l] - or + 0.1
            # So I decided to use a normal random variable with a sd of 0.05
            depl <- my.l[l]
            while (depl == my.l[l]) depl <- round(my.l[l] + rnorm(1, 0, 0.1), 1)
            msy.dat[grep("PREMANAGEMENT DEPLETION", msy.dat)] <-
              paste0("PREMANAGEMENT DEPLETION (1)          DEPL     ", depl)
            msy.dat[grep("TRUE MSYL",  msy.dat)] <-
              paste0("TRUE MSYL(1)                         MSYL     ", my.l[l])
            msy.dat[grep("TRUE MSY RATE", msy.dat)] <-
              paste0("TRUE MSY RATE(1)                     MSYR1    ",
                     format(my.r[r], nsmall = 6))
            msy.dat[grep("YEAR OF LAST SURVEY", msy.dat)] <-
              paste0("YEAR OF LAST SURVEY                  ENDSUR ", nyears + 1)
            msy.dat[grep("YEAR CV CHANGES", msy.dat)] <-
              paste0("YEAR CV CHANGES                      IYRCV  ", nyears + 1)
            msy.dat[grep("COMPONENT ", msy.dat)] <-
              gsub("2", component, grep("COMPONENT ", msy.dat, value = TRUE))
            if (deterministic) {
              msy.dat[grep("ETA", msy.dat)] <-
                "PROCESS ERROR PARAMETER              ETA      0.00"
            }

            writeLines(msy.dat, dat.out)
              message(paste(my.l[l], ":", my.r[r]))
              flush.console()
            converged <- FALSE
            test <- system(executable, intern = TRUE)
              if (length(test) > 0) {
                message(test)
                results <- NULL
              } else {
                converged <- TRUE
                data <- readLines(dat.results)
                file.copy(dat.results, file.path("..", "results",
                  paste(dat.results, l, r, component, mortality, ".txt", sep = "_")),
                  overwrite = TRUE)
                counter <- counter + 1
                res[[counter]] <- get_results(data)
              }
          }
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
#### Plot the results
###############################################################################
###############################################################################
# Plot
my.cex <- 1.2

for (mat in 1:2) {
  if (mat == 1) {
    data.u <- res[which(sapply(sapply(res, "[", "input"), "[", "Component") == "MATURE")]
  }
  if (mat == 2) {
    data.u <- res[which(sapply(sapply(res, "[", "input"), "[", "Component") != "MATURE")]
  }


for (dat in 1:2) {
  if (dat == 1) data <- data.u[which(sapply(sapply(data.u, "[", "input"), "[", "DD") == "fecundity")]
  if (dat == 2) data <- data.u[which(sapply(sapply(data.u, "[", "input"), "[", "DD") != "fecundity")]

  data <- data[!sapply(data, is.null)]

  png(file.path(dir.main, "ms", "figures", paste0("figure0", mat, dat, ".png")),
      width = 500, height = 600)
  par(mfrow = c(2, 1), mar = c(0, 4, 0, 1), oma = c(4, 1, 3, 2))
  for (val in c("depletion", "msyr")){
    if (val == "depletion") {
      plot(0, 0, cex = my.cex, xlim = c(0, max(my.r)), ylim = c(0, max(my.l)),
           las = 1, ylab = "MSYL", xaxt = "n", type = "n")
      legend("topleft", bty = "n", legend = my.l, lty = 1, col = seq_along(my.l),
        title = "MSYL(true)", horiz = FALSE, cex = my.cex)
    } else {
      plot(0, 0, xlim = c(0, max(my.r)), ylim = c(0, max(my.r)),
           ylab = "MSYR",  las = 1, cex = my.cex, type = "n")
    }
    for (ind in seq_along(my.l)) {
      keep <- which(sapply(data, function(x) x$"input"["MSYL"]) == my.l[ind])
      p.x <- as.numeric(sapply(sapply(data, "[", "input"), "[", "MSYR"))[keep]
      p.y <- unlist(sapply(data, "[", val))[keep]
      p.pch <- as.numeric(sapply(sapply(data, "[", "input"), "[", "MSYL"))[keep]
      lines(p.x + .0005*ind, p.y, col = ind)
    }
  }
  mtext(side = 1, "MSYR(true)", outer = TRUE, line = 2)
  mtext(side = 3, ifelse(dat == 1, "fecundity", "mortality"), outer = TRUE,
        line = 1)
  dev.off()

  png(file.path(dir.main, "ms", "figures", paste0("figure1", mat, dat, ".png")),
      width = 500)
  par(mfrow = c(2, 1), mar = c(0, 4, 0, 1), oma = c(4, 1, 3, 2))
  for(val in 1:2) {
    plot(0, 0, las = 1, cex = my.cex, type = "n",
         xlim = c(0, max(my.l) + 0.2), ylim = c(0, 1.2), xaxt = "n",
         ylab = "Catch in the terminal year")
    mtext(side = 3, adj = 1, line = -1.5,
      ifelse(val == 1, expression(italic(K_mature)), expression(italic(K_1))))
    if (val == 1) {
      legend("topleft", bty = "n", legend = my.l, lty = 1, col = seq_along(my.l),
        title = "MSYL(true)", horiz = FALSE, cex = my.cex)
    }
    for (ind in seq_along(my.l)) {
        keep <- which(sapply(data, function(x) x$"input"["MSYL"]) == my.l[ind])
        if (val == 1) {
          p.x <- unlist(sapply(data, "[", "depletion"))[keep]
        } else {
          p.x <- unlist(sapply(data, "[", "ptrueterm"))[keep] /
                 unlist(sapply(data, "[", "k1"))[keep]
        }
        p.y <- unlist(sapply(data, "[", "catchterm"))[keep]
        p.pch <- as.numeric(sapply(sapply(data, "[", "input"), "[", "MSYL"))[keep]
        lines(p.x, p.y, col = ind, lwd = 1.5)
    }
  }
  axis(1)
  mtext(side = 1, "Depletion", outer = TRUE)
  mtext(side = 3, ifelse(dat == 1, "fecundity", "mortalilty"), outer = TRUE,
        line = 1)
  dev.off()
}
}
