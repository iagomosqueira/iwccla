###############################################################################
###############################################################################
#-----------------------------------------------------------------------------#
####Author     : Kelli Faye Johnson
####Contact    : kellifayejohnson@gmail.com
####Lastupdate : 2015-04-16
####Purpose    : 2015 International Whaling Commission in San Diego, CA
####Packages   : akima
####Inputs     :
####Outputs    :
####Remarks    : Character width = 80
#-----------------------------------------------------------------------------#
###############################################################################
###############################################################################
dir.main <- "c:/iwccla"

testname <- "SC66aRMPX-IWC15"
executable.in <- "Man-v14z.for"
executable <- "a.exe"
dat.in <- "T1A-d1.dat"

# Switches
# Length of simulation
nyears <- 1999
# Turn off process error
deterministic <- FALSE
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
dir.run <- file.path(dir.main, "SC66aRMPX-IWC15")
dir.create(file.path(dir.run, "results"),
  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(dir.run, "input"), showWarnings = FALSE)

done <- file.copy(dir.lib, dir.run, recursive = TRUE)
done <- mapply(source, dir(file.path(dir.main, "R"), full.names = TRUE))

# create the executable
setwd(file.path(dir.run, tail(unlist(strsplit(dir.lib, "/")), 1)))
system(paste("gfortran", executable.in))
if (length(dir(pattern = ".exe")) == 1) {
  timesince <- as.numeric(Sys.time() - file.info(executable)$mtime, units = "mins")
  message("Created executable")
  if (timesince > 1) {
    message(paste(executable.in, "was compiled", timesince, "mins ago.",
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
  my.l <- c(0.3)   #seq(0.3,  0.8,  by = 0.1)
  my.r <- c(0.02) #seq(0.01, 0.08, by = 0.01)
  for(mortality in c(TRUE)) {
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
            depl <- 0.6 # my.l[l]
            #while (depl == my.l[l]) depl <- round(my.l[l] + rnorm(1, 0, 0.1), 1)
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
            if (component == 1) {
              msy.dat[grep("RECRUITMENT PARAMETER", msy.dat)] <-
                "RECRUITMENT PARAMETER                REC1     1.00"
              msy.dat[grep("RECRUITMENT SIGMA", msy.dat)] <-
                "RECRUITMENT SIGMA                    RSIG     0.00"
            }

            writeLines(msy.dat, dat.out)
              message(paste0(ifelse(mortality, "M(", "1+("), component, ") ",
                "MSYL: ", my.l[l], " - ", "MSYR: ", my.r[r]))
              flush.console()
            converged <- FALSE
            test <- system(executable, intern = TRUE)
            com <- ifelse(component == 1, "all", "mat")
            mor <- ifelse(mortality == FALSE, "fec", "mor")
            file.copy("XY", file.path("..", "input",
              paste0("XY", com, mor, "_L", my.l[l], "_R", my.r[r], ".txt")),
              overwrite = TRUE)
            file.copy(dat.out, file.path("..", "input",
              paste0(com, mor, "_L", my.l[l], "_R", my.r[r], ".dat")),
              overwrite = TRUE)
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
  res <- lapply(dir(file.path(dir.run, "results"), full.names = TRUE),
    function(x) {
      data <- readLines(x)
      get_results(data)
    })
}

setwd(dir.run)

###############################################################################
###############################################################################
#### Step
#### Plot the results
###############################################################################
###############################################################################
# Figure 01
jpeg(file.path("results", "SC66aRMPX-IWC15_Fig01.jpeg"), quality = 100)
  source(file.path("..", "scripts", "SC66aRMPX-IWC15_Fig01.R"))
  dev.off()

# Figure 02
jpeg(file.path("results", "SC66aRMPX-IWC15_Fig02.jpeg"), quality = 100)
  source(file.path("..", "scripts", "SC66aRMPX-IWC15_Fig02.R"))
  dev.off()

###############################################################################
###############################################################################
#### Step
#### End of script
###############################################################################
###############################################################################
setwd(dir.main)
