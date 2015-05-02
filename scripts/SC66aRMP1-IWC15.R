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
# Length of simulation
nyears <- 1999

# Switches
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

###############################################################################
###############################################################################
#### Step
#### Change the executable such that it manipulates the for loop for finding
#### A and Z in MANTSTALT.FOR
####       DO 2000 MYLOOP = 1,75 and DO 2000 MYLOOP2 = 3,102
###############################################################################
###############################################################################
alt.name <- file.path(dir.run, "lib", "MANTSTALT.FOR")
alt.data <- readLines(alt.name)
alt.data[grep("DO 2000 MYLOOP = 1,", alt.data)] <- "DO 2000 MYLOOP = 1,75"
alt.data[grep("DO 2000 MYLOOP = 3,", alt.data)] <- "DO 2000 MYLOOP = 3,102"
writeLines(alt.data, alt.name)

###############################################################################
###############################################################################
#### Step
#### create the executable
###############################################################################
###############################################################################
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
####
###############################################################################
###############################################################################
if (!loadold) {
  res <- list()
  counter <- 0
  for(mortality in c(TRUE)) {
    for (component in 1:2) {
      temp <- list()

      create_dat(out = dat.out, case = testname, depl = 0.6,
        optdt = ifelse(mortality == TRUE, 1, 0), component = component,
        ntrial = 2, nyear = nyears, msyl = 0.3, msyr1 = 0.02, erate = 0)

      message(paste0(ifelse(mortality, "M(", "1+("), component, ") ")
      flush.console()
      converged <- FALSE
      test <- system(executable, intern = TRUE)
      com <- ifelse(component == 1, "all", "mat")
      mor <- ifelse(mortality == FALSE, "fec", "mor")
      file.copy("XY", file.path("..", "input", paste0("XY", com, mor, ".txt")),
        overwrite = TRUE)
      file.copy(dat.out, file.path("..", "input", paste0(com, mor, ".dat")),
        overwrite = TRUE)
        if (length(test) > 0) {
          message(test)
          results <- NULL
        } else {
          converged <- TRUE
          data <- readLines(dat.results)
          file.copy(dat.results, file.path("..", "results",
            paste(dat.results, component, mortality, ".txt", sep = "_")),
            overwrite = TRUE)
          counter <- counter + 1
          res[[counter]] <- get_results(data)
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
jpeg(file.path("results", "SC66aRMP1-IWC15_Fig01.jpeg"), quality = 100)
  source(file.path("..", "scripts", "SC66aRMPX-IWC15_Fig01.R"))
  dev.off()

# Figure 02
jpeg(file.path("results", "SC66aRMP1-IWC15_Fig02.jpeg"), quality = 100)
  source(file.path("..", "scripts", "SC66aRMPX-IWC15_Fig02.R"))
  dev.off()

###############################################################################
###############################################################################
#### Step
#### End of script
###############################################################################
###############################################################################
setwd(dir.main)
