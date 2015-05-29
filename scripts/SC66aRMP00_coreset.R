###############################################################################
###############################################################################
#-----------------------------------------------------------------------------#
####Author     : Kelli Faye Johnson
####Contact    : kellifayejohnson@gmail.com
####Lastupdate : 2015-04-21
####Purpose    : Generate the files and run 100 and 300 year trials
####             for the evaluation of the core set of trials needed between
####             tuning algorithms
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
#### Set variable inputs
###############################################################################
###############################################################################
base <- "c:/iwccla"
setwd(base)

dirs <- c("prob.7_100", "prob.7_300", "pslope1_100", "pslope1_300")
verbose <- TRUE # Display output to console or not
run <- TRUE

trials <- read.csv("Trials_KFJ_tune.csv", header = TRUE)
torun <- 1:NROW(trials)
# source function to write dat files
source(file.path("R", "create_dat.R"))

mapply(dir.create, dirs, MoreArgs = list(showWarnings = verbose))

###############################################################################
###############################################################################
#### Step
#### Run the trials
###############################################################################
###############################################################################
for(fd in dirs) {
  setwd(fd)

if (grepl("100$", getwd())) {
  trials$nyear <- 100
}
if (grepl("300$", getwd())) { trials$nyear <- 300 }

for (ind in torun) {
  # If run == FALSE then a .dat file is produced but no trial is run
  if (run) {
    dir.forthisiter <- as.character(trials$name[ind])
    dir.create(dir.forthisiter, showWarnings = FALSE)
    setwd(as.character(dir.forthisiter))

    # Copy files over to the new directory, assumes compiled programs exists
    files2get <- dir(file.path(base, "lib"), full.names = TRUE)
    done <- mapply(file.copy, from = files2get, MoreArgs = list(to = getwd(),
      overwrite = TRUE))
  }

  # If working with alternative use the Norwegian par file w/ Pslope = 4.7157
  if (grepl("pslope4", getwd())) {
    ignore <- file.copy("CLC-N.pslope.4.7157.PAR", "CLC-N.PAR", overwrite = TRUE)
  }
  if (grepl("pslope1", getwd())) {
    ignore <- file.copy("CLC-N.pslope.1.83.PAR", "CLC-N.PAR", overwrite = TRUE)
  }
  if (grepl("prob.7", getwd())) {
    ignore <- file.copy("CLC-N.prob.7690.PAR", "CLC-N.PAR", overwrite = TRUE)
  }
  # Create the data file and overwrite COPY.dat
  datname <- ifelse(run, "COPY.dat", paste0(trials[ind, "name"], ".dat"))
  create_dat(out = datname,
    case = trials[ind, "name"],
    nyear = trials[ind, "nyear"], optdt = trials[ind, "dt"],
    depl = trials[ind, "depl"], component = trials[ind, "component"],
    msyr1 = trials[ind, "msyr"], msyl = trials[ind, "msyl"],
    ifreq = trials[ind, "survyr"], mat1 = trials[ind, "agemat"],
    bias0 = trials[ind, "survbias"], nprot = trials[ind, "nprot"],
    npcat = trials[ind, "catch"], k99 = trials[ind, "k99"],
    k99yr = trials[ind, "kyear"], optb = trials[ind, "biasopt"],
    msyr99 = trials[ind, "msyr99"], msyr99yr = trials[ind, "msyryr"],
    erate = trials[ind, "epd"], istep = trials[ind, "istep"],
    inita = trials[ind, "inita"], initz = trials[ind, "initz"],
    optc = trials[ind, "cerror"])

  if (run) {
    system("az",  show.output.on.console = verbose)
    system("a",   show.output.on.console = verbose)
    setwd("..")
  }

}

if (verbose) message(paste("Trials in", getwd(), "are done."))
flush.console()
setwd("..")
}
setwd(base)

###############################################################################
###############################################################################
#### Step
#### End of file
###############################################################################
###############################################################################

