###############################################################################
###############################################################################
#-----------------------------------------------------------------------------#
####Author     : Kelli Faye Johnson
####Contact    : kellifayejohnson@gmail.com
####Lastupdate : 2015-04-21
####Purpose    : Generate the base case data files for IWC CLA
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
dir.base <- getwd()
dir.basetrials <- "basetrials"
verbose <- FALSE
torun <- 1:12

###############################################################################
###############################################################################
#### Step
#### Set the working directory and source some files
###############################################################################
###############################################################################
# Create the directory for base trials
dir.create(dir.basetrials, showWarnings = FALSE)

# source function to write dat files
source(file.path("R", "create_dat.R"))

# Check that base trials csv exists
if (!file.exists("Trials_KFJ_base.csv")) {
  stop("Base case file (Trials_KFJ_base.csv) does not exist")
}
basetrials <- read.csv("Trials_KFJ_base.csv", header = TRUE)

###############################################################################
###############################################################################
#### Step
#### Run the trials
###############################################################################
###############################################################################
setwd(dir.basetrials)

for (ind in torun) {
  dir.create(as.character(ind), showWarnings = FALSE)
  setwd(as.character(ind))
  files2get <- dir(file.path(dir.base, "lib"), full.names = TRUE)
  done <- mapply(file.copy, from = files2get, MoreArgs = list(to = getwd(),
    overwrite = TRUE))
  # Create the data file and overwrite COPY.dat
  create_dat(out = "COPY.dat", optdt = basetrials[ind, "dt"],
    depl = basetrials[ind, "depl"], component = basetrials[ind, "component"],
    msyr1 = basetrials[ind, "msyr"], msyl = basetrials[ind, "msyl"],
    ifreq = basetrials[ind, "survyr"], mat1 = basetrials[ind, "agemat"],
    bias0 = basetrials[ind, "survbias"], nprot = basetrials[ind, "nprot"],
    npcat = basetrials[ind, "catch"], k99 = basetrials[ind, "k99"],
    k99yr = basetrials[ind, "kyear"], optb = basetrials[ind, "biasopt"],
    msyr99 = basetrials[ind, "msyr99"], msyr99yr = basetrials[ind, "msyryr"],
    erate = basetrials[ind, "epd"], istep = basetrials[ind, "istep"])

  system("gfortran Man-v14z.for -o az.exe", show.output.on.console = verbose)
  system("gfortran Man-v14.for -o a.exe", show.output.on.console = verbose)
  system("gfortran MANRESV9.for -o res.exe", show.output.on.console = verbose)

  if (any(Sys.time() - file.info(dir(pattern = ".exe"))$mtime > 2)) {
    stop(paste("An executable was not compiled correctly in", getwd()))
  }
  system("az", show.output.on.console = verbose)
  system("a", show.output.on.console = verbose)
  system("res", show.output.on.console = verbose)
  setwd("..")
}

if (verbose) message(paste("Trials in", getwd(), "are done."))

###############################################################################
###############################################################################
#### Step
#### End of file
###############################################################################
###############################################################################
setwd(dir.base)
