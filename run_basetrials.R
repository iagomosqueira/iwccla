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
#### ToDo list
###############################################################################
###############################################################################
#

###############################################################################
###############################################################################
#### Step
#### Set variable inputs
###############################################################################
###############################################################################
base <- getwd()
dirs <- c("orig_100", "orig_300", "pslope4_100", "pslope4_300")
verbose <- TRUE # Display output to console or not

run <- TRUE # If FALSE only dat files are produced

###############################################################################
###############################################################################
#### Step
#### Set the working directory and source some files
###############################################################################
###############################################################################
# Create the directories for base trials
mapply(dir.create, dirs, MoreArgs = list(showWarnings = verbose))

# source function to write dat files
source(file.path("R", "create_dat.R"))

# Check that base trials csv exists
if (!file.exists("Trials_KFJ_base.csv")) {
  stop("Base case file (Trials_KFJ_base.csv) does not exist")
}
basetrials <- read.csv("Trials_KFJ_base.csv", header = TRUE)
torun <- 1:NROW(basetrials)
###############################################################################
###############################################################################
#### Step
#### Run the trials
###############################################################################
###############################################################################
for(fd in dirs) {
  setwd(fd)

if (grepl("100$", getwd())) {
  basetrials$nyear <- 100
}
if (grepl("300$", getwd())) { basetrials$nyear <- 300 }

for (ind in torun) {
  # Determine if time-varying
  # All time-varying trials need to use the ISCALE.DAT = 1 for proper results
  timevarying <- ifelse(any(is.null(basetrials[ind, c("kyear", "msyryr")])),
    TRUE, FALSE)
  if (basetrials[ind, "istep"] > 0) timevarying <- TRUE
  # Create the directory: if time-varying and nyear = 300 then skip
  if (timevarying & basetrials$nyear[ind] == 300) next
  if (basetrials[ind, "epd"] > 0) timevarying <- TRUE

  # If run == FALSE then a .dat file is produced but no trial is run
  if (run) {
    dir.forthisiter <- as.character(basetrials$name[ind])
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

  # If time-varying change the ISCALE.dat
  if (timevarying) {
    iscale <- writeLines("1", "ISCALE.dat")
  }

  # Create the data file and overwrite COPY.dat
  create_dat(out = paste0(ifelse(run, "COPY", basetrials[ind, "name"]), ".dat"),
    case = basetrials[ind, "name"],
    nyear = basetrials[ind, "nyear"], optdt = basetrials[ind, "dt"],
    depl = basetrials[ind, "depl"], component = basetrials[ind, "component"],
    msyr1 = basetrials[ind, "msyr"], msyl = basetrials[ind, "msyl"],
    ifreq = basetrials[ind, "survyr"], mat1 = basetrials[ind, "agemat"],
    bias0 = basetrials[ind, "survbias"], nprot = basetrials[ind, "nprot"],
    npcat = basetrials[ind, "catch"], k99 = basetrials[ind, "k99"],
    k99yr = basetrials[ind, "kyear"], optb = basetrials[ind, "biasopt"],
    msyr99 = basetrials[ind, "msyr99"], msyr99yr = basetrials[ind, "msyryr"],
    erate = basetrials[ind, "epd"], istep = basetrials[ind, "istep"],
    inita = basetrials[ind, "inita"], initz = basetrials[ind, "initz"],
    optc = basetrials[ind, "cerror"])

  if (run) {
    system("az",  show.output.on.console = verbose)
    system("a",   show.output.on.console = verbose)
    setwd("..")
  }
}

if (verbose) message(paste("Trials in", getwd(), "are done."))
flush.console()
setwd(base)
}

###############################################################################
###############################################################################
#### Step
#### End of file
###############################################################################
###############################################################################

