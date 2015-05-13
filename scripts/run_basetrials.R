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
# a. Add a few things to increase the capability of MANTST14.FOR

###############################################################################
###############################################################################
#### Step
#### Set variable inputs
###############################################################################
###############################################################################
base <- getwd()
dirs <- c("orig100", "orig300", "pslope4100", "pslope4300")

verbose <- FALSE
torun <- :200

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

###############################################################################
###############################################################################
#### Step
#### Run the trials
###############################################################################
###############################################################################
for(fd in dirs) {
  setwd(eval(parse(fd)))

if (grepl("100$", getwd())) {
  basetrials$nyear <- 100
}

for (ind in torun) {
  # Determine if time-varying
  timevarying <- ifelse(any(!is.null(basetrials[, c("kyear", "msyryr")])),
    TRUE, FALSE)

  # Create the directory: if time-varying and nyear = 300 then skip
  if (timevarying & basetrials$nyear[ind] == 300) next
  dir.create(as.character(ind), showWarnings = FALSE)
  setwd(as.character(ind))

  # Copy files over to the new directory, assumes compiled programs exists
  files2get <- dir(file.path(base, "lib"), full.names = TRUE)
  done <- mapply(file.copy, from = files2get, MoreArgs = list(to = getwd(),
    overwrite = TRUE))

  # If working with alternative use the Norwegian par file w/ Pslope = 4.7157
  if (grepl("pslope4", getwd())) {
    ignore <- file.copy("CLC-N.pslope.4.7157.PAR", "CLC-N.PAR", overwrite = TRUE)
  }

  # If time-varying change the ISCALE.dat
  if (timevarying) {
    iscale <- writeLines("1", "ISCALE.dat")
  }

  # Create the data file and overwrite COPY.dat
  create_dat(out = "COPY.dat", case = basetrials[ind, "name"],
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

  system("az",  show.output.on.console = verbose)
  system("a",   show.output.on.console = verbose)
  system("res", show.output.on.console = verbose)
  setwd("..")
}

if (verbose) message(paste("Trials in", getwd(), "are done."))
setwd(base)
}

###############################################################################
###############################################################################
#### Step
#### End of file
###############################################################################
###############################################################################

