###############################################################################
###############################################################################
#-----------------------------------------------------------------------------#
####Author     : Kelli Faye Johnson
####Contact    : kellifayejohnson@gmail.com
####Lastupdate : 2015-02-06
####Purpose    :
####Packages   :
####Inputs     :
####Outputs    :
####Remarks    : Character width = 80
#-----------------------------------------------------------------------------#
###############################################################################
###############################################################################

dir.main <- "c:/iwccla"
dir.lib <- file.path(dir.main, "lib")
dir.test <- file.path(dir.main, "test")

testname <- "test-nocatch"
executable <- "a.exe"

###############################################################################
###############################################################################
#### Step
#### Do some directory work and load necessary packages
###############################################################################
###############################################################################
library(testthat)

file.copy(dir.lib, dir.test, recursive = TRUE)

# create the executable
setwd("lib")
system("gfortran Man-v14z.for")
if (length(dir(pattern = ".exe")) == 1) {
    message("Created executable")
}

file.copy("det.dat", "COPY.dat", overwrite = TRUE)
system(executable)
det <- readLines("RES0")

get_results <- function(data) {
  results <- list()
  results$biomass <- data[grep("Year", data):(grep("NZERO", data) - 2)]
  use <- lapply(results$biomass, function(x) {
    temp <- unlist(strsplit(x, "[[:space:]]+")[[1]])
    temp[!temp %in% ""]
    })
  use.table <- do.call("rbind", use[-1])
  results$biomass <- apply(use.table, 2, as.numeric)
  # c("Year", "PTRUE", "PSURV", "Catch", "BIRSUM")
  colnames(results$biomass) <- unlist(use[1])

  invisible(return(results))
}

res <- get_results(det)

test_that("No catches, not why it is negative 1.", {
    expect_equal(sum(res$biomass[, "CM"]), -1)
})