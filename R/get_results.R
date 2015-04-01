#' Read in results from Man-v14z.for

#' @param data A \code{RES0} file as output from \code{Man-v14z.for}
#' @return A list of data

#' @author Kelli Faye Johnson

get_results <- function(data) {

  getval <- function(data, word) {
    temp <- grep(word, data, value = TRUE)
    temp <- sapply(strsplit(temp, "[[:space:]]+"), tail, 1)
    names(temp) <- NULL
    if (any(grepl("[a-z|A-Z]", temp))) {
      return(temp)
    } else{
        return(as.numeric(temp))
      }
  }

  trim <- function(x) {
    sub("^\\s+", "", x)
  }

  results <- list()
  data <- sapply(data, trim)

  # ntruetrials: number of set trials
  results$ntruetrials <- getval(data, "Number of trials")

  # ntrials: number of trials
  results$ntrials <- sum(grepl("Trial", data))

  # nyears: number of years in simulation
  results$nyears <- getval(data, "Number of years in simulation")

  # dd: density dependence type
  results$dd <- getval(data, "Density-Dependence Type")

  # msyl: MSYL Outputs
  # MSYLT1, MSYLT0, MSYLT2, AMSYR1, AMSYL1, AMSYR0, AMSYL0, AMSYR2, AMSYL2
  out <- sapply(strsplit(grep("MSYL Outputs", data, value = TRUE), ":"), "[[", 2)
  out <- do.call("rbind", strsplit(trim(out), "[[:space:]]+"))
  rownames(out) <- 1:dim(out)[1]
  colnames(out) <- c("MSYLT1", "MSYLT0", "MSYLT2", "AMSYR1", "AMSYL1",
                      "AMSYR0", "AMSYL0", "AMSYR2", "AMSYL2")
  out <- apply(out, 2, as.numeric)
  results$msyl <- out

  # km: Carrying capacity of the mature population

  results$km <- getval(data, "Carrying capacity \\(mature\\)")

  # k1: Carrying capacity of the 1+ population
  results$k1 <- getval(data, "Carrying capacity \\(1\\+\\)")

  # in
  results$input <- c(
    "MSYL" = getval(data, "MSYL      "),
    "MSYR" = getval(data, "MSY rate"),
    "DD" = tolower(results$dd),
    "Deterministic" = ifelse(getval(data, "ETA") == 1, "Stochastic", "Deterministic"),
    "Component" = gsub(" ", "", strsplit(grep("MSYR component", data, value = TRUE), "[0-9]")[[1]][2]))

  results$zz <- getval(data, "Density dependent exponent")
  results$aa <- getval(data, "Resilience parameter")

  # biomass:
  # c("Year", "PTRUE", "PSURV", "Catch", "BIRSUM")

  results$biomass <- list()
  counter <- 1
  results$ptrueterm    <- vector(length = results$ntrials)
  results$psurvterm    <- vector(length = results$ntrials)
  results$catchterm    <- vector(length = results$ntrials)
  results$depletion    <- vector(length = results$ntrials)
  results$depletionall <- vector(length = results$ntrials)
  results$msyr <- vector(length = results$ntrials)

  for (yr in (grep("CM 1", data) + 1)) {
    out <- do.call("rbind",
      strsplit(trim(data[yr:(yr + results$nyears)]), "[[:space:]]+"))
    out <- data.frame(apply(out, 2, as.numeric), stringsAsFactors = FALSE)
    colnames(out) <- c("year", "ptrue", "psurv", "catch", "birsum")
    if (tail(out, 1)[4] < 0) {
      out <- out[-dim(out)[1], ]
    }
    out$trial <- rep(counter, dim(out)[1])
    results$biomass[[counter]] <- out
    results$ptrueterm[counter] <- tail(out$ptrue, 1)
    results$psurvterm[counter] <- tail(out$psurv, 1)
    results$catchterm[counter] <- tail(out$catch, 1)
    results$depletion[counter] <- results$ptrueterm[counter] / results$km[counter]
    results$depletionall[counter] <- results$ptrueterm[counter] / results$k1[counter]
    results$msyr[counter] <-
      results$catchterm[counter] / results$ptrueterm[counter]

    counter <- counter + 1
  }
  results$biomass <- do.call("rbind", results$biomass)


  # nzero: NZERO
  out <- unlist(strsplit(grep("NZERO", data, value = TRUE), ":"))[2]
  results$nzero <- as.numeric(gsub(" ", "", out))

  invisible(return(results))
}
