readRRR2 <- function (filename) {
  widths <- c(14, 6, 8, 6, 6, 6, 8, 6, 6, 8, 6, 6, 8, 6, 6, 10, 8, 6, 10, 6, 11)
  output <- read.fwf(filename, widths, strip.white = TRUE, stringsAsFactors = FALSE)
  colnames <- c("trial", "depl", "msyr", "tc5", "tcMed", "tc95", "tcMean",
  "pf5", "pfMed", "pf95", "pl5", "pl10", "pl25",
  "ccc1", "ccc2", "ccc3", "AAV", "crpl1", "crpl2", "crr1", "crr2")
  colnames(output) <- colnames
  if (is.factor(output$AAV)) {
    output$AAV <-  gsub("[[:punct:]]$", "", output$AAV)
  }

    return(output)
}
