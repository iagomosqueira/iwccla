readRRR <- function (file, widths, header = FALSE, sep = "\t", skip = 0,
    row.names, col.names, n = -1, buffersize = 2000, ...) {
    doone <- function(x) {
        x <- substring(x, first, last)
        x[!nzchar(x)] <- NA_character_
        x
    }
    if (is.list(widths)) {
        recordlength <- length(widths)
        widths <- do.call("c", widths)
    }
    else recordlength <- 1L
    drop <- (widths < 0L)
    widths <- abs(widths)
    buffersize <- (buffersize%/%recordlength) * recordlength
    FILENAME <- tempfile("Rfwf.")
    on.exit(unlink(FILENAME))
    FILE <- file(FILENAME, "a")
    on.exit(close(FILE), add = TRUE)
    if (is.character(file)) {
        file <- file(file, "rt")
        on.exit(close(file), add = TRUE)
    }
    else if (!isOpen(file)) {
        open(file, "rt")
        on.exit(close(file), add = TRUE)
    }
    if (skip)
        readLines(file, n = skip, warn = FALSE, skipNul = TRUE)
    if (header) {
        headerline <- readLines(file, n = 1L, warn = FALSE, skipNul = TRUE)
        cat(file = FILE, headerline, "\n")
    }
    repeat ({
        if (n == 0L)
            break
        if (n == -1L)
            thisblock <- buffersize
        else thisblock <- min(buffersize, n * recordlength)
        raw <- readLines(file, n = thisblock, warn = FALSE, skipNul = TRUE)
        nread <- length(raw)
        if (recordlength > 1L && nread%%recordlength) {
            raw <- raw[1L:(nread - nread%%recordlength)]
            warning(sprintf(ngettext(nread%%recordlength, "last record incomplete, %d line discarded",
                "last record incomplete, %d lines discarded"),
                nread%%recordlength), domain = NA)
        }
        if (recordlength > 1L) {
            raw <- matrix(raw, nrow = recordlength)
            raw <- apply(raw, 2L, paste, collapse = "")
        }
        st <- c(1L, 1L + cumsum(widths))
        first <- st[-length(st)][!drop]
        last <- cumsum(widths)[!drop]
        cat(file = FILE, sapply(raw, doone), sep = c(rep_len(sep,
            length(first) - 1L), "\n"))
        if (nread < thisblock)
            break
        if (n > 0L)
            n <- n - length(raw)
    })
    close(FILE)
    FILE <- file(FILENAME, "r")
    output <- read.table(file = FILE, header = header, sep = sep, row.names = row.names,
        col.names = col.names, quote = "", as.is = FALSE, stringsAsFactors = FALSE,
        strip.white = TRUE)
    output[, 1] <- gsub("^[[:space:]]+|[[:space:]]+$", "", output[, 1])
    output[, 1] <- gsub("^[[:punct:]]+[[:space:]]+", "\\1", output[, 1])
    colnames(output) <- c("trial", "TC", "TC", "TC", "tC", "PF", "PF", "PF",
      "Pf1", "PF1", "PF2", "PL", "PL", "PL", "PL1", "PL1", "PL1", "MF", "MF",
      "MF", "MF1", "MF1", "MF1", "AAV")
    if (is.factor(output$AAV)) {
      output$AAV <-  gsub("[[:punct:]]$", "", output$AAV)
    }

    return(output)

}
