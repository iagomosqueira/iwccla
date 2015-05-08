#' Create a data file for the IWC CLA MANTST program
#'
#' @details
#'
#' @param out A character value specifying where to write the final output.
#'   If a partial path is provided it will be writen in relation to the current
#'   working directory, or a full path can be specified. If \code{NULL}, no
#'   file is written and instead the output can be seen by the return value.
#' @param case
#' @param comment
#' @param optran A switch specifying whether or no there should be random parameters,
#'   where \code{0} and \code{1} specify no and yes, respectively.
#' @param optb A switch specifying whether or not there is variable bias,
#'   where \code{0} and \code{1} specifying no and yes, respectively.
#' @param optc A switch specifying the reported catch option,
#'   where \code{0} and \code{1} specifying no and yes, respectively.
#' @param optdt A switch specifying the density-dependence type,
#'   where \code{0} and \code{1} specify density-dependence on fecundity
#'   and natural mortality, respectively.
#' @param optdet A switch specifying the stochasticity type,
#'   where \code{0} and \code{1} specifying no and yes, respectively.
#' @param optsur A switch specifying the survey cost option,
#'   where \code{0} and \code{1} specifying no and yes, respectively.
#' @param ntrial A numeric value specifying the number of trials to run.
#' @param nyear A numeric value specifying the number of years in a trial.
#' @param npcat A numeric value specifying the number of years of pre-management
#'   catch.
#' @param nprot A numeric value specifying the number of years of pre-management
#'   protection.
#' @param msyl A numeric value specifying the true maximum sustainable yield level.
#'   Where level refers to the terminal population divided by carrying capacity.
#' @param msyr1 A numeric value specifying the true maximum sustainable yield rate.
#'   Where rate refers to the fishing rate to obtain the maximum sustainable yield.
#' @param depl A numeric value specyifying the initial depletion level.
#' @param k99 A numeric value specifying how carying capacity changes with time.
#'   More details are needed here. \link{\code{k99yr}}.
#' @param msyr99 A numeric value specifying how the maximum sustainable yield rate
#'   changes with time. More details are needed here. \link{\code{msyr99yr}}.
#' @param istep A numeric value specifying the chosen option for time-varying
#'   maximum sustainable yield rate. More details are needed here.
#' @param mat1 A numeric value specifying maturity 1. More details are needed here.
#' @param erate A numeric value specifying the probability that the population will
#'   undergo an epidemic in any given year.
#' @param component A numeric value of \code{0:2} specifying which component
#'   density-dependence acts on. Where \code{0} is the exploitable, \code{1} is the
#'   1+ population, and \code{2} is the mature population.
#' @param ifreq A numeric value specifying the frequency at which surveys
#'   are conducted.
#' @param bias0 A numeric value specifying the bias in abundance estimates.
#' @param cv1est A numeric value specifying the coefficient of variation for the
#'   coefficient of variation estimates.
#' @param inita A numeric value specifying an intial value for the parameter
#'   specifying the resilience parameter *A*.
#' @param initz A numeric value specifying an intial value for the parameter
#'   specifying the degree of compensation *z*.
#' @param k99yr A numeric value specyifying the year in which carrying capacity
#'   changes to linearly, as specified by \code{k99} * *K*.
#' @param msyr99yr A numeric value specyifying the year in which maximum
#'   sustainable yield rate changes to linearly, as specified by
#'   \code{msyr99} * *MSYR*.
#' @return Invisibly returns lines necessary to produce a data file for the IWC
#'   CLA MANTST program.
##' @seealso \code{\link{functionname}}
#' @author Kelli Faye Johnson
#' @export

create_dat <- function(out, case = "T1A-D1",
  comment = "Written by create_dat by (KFJ)", optran = 0, optb = 0, optc = 0,
  optdt = 0, optdet = 0, optsur = 0, ntrial = 400, nyear = 300, npcat = 30,
  nprot = 0, msyl = 0.6, msyr1 = 0.01, depl = 0.99, k99 = 0.000, msyr99 = 0.000,
  istep = 0, mat1 = 7.00, erate = 0.00, component = 1, ifreq = 5, bias0 = 1.0,
  cv1est = 0.20, inita = 0.20, initz = 0.20, k99yr = NULL, msyr99yr = NULL) {

  # Set up which component density dependence acts on
  optf <- optmsyl <- optdd <- component

  # Set up the dat object
  dat <- vector(length = 47)
  dat[1] <- paste0("MANAGEMENT PARAMETERS         CASE  ", case)
  dat[2] <- paste0(case, " Using create_dat (KFJ)")
  dat[3] <- ""
  if (!optran %in% 0:1) stop(paste("optran (0:1) is", optran))
  dat[4] <- paste0("RANDOM PARAMETERS OPTION             OPTRAN    ", optran)
  if (!optb %in% 0:1) stop(paste("optb (0:1) is", optb))
  dat[5] <- paste0("VARIABLE BIAS OPTION                 OPTB      ", optb)
  if (!optc %in% 0:1) stop(paste("optc (0:1) is", optc))
  dat[6] <- paste0("REPORTED CATCH OPTION                OPTC      ", optc)
  dat[7] <- paste0("PRODUCTION MODEL OPTION              OPTMOD    5")
  dat[8] <- paste0("P>K BIRTH CALCULATION OPTION         OPTDK     0")
  if (!optdt %in% 0:1) stop(paste("optdt (0:1) is", optdt))
  dat[9] <- paste0("DENSITY-DEPENDENCE TYPE              OPTDT     ", optdt)
  if (!optdet %in% 0:1) stop(paste("optdet (0:1) is", optdet))
  dat[10] <- paste0("STOCHASTICITY OPTION                 OPTDET    ", optdet)
  if (!optsur %in% 0:1) stop(paste("optsur (0:1) is", optdet))
  dat[11] <- paste0("SURVEY COSTS OPTION                  OPTSUR    ", optsur)
  dat[12] <- paste0("No. OF TRIALS                        NTRIAL  ", ntrial)
  dat[13] <- paste0("No. OF YEARS IN SIMULATION           NYEAR  ", nyear)
  dat[14] <- paste0("No. OF YEARS OF PREMANAGEMENT CATCH  NPCAT    ", npcat)
  dat[15] <- paste0("YEARS OF PREMANAGEMENT PROTECTION    NPPROT    ", nprot)
  dat[16] <- paste0("TRUE MSYL(1)                         MSYL     ", msyl)
  dat[17] <- paste0("TRUE MSY RATE(1)                     MSYR1    ", msyr1)
  dat[18] <- paste0("PREMANAGEMENT DEPLETION (1)          DEPL     ", depl)
  dat[19] <- paste0("CHANGING K OPTION                    K99        ", k99)
  dat[20] <- paste0("CHANGING MSYR OPTION                 MSYR99     ", msyr99)
  dat[21] <- paste0("CHANGING MSYR STEP                   ISTEP     ", istep)
  dat[22] <- paste0("MATURITY PARAMETER                   MAT1     ", mat1)
  dat[23] <- paste0("MATURITY SIGMA                       MSIG     1.20")
  dat[24] <- paste0("RECRUITMENT PARAMETER                REC1     7.00")
  dat[25] <- paste0("RECRUITMENT SIGMA                    RSIG     1.20")
  dat[26] <- paste0("MORTALITY PARAMETER 1                MORT1    0.04")
  dat[27] <- paste0("MORTALITY PARAMETER 2                MORT2    0.07")
  dat[28] <- ""
  dat[29] <- paste0("MORTALITY FUNCTION                   MORTIP   -1")
  dat[30] <- paste0("MAXIMUM AGE                          MAXAGE   20")
  dat[31] <- paste0("MINIMUM AGE OF MATURITY              MINMAT    0")
  dat[32] <- paste0("EPIDEMIC RATE                        ERATE    ", erate)
  dat[33] <- paste0("COMPONENTS (0=EXPLOITABLE; 1=TOTAL1+; 2=MATURE)")
  dat[34] <- paste0("MSYR COMPONENT                       OPTF     ", optf)
  dat[35] <- paste0("MSYL COMPONENT                       OPTMSYL  ", optmsyl)
  dat[36] <- paste0("DENSITY-DEPENDENT COMPONENT          OPTDD    ", optdd)
  dat[37] <- paste0("FREQUENCY OF ABUNDANCE ESTIMATES     IFREQ    ", ifreq)
  dat[38] <- paste0("YEAR OF LAST SURVEY                  ENDSUR ", nyear)
  dat[39] <- paste0("YEAR CV CHANGES                      IYRCV  ", nyear)
  dat[40] <- paste0("BIAS IN ABUNDANCE ESTIMATES          BIAS0    ",
    as.character(format(bias0, digits = 2, nsmall = 2)))
  dat[41] <- paste0("CV OF CV ESTIMATES (1st)             CV1EST   ", cv1est)
  dat[42] <- paste0("PROCESS ERROR PARAMETER              ETA      1.00")
  dat[43] <- paste0("MINIMUM No. OF DEGREES OF FREEDOM    DOFMIN   5.00")
  dat[44] <- paste0("STARTING VALUE FOR A                 INITA    ", inita)
  dat[45] <- paste0("STARTING VALUE FOR Z                 INITZ    ", initz)
  dat[46] <- paste0("DEPLETION (0:SINGLE VALUE;1:READ IN) OPTDPL    0")
  dat[47] <- paste0("DEPLETION FILE (IF OPTDPL=1)                 DEPL99.CSV")

  # Set up variable inputs based on previous arguments for time-varying K and MSYR
  if (k99 > 0) {
    dat <- append(dat, values = paste0("K99 YEAR                             MSIG     ",
      k99yr), after = 22)
  }
  if (msyr99 > 0) {
    dat <- append(dat, values = paste0("K99 YEAR                             MSIG     ",
      msyr99yr), after = ifelse(is.null(k99yr), 22, 23))
  }

  if (!is.null(out)) writeLines(dat, out)
  invisible(dat)
}
