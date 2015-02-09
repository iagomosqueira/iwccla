MANTST
===============================================================================

# Summary:
#### Using the International Whaling Commission's (IWC's) procedure for setting
safe catch limits for commercial whaling, also known as the Revised Management
Procedure (RMP), determine the acceptable level of catch such that the stock
not be depleted (given a certain level of probability) below an established
fraction of carrying capacity such that the risk of extinction is not seriously
increased by exploitation.

## Program description:
A simple population dynamics model parameterized with prior distributions for
all parameters. A procedure is used to calculate posterior distributions of
historic catches and available abundance estimates, where standard errors are
nominal and correlations (on log scale) are quadrupled. A catch control rule
determines catch limits. Input data consists of: a) time series of historic
annual catches, b) time series of absolute abundance estimates, c) standard
errors of abundance estimates, d) correlation of abundance estimates. Catches
begin in year 0, where they are assumed to be known without error and come from
a population at equilibrium.

## TODO:
 * [x] Compile code
 * [x] Create a list of scenarios to run
 * [] Create checks that verify the code
 * [] Run scenarios
 * [] Create LaTeX code that compiles the results
 * [] Create report

## Files:
 * Mav-v14.for
 * Man-v14z.for
 * MANEXTRD.FOR
 * MANRESV8.FOR
 * MANTST14.FOR
 * MANSTALT.FOR
 * NRMP-INC.FOR
 * NRMP-INF.FOR
 * Nrmp.for
 * RANDOM.NUM
 * T1A-d1.dat
 * T1A-r1.dat
 * T1A-s1.dat
 * T1B-d1.dat
 * T1B-r1.dat
 * T1B-s1.dat

## Questions

## Notes