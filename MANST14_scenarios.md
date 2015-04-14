MANTST Scenarios
===============================================================================

## Summary:
A list of scenarios needed for 2015 IWC with respect to the
MANTST.

**Authors**: Kelli Faye Johnson, Cherry Allison, and Andr&eacute; E. Punt

**Last Edited**: 2015-04-12

### Base:
 * Length of simulation = 100 years
 * Number of iterations per trial = 400
 * Catches = a. no catches and b. catches specified by CLC.
 * Pre-management depletion =
    a. 0.99K
    b. 0.60K
    c. 0.40K
    d. 0.30K
    e. 0.20K
    f. 0.05K
    g. Depletion sets for response curves:
        1. Low: 0.05, 0.10, 0.20, 0.30, 0.40, 0.50
        2. Full: 0.05, 0.20, 0.40, 0.60, 0.80, 0.99
 * Density-dependence type = a. fecundity and b. natural mortality (*M*).
 * $MSYR_{mat}$ =
    a. 0.010: all trials
    b. 0.015: all trials to represent $MSYR_{1+}$ = 0.010
    c. 0.040: all trials except survey bias, $P_0 = 0.6K$, and $P_0 = 0.05K$
    d. 0.070: only for $P_0 = 0.99K$ and $P_0 = 0.3K$
 * $MSYL$ =
    a. 0.30K
    b. 0.45K
    c. 0.60K
    d. 0.75K
    e. 0.90K
 * Survey bias =
    a. 0.5
    b. 1.0
    c. 1.2
    d. 1.5
    e. 1.6
    f. 1.8
    g. 2.0
 * Historic error in catch (percent of true):
    a. 1.00
    b. 0.80
    c. 0.60
    d. 0.50
    e. 0.40
    f. 0.20
    g. 0.01
 * Episodic events (yearly chance the population is halved): a. 0.0 and b. 0.02
 * Changes in carrying capacity over the management period: a. doubles, b. same, and c. halves
 * Historic catch: 30 years
 * Period of protection: 0 years
 * $CV_{obs}$: 0.2
 * Process error factor: 1.0
 * Survey frequency: 5 years
 * Surveys cease after: terminal year

### Response curves:
 * $MSYL$ =
    a. 0.30K
    b. 0.45K
    c. 0.60K
    d. 0.75K
    e. 0.90K
 * $MSYR_{mat}$ =
    a. 0.005:
    b. 0.010:
    c. 0.015:
    d. 0.025:
    e. 0.040:
    f. 0.070:
    g. Time-varying $MSYR$
        1. 0.01: a. 14 year steps 1-4-1, b. 14 year steps 4-1-4, c. 33 year steps 1-4-1, d. 33 year steps 4-1-4
        2. 0.01: a. Linear increase from 0.01 to 0.02, b. linear increase from 0.01 to 0.04, c. linear decrease from 0.04 to 0.01, d. linear decrease from 0.04 to 0.02, and e. constant at 0.04.
 * Age-at-maturity: a. 7 and b. 10 years
 * Recruitment: a. standard, b. maximum recruitment limitation, and c. 25 year recruitment delay
 * Historic catch: a. 10, b. 30, and c. 50 years
 * Period of protection: a. 0, b. 15, and c. 30 years
 * Environmental decrease in *K* and *MSYR* (linear decrease):
     a. 1.00
     b. 0.80
     c. 0.60
     d. 0.40
     e. 0.20
     f. 0.01
 * Time-varying *K*:
     a. constant
     b. increases linearly to 2.0
     c. decreases linearly to 0.5
     d. cyclical starting at the minimum value
     e. cyclical starting at the maximum value
 * Bias in survey:
     a. 0.50
     b. 1.00
     c. 1.50
     d. increases linearly from 0.50 to 1.00
     e. decreases linearly from 1.50 to 1.00
 * $CV_{obs}$:
     a. 0.1
     b. 0.2
     c. 0.4
     d. 0.6
     e. 0.8
     f. 1.0
 * Process error factor (with $CV_{obs}$ = 0.2:
     a. 0.0
     b. 0.5
     c. 1.0
     d. 2.0
     e. 3.0
     f. 4.0
 * Survey frequency (years):
     a. 2
     b. 5
     c. 7
     d. 10
     e. 20
 * Surveys cease after (years):
     a. 0
     b. 20
     c. 40
     d. 60
     e. 80
     f. terminal year

### Need:
 * Length of simulation = 300 years (@aldrin_2008)
 * Parameter sensitivity: to be informed by Lars Wall&oslash;e
