Testing MSYL and MSYR with density dependent fecundity and natural mortality.
===============================================================================

**Authors**: Kelli Faye Johnson

**Last Edited**: 2015-02-20

### Abstract
Using simulation to verify the yield function for the BALEEN II population
dynamics model when density dependence operates on natural mortality or fecundity.
Plots of yield versus depletion were plotted across a range of
MSYL and MSYR values.

### Methods
The BALEEN II (@punt_1999) population dynamics model, parameterized for the
Norewegian harvest of minke whales, was used as an operating model (OM).
The OM time series included 1999 years of data and was simulated for 1 iteration,
as the model was ran deterministically with no error in
with each iteration containing zero process error, but having observation error
in the estimates of abundance.

A factorial design was performed where density dependence acted on either the
mature population or the total 1+ population either
through fecundity (*f*) or natural mortality (*m*). All combinations of true
MSYL and MSYR values were simulated for all four density dependence scenarios.

### Results
The least biased and most consistent results were obtained when density dependence
acted on the mature population through fecundity rather than mortality (Figure 1).
The calculated MSYR increased linearly as the true MSYR increased and followed a 1:1
relationship for all explored scenarios.
The model experienced some convergence issues for values of MSYR greater than 0.4 when
density dependence acted on *m* (Figure 2). Convergence was worse overall when density
dependence operated on the 1+ population (Figures 3 and 4) but was worse so (i.e.,
values of MSYR greater than 0.3) when density dependence operated on the 1+ population
through natural mortality. I am not sure why but for all scenarios with a true MSYL of
0.3 catches went to zero, particularly for higher values of MSYR (Figures 1, 2, 3, and 4).

Bias in MSYL (Catch in the terminal year / carrying capacity of the mature population)
increased the further MSYL(True) was from 0.3 (Figure 5, 6, 7, 8).
When density dependence acted on the mature population and depletion was calculated using
the carrying capacity of the 1+ population the bias was removed. This seems counter-intuitive
because this is not the population in which density dependence is acting on.
For different MSYR values catches increased as MSYR increased but the level of depletion
remained constant. The previous statement is really only true when density dependence
acted on the mature population through fecundity (Figure 5). The remaining plots are lacking
straight lines and have no real pattern (Figures 6, 7, and 8).

### Figures
![Figure 01][fig011]
Figure 01. Calculated MSYL (top panel) and MSYR (bottom panel) across
combinations of true MSYR (0.005 - 0.04; x axis) and MSYL (0.4 - 0.8; see legend),
when density dependence operates on the mature population through fecundity.
Lines in the lower panel are slightly jittered to see that they all follow a 1:1
relationship as expected.

![Figure 02][fig012]
Figure 02. Calculated MSYL (top panel) and MSYR (bottom panel) across combinations
of true MSYR (0.005 - 0.04; x axis) and MSYL (0.4 - 0.8; see legend), when density
dependence operates on the mature population through natural mortality.
Lines in the lower panel are slightly jittered to see that they all follow a 1:1
relationship as expected.
Higher values of MSYR (i.e., greater than 0.4) had convergence issues.


![Figure 03][fig021]
Figure 03. Calculated MSYL (top panel) and MSYR (bottom panel) across
combinations of true MSYR (0.005 - 0.04; x axis) and MSYL (0.4 - 0.8; see legend),
when density dependence operates on the 1+ population through fecundity.
Lines in the lower panel are slightly jittered to see that they all follow a 1:1
relationship as expected.
Higher values of MSYR (i.e., greater than 0.4) had convergence issues.

![Figure 04][fig022]
Figure 04. Calculated MSYL (top panel) and MSYR (bottom panel) across combinations
of true MSYR (0.005 - 0.04; x axis) and MSYL (0.4 - 0.8; see legend), when density
dependence operates on the 1+ population through natural mortality.
Lines in the lower panel are slightly jittered to see that they all follow a 1:1
relationship as expected.
Higher values of MSYR (i.e., greater than 0.3) had convergence issues.

![Figure 05][fig111]
Figure 05. Yield across a range of true MSYL values (0.4 - 0.8; see legend).
Points represent yield versus depletion level in the terminal year where carrying capacity
is either calculated from the mature population (top panel) or the 1+ population (bottom panel),
when density dependence operates on the mature population through fecundity.

![Figure 06][fig112]
Figure 06. Yield across a range of true MSYL values (0.4 - 0.8; see legend).
Points represent yield versus depletion level in the terminal year where carrying capacity
is either calculated from the mature population (top panel) or the 1+ population (bottom panel),
when density dependence operates on the mature population through natural mortality.

![Figure 07][fig121]
Figure 07. Yield across a range of true MSYL values (0.4 - 0.8; see legend).
Points represent yield versus depletion level in the terminal year where carrying capacity
is either calculated from the mature population (top panel) or the 1+ population (bottom panel),
when density dependence operates on the 1+ population through fecundity.

![Figure 08][fig122]
Figure 08. Yield across a range of true MSYL values (0.4 - 0.8; see legend).
Points represent yield versus depletion level in the terminal year where carrying capacity
is either calculated from the mature population (top panel) or the 1+ population (bottom panel),
when density dependence operates on the 1+ population through natural mortality.

[fig011]: figures/figure011.png "Figure 01"
[fig012]: figures/figure012.png "Figure 02"
[fig021]: figures/figure021.png "Figure 03"
[fig022]: figures/figure022.png "Figure 04"
[fig111]: figures/figure111.png "Figure 05"
[fig112]: figures/figure112.png "Figure 06"
[fig121]: figures/figure121.png "Figure 07"
[fig122]: figures/figure122.png "Figure 08"

### References