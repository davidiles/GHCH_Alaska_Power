# Motivation

The goal of this study is to evaluate the sample size requirements to
estimate population sizes of Gray-headed Chickadee in Alaska.

# Simulation description

These simulations place home ranges on a hypothetical landscape, and
conduct simple random sampling across that landscape to estimate the
expected number of detections that would occur under various
population/sampling scenarios.

The simulated data are then analyzed with a Bayesian model to produce
estimates of population size, which can then be compared to the ‘true’
(i.e., simulated) numbers of birds. These estimates of population size
are then used to conduct precision and power analyses.

### Simulation assumptions

1.  bird territories are distributed randomly across available habitat
2.  bird territory boundaries do not overlap
3.  the area of potentially suitable GHCH habitat in AK is 10117
    km<sup>2</sup> (estimated based on remote sensing)
4.  surveys are positioned independently of bird occurrence
5.  surveys are not clustered

### Example of simulated landscape and survey

We assume in this example that there are 1000 GHCH pairs distributed
across the landscape, such that density is 0.099 birds/km<sup>2</sup>.

![](README_files/figure-markdown_github/chunk1-1.png)

# Simulation results

You can also embed plots, for example:

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
