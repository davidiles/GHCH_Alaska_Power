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

``` r

  n_males = 1000
  n_ARU = 300
  
  # --------------------------------------------------
  # Simulate bird territories
  # --------------------------------------------------
  
  # Bird territories
  bird_centroids <- sample_n(birdgrid,n_males)
  
  # Detectable radius of each location
  bird_TD = st_buffer(bird_centroids, bird_TrD)
  
  # Plot bird territories
  bird_radius_m = round(bird_Tr*1000)
  
  ggplot() +
    geom_sf(data = bird_TD,fill = "transparent", col = "blue")+
    scale_fill_manual(values = c("transparent","orangered"),na.value = "transparent",name="Detection")+
    coord_sf(xlim = c(-dim,dim), ylim = c(-dim,dim))+
    xlab("km")+ylab("km")+
    ggtitle(paste0("Number of males = ",n_males,"\nBird Territory Radius = ",bird_radius_m," m\nBird EDR = ",bird_EDR*1000," m"))
```

# Simulation results

You can also embed plots, for example:

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
