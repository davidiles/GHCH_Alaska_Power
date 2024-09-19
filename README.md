## Overview

The goal of this analysis is to ...

library(sp)
#library(rgeos)
library(sf)
library(tidyverse)
library(jagsUI)

rm(list=ls())

# --------------------------------------------------
# Simulate bird territories
# --------------------------------------------------

# Parameters
bird_dens = 0.05        # birds / km2
bird_EDR = 0.045       # bird EDR
bird_TA = 0.17        # km2 territory size for each bird
bird_Tr = sqrt(bird_TA/pi) # radius of bird territories
bird_TrD = bird_Tr + bird_EDR  # Radius of detectable bird territories

# Study area
plot_area = 3303
expected = plot_area*bird_dens  # Expected number of birds
plot_dim = sqrt(plot_area)
dim <- plot_dim/2

# Bird territories
xy = data.frame(x = runif(expected,-dim,dim),y = runif(expected,-dim,dim)) # Centroid of each bird territory
bird_centroids <- SpatialPoints(coords = xy)

# Territories as polygons
bird_T_sf <- st_as_sf(bird_T_poly)
bird_T_poly = st_buffer(bird_centroids, byid = TRUE, width = bird_Tr)

# Add EDR onto edge of territory
bird_TD_poly = gBuffer(bird_centroids, byid = TRUE, width = bird_TrD)
bird_TD_sf <- st_as_sf(bird_TD_poly)

# Plot
bird_radius_m = round(bird_Tr*1000)
ggplot(bird_T_sf) +
  geom_sf(fill = "transparent", col = "blue")+
  scale_fill_manual(values = c("transparent","orangered"),na.value = "transparent",name="Detection")+
  coord_sf(xlim = c(-dim,dim), ylim = c(-dim,dim))+
  xlab("km")+ylab("km")+
  ggtitle(paste0("Bird Density = ",bird_dens," male / km^2\nBird Territory Radius = ",bird_radius_m," m\nBird EDR = ",bird_EDR*1000," m"))
