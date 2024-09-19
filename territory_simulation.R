library(tidyverse)
library(sf)

# --------------------------------------------------
# Simulate birds within a 100 km x 100 km study area
# --------------------------------------------------

plot_dim = 25 # Size of landscape (x and y dimension)

n_males = 50

# Bird territories
bird_TA = 0.2         # km2 territory area for each bird
bird_Tr = sqrt(bird_TA/pi) # radius of bird territories

# Centroid of each bird territory
bird_xy = data.frame(x = runif(n_males,-plot_dim/2,plot_dim/2),y = runif(n_males,-plot_dim/2,plot_dim/2)) 
bird_centroids <- st_as_sf(bird_xy,coords = c("x", "y"))

# Polygon for each territory
bird_territory = st_buffer(bird_centroids, bird_Tr)

# All territories
ggplot()+
  geom_sf(data = bird_territory)+
  geom_sf(data = bird_centroids, size = 0.1)+
  theme_bw()

# Zoom in on a single territory
ggplot()+
  geom_sf(data = bird_territory[1,])+
  geom_sf(data = bird_centroids[1,])+
  theme_bw()

# --------------------------------------------------
# Simulate ARU placement
# --------------------------------------------------

n_ARU = 100
EDR = 0.05

# ARU locations
ARU_xy = data.frame(x = runif(n_ARU,-plot_dim/2,plot_dim/2),y = runif(n_males,-plot_dim/2,plot_dim/2)) 
ARU_centroids <- st_as_sf(ARU_xy,coords = c("x", "y"))

# Polygon representing ARU edr
ARU_poly <- st_buffer(ARU_centroids,EDR)

# Overlay ARU locations/EDR
ggplot()+
  geom_sf(data = bird_territory)+
  geom_sf(data = bird_centroids, size = 0.1)+
  
  geom_sf(data = ARU_poly, fill = "orangered")+
  geom_sf(data = ARU_centroids, size = 0.1, col = "red")+
  
  theme_bw()

# --------------------------------------------------
# Identify the number of birds (i.e., territories) detected
# --------------------------------------------------

detected <- st_intersection(ARU_poly,bird_territory)
n_detected <- nrow(detected)

# --------------------------------------------------
# Calculated the expected number of detections
# - requires knowledge of landscape size, number of birds, 
#   bird territory radius, number of ARUs, and ARU_edr
# --------------------------------------------------

total_study_area <- plot_dim^2
total_detectable_area <- n_ARU*pi*(EDR + bird_Tr)^2
total_territory_area <- n_males*pi*(bird_Tr)^2

proportion_area_detectable <- total_detectable_area/total_study_area
proportion_area_territory <- total_territory_area/total_study_area

expected_detections