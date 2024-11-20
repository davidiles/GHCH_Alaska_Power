library(sp)
library(sf)
library(tidyverse)
library(jagsUI)

rm(list=ls())

setwd("C:/Users/IlesD/OneDrive - EC-EC/Iles/Projects/Landbirds/YT-GHCH-COSEWIC/GHCH_Alaska_Power")

# Set up study area
plot_area = 10117
plot_dim = sqrt(plot_area)
dim <- plot_dim/2

study_area_boundary <- data.frame(x = c(-dim,dim),y=c(-dim,dim)) %>%
  st_as_sf(coords = c("x", "y")) %>% 
  st_bbox() %>% 
  st_as_sfc()

# Bird territory/detectability parameters
bird_EDR = 0.045        # bird EDR
bird_TA = 0.17          # km2 territory area for each bird
bird_Tr = sqrt(bird_TA/pi) # radius of bird territories
bird_TrD = bird_Tr + bird_EDR  # Radius of detectable bird territories

# Hypothetically saturated landscape (this step ensures territories do not overlap)
birdgrid <- st_make_grid(study_area_boundary, cellsize = bird_TrD*2, square=FALSE, what = "polygons") %>%
  st_as_sf() %>% mutate(hexid = 1:nrow(.)) %>% dplyr::rename(geometry = x) %>%
  st_centroid()

# ARU grid (sampling frame) - assume no ARUs can be closer than 300m from each other
ARUgrid <- st_make_grid(study_area_boundary, cellsize = 0.3, square=FALSE, what = "polygons") %>%
  st_as_sf() %>% mutate(hexid = 1:nrow(.)) %>% dplyr::rename(geometry = x) %>%
  st_centroid()

# Range of scenarios to consider
scenarios <- expand.grid(n_males = c(500,1000,2000),
                         n_ARU = c(150,300,600),
                         run_number = 1:1000)

all_results <- data.frame()

for (i in 1:nrow(scenarios)){
  
  print(i)
  n_males = scenarios$n_males[i]
  n_ARU = scenarios$n_ARU[i]
  
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

  # --------------------------------------------------
  # Simulate ARU placement
  # --------------------------------------------------
  
  # SRS sampling
  aru_sf <- sample_n(ARUgrid,n_ARU)
  
  aru_detections <- sf::st_intersects(bird_TD,aru_sf) %>% as.data.frame()
  
  bird_TD$det = "no"
  
  if (nrow(aru_detections) > 0){
    bird_TD$det[aru_detections[,1]] <- "yes"
  }
  
  # Plot ARU locations
  ggplot() +
    geom_sf(data = bird_TD,aes(fill = det), col = "red", linetype = 2)+
    geom_sf(data = aru_sf, shape = 19)+
    scale_fill_manual(values = c("transparent","orangered"),na.value = "transparent",name="Detection")+
    coord_sf(xlim = c(-dim,dim), ylim = c(-dim,dim))+
    xlab("km")+ylab("km")+
    ggtitle(paste0("Number of males = ",n_males,"\nBird Territory Radius = ",bird_radius_m," m\nBird EDR = ",bird_EDR*1000," m"))

  
  # --------------------------------------------------
  # Calculate number of ARUs with detections
  # --------------------------------------------------
  n_ARU_det <- length(unique(aru_detections$col.id))
  n_ARU_det
  
  # --------------------------------------------------
  # Conduct analysis
  # --------------------------------------------------
  
  sink("ghch_model.jags")
  cat("

    model {

      # Moderately informative prior on density (assume it is low, but with enough uncertainty to capture higher densities)
      dens_prior_median <- 0.2
      dens_prior_sd <- 2
      
      dens ~ dlnorm(log(dens_prior_median),pow(dens_prior_sd,-2))
      popsize <- IAO_area * dens
      
      total_area_detectable <- popsize * pi * (territory_radius + EDR)^2
      proportion_detectable <- total_area_detectable/IAO_area
      
      # Likelihood
      y ~ dbin(proportion_detectable, n_ARU)
      
      dens_prior ~ dlnorm(log(dens_prior_median),pow(dens_prior_sd,-2))
      popsize_prior <- IAO_area * dens_prior
    }
    
",fill = TRUE)
  sink()
  
  # Analyze using JAGS
  jags.data <- list(y = n_ARU_det,   # Number of ARUs that detected the species
                    n_ARU = n_ARU,
                    IAO_area = plot_area,
                    territory_radius = bird_Tr,
                    EDR = bird_EDR,
                    pi = pi)
  
  out <- jags(data = jags.data,
              model.file = "ghch_model.jags",
              parameters.to.save = c("dens","popsize","popsize_prior","dens_prior"),n.chains = 3,
              n.thin = 1,
              n.iter = 6000,
              n.burnin = 1000,
              verbose = FALSE)
  
  # --------------------------------------------------
  # Store results
  # --------------------------------------------------
  
  # Summary statistics
  run_results <- data.frame(run_number = scenarios$run_number[i],
                            plot_area = plot_area,
                            n_males = n_males,
                            true_density = n_males/plot_area,
                            n_ARU = n_ARU,
                            n_detections = n_ARU_det,
                            
                            popsize_est_mean = mean(out$sims.list$popsize),
                            popsize_est_q50 = median(out$sims.list$popsize),
                            popsize_est_q025 = quantile(out$sims.list$popsize,0.025),
                            popsize_est_q975 = quantile(out$sims.list$popsize,0.975),
                            
                            prob_less_than_100 = mean(out$sims.list$popsize<100),
                            prob_less_than_1000 = mean(out$sims.list$popsize<1000),
                            
                            dens_est_mean = mean.Date(out$sims.list$dens),
                            dens_est_q50 = median(out$sims.list$dens),
                            dens_est_q025 = quantile(out$sims.list$dens,0.025),
                            dens_est_q975 = quantile(out$sims.list$dens,0.975),
                            
                            popsize_prior_mean = mean(out$sims.list$popsize_prior),
                            popsize_prior_q50 = median(out$sims.list$popsize_prior),
                            popsize_prior_q025 = quantile(out$sims.list$popsize_prior,0.025),
                            popsize_prior_q975 = quantile(out$sims.list$popsize_prior,0.975),
                            
                            dens_prior_mean = mean(out$sims.list$dens_prior),
                            dens_prior_q50 = median(out$sims.list$dens_prior),
                            dens_prior_q025 = quantile(out$sims.list$dens_prior,0.025),
                            dens_prior_q975 = quantile(out$sims.list$dens_prior,0.975)
                            
                            
  )
  
  all_results <- rbind(all_results,run_results)
}


#load("power_wksp.RData")

# --------------------------------------------------
# Summarize results of repeated simulations
# --------------------------------------------------

# Text labels for plots
all_results$n_male_label <- paste0("n_males = ",all_results$n_males) %>% factor(levels = unique(.))
all_results$n_ARU_label <- paste0("n_ARU = ",all_results$n_ARU) %>% factor(levels = unique(.))
all_results$study_area_label <- paste0("study_area = ",all_results$plot_area," km^2") %>% factor(levels = unique(.))
all_results$cov <- all_results$popsize_est_q025 < all_results$n_males & all_results$popsize_est_q975 > all_results$n_males
all_results$detected_species <- all_results$n_detections>0

# Summary of results across repeated simulations
result_summary <- all_results %>%
  group_by(n_male_label,n_ARU_label,study_area_label) %>%
  summarize_all(mean)


# --------------------------------------------------
# Plot expected number of detections under each scenario
# --------------------------------------------------

ggplot(all_results)+
  geom_histogram(aes(x = n_detections), binwidth = 0.5)+
  facet_grid( study_area_label + n_ARU_label ~ n_male_label)+
  geom_text(data = result_summary,aes(x = 10, y = 150, label = paste0("Prob. of at least 1 detection = ",signif(result_summary$detected_species,2))),hjust=0)+
  theme_bw()+
  xlab("Number of detections")+
  ylab("Frequency (number of simulations)")+
  ggtitle("Summary of GHCH detections across repeated simulations")

# --------------------------------------------------
# Plot expected counts under each scenario
# --------------------------------------------------

ggplot(result_summary)+
  geom_hline(aes(yintercept = n_males), linetype = 2)+
  
  # Plot prior
  #geom_errorbar(aes(x = run_number,ymin = popsize_prior_q025, ymax = popsize_prior_q975), width = 0, col = "dodgerblue", size = 3, alpha = 0.2)+
  #geom_point(aes(x = run_number,y = popsize_prior_q50), col = "dodgerblue", size = 3, alpha = 0.2)+
  
  # Plot actual estimate (prior x data)
  geom_errorbar(aes(x = n_ARU,ymin = popsize_est_q025, ymax = popsize_est_q975), width = 0)+
  geom_point(aes(x = n_ARU,y = popsize_est_mean))+
  
  facet_grid( study_area_label ~ n_male_label)+
  theme_bw()+
  xlab("Number of ARUs")+
  ylab("Population Estimate")+
  ggtitle(paste0("Population estimates for different combinations of\nn_ARU and n_males"))

# --------------------------------------------------
# Check that priors were reasonable
# --------------------------------------------------

ggplot(result_summary)+
  geom_hline(aes(yintercept = n_males), linetype = 2)+
  
  # Plot prior
  geom_errorbar(aes(x = n_ARU,ymin = popsize_prior_q025, ymax = popsize_prior_q975), width = 0, col = "dodgerblue", size = 3, alpha = 0.2)+
  geom_point(aes(x = n_ARU,y = popsize_prior_q50), col = "dodgerblue", size = 3, alpha = 0.2)+
  
  facet_grid( study_area_label ~ n_male_label)+
  theme_bw()+
  xlab("Number of ARUs")+
  ylab("Population Estimate")+
  ggtitle(paste0("Population estimates for different combinations of\nn_ARU and n_males"))

save.image("power_wksp.RData")