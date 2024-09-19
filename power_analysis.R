library(sp)
library(sf)
library(tidyverse)
library(jagsUI)

rm(list=ls())

setwd("C:/Users/IlesD/OneDrive - EC-EC/Iles/Projects/Landbirds/YT-GHCH-COSEWIC/GHCH_Alaska_Power")

# Study area properties
plot_area = 3303 #km2
plot_dim = sqrt(plot_area)
dim <- plot_dim/2

# Range of scenarios to consider
scenarios <- expand.grid(n_males = c(100,500,1000),
                         n_ARU = c(50,100,200,1000))

all_results <- data.frame()


for (run_number in seq(1,100)){
  
  for (i in 1:nrow(scenarios)){
    
    n_males = scenarios$n_males[i]
    n_ARU = scenarios$n_ARU[i]
    
    # --------------------------------------------------
    # Simulate bird territories
    # --------------------------------------------------
    
    # Parameters
    bird_EDR = 0.045        # bird EDR
    bird_TA = 0.17          # km2 territory area for each bird
    bird_Tr = sqrt(bird_TA/pi) # radius of bird territories
    bird_TrD = bird_Tr + bird_EDR  # Radius of detectable bird territories
    
    # Bird territories
    xy = data.frame(x = runif(n_males,-dim,dim),y = runif(n_males,-dim,dim)) # Centroid of each bird territory
    bird_centroids <- st_as_sf(xy,coords = c("x", "y"))
    
    # Detectable radius of each location
    bird_TD = st_buffer(bird_centroids, bird_TrD)
    
    # Plot
    bird_radius_m = round(bird_Tr*1000)
    ggplot() +
      geom_sf(data = bird_TD,fill = "transparent", col = "blue")+
      scale_fill_manual(values = c("transparent","orangered"),na.value = "transparent",name="Detection")+
      coord_sf(xlim = c(-dim,dim), ylim = c(-dim,dim))+
      xlab("km")+ylab("km")+
      ggtitle(paste0("Number of males = ",n_males," male / km^2\nBird Territory Radius = ",bird_radius_m," m\nBird EDR = ",bird_EDR*1000," m"))
    
    # --------------------------------------------------
    # Simulate ARU placement
    # --------------------------------------------------
    
    # SRS sampling
    aru_sf = data.frame(x = runif(n_ARU,-dim,dim), y = runif(n_ARU,-dim,dim)) %>% st_as_sf(coords = c("x","y"))
    aru_detections <- sf::st_intersects(bird_TD,aru_sf) %>% as.data.frame()
    
    bird_TD$det = "no"
    
    if (nrow(aru_detections) > 0){
      bird_TD$det[aru_detections[,1]] <- "yes"
    }
    
    ggplot() +
      geom_sf(data = bird_TD,aes(fill = det), col = "red", linetype = 2)+
      geom_sf(data = aru_sf, shape = 19)+
      scale_fill_manual(values = c("transparent","orangered"),na.value = "transparent",name="Detection")+
      coord_sf(xlim = c(-dim,dim), ylim = c(-dim,dim))+
      xlab("km")+ylab("km")+
      ggtitle(paste0("Number of males = ",n_males," male / km^2\nBird Territory Radius = ",bird_radius_m," m\nBird EDR = ",bird_EDR*1000," m"))
    
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

      # Informative prior on density
      dens_prior_median <- 0.1
      dens_prior_sd <- 1.5
      
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
                parameters.to.save = c("male_dens","popsize","popsize_prior"),n.chains = 3,
                n.thin = 1,
                n.iter = 210000,
                n.burnin = 10000)
    
    # Summary statistics
    run_results <- data.frame(run_number = run_number,
                              n_males = n_males,
                              true_density = n_males/plot_area,
                              n_ARU = n_ARU,
                              n_detections = n_ARU_det,
                              popsize_est_q50 = median(out$sims.list$popsize),
                              popsize_est_q025 = quantile(out$sims.list$popsize,0.025),
                              popsize_est_q975 = quantile(out$sims.list$popsize,0.975),
                              prob_less_than_100 = mean(out$sims.list$popsize<100),
                              prob_less_than_1000 = mean(out$sims.list$popsize<1000),
                              prob_less_than_10000 = mean(out$sims.list$popsize<10000),
                              
                              popsize_prior_q50 = median(out$sims.list$popsize_prior),
                              popsize_prior_q025 = quantile(out$sims.list$popsize_prior,0.025),
                              popsize_prior_q975 = quantile(out$sims.list$popsize_prior,0.975)                      
    )
    
    all_results <- rbind(all_results,run_results)
  }
}

# Labels for plot
all_results$n_male_label <- paste0("n_males = ",all_results$n_males) %>% factor(levels = unique(.))
all_results$n_ARU_label <- paste0("n_ARU = ",all_results$n_ARU) %>% factor(levels = unique(.))

ggplot(all_results)+
  geom_hline(aes(yintercept = n_males), linetype = 2)+
  
  # Plot prior
  #geom_errorbar(aes(x = run_number,ymin = popsize_prior_q025, ymax = popsize_prior_q975), width = 0, col = "dodgerblue", size = 3, alpha = 0.2)+
  #geom_point(aes(x = run_number,y = popsize_prior_q50), col = "dodgerblue", size = 3, alpha = 0.2)+
  
  # Plot actual estimate (prior x data)
  geom_errorbar(aes(x = run_number,ymin = popsize_est_q025, ymax = popsize_est_q975), width = 0)+
  geom_point(aes(x = run_number,y = popsize_est_q50))+
  
  facet_grid(n_male_label~n_ARU_label, scales = "free_y")+
  theme_bw()+
  xlab("Simulation number")+
  ylab("Population Estimate")+
  ggtitle(paste0("Population estimates for different combinations of\nn_ARU and n_males across a study area of ",plot_area," km^2"))

save.image("power_wksp.RData")
