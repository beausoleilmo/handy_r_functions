#### ### ### ## #### ### ### ## #### ### ### ##
# Created by Marc-Olivier Beausoleil
# November 2, 2020 at 21:25
# Why: Example of casting random quadrats in a confined area 
# Output: Image of sampling an ecological community
# Requires: 
# NOTES: https://cran.r-project.org/web/packages/mobsim/vignettes/Sampling_communities.html
#### ### ### ## #### ### ### ## #### ### ### ## 

install.packages("mobsim")
library(vegan)
library(mobsim)
sim_com1 <- sim_poisson_community(s_pool = 100, n_sim = 200)

plot(sim_com1)
sample1 <- sample_quadrats(sim_com1)

specnumber(sample1$spec_dat)
diversity(sample1$spec_dat, index = "shannon")

sample2 <- sample_quadrats(sim_com1, n_quadrats= 2, quadrat_area = 0.1,
                           avoid_overlap = T,plot = TRUE)
specnumber(sample2$spec_dat)
diversity(sample2$spec_dat, index = "shannon")
sample3 <- sample_quadrats(sim_com1, n_quadrats= 20, quadrat_area = 0.001,
                           avoid_overlap = T)
sample4 <- sample_quadrats(sim_com1, n_quadrats= 10, quadrat_area = 0.005,
                           method = "transect", x0 = 0, y0 = 0.5, delta_x = 0.1,
                           delta_y = 0)
sample5 <- sample_quadrats(sim_com1, n_quadrats= 10, quadrat_area = 0.005,
                           method = "transect", x0 = 0, y0 = 0, delta_x = 0.1,
                           delta_y = 0.1)
sample6 <- sample_quadrats(sim_com1, n_quadrats= 25, quadrat_area = 0.005,
                           method = "grid", x0 = 0, y0 = 0, delta_x = 0.1,
                           delta_y = 0.1)
sample7 <- sample_quadrats(sim_com1, n_quadrats= 25, quadrat_area = 0.005,
                           method = "grid", x0 = 0.05, y0 = 0.05, delta_x = 0.2,
                           delta_y = 0.2)
sample7a <- sample_quadrats(sim_com1, n_quadrats= 25, quadrat_area = 0.005,
                            method = "grid", x0 = 0.05, y0 = 0.05, delta_x = 0.2,
                            delta_y = 0.2, plot = F)
head(sample7a$spec_dat[,1:10])

