# load (potentially) required packages
library(mice)
library(dplyr)
library(purrr)
library(furrr)
library(magrittr)
library(psych)

# load previous state
load("Workspaces/2. Generate_Not_Applicables.RData")
load("Workspaces/6. Simulation_MAR.RData")
nsim <- length(sim.mar)

# COMPLETE DATA ANALYSES
# 1. Remove all cases with na's and then analyse

# true estimates for population data
truth.CA.1 <- na_data %>% 
  na_if(0) %>% 
  na.omit() %>% 
  mutate(principal(cbind(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)) %>% 
           pluck("scores") %>% as.data.frame %>% 
           rename(Scale_A = "PC1"), 
         principal(cbind(E1, E2, E3, E4, E5, E6, E7, E8, E9, E10)) %>% 
           pluck("scores") %>% as.data.frame %>% 
           rename(Scale_E = "PC1"), 
         principal(cbind(G1, G2, G3, G4, G5, G6, G7, G8, G9, G10)) %>% 
           pluck("scores") %>% as.data.frame %>% 
           rename(Scale_G = "PC1"), 
         principal(cbind(K1, K2, K3, K4, K5, K6, K7, K8, K9, K10)) %>% 
           pluck("scores") %>% as.data.frame %>% 
           rename(Scale_K = "PC1")) %$% 
  lm(Scale_G ~ Scale_A + Scale_E + Scale_K) %>% 
  summary() %>% .$coefficients

# bootstrap estimates (average over all bootstrap sets without missingness)
boot.CA.1 <- sim.mar%>% 
  map("boot") %>% 
  map(~.x %>% 
        na_if(0) %>% 
        na.omit() %>% 
        mutate(principal(cbind(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)) %>% 
                 pluck("scores") %>% as.data.frame %>% 
                 rename(Scale_A = "PC1"), 
               principal(cbind(E1, E2, E3, E4, E5, E6, E7, E8, E9, E10)) %>% 
                 pluck("scores") %>% as.data.frame %>% 
                 rename(Scale_E = "PC1"), 
               principal(cbind(G1, G2, G3, G4, G5, G6, G7, G8, G9, G10)) %>% 
                 pluck("scores") %>% as.data.frame %>% 
                 rename(Scale_G = "PC1"), 
               principal(cbind(K1, K2, K3, K4, K5, K6, K7, K8, K9, K10)) %>% 
                 pluck("scores") %>% as.data.frame %>% 
                 rename(Scale_K = "PC1")) %$% 
        lm(Scale_G ~ Scale_A + Scale_E + Scale_K) %>% 
        summary() %>% .$coefficients) %>% Reduce("+", .) / nsim


# 2. All na's equal to zero and then analyse
# true estimates for population data
truth.CA.2 <- na_data %>% 
  mutate(principal(cbind(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)) %>% 
           pluck("scores") %>% as.data.frame %>% 
           rename(Scale_A = "PC1"), 
         principal(cbind(E1, E2, E3, E4, E5, E6, E7, E8, E9, E10)) %>% 
           pluck("scores") %>% as.data.frame %>% 
           rename(Scale_E = "PC1"), 
         principal(cbind(G1, G2, G3, G4, G5, G6, G7, G8, G9, G10)) %>% 
           pluck("scores") %>% as.data.frame %>% 
           rename(Scale_G = "PC1"), 
         principal(cbind(K1, K2, K3, K4, K5, K6, K7, K8, K9, K10)) %>% 
           pluck("scores") %>% as.data.frame %>% 
           rename(Scale_K = "PC1")) %$% 
  lm(Scale_G ~ Scale_A + Scale_E + Scale_K) %>% 
  summary() %>% .$coefficients

# bootstrap estimates (average over all bootstrap sets without missingness)
boot.CA.2 <- sim.mar%>% 
  map("boot") %>% 
  map(~.x %>% 
        mutate(principal(cbind(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)) %>% 
                 pluck("scores") %>% as.data.frame %>% 
                 rename(Scale_A = "PC1"), 
               principal(cbind(E1, E2, E3, E4, E5, E6, E7, E8, E9, E10)) %>% 
                 pluck("scores") %>% as.data.frame %>% 
                 rename(Scale_E = "PC1"), 
               principal(cbind(G1, G2, G3, G4, G5, G6, G7, G8, G9, G10)) %>% 
                 pluck("scores") %>% as.data.frame %>% 
                 rename(Scale_G = "PC1"), 
               principal(cbind(K1, K2, K3, K4, K5, K6, K7, K8, K9, K10)) %>% 
                 pluck("scores") %>% as.data.frame %>% 
                 rename(Scale_K = "PC1")) %$% 
        lm(Scale_G ~ Scale_A + Scale_E + Scale_K) %>% 
        summary() %>% .$coefficients) %>% Reduce("+", .) / nsim

# 3. All na's equal to one (lowest likert value) and then analyse
# true estimates for population data
truth.CA.3 <- na_data %>% 
  mutate_each(~ replace(., . == 0, 1)) %>% 
  mutate(principal(cbind(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)) %>% 
           pluck("scores") %>% as.data.frame %>% 
           rename(Scale_A = "PC1"), 
         principal(cbind(E1, E2, E3, E4, E5, E6, E7, E8, E9, E10)) %>% 
           pluck("scores") %>% as.data.frame %>% 
           rename(Scale_E = "PC1"), 
         principal(cbind(G1, G2, G3, G4, G5, G6, G7, G8, G9, G10)) %>% 
           pluck("scores") %>% as.data.frame %>% 
           rename(Scale_G = "PC1"), 
         principal(cbind(K1, K2, K3, K4, K5, K6, K7, K8, K9, K10)) %>% 
           pluck("scores") %>% as.data.frame %>% 
           rename(Scale_K = "PC1")) %$% 
  lm(Scale_G ~ Scale_A + Scale_E + Scale_K) %>% 
  summary() %>% .$coefficients

# bootstrap estimates (average over all bootstrap sets without missingness)
boot.CA.3 <- sim.mar%>% 
  map("boot") %>% 
  map(~.x %>% 
        mutate_each(~ replace(., . == 0, 1)) %>% 
        mutate(principal(cbind(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)) %>% 
                 pluck("scores") %>% as.data.frame %>% 
                 rename(Scale_A = "PC1"), 
               principal(cbind(E1, E2, E3, E4, E5, E6, E7, E8, E9, E10)) %>% 
                 pluck("scores") %>% as.data.frame %>% 
                 rename(Scale_E = "PC1"), 
               principal(cbind(G1, G2, G3, G4, G5, G6, G7, G8, G9, G10)) %>% 
                 pluck("scores") %>% as.data.frame %>% 
                 rename(Scale_G = "PC1"), 
               principal(cbind(K1, K2, K3, K4, K5, K6, K7, K8, K9, K10)) %>% 
                 pluck("scores") %>% as.data.frame %>% 
                 rename(Scale_K = "PC1")) %$% 
        lm(Scale_G ~ Scale_A + Scale_E + Scale_K) %>% 
        summary() %>% .$coefficients) %>% Reduce("+", .) / nsim

save(boot.CA.1, boot.CA.2, boot.CA.3,
     truth.CA.1, truth.CA.2, truth.CA.3, file = "Workspaces/7. Evaluations_CCA_MAR.RData")
