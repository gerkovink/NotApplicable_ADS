# load (potentially) required packages
library(mice)
library(dplyr)
library(purrr)
library(furrr)
library(magrittr)
library(psych)
library(tibble)

# load previous state
load("Workspaces/6. Simulation_MAR.RData")
load("Workspaces/7. Evaluations_CCA_MAR.RData")
nsim <- length(sim.mar)

# SIMULATIONS
##################################################
# 1. Remove all cases with na's and then analyse
##################################################
sim1.pmm <- sim.mar %>% 
  map("imp") %>% 
  map(~.x %>% 
        complete("all") %>%  
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
              lm(Scale_G ~ Scale_A + Scale_E + Scale_K)) %>% 
        pool() %>% summary(conf.int = TRUE) %>% 
        as.data.frame() %>% column_to_rownames(var = "term") %>% 
        mutate(cov.pop = `2.5 %` < truth.CA.1[, 1] & truth.CA.1[, 1] < `97.5 %`,
               ciw.pop = `97.5 %` - `2.5 %`, 
               cov.boot = `2.5 %` < boot.CA.1[, 1] & boot.CA.1[, 1] < `97.5 %`))

sim1.pmm.exclude <- sim.mar %>% 
  map("imp.exclude") %>% 
  map(~.x %>% 
        complete("all") %>%  
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
              lm(Scale_G ~ Scale_A + Scale_E + Scale_K)) %>% 
        pool() %>% summary(conf.int = TRUE) %>% 
        as.data.frame() %>% column_to_rownames(var = "term") %>% 
        mutate(cov.pop = `2.5 %` < truth.CA.1[, 1] & truth.CA.1[, 1] < `97.5 %`,
               ciw.pop = `97.5 %` - `2.5 %`, 
               cov.boot = `2.5 %` < boot.CA.1[, 1] & boot.CA.1[, 1] < `97.5 %`))

sim1.pmm.overimpute <- sim.mar %>% 
  map("imp.overimpute") %>% 
  map(~.x %>% 
        complete("all") %>%  
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
              lm(Scale_G ~ Scale_A + Scale_E + Scale_K)) %>% 
        pool() %>% summary(conf.int = TRUE) %>% 
        as.data.frame() %>% column_to_rownames(var = "term") %>% 
        mutate(cov.pop = `2.5 %` < truth.CA.1[, 1] & truth.CA.1[, 1] < `97.5 %`,
               ciw.pop = `97.5 %` - `2.5 %`, 
               cov.boot = `2.5 %` < boot.CA.1[, 1] & boot.CA.1[, 1] < `97.5 %`))
 
sim1.removebefore <- sim.mar %>% 
  map("imp.removebefore") %>% 
  map(~.x %>% 
        complete("all") %>%  
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
              lm(Scale_G ~ Scale_A + Scale_E + Scale_K)) %>% 
        pool() %>% summary(conf.int = TRUE) %>% 
        as.data.frame() %>% column_to_rownames(var = "term") %>% 
        mutate(cov.pop = `2.5 %` < truth.CA.1[, 1] & truth.CA.1[, 1] < `97.5 %`,
               ciw.pop = `97.5 %` - `2.5 %`, 
               cov.boot = `2.5 %` < boot.CA.1[, 1] & boot.CA.1[, 1] < `97.5 %`)) 

##################################################
# 2. All na's equal to zero and then analyse
##################################################
sim2.pmm <- sim.mar %>% 
  map("imp") %>% 
  map(~.x %>% 
        complete("all") %>%  
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
              lm(Scale_G ~ Scale_A + Scale_E + Scale_K)) %>% 
        pool() %>% summary(conf.int = TRUE) %>% 
        as.data.frame() %>% column_to_rownames(var = "term") %>% 
        mutate(cov.pop = `2.5 %` < truth.CA.2[, 1] & truth.CA.2[, 1] < `97.5 %`,
               ciw.pop = `97.5 %` - `2.5 %`, 
               cov.boot = `2.5 %` < boot.CA.2[, 1] & boot.CA.2[, 1] < `97.5 %`))

sim2.pmm.exclude <- sim.mar %>% 
  map("imp.exclude") %>% 
  map(~.x %>% 
        complete("all") %>%  
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
              lm(Scale_G ~ Scale_A + Scale_E + Scale_K)) %>% 
        pool() %>% summary(conf.int = TRUE) %>% 
        as.data.frame() %>% column_to_rownames(var = "term") %>% 
        mutate(cov.pop = `2.5 %` < truth.CA.2[, 1] & truth.CA.2[, 1] < `97.5 %`,
               ciw.pop = `97.5 %` - `2.5 %`, 
               cov.boot = `2.5 %` < boot.CA.2[, 1] & boot.CA.2[, 1] < `97.5 %`))

sim2.pmm.overimpute <- sim.mar %>% 
  map("imp.overimpute") %>% 
  map(~.x %>% 
        complete("all") %>%  
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
              lm(Scale_G ~ Scale_A + Scale_E + Scale_K)) %>% 
        pool() %>% summary(conf.int = TRUE) %>% 
        as.data.frame() %>% column_to_rownames(var = "term") %>% 
        mutate(cov.pop = `2.5 %` < truth.CA.2[, 1] & truth.CA.2[, 1] < `97.5 %`,
               ciw.pop = `97.5 %` - `2.5 %`, 
               cov.boot = `2.5 %` < boot.CA.2[, 1] & boot.CA.2[, 1] < `97.5 %`))

sim2.removebefore <- sim.mar %>% 
  map("imp.removebefore") %>% 
  map(~.x %>% 
        complete("all") %>%  
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
              lm(Scale_G ~ Scale_A + Scale_E + Scale_K)) %>% 
        pool() %>% summary(conf.int = TRUE) %>% 
        as.data.frame() %>% column_to_rownames(var = "term") %>% 
        mutate(cov.pop = `2.5 %` < truth.CA.2[, 1] & truth.CA.2[, 1] < `97.5 %`,
               ciw.pop = `97.5 %` - `2.5 %`, 
               cov.boot = `2.5 %` < boot.CA.2[, 1] & boot.CA.2[, 1] < `97.5 %`)) 


##################################################
# 3. All na's equal to one (lowest likert value) and then analyse
##################################################
sim3.pmm <- sim.mar %>% 
  map("imp") %>% 
  map(~.x %>% 
        complete("all") %>%  
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
              lm(Scale_G ~ Scale_A + Scale_E + Scale_K)) %>% 
        pool() %>% summary(conf.int = TRUE) %>% 
        as.data.frame() %>% column_to_rownames(var = "term") %>% 
        mutate(cov.pop = `2.5 %` < truth.CA.3[, 1] & truth.CA.3[, 1] < `97.5 %`,
               ciw.pop = `97.5 %` - `2.5 %`, 
               cov.boot = `2.5 %` < boot.CA.3[, 1] & boot.CA.3[, 1] < `97.5 %`))

sim3.pmm.exclude <- sim.mar %>% 
  map("imp.exclude") %>% 
  map(~.x %>% 
        complete("all") %>%  
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
              lm(Scale_G ~ Scale_A + Scale_E + Scale_K)) %>% 
        pool() %>% summary(conf.int = TRUE) %>% 
        as.data.frame() %>% column_to_rownames(var = "term") %>% 
        mutate(cov.pop = `2.5 %` < truth.CA.3[, 1] & truth.CA.3[, 1] < `97.5 %`,
               ciw.pop = `97.5 %` - `2.5 %`, 
               cov.boot = `2.5 %` < boot.CA.3[, 1] & boot.CA.3[, 1] < `97.5 %`))

sim3.pmm.overimpute <- sim.mar %>% 
  map("imp.overimpute") %>% 
  map(~.x %>% 
        complete("all") %>%  
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
              lm(Scale_G ~ Scale_A + Scale_E + Scale_K)) %>% 
        pool() %>% summary(conf.int = TRUE) %>% 
        as.data.frame() %>% column_to_rownames(var = "term") %>% 
        mutate(cov.pop = `2.5 %` < truth.CA.3[, 1] & truth.CA.3[, 1] < `97.5 %`,
               ciw.pop = `97.5 %` - `2.5 %`, 
               cov.boot = `2.5 %` < boot.CA.3[, 1] & boot.CA.3[, 1] < `97.5 %`))

sim3.removebefore <- sim.mar %>% 
  map("imp.removebefore") %>% 
  map(~.x %>% 
        complete("all") %>%  
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
              lm(Scale_G ~ Scale_A + Scale_E + Scale_K)) %>% 
        pool() %>% summary(conf.int = TRUE) %>% 
        as.data.frame() %>% column_to_rownames(var = "term") %>% 
        mutate(cov.pop = `2.5 %` < truth.CA.3[, 1] & truth.CA.1[, 1] < `97.5 %`,
               ciw.pop = `97.5 %` - `2.5 %`, 
               cov.boot = `2.5 %` < boot.CA.3[, 1] & boot.CA.3[, 1] < `97.5 %`)) 

# RESULTS

# impute and allow NA's as imputations
# 1 = Remove all cases with na's and then analyse
# 2 = All na's equal to zero and then analyse
# 3 = All na's equal to one (lowest likert value) and then analyse

pmm1 <- sim1.pmm %>% Reduce("+", .) / nsim # average over the 100 sims
pmm2 <- sim2.pmm %>% Reduce("+", .) / nsim # average over the 100 sims
pmm3 <- sim3.pmm %>% Reduce("+", .) / nsim # average over the 100 sims

# impute and exclude NA's as imputations
pmm.exclude1 <- sim1.pmm.exclude %>% Reduce("+", .) / nsim # average over the 100 sims
pmm.exclude2 <- sim2.pmm.exclude %>% Reduce("+", .) / nsim # average over the 100 sims
pmm.exclude3 <- sim3.pmm.exclude %>% Reduce("+", .) / nsim # average over the 100 sims

# impute and set all NA's to missing and overimpute
pmm.overimpute1 <- sim1.pmm.overimpute %>% Reduce("+", .) / nsim # average over the 100 sims
pmm.overimpute2 <- sim2.pmm.overimpute %>% Reduce("+", .) / nsim # average over the 100 sims
pmm.overimpute3 <- sim3.pmm.overimpute %>% Reduce("+", .) / nsim # average over the 100 sims

# remove all rows with NA's before imputation (lower sample size)
pmm.removebefore1 <- sim1.removebefore %>% Reduce("+", .) / nsim # average over the 100 sims
pmm.removebefore2 <- sim2.removebefore %>% Reduce("+", .) / nsim # average over the 100 sims
pmm.removebefore3 <- sim3.removebefore %>% Reduce("+", .) / nsim # average over the 100 sims

# Save all
save(pmm1, pmm2, pmm3, 
     pmm.exclude1, pmm.exclude2, pmm.exclude3, 
     pmm.overimpute1, pmm.overimpute2, pmm.overimpute3, 
     pmm.removebefore1, pmm.removebefore2, pmm.removebefore3,
     file = "Workspaces/8. Evaluation_SIMS_MAR.RData")
