# load (potentially) required packages
library(mice)
library(dplyr)
library(purrr)
library(furrr)
library(magrittr)
library(psych)
library(tidyr)
library(tibble)

# load previous state
load("Workspaces/2. Generate_Not_Applicables.RData")
load("Workspaces/3. Simulation_MCAR.RData")
nsim <- length(sim)


# REGRESSION FOR WHEN NA IS ANALYZED AS 0
true.mean.regression_as0 <- na_data  %>% 
  mutate(MEAN_A = rowMeans(select(., contains("A"))), 
         MEAN_E = rowMeans(select(., contains("E"))),
         MEAN_G = rowMeans(select(., contains("G"))),
         MEAN_K = rowMeans(select(., contains("K")))) %$%
  lm(MEAN_G ~ MEAN_A + MEAN_E + MEAN_K) %>% 
  summary() %>% 
  .$coefficients

pmm.mean.regression_as0 <- sim %>% 
  map("imp") %>% 
  map(~.x %>% 
        complete("all") %>% 
        map(~.x %>% 
              mutate(MEAN_A = rowMeans(select(., contains("A"))), 
                     MEAN_E = rowMeans(select(., contains("E"))),
                     MEAN_G = rowMeans(select(., contains("G"))),
                     MEAN_K = rowMeans(select(., contains("K")))) %$%
              lm(MEAN_G ~ MEAN_A + MEAN_E + MEAN_K)) %>% 
        pool() %>% summary(conf.int = TRUE) %>% 
        as.data.frame() %>% column_to_rownames(var = "term") %>% 
        mutate(cov.pop = `2.5 %` < true.mean.regression_as0[, 1] & true.mean.regression_as0[, 1] < `97.5 %`,
               ciw.pop = `97.5 %` - `2.5 %`)) %>% 
        Reduce("+", .)/length(sim)

# REGRESSION FOR WHEN NA IS ANALYZED AS 1
true.mean.regression_as1 <- na_data  %>% 
  mutate_each(~ replace(., . == 0, 1)) %>% 
  mutate(MEAN_A = rowMeans(select(., contains("A"))), 
         MEAN_E = rowMeans(select(., contains("E"))),
         MEAN_G = rowMeans(select(., contains("G"))),
         MEAN_K = rowMeans(select(., contains("K")))) %$%
  lm(MEAN_G ~ MEAN_A + MEAN_E + MEAN_K) %>% 
  summary() %>% 
  .$coefficients

pmm.mean.regression_as1 <- sim %>% 
  map("imp") %>% 
  map(~.x %>% 
        complete("all") %>% 
        map(~.x %>% 
              mutate_each(~ replace(., . == 0, 1)) %>% 
              mutate(MEAN_A = rowMeans(select(., contains("A"))), 
                     MEAN_E = rowMeans(select(., contains("E"))),
                     MEAN_G = rowMeans(select(., contains("G"))),
                     MEAN_K = rowMeans(select(., contains("K")))) %$%
              lm(MEAN_G ~ MEAN_A + MEAN_E + MEAN_K)) %>% 
        pool() %>% summary(conf.int = TRUE) %>% 
        as.data.frame() %>% column_to_rownames(var = "term") %>% 
        mutate(cov.pop = `2.5 %` < true.mean.regression_as1[, 1] & true.mean.regression_as1[, 1] < `97.5 %`,
               ciw.pop = `97.5 %` - `2.5 %`)) %>% 
  Reduce("+", .)/length(sim)

# REGRESSION FOR WHEN NA IS REMOVED
true.mean.regression_na.omit <- na_data  %>% 
  na_if(0) %>% na.omit() %>% # remove rows with NA in it
  mutate(MEAN_A = rowMeans(select(., contains("A"))), 
         MEAN_E = rowMeans(select(., contains("E"))),
         MEAN_G = rowMeans(select(., contains("G"))),
         MEAN_K = rowMeans(select(., contains("K")))) %$%
  lm(MEAN_G ~ MEAN_A + MEAN_E + MEAN_K) %>% 
  summary() %>% 
  .$coefficients

pmm.mean.regression_na.omit <- sim %>% 
  map("imp") %>% 
  map(~.x %>% 
        complete("all") %>% 
        map(~.x %>% 
              na_if(0) %>% na.omit() %>% # remove rows with NA in it
              mutate(MEAN_A = rowMeans(select(., contains("A"))), 
                     MEAN_E = rowMeans(select(., contains("E"))),
                     MEAN_G = rowMeans(select(., contains("G"))),
                     MEAN_K = rowMeans(select(., contains("K")))) %$%
              lm(MEAN_G ~ MEAN_A + MEAN_E + MEAN_K)) %>% 
        pool() %>% summary(conf.int = TRUE) %>% 
        as.data.frame() %>% column_to_rownames(var = "term") %>% 
        mutate(cov.pop = `2.5 %` < true.mean.regression_na.omit[, 1] & true.mean.regression_na.omit[, 1] < `97.5 %`,
               ciw.pop = `97.5 %` - `2.5 %`)) %>% 
  Reduce("+", .)/length(sim)

# REGRESSION FOR WHEN ROWMEANS ARE CALCULATED ON OBSERVED ITEMS 
# So if A1 and A2 are NA and all other A are observed, then MEAN_A = rowMeans(A3:A10)
true.mean.regression_obs <- na_data  %>% 
  na_if(0)  %>% 
  mutate(MEAN_A = rowMeans(select(., contains("A"))), 
         MEAN_E = rowMeans(select(., contains("E"))),
         MEAN_G = rowMeans(select(., contains("G"))),
         MEAN_K = rowMeans(select(., contains("K")))) %$%
  lm(MEAN_G ~ MEAN_A + MEAN_E + MEAN_K) %>% 
  summary() %>% 
  .$coefficients

pmm.mean.regression_obs <- sim %>% 
  map("imp") %>% 
  map(~.x %>% 
        complete("all") %>% 
        map(~.x %>% 
              na_if(0) %>% 
              mutate(MEAN_A = rowMeans(select(., contains("A")), na.rm = TRUE), 
                     MEAN_E = rowMeans(select(., contains("E")), na.rm = TRUE),
                     MEAN_G = rowMeans(select(., contains("G")), na.rm = TRUE),
                     MEAN_K = rowMeans(select(., contains("K")), na.rm = TRUE)) %$%
              lm(MEAN_G ~ MEAN_A + MEAN_E + MEAN_K)) %>% 
        pool() %>% summary(conf.int = TRUE) %>% 
        as.data.frame() %>% column_to_rownames(var = "term") %>% 
        mutate(cov.pop = `2.5 %` < true.mean.regression_obs[, 1] & true.mean.regression_obs[, 1] < `97.5 %`,
               ciw.pop = `97.5 %` - `2.5 %`)) %>% 
  Reduce("+", .)/length(sim)
