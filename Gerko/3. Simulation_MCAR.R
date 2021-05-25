# load (potentially) required packages
library(mice)
library(dplyr)
library(purrr)
library(furrr)
library(magrittr)

# load previous state
load("Workspaces/2. Generate_Not_Applicables.RData")

# fix RNG seed (for patterns and ampute)
set.seed(123)

# determine sample size
n = nrow(na_data)

# how many simulations?
nsim = 100

# define simulation function as future
plan(multisession)
sim <- future_map(1:nsim, function(x){
  # draw bootstrap sample
  boot <- na_data[sample(1:n, size = 2500, replace = TRUE), ]
  # make missing MCAR with 40% of all cells missing
  patterns <- matrix(sample(0:1, size = 400, replace = TRUE, prob = c(.4, .6)), 
                     nrow = 10, 
                     ncol = 40)
  incomplete <- ampute(boot, 
                       prop = .5, 
                       pattern = patterns, 
                       mech = "MCAR")
  # impute
  imp <- mice(incomplete$amp, 
              method = "pmm", 
              printFlag = FALSE,
              exclude = -Inf, 
              threshold = 1, 
              eps = 0)
  imp.exclude <- mice(incomplete$amp, 
                      method = "pmm.exclude", 
                      printFlag = FALSE,
                      exclude = 0, 
                      threshold = 1, 
                      eps = 0)
  imp.overimpute <- mice(incomplete$amp %>% na_if(0), 
                         method = "pmm", 
                         printFlag = FALSE,
                         exclude = -Inf, 
                         threshold = 1, 
                         eps = 0)
  indic <- apply(incomplete$amp, MARGIN = 1, 
        function(x) any(x == 0, na.rm = TRUE))
  imp.removebefore <- mice(incomplete$amp[!indic, ], 
                         method = "pmm", 
                         printFlag = FALSE,
                         exclude = -Inf, 
                         threshold = 1, 
                         eps = 0)
  return(list(boot = boot, 
              amp = incomplete$amp, 
              imp = imp, 
              imp.exclude = imp.exclude,
              imp.overimpute = imp.overimpute, 
              imp.removebefore = imp.removebefore)) # sims to return
}, 
.options = furrr_options(seed = as.integer(123)), 
.progress = TRUE)


# save simulation
save(sim, file = "Workspaces/3. Simulation_MCAR.RData")
