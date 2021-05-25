# load (potentially) required packages
library(mice)
library(dplyr)
library(purrr)
library(furrr)
library(magrittr)

# fix RNG seed (for patterns and ampute)
set.seed(123)

# load previous state
load("Workspaces/1. Data_import_and_preprocess.RData")

# specify patterns
pA1 <- sample(1:0, 10, replace = TRUE, prob = c(.7, .3))
pA2 <- sample(1:0, 10, replace = TRUE, prob = c(.7, .3))
pE1 <- sample(1:0, 10, replace = TRUE, prob = c(.5, .5))
pE2 <- sample(1:0, 10, replace = TRUE, prob = c(.5, .5))
pG1 <- sample(1:0, 10, replace = TRUE, prob = c(.6, .4))
pG2 <- sample(1:0, 10, replace = TRUE, prob = c(.7, .3))
pK1 <- sample(1:0, 10, replace = TRUE, prob = c(.6, .4))
pK2 <- sample(1:0, 10, replace = TRUE, prob = c(.7, .3))

pattern <- matrix(c(pA1, rep(1, 30),
                    pA2, pE2, rep(1, 20),
                    rep(1, 10), pE1, pG1, pK1,
                    rep(1, 10), pE2, pG1, rep(1, 10),
                    pA1, pE1, pG1, rep(1, 10),
                    rep(1, 10), rep(1, 10), pG2, rep(1, 10),
                    rep(1, 10), rep(1, 10), rep(1, 10), pK1,
                    pA1, rep(1, 10), rep(1, 10), pK2), 
                  nrow = 8, 
                  ncol = 40,
                  byrow = TRUE)


# columnMeans of patterns
colMeans(pattern)

# generate NA's
na_process <- ampute(data, 
                     prop = .80, 
                     patterns = pattern, 
                     freq = sample(c(.15, .35, .2, .3), 
                                   size = 8, 
                                   replace = TRUE),
                     mech = "MAR", 
                     type = "LEFT")

# extract na_data
amp <- na_process$amp

# pattern inspection
md.pattern(amp)

# replace all na's with 0
na_data <- data
na_data[is.na(amp)] <- 0

# save na_data
save(na_data, na_process, file = "Workspaces/2. Generate_Not_Applicables.RData")

