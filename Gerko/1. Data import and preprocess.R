# load required packages
library(readr)
library(dplyr)
library(magrittr)
library(countrycode)

# The following data are obtained from [openpsychometrics.org](https://openpsychometrics.org/_rawdata/)
data <- read_delim("Data/data.csv", "\t", 
                   quote = "\\\"", escape_double = FALSE, 
                   trim_ws = TRUE)

# there are initial missings in the data (coded with 0)
# let's remove them
data %<>% na_if(0) %>% na.omit

# select relevant columns
data %<>% select(contains(c("A", "E", "G", "K"), ignore.case = FALSE))
data1 %<>% select(contains(c("P", "C", "I", "M"), ignore.case = FALSE))
# G = P; A = C; I = E; K = M
data[, 1:40] <- data1[c("C", "I", "P", "M"), 1:40]

# save resulting data set
save(data, file = "Workspaces/1. Data_import_and_preprocess.RData")
