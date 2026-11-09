library(tidyverse)
library(rdrobust)
library(rddensity)
library(fixest)
library(haven)
library(here)
library(modelsummary)
library(tinytable)

# rdd is an archived package and has to be installed
# by running the following in R console:
# install.packages("remotes", repos = "https://cloud.r-project.org")
# remotes::install_version("rdd", version = "0.57", repos = "https://cloud.r-project.org")

library(rdd)

df <- read_dta(here("Data", "regdata0.dta"))