library(tidyverse)
library(rdrobust)
library(rddensity)
library(fixest)
library(haven)
library(here)
library(modelsummary)

df <- read_dta(here("Data", "regdata0.dta"))