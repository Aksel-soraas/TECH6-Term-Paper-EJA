# Replication and extensions of Meyersson (2014), *Econometrica*, 82(1)

Term paper for TECH6 - Data Science and Econometrics, NHH, autumn 2026

Group EJA:
- Candidate 36
- Candidate 56
- Candidate 11

Replicates Tables I, II, III and VII and Figures 1-4 of Meyersson, E. (2014), 
"Islamic Rule and the Empowerment of the Poor and Pious",
and adds two extensions.

## How to run

From the project folder (the folder containing `run_all.R`):

```r
source("run_all.R")
```
or from a terminal: `Rscript run_all.R`. This runs every script in `R/` in order and writes all tables and figures to `output/`. Folders are created if missing.

## Requirements

R 4.6.1. Install the packages once:

```r
install.packages(c("tidyverse", "rdrobust", "rddensity", "fixest", "haven", "here",
                   "modelsummary", "tinytable", "patchwork", "MatchIt", "cobalt",
                   "remotes", "lmtest", "AER"))
remotes::install_version("rdd", version = "0.57", upgrade = "never")
```

`rdd` (McCrary density test, Figure 2) has been archived on CRAN and must be
installed from the archive as above.

Versions used: tidyverse 2.0.0, rdrobust 4.0.0, rddensity 3.0, fixest 0.14.2,
haven 2.5.5, here 1.0.2, modelsummary 2.6.0, tinytable 0.19.0, patchwork 1.3.2,
MatchIt 4.7.2, cobalt 5.0.0, rdd 0.57.

The prediction part of Extension I (`ML.ipynb`) is written in Python 3 and requires
`jupyter`, `numpy`, `pandas`, `scikit-learn` and `matplotlib`.


## Data
`Data/regdata0.dta` is the municipal dataset from Meyersson's replication package
(supplementary material to the article, available at
[econometricsociety.org](https://www.econometricsociety.org/publications/econometrica/2014/01/01/islamic-rule-and-empowerment-poor-and-pious)).
It is the only input; every table and figure is generated from it.

## Structure

| Script | Content | Output |
|---|---|---|
| `R/00_Setup.R` | Loads packages and data | – |
| `R/02_helpers.R` | Sample, controls, IK bandwidth, shared functions | – |
| `R/10_table1.R` | Table I: summary statistics | `table1.tex/.html` |
| `R/11_table2.R` | Table II: main RD results | `table2.tex/.html` |
| `R/12_table3.R` | Table III: bandwidths and polynomials | `table3.tex/.html` |
| `R/13_table7.R` | Table VII | `table7.tex/.html` |
| `R/20_figure1.R` – `R/23_figure4.R` | Figures 1–4 | `figure1.png` … `figure4.png` |
| `R/30_extension1.R` | Extension I: DiD at the RD cutoff | `table_e2`, `table_e3_checks`, `figure_e1.png`, `ext1_power.txt` |
| `R/31_extension2.R` | Extension II: propensity score matching | `ext2_results`, `ext2_versions`, `ext2_overlap.png`, `ext2_love.png` |
| `ML.ipynb` | Extension I: predicting treatment with OLS, Ridge and Lasso (Python) | `ml_summary_table.png` |


Tables are saved as both `.tex` and `.html`.

## Notes

- The IK bandwidth is translated from `rdob.ado` in the original replication package.
- Running the code prints collinearity messages from `fixest` and `rdrobust`
  ("redundant covariates dropped"). They are expected: some province dummies have
  no variation inside narrow windows and are dropped automatically. Results are
  unaffected.
- Table and figure deviations from the article are documented in the assignment paper.