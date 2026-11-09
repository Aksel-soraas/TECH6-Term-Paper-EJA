source(here::here("R", "00_Setup.R"))
source(here::here("R", "02_helpers.R"))

t1_data <- matched_sample
nrow(t1_data)

t1_labels <- c(
    hischshr1520f  = "Share women aged 15-20 with high school education",
    hischshr1520m  = "Share men aged 15-20 with high school education",
    i94            = "Islamic mayor in 1994",
    vshr_islam1994 = "Islamic vote share 1994",
    partycount     = "Number of parties receiving votes 1994",
    lpop1994       = "Log population in 1994",
    ageshr19       = "Population share below 19 in 2000",
    ageshr60       = "Population share above 60 in 2000",
    sexr           = "Gender ratio in 2000",
    shhs           = "Household size in 2000",
    merkezi        = "District center",
    merkezp        = "Province center",
    subbuyuk       = "Sub-metro center")

t1_stars <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ""))))

balance_row <- function(v) {
    x   <- t1_data[[v]]
    isl <- x[t1_data$i94 == 1 & !is.na(x)]
    sec <- x[t1_data$i94 == 0 & !is.na(x)]
    n1 <- length(isl)
    n2 <- length(sec)
    diff <- mean(isl) - mean(sec)
    pooled_var <- ((n1 - 1) * var(isl) + (n2 - 1) * var(sec)) / (n1 + n2 - 2)
    se <- sqrt(pooled_var * (1 / n1 + 1 / n2))
    p  <- if (se > 0) 2 * pt(-abs(diff / se), df = n1 + n2 - 2) else NA
    f  <- function(z) sprintf("%.3f", z)
    rbind(c(t1_labels[[v]], f(mean(x, na.rm = TRUE)), f(mean(isl)), f(mean(sec)), paste0(f(diff), t1_stars(p))),
          c("", sprintf("(%.3f)", c(sd(x, na.rm = TRUE), sd(isl), sd(sec), se))))
}

table1 <- do.call(rbind, lapply(names(t1_labels), balance_row))
n_isl  <- sum(t1_data$i94 == 1)
table1 <- rbind(table1, c("Observations", nrow(t1_data), n_isl, nrow(t1_data) - n_isl, nrow(t1_data)))
table1 <- as.data.frame(table1)
names(table1) <- c(" ", "All", "Islamic", "Secular", "Difference")
table1

t1_tt <- tt(table1, notes = "Columns 1-3: means with standard deviations in parentheses. Column 4: difference in means (Islamic - Secular), standard errors from a two-sample t-test with equal variances in parentheses. * p<0.05, ** p<0.01, *** p<0.001.") |>
    group_tt(j = list("Mayor type" = 3:4),
             i = list("Main outcome variables" = 1, "Main explanatory variable" = 5, "Covariates" = 7))
t1_tt

save_tt(t1_tt, here("output", "tables", "table1.tex"),  overwrite = TRUE)
save_tt(t1_tt, here("output", "tables", "table1.html"), overwrite = TRUE)

