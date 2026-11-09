source(here::here("R", "00_Setup.R"))
source(here::here("R", "02_helpers.R"))

#Create row labels
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

balance_row <- function(v) {
    x   <- matched_sample[[v]]
    #Split the variable into islamic and secular municipalities
    isl <- x[matched_sample$i94 == 1 & !is.na(x)]
    sec <- x[matched_sample$i94 == 0 & !is.na(x)]
    n1 <- length(isl)
    n2 <- length(sec)
    #Calculate the difference and standard error, using t-test as used in the replication package, which assumes equal variance between the two groups
    diff <- mean(isl) - mean(sec)
    pooled_var <- ((n1 - 1) * var(isl) + (n2 - 1) * var(sec)) / (n1 + n2 - 2)
    se <- sqrt(pooled_var * (1 / n1 + 1 / n2))
    #Calculate the p-value for cases where SE > 0. Using 2 * pt returns the two-sided t-distribution p-value.
    p  <- if (se > 0) 2 * pt(-abs(diff / se), df = n1 + n2 - 2) else NA
    f  <- function(z) sprintf("%.3f", z)
    #Stars at 10/5/1 as specified in the paper's table 1 footnote
    #The paper's printed Table I instead uses the default 5/1/0.1
    rbind(c(t1_labels[[v]], f(mean(x, na.rm = TRUE)), f(mean(isl)), f(mean(sec)), paste0(f(diff), if (is.na(p)) "" else stars(p))),
          c("", sprintf("(%.3f)", c(sd(x, na.rm = TRUE), sd(isl), sd(sec), se))))
}

#Run balance_row for the variables, stack the results
table1 <- do.call(rbind, lapply(names(t1_labels), balance_row))
#Create bottom row, showing observation count
n_isl  <- sum(matched_sample$i94 == 1)
table1 <- rbind(table1, c("Observations", nrow(matched_sample), n_isl, nrow(matched_sample) - n_isl, nrow(matched_sample)))
#Convert table to data frame, set column names
table1 <- as.data.frame(table1)
names(table1) <- c(" ", "All", "Islamic", "Secular", "Difference")

#Create table using tinytables, add footnotes
t1_tt <- tt(table1, notes = "Columns 1-3: means with standard deviations in parentheses. Column 4: difference in means (Islamic - Secular), standard errors from a two-sample t-test with equal variances in parentheses. * p<0.10, ** p<0.05, *** p<0.01.") |>
    #Mayor types for columns 3 and 4
    group_tt(j = list("Mayor type" = 3:4),
            #Insert section headings 
            i = list("Main outcome variables" = 1, "Main explanatory variable" = 5, "Covariates" = 7))

save_tt(t1_tt, here("output", "tables", "table1.tex"),  overwrite = TRUE)
save_tt(t1_tt, here("output", "tables", "table1.html"), overwrite = TRUE)

