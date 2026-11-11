source(here::here("R", "00_Setup.R"))
source(here::here("R", "02_helpers.R"))

#Only keep municipalities that also have high school shares from 1990
did_sample <- matched_sample |>
    drop_na(c90hischshr1520f, c90hischshr1520m) |>
    #Change in high school share from 1990 to 2000 for each gender, gender gap in 1990 and 2000, difference in change between genders
    mutate(d_f = hischshr1520f - c90hischshr1520f,
           d_m = hischshr1520m - c90hischshr1520m,
           gap_1990 = c90hischshr1520f - c90hischshr1520m,
           gap_2000 = hischshr1520f - hischshr1520m,
           d_gap = d_f - d_m)

#Bandwidth from the paper, used for every outcome, allowing comparison of the same municipalities
h_ext1 <- ik_bandwidth(matched_sample$hischshr1520f, matched_sample$iwm94)

#Local linear RD of outcome within bandwidth, with chosen controls and polynomial, returning the estimate, SE, p-value, confidence interval, N and bandwidth
rd_ext1 <- function(y, bw = h_ext1, ctrl = controls, poly = lin) {
    window <- filter(did_sample, abs(iwm94) < bw)
    fit <- feols(reformulate(c("i94", poly, ctrl), response = y), data = window, cluster = ~pcode)
    ci <- confint(fit)["i94", ]
    c(est = coef(fit)[["i94"]], se = se(fit)[["i94"]], p = pvalue(fit)[["i94"]], ci_low = ci[[1]], ci_high = ci[[2]], n = nobs(fit), h = bw)
}

#Outcome for each row of table ext1: 1990, 2000, change from 1990 to 2000
ext1_outcomes <- list(
    Women         = c("c90hischshr1520f", "hischshr1520f", "d_f"),
    Men           = c("c90hischshr1520m", "hischshr1520m", "d_m"),
    "Women - men" = c("gap_1990", "gap_2000", "d_gap"))

#rdrobust estimate of outcome y on all municipalities with 1990 data, with chosen or own bandwidth and control set, returning estimate, robust p-value and confidence interval, N and bandwidth
rdrobust_ext1 <- function(y, bw = NULL, ctrl = controls) {
    #Controls passed to rdrobust: none if ctrl is NULL, otherwise the chosen columns, rdrobust would reject an empty table
    covariates <- if (is.null(ctrl)) NULL else did_sample[, ctrl]
    #RD with a line on each side, weighting municipalities closer to the cutoff more, clustered by province
    fit <- rdrobust(y = did_sample[[y]], x = did_sample$iwm94, c = 0, h = bw,
                    covs = covariates, cluster = did_sample$pcode)
    #Pick the conventional estimate, robust p-value and confidence interval, number of municipalities inside bandwidth and the bandwidth
    c(est = fit$coef[["Conventional", 1]], p = fit$pv[["Robust", 1]],
      ci_low = fit$ci[["Robust", 1]], ci_high = fit$ci[["Robust", 2]],
      n = sum(fit$N_h), h = fit$bws[["h", "left"]])
}

#Run rd_ext1 and rdrobust_ext1, for every outcome, a matrix per row
ext1_run <- lapply(ext1_outcomes, function(y_values) sapply(y_values, rd_ext1))
ext1_robust <- lapply(ext1_outcomes, function(y_values) sapply(y_values, rdrobust_ext1))

#Controls measured before 1994 election, dropping covariates from 2000
pre_controls <- c(setdiff(covs, c("shhs", "ageshr19", "ageshr60", "sexr")), pd)

#Women's outcomes
women_ext1 <- ext1_outcomes$Women

#Robustness checks, each one of these checks the result of changing one setting
ext1_checks <- list(
    "Main specification"                   = sapply(women_ext1, rd_ext1),
    "Half bandwidth"                       = sapply(women_ext1, rd_ext1, bw = h_ext1 / 2),
    "Double bandwidth"                     = sapply(women_ext1, rd_ext1, bw = h_ext1 * 2),
    "No controls"                          = sapply(women_ext1, rd_ext1, ctrl = NULL),
    "Only pre-election controls"           = sapply(women_ext1, rd_ext1, ctrl = pre_controls),
    "Quadratic polynomial"                 = sapply(women_ext1, rd_ext1, poly = quad),
    "IK bandwidth for each outcome"        = sapply(women_ext1, function(y) rd_ext1(y, bw = ik_bandwidth(did_sample[[y]], did_sample$iwm94))),
    "rdrobust, paper's bandwidth"          = sapply(women_ext1, rdrobust_ext1, bw = h_ext1),
    "rdrobust, no controls"                = sapply(women_ext1, rdrobust_ext1, ctrl = NULL),
    "rdrobust, only pre-election controls" = sapply(women_ext1, rdrobust_ext1, ctrl = pre_controls))

#Women's estimate in the paper's main specification(full sample 2000): the effect size the DiD should be able to detect
paper_fit <- feols(reformulate(c("i94", lin, controls), response = "hischshr1520f"),
                   data = filter(matched_sample, abs(iwm94) < h_ext1), cluster = ~pcode)
paper_effect <- coef(paper_fit)[["i94"]]

#Standard error of the women's Did in the main specification
did_se <- ext1_run$Women["se", "d_f"]

#Minimum detectable effect at 5% significance and 80% power, and the power to detect the paper's effect
ext1_mde   <- (qnorm(0.975) + qnorm(0.8)) * did_se 
ext1_power <- pnorm(paper_effect / did_se - qnorm(0.975))
