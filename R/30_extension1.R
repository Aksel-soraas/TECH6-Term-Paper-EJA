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
    "Gap (women - men)" = c("gap_1990", "gap_2000", "d_gap"))

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

#Controls in the paper's Table IV column 2: municipality type, Islamic vote share 1994, province dummies and 1990 census variables
t4_controls <- c("merkezi", "merkezp", "subbuyuk", "vshr_islam1994", "anyc", pd, "c90hischshr1520f", "c90hischshr1520m",
                  "c90empf", "c90mrf", "c90sexr", "c90ageshr19", "c90ageshr60", "c90lpop")

#Paper's Table IV column 2: its own sample and IK bandwidth, women's 2000 share controlling for the 1990 census variables
t4_sample <- df |>
    drop_na(all_of(c("hischshr1520f", "i94", lin, t4_controls)))
t4_h <- ik_bandwidth(t4_sample$hischshr1520f, t4_sample$iwm94)
t4_fit <- feols(reformulate(c("i94", lin, t4_controls), response = "hischshr1520f"),
                 data = filter(t4_sample, abs(iwm94) < t4_h), cluster = ~pcode)
t4_ci <- confint(t4_fit)["i94", ]
t4_result <- c(est = coef(t4_fit)[["i94"]], se = se(t4_fit)[["i94"]], p = pvalue(t4_fit)[["i94"]],
               ci_low = t4_ci[[1]], ci_high = t4_ci[[2]], n = nobs(t4_fit), h = t4_h)

#Place a single result in the change column, leave 1990 and 2000 empty
only_change <- function(result) cbind(c90hischshr1520f = NA, hischshr1520f = NA, d_f = result)

#Women's estimate in the paper's main specification(full sample 2000): the effect size the DiD should be able to detect
paper_fit <- feols(reformulate(c("i94", lin, controls), response = "hischshr1520f"),
                   data = filter(matched_sample, abs(iwm94) < h_ext1), cluster = ~pcode)
paper_effect <- coef(paper_fit)[["i94"]]

#Full-sample result in the same format as the checks, placed in the 2000 column
full_ci <- confint(paper_fit)["i94", ]
full_result <- c(est = paper_effect, se = se(paper_fit)[["i94"]], p = pvalue(paper_fit)[["i94"]],
                 ci_low = full_ci[[1]], ci_high = full_ci[[2]], n = nobs(paper_fit), h = h_ext1)
only_2000 <- function(result) cbind(c90hischshr1520f = NA, hischshr1520f = result, d_f = NA)

#Robustness checks, first how the 1990 data is used, then other checks of the main specification
ext1_checks <- list(
    "No adjustments: full sample (paper's Table II col 4)" = only_2000(full_result),
    "Differencing (DiD): main specification"               = sapply(women_ext1, rd_ext1),
    "Controlling for 1990: our specification"              = only_change(rd_ext1("hischshr1520f", ctrl = c("c90hischshr1520f", controls))),
    "Controlling for 1990: paper's Table IV col 2"         = only_change(t4_result),
    "Half bandwidth"                       = sapply(women_ext1, rd_ext1, bw = h_ext1 / 2),
    "Double bandwidth"                     = sapply(women_ext1, rd_ext1, bw = h_ext1 * 2),
    "No controls"                          = sapply(women_ext1, rd_ext1, ctrl = NULL),
    "Only pre-election controls"           = sapply(women_ext1, rd_ext1, ctrl = pre_controls),
    "Quadratic polynomial"                 = sapply(women_ext1, rd_ext1, poly = quad),
    "IK bandwidth for each outcome"        = sapply(women_ext1, function(y) rd_ext1(y, bw = ik_bandwidth(did_sample[[y]], did_sample$iwm94))),
    "rdrobust, paper's bandwidth"          = sapply(women_ext1, rdrobust_ext1, bw = h_ext1),
    "rdrobust, no controls"                = sapply(women_ext1, rdrobust_ext1, ctrl = NULL),
    "rdrobust, only pre-election controls" = sapply(women_ext1, rdrobust_ext1, ctrl = pre_controls))

#Standard error of the women's Did in the main specification
did_se <- ext1_run$Women["se", "d_f"]

#Minimum detectable effect at 5% significance and 80% power, and the power to detect the paper's effect
ext1_mde   <- (qnorm(0.975) + qnorm(0.8)) * did_se 
ext1_power <- pnorm(paper_effect / did_se - qnorm(0.975))

#Write the power calulations to a text file
writeLines(c(
    sprintf("Standard error of the women's DiD: %.4f", did_se),
    sprintf("Paper's effect (full sample, 2000): %.4f", paper_effect),
    sprintf("Minimum detectable effect (5%% significance, 80%% power): %.4f", ext1_mde),
    sprintf("Power to detect the paper's effect: %.3f", ext1_power)),
    here("output", "tables", "ext1_power.txt"))

#Data for Figure E1: municipalities with 1990 data within 0.5 of the cutoff
fig_e1_data <- did_sample |>
    filter(abs(iwm94) < 0.5)

#One panel: binned means of y_var, and a straight line on each side of the cutoff inside the bandwidth
e1_panel <- function(y_var, title, y_side = "left") {
    panel_xy <- fig_e1_data |>
        select(x = iwm94, y = all_of(y_var))
    bins <- panel_xy |>
        mutate(bin = floor(x / 0.04)) |>
        group_by(bin) |>
        summarise(mid = (first(bin) + 0.5) * 0.04, mean_y = mean(y))
    in_window <- panel_xy |>
        filter(abs(x) < h_ext1) |>
        mutate(side = x >= 0)
    ggplot(panel_xy, aes(x = x, y = y)) + 
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.3) +
        geom_point(data = bins, aes(x = mid, y = mean_y), color = "grey50") + 
        geom_smooth(data = in_window, aes(group = side), method = "lm", formula = y ~x, 
                    color = "black", fill = "grey80", linewidth = 0.6) + 
        theme_classic(base_size = 14) +
        theme(text = element_text(family = "serif"),
               axis.line = element_line(linewidth = 0.3),
               axis.ticks = element_line(linewidth = 0.3),
               plot.title = element_text(hjust = 0.5)) + 
        labs(x = "Islamic win margin", y = NULL, title = title) + 
        scale_y_continuous(breaks = y_e1, position = y_side) + 
        scale_x_continuous(breaks = x_e1) + 
        coord_cartesian(ylim = range(y_e1))
}

#Axis ticks 
y_e1 <- c(0, 0.05, 0.1, 0.15, 0.2)
x_e1 <- c(-0.5, -0.25, 0, 0.25, 0.5)

#Women and men figures, placed side by side
fig_e1a <- e1_panel("d_f", "(a) Women, change 1990-2000")
fig_e1b <- e1_panel("d_m", "(b) Men, change 1990-2000", y_side = "right")
fig_e1 <- fig_e1a |  fig_e1b
#Save in folder
ggsave(here("output", "figures", "figure_e1.png"), fig_e1, width = 9, height = 4.5, dpi = 300)

#Turn one row of results into the text lines shown in the table
format_cells <- function(m, label, with_n_h = FALSE) {
    #Print values, rounded to the nearest thousandth. Round values that would return -0.000 to 0.000
    f <- function(z) sprintf("%.3f", ifelse(round(z, 3) == 0, 0, z))
    #Stack three lines with text, followed by values
    cells <- rbind(
        c(label, paste0(f(m["est", ]), stars(m["p", ]))),
        c("", sprintf("[%s, %s]", f(m["ci_low", ]), f(m["ci_high", ]))),
        c("p-value", f(m["p", ])))
    #Add two lines, number of municipalities and bandwidth, only when with_n_h = TRUE (this is for Panel B)
    if (with_n_h) {
        cells <- rbind(cells, c("N", sprintf("%.0f", m["n", ])), c("Bandwidth", f(m["h", ])))
    }
    #Show missing values (outcomes not estimated in this row) as empty cells
    cells[grepl("NA", cells)] <- ""
    cells
}

#Panel A: paper's method with N and bandwidth displayed once at the bottom
panel_a_e1 <- do.call(rbind, Map(format_cells, ext1_run, names(ext1_run)))
panel_a_e1 <- rbind(panel_a_e1, c("N", sprintf("%.0f", ext1_run$Women["n", ])),
                                c("Bandwidth", sprintf("%.3f", ext1_run$Women["h", ])))

#Panel B: rdrobust with N and bandwidth under every estimate
panel_b_e1 <- do.call(rbind, Map(format_cells, ext1_robust, names(ext1_robust), with_n_h = TRUE))

#Stack the panels into one table and name the columns
table_e1 <- as.data.frame(rbind(panel_a_e1, panel_b_e1))
names(table_e1) <- c(" ", "1990 (pre-election)", "2000", "Change 1990-2000")

#Women's share on the secular side at the cutoff. The intercept of a line within the bandwidth fitted to secular municipalities
secular_at_cutoff <- sapply(women_ext1, function(y)
    coef(lm(reformulate("iwm94", y), data = filter(did_sample, iwm94 < 0, iwm94 > -h_ext1)))[[1]])

#Number of provinces (clusters) inside the bandwidth
n_clusters <- n_distinct(filter(did_sample, abs(iwm94) < h_ext1)$pcode)

#Create table with notes, columns headings and panel headings
e1_tt <- tt(table_e1, notes = c(
    "Sample: 1609 municipalities with high school shares for both genders in 1990 and 2000. Treatment is an Islamic mayor elected in 1994; the running variable is the Islamic win margin.",
    "The 1990 shares are measured before the 1994 election and cannot be affected by it; the 1990 column is therefore a placebo test. Change 1990-2000 is the 2000 share minus the 1990 share. Gap (women - men) is the women's share minus the men's share.",
    "Panel A: local linear regression within the paper's IK bandwidth, uniform kernel, the paper's controls, standard errors clustered by province.",
    "Panel B: rdrobust with its own MSE-optimal bandwidth per outcome, triangular kernel, the same controls, conventional estimate with robust bias-corrected confidence interval and p-value, clustered by province. In Panel B the bandwidth is chosen separately for each outcome, so the change estimate is not the 2000 estimate minus the 1990 estimate.",
    sprintf("Women's share on the secular side at the cutoff: %.3f (1990), %.3f (2000), %.3f (change). %d provinces (clusters) inside the bandwidth.",
             secular_at_cutoff[1], secular_at_cutoff[2], secular_at_cutoff[3], n_clusters),
    "95% confidence intervals in brackets. * p<0.10, ** p<0.05, *** p<0.01.")) |>
    group_tt(j = list("High school share, age 15-20" = 2:4),
             i = list("Panel A: Paper's method" = 1, "Panel B: rdrobust" = nrow(panel_a_e1) + 1))

save_tt(e1_tt, here("output", "tables", "table_e1.tex"), overwrite = TRUE)
save_tt(e1_tt, here("output", "tables", "table_e1.html"), overwrite = TRUE)

#Robustness table: each check formatted like Table E1
checks_rows <- Map(function(m, label) {
    cbind(format_cells(m, label),
           c(sprintf("%.0f", tail(na.omit(m["n", ]), 1)), "", ""),
           c(sprintf("%.3f", tail(na.omit(m["h", ]), 1)), "", ""))
}, ext1_checks, names(ext1_checks))

#Stack the checks into one table and name the columns
table_e1_checks <- as.data.frame(do.call(rbind, checks_rows))
names(table_e1_checks) <- c(" ", "1990 (pre-election)", "2000", "Change 1990-2000", "N", "Bandwidth")

#Create table with notes, column headings and method headings
checks_tt <- tt(table_e1_checks, notes = c(
    "Sample: 1609 municipalities with high school shares for both genders in 1990 and 2000. Outcome: women's high school share, age 15-20. Each check changes one setting of the main specification.",
    "Paper's method: local linear regression, uniform kernel, standard errors clustered by province. Unless stated otherwise: the paper's IK bandwidth (0.240) and the paper's controls. Pre-election controls drop household size, the two age shares and the gender ratio, which are measured in 2000.",
    "rdrobust: triangular kernel, robust bias-corrected confidence interval and p-value, clustered by province. Unless stated otherwise: own MSE-optimal bandwidth per outcome and the paper's controls.",
    "In the rows 'IK bandwidth for each outcome', 'rdrobust, no controls' and 'rdrobust, only pre-election controls' the bandwidth is chosen separately for each outcome, so the change estimate is not the 2000 estimate minus the 1990 estimate. N and bandwidth refer to the change column, or the 2000 column for the no-adjustment row.",
    "The 'Controlling for 1990' rows estimate the 2000 effect controlling for 1990 values instead of differencing, shown in the change column. The paper's Table IV column 2 uses its own sample, bandwidth and controls.",
    "95% confidence intervals in brackets. * p<0.10, ** p<0.05, *** p<0.01.")) |>
    group_tt(j = list("Women's high school share, age 15-20" = 2:4),
             i = list("How the 1990 data is used" = 1, "Other checks, paper's method" = 3 * 4 + 1, "rdrobust" = 3 * 10 + 1)) |>
             #Shade the first block, including the heading and the 12 lines of the 4 rows
             style_tt(i = 1:13, background = "#eeeeee")

save_tt(checks_tt, here("output", "tables", "table_e1_checks.tex"), overwrite = TRUE)
save_tt(checks_tt, here("output", "tables", "table_e1_checks.html"), overwrite = TRUE)