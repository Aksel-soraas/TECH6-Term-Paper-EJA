source(here::here("R", "00_Setup.R"))
source(here::here("R", "02_helpers.R"))

# Placebo outcomes. High school shares in 1990.
placebo_outcomes <- c('c90hischshr1520f', 'c90hischshr1520m')

# Keep only female and male high school shares, Islamic mayor indicator, win margin, covariates, province code and the 1990 high school shares.
match_df <- matched_sample |> 
    select(hischshr1520f, hischshr1520m, i94, iwm94, all_of(covs), pcode, all_of(placebo_outcomes))

# Define the propensity score formula, i94 regressed on the covariates.
propensity_formula <- reformulate(covs, response = 'i94')

# The propensity score model. Gives the probability that a municipality elected an islamic mayor given the covariates.
# Logit models yes/no outcomes and estimates the probability of yes based on the covariates.
propensity_model <- glm(
    propensity_formula,
    family = binomial(link = 'logit'),
    data = match_df
)

# Saves the propensity score in the dataframe.
match_df$pscore <- predict(propensity_model, type = "response")


# setdiff returns all covariates except vshr_islam1994 
covs_no_islam <- setdiff(covs, 'vshr_islam1994')

# Keeps only the municipalities containing 1990 data, for the placebo test.
placebo_df <- match_df |> filter(!is.na(c90hischshr1520f))

# Default matching based on the propensity score, matching to the nearest neighbor.
m_default <- matchit( 
    propensity_formula,
    data = match_df,
    method = 'nearest',
    distance = 'glm'
)

# Includes replacement, allowing a secular municipality to be matched to several Islamic-run municipalities.
m_replace <- matchit(
    propensity_formula,
    data = match_df,
    method = 'nearest',
    distance = 'glm',
    replace = TRUE
)

# Includes caliper, which decides the maximum distance in a match.
# It drops municipalities with no close neighbours. 

caliper_val <- 0.05

m_caliper <- matchit(
    propensity_formula,
    data = match_df,
    method = 'nearest',
    distance = 'glm',
    caliper = caliper_val
)

# Matched sample
md <- match.data(m_caliper)

# IPW (ATT), 1 for Islamic municipalities, and pscore/(1-pscore) for secular ones.
# A secular municipality with high pscore is more similar to the Islamic ones and gets a higher weight.
match_df <- match_df |>
    mutate(w_ipw = if_else(i94 == 1, 1, pscore / (1 - pscore)))

# Propensity formula without islamic vote share
propensity_formula_no_islam <- reformulate(covs_no_islam, response = 'i94')

# Same matching, excluding islamic vote share
m_no_islam <- matchit(
    propensity_formula_no_islam,
    data = match_df,
    method = 'nearest',
    distance = 'glm', 
    caliper = caliper_val
)

# Matched sample without islamic vote share
md_no_islam <- match.data(m_no_islam)

# Same matching, on municipalities with 1990 data.
m_placebo <- matchit(
    propensity_formula,
    data = placebo_df,
    method = 'nearest',
    distance = 'glm',
    caliper = caliper_val
)
# Matched sample on municipalities with 1990 data.
md_placebo <- match.data(m_placebo)

# Histogram of pscore by islamic vote share in match_df, one panel per group.
overlap_data <- match_df |>
    mutate(group = if_else(i94 == 1, '(b) Islamic mayor', '(a) Secular mayor'))

histo <- ggplot(overlap_data, aes(x = pscore)) +
            geom_histogram(aes(y = after_stat(density)),
            binwidth = 0.02, boundary = 0, fill = 'grey60', 
            color = 'grey20', linewidth = 0.2) +
            facet_wrap(~ group, ncol = 1, scales = 'free_y') +
            labs(x = 'Estimated propensity score', y = 'Density') +
            scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c(0, 0.25, 0.5, 0.75, 1)) +
            theme_classic(base_size = 14) +
            theme(text = element_text(family = 'serif'),
            axis.line = element_line(linewidth = 0.3),
            axis.ticks = element_line(linewidth = 0.3),
            strip.background = element_blank(),
            strip.text = element_text(size = 14))
ggsave(here('output', 'figures', 'ext2_overlap.png'), histo, width = 7, height = 5, dpi = 300)

# Love plot: covariate balance before and after matching (m_caliper)
love_names <- c(distance = 'Propensity score',
                vshr_islam1994 = 'Islamic vote share',
                partycount = 'Number of parties', 
                shhs = 'Household size', 
                lpop1994 = 'Log population',
                ageshr19 = 'Age 19-',
                ageshr60 = 'Age 60+',
                sexr = 'Gender', 
                merkezi = 'District center', 
                merkezp = 'Province center',
                subbuyuk = 'Sub-metro center',
                buyuk = 'Metro center'
)

love <- love.plot(m_caliper, abs = TRUE, thresholds = 0.1, stars = 'raw',
            var.names = love_names,
            colors = c('grey60', 'black'),
            sample.names = c('Before matching', 'After matching'),
            title = NULL) +
            theme_classic(base_size = 14) +
            theme(text = element_text(family = 'serif'),
                axis.line = element_line(linewidth = 0.3),
                axis.ticks = element_line(linewidth = 0.3),
                legend.title = element_blank(),
                legend.position = 'bottom')
                
ggsave(here('output', 'figures', 'ext2_love.png'), love, width = 7, height = 5, dpi = 300)

# Table comparing the three matching versions:

# Version_row builds a row for the versions table from a matchit object and returns:
# The counts of the Islamic and secular municipalities
# The Std. mean diff. (SMD) of Islamic vote share and the largest SMD among the other covariates
version_row <- function(m, label, before = FALSE) {
    s <- summary(m)

    smd <- abs(if (before) s$sum.all[, 'Std. Mean Diff.'] else s$sum.matched[, 'Std. Mean Diff.'])
    n_row <- if (before) 'All' else 'Matched'
    c(label,
    as.character(s$nn[n_row, 'Treated']),
    as.character(s$nn[n_row, 'Control']),
    sprintf('%.2f', smd['vshr_islam1994']),
    sprintf('%.2f', max(smd[setdiff(covs, 'vshr_islam1994')])))
}

versions <- rbind(
    version_row(m_default, 'Before matching', before = TRUE),
    version_row(m_default, 'Default'),
    version_row(m_replace, 'With replacement'),
    version_row(m_caliper, 'With caliper (main)')
)

versions <- as.data.frame(versions)
names(versions) <- c(' ', 'Islamic', 'Secular', 'Islamic vote share SMD', 'Largest other SMD')
version_tt <- tt(versions) |>
    group_tt(j = list('Municipalities used' = 2:3, 'Balance' = 4:5))

save_tt(version_tt, here('output', 'tables', 'ext2_versions.tex'), overwrite = TRUE)
save_tt(version_tt, here('output', 'tables', 'ext2_versions.html'), overwrite = TRUE)

# Results table: matching, IPW, matching without vote share, placebo

# Runs a weighted regression of outcome y on i94. Clusters standard errors by province
reg <- function(y, data, w) {
    feols(reformulate('i94', response = y), data = data,
    weights = reformulate(w), cluster = ~pcode)
}

# Runs reg for woman and men and returns two table rows:
# The estimate with stars (pvalue) and standard error, and number of municipalities used.
result_rows <- function(y_f, y_m, data, w, label) {
    fits <- list(reg(y_f, data, w), reg(y_m, data, w))
    est <- sapply(fits, function(f) sprintf('%.1f%s', 100 * coef(f)[['i94']], stars(pvalue(f)[['i94']])))
    se <- sapply(fits, function(f) sprintf('(%.1f)', 100 * se(f)[['i94']]))
    n <- format(nobs(fits[[1]]), big.mark = ',')
    rbind(c(label, est, n), c('', se, ''))
}

results <- rbind(
    result_rows('hischshr1520f', 'hischshr1520m', md, 'weights', 'Matching with caliper (main)'),
    result_rows('hischshr1520f', 'hischshr1520m', match_df, 'w_ipw', 'IPW'),
    result_rows('hischshr1520f', 'hischshr1520m', md_no_islam, 'weights', 'Matching without Islamic vote share'),
    result_rows('c90hischshr1520f', 'c90hischshr1520m', md_placebo, 'weights', 'High school share in 1990 (placebo)'),
    result_rows('hischshr1520f', 'hischshr1520m', md_placebo, 'weights', 'Same municipalities: High school share in 2000')
)

results <- as.data.frame(results)
names(results) <- c(' ', 'Women', 'Men', 'Municipalities')

results_tt <- tt(results, notes = 'Estimates are in percentage points. Standard errors in parentheses are clustered by province. * p < 0.1, ** p < 0.05, *** p < 0.01.')

save_tt(results_tt, here('output', 'tables', 'ext2_results.tex'), overwrite = TRUE)
save_tt(results_tt, here('output', 'tables', 'ext2_results.html'), overwrite = TRUE)

# Table showing the mean win margins of the dropped Islamic run municipalities and the matched Islamic  municipalities.
win_margin <- match_df |>
    mutate(group = case_when(
        i94 == 1 & m_caliper$weights > 0 ~ 'Matched Islamic-run',
        i94 == 1 ~ 'Dropped Islamic-run')) |>
        filter(!is.na(group)) |>
        group_by(group) |>
        summarise(n = n(), mean = round(100 * mean(iwm94), 1),
                    n_12 = sum(abs(iwm94) >= 0.12), n_24 = sum(abs(iwm94) >= 0.24))

names(win_margin) <- c(' ', 'Municipalities', 'Mean win margin (pp)', '> 12 pp.', '> 24 pp.')
win_margin_tt <- tt(win_margin)
save_tt(win_margin_tt, here('output', 'tables', 'ext2_win_margin.tex'), overwrite = TRUE)
save_tt(win_margin_tt, here('output', 'tables', 'ext2_win_margin.html'), overwrite = TRUE)