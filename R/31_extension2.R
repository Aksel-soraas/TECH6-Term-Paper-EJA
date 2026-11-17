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

# Includes caliper = 0.2, which says the maximum distance in a match is 0.2 standard deviations.
# It drops municipalities with no close neighbours. 
m_caliper <- matchit(
    propensity_formula,
    data = match_df,
    method = 'nearest',
    distance = 'glm',
    caliper = 0.2
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
    caliper = 0.2
)

# Matched sample without islamic vote share
md_no_islam <- match.data(m_no_islam)

# Same matching, on municipalities with 1990 data.
m_placebo <- matchit(
    propensity_formula,
    data = placebo_df,
    method = 'nearest',
    distance = 'glm',
    caliper = 0.2
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
names(versions) <- c(' ', 'Islamic', 'Secular', 'Islamic vote share', 'Largest other')
version_tt <- tt(versions, notes = 'Nearest neighbor matching on the propensity score. Default: one-to-one without replacement. With replacement: a secular-run municipality can be matched to multiple Islamic-run ones. With caliper: the maximum distance between two matched municipalities’ propensity score is 0.2 standard deviations. Islamic-run municipalities without a match are dropped. Balance: the absolute standardized mean difference, before matching in the first row, and after in the following rows. Values below 0.1 are considered balanced. Largest other: the largest value among the other covariates.') |>
    group_tt(j = list('Municipalities used' = 2:3, 'Balance' = 4:5))

save_tt(version_tt, here('output', 'tables', 'ext2_versions.tex'), overwrite = TRUE)
save_tt(version_tt, here('output', 'tables', 'ext2_versions.html'), overwrite = TRUE)

# Results table: matching, IPW, matching without vote share, placebo

# Runs a weighted regression of outcome y on i94. Clusters standard errors by province
reg <- function(y, data, w) {
    feols(reformulate('i94', response = y), data = data,
    weights = reformulate(w), cluster = ~pcode)
}

# Runs reg for woman and men and returns three table rows:
#The estimate, standard error, and the number of observations
result_rows <- function(y_f, y_m, data, w, label) {
    fits <- list(reg(y_f, data, w), reg(y_m, data, w))
    est <- sapply(fits, function(f) sprintf('%.3f', coef(f)[['i94']]))
    se <- sapply(fits, function(f) sprintf('(%.3f)', se(f)[['i94']]))
    pv <- sapply(fits, function(f) sprintf('[%.3f]', pvalue(f)[['i94']]))
    n <- sapply(fits, function(f) as.character(nobs(f)))
    ctrl <- data[data$i94 == 0, ]
    cm <- sapply(c(y_f, y_m), function(y) sprintf('%.3f', weighted.mean(ctrl[[y]], ctrl[[w]])))
    rbind(c(label, est), c('', se), c('', pv), c('Control mean', cm), c('Observations', n))
}

results <- rbind(
    result_rows('hischshr1520f', 'hischshr1520m', md, 'weights', 'Matching with caliper (main)'),
    result_rows('hischshr1520f', 'hischshr1520m', match_df, 'w_ipw', 'IPW'),
    result_rows('hischshr1520f', 'hischshr1520m', md_no_islam, 'weights', 'Matching without Islamic vote share'),
    result_rows('c90hischshr1520f', 'c90hischshr1520m', md_placebo, 'weights', 'High school share in 1990 (placebo)'),
    result_rows('hischshr1520f', 'hischshr1520m', md_placebo, 'weights', 'High school share in 2000')
)

results <- as.data.frame(results)
names(results) <- c(' ', 'Women', 'Men')

results_tt <- tt(results, notes = 'The estimate is the coefficient on Islamic mayor in 1994 from a weighted regression of the outcome on the mayor indicator. Panel A: the share aged 15-20 with high school education in 2000. Matching: one-to-one nearest neighbor matching on the propensity score with caliper = 0.2. IPW uses all municipalities with weight 1 for Islamic-run and p/(1-p) for secular-run. Then estimated excluding Islamic vote share repeats the matching, but drops vote share from the propensity score. Panel B: repeats main matching on municipalities with 1990 census data. Observations are the municipalities used in each regression. Standard errors are clustered by province in parentheses. P-values in brackets.') |>
    group_tt(i = list('Panel A: Effect of an Islamic mayor' = 1, 'Panel B: Placebo sample' = 16))

save_tt(results_tt, here('output', 'tables', 'ext2_results.tex'), overwrite = TRUE)
save_tt(results_tt, here('output', 'tables', 'ext2_results.html'), overwrite = TRUE)