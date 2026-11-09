source(here::here("R", "00_Setup.R"))
source(here::here("R", "02_helpers.R"))

fig3_data <- df |>
    drop_na(iwm94) |>
    filter(abs(iwm94) < 0.3)


make_panel <- function(y_var, y_scale, x_scale, y_label, x_label, y_side = "left") {

panel_data <- fig3_data |>
    drop_na(all_of(y_var))

panel_xy <- panel_data |>
    select(x = iwm94, y = all_of(y_var))

bins <- panel_xy |>
    mutate(bin = floor(x / 0.08)) |>
        group_by(bin) |>
        summarise(mid = (first(bin) + 0.5) * 0.08, mean_y = mean(y))

# Local linear fit on each side of the cutoff.
# Assumed Epanechnikov-type weights. As defined in https://www.stata.com/manuals/rkdensity.pdf. It is stata's default kernel so we will use it as the paper does not specifically state anything.
local_fhat_at <- function(x0, data, h = 0.06) { # h is smoothing variable defined for figure 3 in authors replication file graphs_R4_final.do
    side <- data |> 
        filter(if (x0 < 0) x < 0 else x >= 0) |> 
        mutate(dist = x - x0, 
        w = pmax(0, 1 - dist^2 / (5*h^2)))
    fit <- lm(y ~ dist, data = side, weights = w)
    summary(fit)$coefficients[1, 1:2]
}

grid_left <- seq(-0.3, -1e-9, length.out = 100)
grid_right <- seq(0, 0.3, length.out = 100)

fit_left <- t(sapply(grid_left, local_fhat_at, data = panel_xy)) |>
    as_tibble() |>
    rename(fit = Estimate, se = "Std. Error") |>
    mutate(x = grid_left, side = "left")

fit_right <- t(sapply(grid_right, local_fhat_at, data = panel_xy))|>
    as_tibble() |>
    rename(fit = Estimate, se = "Std. Error") |>
    mutate(x = grid_right, side = "right")


fit_left$lwr = fit_left$fit - 1.96 * fit_left$se
fit_left$upr = fit_left$fit + 1.96 * fit_left$se

fit_right$lwr = fit_right$fit - 1.96 * fit_right$se
fit_right$upr = fit_right$fit + 1.96 * fit_right$se

fits = bind_rows(fit_left, fit_right)

panel <- ggplot(fits, aes(x = x)) +
    geom_line(data = fit_left, aes(y = fit)) + 
    geom_line(data = fit_right, aes(y = fit)) +
    geom_line(data = fit_left, aes(y = lwr), linewidth = 0.3, color = "grey30") +
    geom_line(data = fit_right, aes(y = lwr), linewidth = 0.3, color = "grey30") +
    geom_line(data = fit_left, aes(y = upr), linewidth = 0.3, color = "grey30") +
    geom_line(data = fit_right, aes(y = upr), linewidth = 0.3, color = "grey30") +

    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.3) + 
    geom_point(data = bins, aes(x = mid, y = mean_y), color = "grey50") +
    theme_classic(base_size = 14) +
    theme(text = element_text(family = "serif"), 
    axis.line = element_line(linewidth = 0.3),
    axis.ticks = element_line(linewidth = 0.3),
    axis.title.y.right = element_text(angle = 90)) +
    labs(x = x_label, y = y_label) +
    scale_y_continuous(breaks = y_scale, labels = y_scale, position = y_side) +
    scale_x_continuous(breaks = x_scale, labels = x_scale) +
    coord_cartesian(ylim = range(y_scale))

    if (is.null(x_scale)) {
        panel <- panel + theme(axis.line.x = element_blank())
    }
    panel
}

p1 <- make_panel("vshr_islam1994", c(0, 0.2, 0.4, 0.6), c(), "Islamic vote share", "")

p2 <- make_panel("ageshr60", c(0.05, 0.07, 0.09, 0.11), c(), "Age 60+", "", "right")

p3 <- make_panel("ageshr19", c(0.35, 0.45, 0.55), c(), "Age 19-", "")

p4 <- make_panel("lpop1994", c(6, 8, 10), c(), "Log Population", "", "right")

p5 <- make_panel("sexr", c(0.9, 1, 1.1, 1.2), c(), "Gender", "")

p6 <- make_panel("anyc", c(0, 0.2, 0.4, 0.6, 0.8), c(), "Any center municipality", "", "right")

p7 <- make_panel("partycount", c(2, 4, 6, 8), c(-0.25, 0, 0.25), "Number of parties", "Islamic win margin")

p8 <- make_panel("shhs", c(4, 6, 8, 10), c(-0.25, 0, 0.25), "Household size", "Islamic win margin", "right")

fig3 <- (p1 | p2) / (p3 | p4) / (p5 | p6) / (p7 | p8)

ggsave(here("output", "figures", "figure3.png"), fig3, width = 8, height = 11, dpi = 300)