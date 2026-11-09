source(here::here("R", "00_Setup.R"))
source(here::here("R", "02_helpers.R"))

# TODO: Add shared titles for x and y axis.

fig4_data <- df |>
    drop_na(iwm94) |>
    filter(abs(iwm94) < 0.5)

make_panel <- function(y_var, y_scale, x_scale, y_label, x_label, y_side = "left", title = NULL) {

panel_data <- fig4_data |>
    drop_na(all_of(y_var))

panel_xy <- panel_data |>
    select(x = iwm94, y = all_of(y_var))

bins <- panel_xy |>
    mutate(bin = floor(x / 0.04)) |>
        group_by(bin) |>
        summarise(mid = (first(bin) + 0.5) * 0.04, mean_y = mean(y))

# Local linear fit on each side of the cutoff.
# Assumed Epanechnikov-type weights. As defined in https://www.stata.com/manuals/rkdensity.pdf. It is stata's default kernel so we will use it as the paper does not specifically state anything.
local_fhat_at <- function(x0, data, h = 0.08) { # h is smoothing variable defined for figure 4 in authors replication file graphs_R4_final.do
    side <- data |> 
        filter(if (x0 < 0) x < 0 else x >= 0) |> 
        mutate(dist = x - x0, 
        w = pmax(0, 1 - dist^2 / (5*h^2)))
    fit <- lm(y ~ dist, data = side, weights = w)
    summary(fit)$coefficients[1, 1:2]
}

grid_left <- seq(-0.5, -1e-9, length.out = 100)
grid_right <- seq(0, 0.5, length.out = 100)

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
    axis.title.y.right = element_text(angle = 90),
    plot.title = element_text(hjust = 0.5)) +
    labs(x = x_label, y = y_label, title = title) +
    scale_y_continuous(breaks = y_scale, labels = y_scale, position = y_side) +
    scale_x_continuous(breaks = x_scale, labels = x_scale) +
    coord_cartesian(ylim = range(y_scale))

    if (is.null(x_scale)) {
        panel <- panel + theme(axis.line.x = element_blank())
    }
    panel
}

y4 = c(-0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.25)
x4 = c(-0.5, -0.25, 0, 0.25, 0.5)

pa <- make_panel("hischshr1520f", y4, x4, NULL, NULL, title = "(a) Women in 2000")

pb <- make_panel("hischshr1520m", y4, x4, NULL, NULL, y_side = "right", title = "(b) Men in 2000")

pc <- make_panel("c90hischshr1520f", y4, x4, NULL, NULL, title = "(c) Women in 1990")

pd <- make_panel("c90hischshr1520m", y4, x4, NULL, NULL,  y_side = "right", title = "(d) Men in 1990")

fig4 <- (pa | pb) / (pc | pd)
ggsave(here("output", "figures", "figure4.png"), fig4, width = 8, height = 9, dpi = 300)