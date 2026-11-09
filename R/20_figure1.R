source(here::here("R", "00_Setup.R"))
source(here::here("R", "02_helpers.R"))

fig1_data <- matched_sample |>
    select(margin =vi1, share = vshr_islam1994) |>
    filter(share <= 0.75) |>
    mutate(close = abs(margin) < 0.02)

diag_line <- tibble(share = c(0, 0.75)) |>
    mutate(margin = 2 * share - 1)

fig1 <- ggplot(fig1_data, aes(x = share, y = margin)) + 
    geom_hline(yintercept = 0, color = "grey70", linewidth = 0.3) +
    geom_point(color = "grey50", size = 0.85) + 
    geom_line(data = diag_line, color = "grey30", linewidth = 0.3) +
    geom_point(data=filter(fig1_data, close), color = "black", size = 0.85) +
    labs(x = "Islamic vote share", y = "Islamic win margin") +
    coord_cartesian(xlim = c(0, 0.75), ylim = c(-1, 0.6)) +
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75), labels = c(0, 0.25, 0.5, 0.75)) +
    scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5), labels = c(-1, -0.5, 0, 0.5)) +
    theme_classic(base_size= 14) +
    theme(aspect.ratio = 0.7, text = element_text(family = "serif")) 

ggsave(here("output", "figures", "figure1.png"), fig1, width = 7, height = 5, dpi = 300)
