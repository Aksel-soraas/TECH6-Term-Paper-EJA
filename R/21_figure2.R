source(here::here("R", "00_Setup.R"))
source(here::here("R", "02_helpers.R"))

fig2a_data <- df |>
    drop_na(iwm94) |>
    select(margin = iwm94)

fig2a <- ggplot(fig2a_data, aes(x = margin)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    geom_histogram(aes(y = after_stat(count / sum(count) * 100)),
                    binwidth = 0.02, boundary = 0,
                    fill = "grey60", color = "grey20") +
    labs(x = "Islamic win margin", y = "Percent") +
    scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5, 1), labels = c(-1, -0.5, 0, 0.5, 1)) +
    scale_y_continuous(limits = c(0, 5.15), expand = c(0, 0), breaks = 0:5) +
    theme_classic(base_size = 14) +
    theme(text = element_text(family = "serif"), 
    axis.line = element_blank(),
    axis.ticks = element_line(linewidth = 0.3),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6))

ggsave(here("output", "figures", "figure2a.png"), fig2a, width = 7, height = 5, dpi = 300)