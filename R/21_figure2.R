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
    labs(x = "Islamic Win Margin", y = "Percent") +
    scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5, 1), labels = c(-1, -0.5, 0, 0.5, 1)) +
    scale_y_continuous(limits = c(0, 5.15), expand = c(0, 0), breaks = 0:5) +
    theme_classic(base_size = 14) +
    theme(text = element_text(family = "serif"), 
    axis.line = element_blank(),
    axis.ticks = element_line(linewidth = 0.3),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6))

ggsave(here("output", "figures", "figure2a.png"), fig2a, width = 7, height = 5, dpi = 300)

# Sample as defined in authors replication file graphs_R4_final.do
fig2b_data <- df |> 
    drop_na(iwm94, hischshr1520f) |>
    filter(abs(iwm94) < 0.6) 

# Run the McCrary density test on the sample data with cutoff at 0. ext.out returns theta, se, bw, binsize and data.
result <- DCdensity(fig2b_data$iwm94, cutpoint = 0, plot = FALSE, ext.out = TRUE)

n <- nrow(fig2b_data)

theta <- result$theta # Theta is the estimated log difference in the density of the margin at the cutoff.
se_theta <- result$se # Standard error of theta
b <- result$binsize # b is the width of each histogram bin
h <- result$bw # h is the bandwidth of the local linear smoothing, it measures how far from a point the bins still have weight.
cells <- result$data # data is the table of bins, one row per bin, holding its midpoint and its density.

pad_n <- ceiling(h / b) # The number of bins that fit within one bandwidth of a bin, rounded up.

# Position the extra bins below the lowest and above the highest real bin, with space b. 
lower_midpoints <- min(cells$cellmp) - b * (pad_n:1)
upper_midpoints <- max(cells$cellmp) + b * (1:pad_n)

# Create a new padded table containing the real bins and the padded bins.
# The reason to add the padding on both ends is that the smoothing function 
# reaches the points within one bandwidth of the bin, and beyond the end of the bins 
# the true density is 0. Including these zero bins lets the smoothed line
# fall towards 0 at the ends, as the true density does. Without them, the fit
# near the ends would only use real bins and follow their local trend instead of 
# falling towards 0.

cells_pad <- tibble(
    cellmp = c(lower_midpoints, cells$cellmp, upper_midpoints), # bin midpoint
    cellval = c(rep(0, pad_n), cells$cellval, rep(0, pad_n)) # bin density
)

# Returns the smoothed density at one point by fitting a weighted
# straight line to the bins on the same side of 0
fhat_at <- function(x0) {
    side <- cells_pad |> 
        filter(if (x0 < 0) cellmp < 0 else cellmp >= 0) |> # Only consider the bins on the same side of 0, to allow for a jump at 0.
        mutate(dist = cellmp - x0, # Creates entry of distance between each bin and evaluation point.
        w = pmax(0, 1 - abs(dist) / h)) # Adds weight 1 at x0, and weight falls to 0 at distance h
    coef(lm(cellval ~ dist, data = side, weights = w))[1] # Models a weighted line of density on distance.
}

cells$fhat <- sapply(cells$cellmp, fhat_at) # Evaluates the curve at every real bin midpoint and stores in cells

# Measures the standard error of the curve at each midpoint. The SE bands are the curve +- 1.96 the McCrary formula. 
# m measures how much of the bandwidth the cutoff has cut off. 
se_at <- function(x, fx) {
    if (x > 0) {
        m <- max(-1, -x / h) # Right side of cutoff
        num <- 2-3*m^11-24*m^10-83*m^9-72*m^8+42*m^7+18*m^6-18*m^5+18*m^4-3*m^3+18*m^2-15*m 
        den <- 1+m^6+6*m^5-3*m^4-4*m^3+9*m^2-6*m 
    } else {
        m <- min(-x / h, 1) # Left side of cutoff
        num <- 2+3*m^11-24*m^10+83*m^9-72*m^8-42*m^7+18*m^6+18*m^5+18*m^4-3*m^3+18*m^2+15*m 
        den <- 1+m^6-6*m^5-3*m^4+4*m^3+9*m^2+6*m
    }
    sqrt((12 * fx) / (5 * n * h) * num / den^2)
}

# cells$se stores standard error, lwr and upr are the curve +- 1.96 * standard error
cells$se <- mapply(se_at, cells$cellmp, cells$fhat)
cells$lwr <- cells$fhat - 1.96 * cells$se
cells$upr <- cells$fhat + 1.96 * cells$se

plot_data <- cells |>
    filter(cellmp > -0.5, cellmp < 0.5) # Restricts data to be between -0.5 and 0.5 as per the paper specifications.

fig2b <- ggplot(plot_data, aes(x = cellmp)) +
    geom_line(data = filter(plot_data, cellmp < 0), aes(y = fhat)) +
    geom_line(data = filter(plot_data, cellmp >= 0), aes(y = fhat)) +
    geom_line(data = filter(plot_data, cellmp < 0), aes(y = lwr), linetype = "longdash", linewidth = 0.3) +
    geom_line(data = filter(plot_data, cellmp >= 0), aes(y = lwr), linetype = "longdash", linewidth = 0.3) +
    geom_line(data = filter(plot_data, cellmp < 0), aes(y = upr), linetype = "longdash", linewidth = 0.3) +
    geom_line(data = filter(plot_data, cellmp >= 0), aes(y = upr), linetype = "longdash", linewidth = 0.3) +
    geom_point(aes(y = cellval), color = "grey50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.3) + 
    labs(x = "Islamic Win Margin 1994", y = "Density", caption = sprintf("Discontinuity est. = %.3f, s.e. = %.3f", theta, se_theta)) +
    scale_x_continuous(breaks = c(-0.5, 0, 0.5), labels = c(-0.5, 0, 0.5), expand = expansion(mult = 0.025)) +
    scale_y_continuous(breaks = c(0, 1.25, 2.5), labels = c(0, 1.25, 2.5), expand = expansion(mult = 0.025)) +
    theme_classic(base_size = 14) +
    theme(text = element_text(family = "serif"), 
    axis.line = element_blank(),
    axis.ticks = element_line(linewidth = 0.3),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    plot.caption = element_text(hjust = 0),
    plot.caption.position = "panel")

ggsave(here("output", "figures", "figure2b.png"), fig2b, width = 7, height = 5, dpi = 300)
