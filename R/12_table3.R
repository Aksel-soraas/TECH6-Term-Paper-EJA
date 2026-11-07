
t3_data <- df |>
    drop_na(all_of(c("hischshr1520f", "i94", "iwm94", controls)))
nrow(t3_data)

polys <- list("None" = NULL,
            "Linear" = lin,
            "Quadratic" = quad, 
            "Cubic" = cub, 
            "Quartic" = c(cub, "vi4", "vi4_i"))
windows <- c(1, 0.5, 0.25, 0.10, 0.05)

t3_reg <- function(g, poly, b) {
    window_data <- t3_data |>
        filter(iwm94 < b, iwm94 > -b) |>
        mutate(vi1 = vi1 / b, vi1_i = vi1_i / b,
               vi2 = vi2 / b^2, vi2_i = vi2_i / b^2,
               vi3 = vi3 / b^3, vi3_i = vi3_i / b^3,
               vi4 = vi4 / b^4, vi4_i = vi4_i / b^4)
    feols(reformulate(c("i94", polys[[poly]], controls), response = paste0("hischshr1520", g)),
            data = window_data, cluster = ~pcode)
}

results <- expand_grid(g = c("f", "m"), poly = names(polys), b = windows)
results$est <- NA
results$se  <- NA
results$p   <- NA
results$n   <- NA


for (i in 1:nrow(results)) {
    fit <- t3_reg(results$g[i], results$poly[i], results$b[i])
    results$est[i] <- coef(fit)[["i94"]]
    results$se[i]  <- se(fit)[["i94"]]
    results$p[i]   <- pvalue(fit)[["i94"]]
    results$n[i]   <- nobs(fit)
}

stars <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))

make_t3_panel <- function(gender) {
    cells <- results |>
        filter(g == gender) |>
        mutate(est_txt = paste0(sprintf("%.3f", est), stars(p)),
                se_txt = sprintf("(%.3f)", se))
    est_rows <- cells |> select(poly, b, est_txt) |>
        pivot_wider(names_from = b, values_from = est_txt)
    se_rows <- cells |> select(poly, b, se_txt) |>
        pivot_wider(names_from = b, values_from = se_txt) |>
        mutate(poly = "")
    obs_row <- cells |> filter(poly == "None") |> select(b, n) |>
        mutate(n = as.character(n)) |>
        pivot_wider(names_from = b, values_from = n) |>
        mutate(poly = "Observations", .before = 1)
    panel <- bind_rows(est_rows, se_rows)
    panel <- panel[c(1, 6, 2, 7, 3, 8, 4, 9, 5, 10), ]
    bind_rows(panel, obs_row)
}

table3 <- bind_rows(make_t3_panel("f"), make_t3_panel("m"))
names(table3) <- c(" ", "1", "0.5", "0.25", "0.1", "0.05")

t3_tt <- tt(table3, notes = "Each cell is an RD estimate using the bandwidth (column) and polynomial control function (row). All specifications include covariates and province dummies. Standard errors clustered by province in parentheses. * p<0.10, ** p<0.05, *** p<0.01.") |>
    group_tt(j = list("Bandwidth" = 2:6), 
             i = list("Panel A: Women" = 1, "Panel B: Men" = 12))
t3_tt

save_tt(t3_tt, here("output", "tables", "table3.tex"),  overwrite = TRUE)
save_tt(t3_tt, here("output", "tables", "table3.html"), overwrite = TRUE)