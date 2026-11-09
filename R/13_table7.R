source(here::here("R", "00_Setup.R"))
source(here::here("R", "02_helpers.R"))

outcomes <- c("ahischshr15_19f", "ahischshr15_19m", "ahischshr15_29f", "ahischshr15_29m", "nevm15_19f", "nevm15_19m")

table7_models <- function(y) {
    ols_data <- df |> drop_na(all_of(c(y, "i94", controls)))
    rd_data  <- df |> drop_na(all_of(c(y, "i94", lin, controls)))
    h <- ik_bandwidth(rd_data[[y]], rd_data$iwm94)

    rd <- function(bw) {
        feols(reformulate(c("i94", lin, controls), response = y),
        data = rd_data |> filter(abs(iwm94) < bw), cluster = ~pcode)
    }

    list(uncond = feols(reformulate("i94", response = y),
                        data = ols_data, cluster = ~pcode),
        cond =    feols(reformulate(c("i94", controls), response = y),
                        data = ols_data, cluster = ~pcode),
        rd_h  = rd(h),
        rd_h2 = rd(h / 2),
        rd_2h = rd(h * 2),
        bw    = c(uncond = 1, cond = 1, rd_h = h, rd_h2 = h /2, rd_2h = h * 2))
}

models <- lapply(outcomes, table7_models)
names(models) <- outcomes

coef_rows <- function(spec, label) {
    fits <- lapply(models, function(m) m[[spec]])
    est  <- sapply(fits, function(f) paste0(sprintf("%.3f", coef(f)[["i94"]]), stars(pvalue(f)[["i94"]])))
    se   <- sapply(fits, function(f) sprintf("(%.3f)", se(f)[["i94"]]))
    rbind(c(label, est), c("", se))
}

info_rows <- function(spec, bw_fmt) {
    mean <- sapply(models, function(m) sprintf("%.3f", fitstat(m[[spec]], "my")[[1]]))
    bw   <- sapply(models, function(m) sprintf(bw_fmt, m$bw[[spec]]))
    n    <- sapply(models, function(m) as.character(nobs(m[[spec]])))
    list(mean = c("Outcome mean", mean), bw = c("Bandwidth", bw), n = c("Observations", n))
}

a  <- info_rows("cond", "%.2f")
b1 <- info_rows("rd_h", "%.3f")
b2 <- info_rows("rd_h2", "%.3f")
b3 <- info_rows("rd_2h", "%.3f")

table7 <- rbind(
    a$mean,
    coef_rows("uncond", "Model 1: Unconditional"),
    coef_rows("cond", "Model 2: Conditional"),
    a$bw, a$n,
    b1$mean,
    coef_rows("rd_h",  "Model 1: Bandwidth = h"),   b1$bw, b1$n,
    coef_rows("rd_h2", "Model 2: Bandwidth = h/2"), b2$bw, b2$n,
    coef_rows("rd_2h", "Model 3: Bandwidth = 2h"),  b3$bw, b3$n
)
table7 <- as.data.frame(table7)
names(table7) <- c(" ", "Women", "Men", "Women ", "Men ", "Women  ", "Men  ")
table7

t7_tt <- tt(table7, notes = "Panel A: OLS on the full sample. Panel B: local linear RD with covariates; h is the Imbens-Kalyanaraman bandwidth, computed separately for each outcome. Standard errors clustered by province in parentheses. * p<0.10, ** p<0.05, *** p<0.01.") |>
    group_tt(j = list("High school, 15-19" = 2:3, "High school, 15-29" = 4:5, "Unmarried, 15-19" = 6:7),
             i = list("Panel A: Global OLS" = 1, "Panel B: Conditional local linear RD" = 8))
t7_tt

save_tt(t7_tt, here("output", "tables", "table7.tex"),  overwrite = TRUE)
save_tt(t7_tt, here("output", "tables", "table7.html"), overwrite = TRUE)
