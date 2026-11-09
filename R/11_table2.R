source(here::here("R", "00_Setup.R"))
source(here::here("R", "02_helpers.R"))

t2_reg <- function(outcome, poly = NULL, use_controls= TRUE, h = Inf){
    rhs <- c("i94", poly, if (use_controls) controls)
    feols(reformulate(rhs, response = outcome),
    data = matched_sample |> filter(abs(iwm94) < h),
    cluster = ~pcode)
}

table2_models <- function(g) {
    y      <- paste0("hischshr1520", g)
    y_stud <- paste0("studshr1530", g)
    h      <- ik_bandwidth(matched_sample[[y]],    matched_sample$iwm94)
    h_stud <- ik_bandwidth(matched_sample[[y_stud]], matched_sample$iwm94)

    list(
        "(1)" = t2_reg(y, use_controls = FALSE),
        "(2)" = t2_reg(y),
        "(3)" = t2_reg(y, lin, use_controls = FALSE, h = h),
        "(4)" = t2_reg(y, lin, h = h),
        "(5)" = t2_reg(y, lin, h = h / 2),
        "(6)" = t2_reg(y, lin, h = h * 2),
        "(7)" = t2_reg(y, quad, h = h),
        "(8)" = t2_reg(y, cub, h = h),
        "(9)" = t2_reg(y_stud, lin, h = h_stud)
    )
}

women <- table2_models("f")
men   <- table2_models("m")

etable(men,   keep = "i94", digits = 3, fitstat = ~my + n + r2)
etable(women, keep = "i94", digits = 3, fitstat = ~my + n + r2)

gender_pvalue <- function(stub, poly = NULL, use_controls = TRUE, h_f = Inf, h_m = Inf) {
    stacked <- bind_rows(
        matched_sample |> filter(abs(iwm94) < h_f) |> mutate(y = .data[[paste0(stub, "f")]], female = 1),
        matched_sample |> filter(abs(iwm94) < h_m) |> mutate(y = .data[[paste0(stub, "m")]], female = 0)
        )
    rhs <- c("i94", poly, if (use_controls) controls)
    fml <- reformulate(paste0("female * (", paste(rhs, collapse = " + "), ")"), response =  "y")
    fit <- feols(fml, data = stacked, cluster = ~pcode, ssc = ssc(K.adj = FALSE))
    2 * pnorm(-abs(coef(fit)[["female:i94"]] / se(fit)[["female:i94"]]))
}

h_f      <- ik_bandwidth(matched_sample$hischshr1520f, matched_sample$iwm94)
h_m      <- ik_bandwidth(matched_sample$hischshr1520m, matched_sample$iwm94)
h_stud_f <- ik_bandwidth(matched_sample$studshr1530f, matched_sample$iwm94)
h_stud_m <- ik_bandwidth(matched_sample$studshr1530m, matched_sample$iwm94)

hs <- "hischshr1520"
p_values <- c(
    "(1)" = gender_pvalue(hs, use_controls = FALSE),
    "(2)" = gender_pvalue(hs),
    "(3)" = gender_pvalue(hs, lin, use_controls = FALSE, h_f = h_f, h_m = h_m),
    "(4)" = gender_pvalue(hs, lin,  h_f = h_f,     h_m = h_m),
    "(5)" = gender_pvalue(hs, lin,  h_f = h_f / 2, h_m = h_m / 2),
    "(6)" = gender_pvalue(hs, lin,  h_f = h_f * 2, h_m = h_m * 2),
    "(7)" = gender_pvalue(hs, quad, h_f = h_f,     h_m = h_m),
    "(8)" = gender_pvalue(hs, cub,  h_f = h_f,     h_m = h_m),
    "(9)" = gender_pvalue("studshr1530", lin, h_f = h_stud_f, h_m = h_stud_m)
)
round(p_values, 3)

make_panel <- function(models, h, h_stud) {
    bw <- c(1, 1, h, h, h/2, h*2, h, h, h_stud)
    means <- sapply(models, function(m) fitstat(m, "my") [[1]])

    extra <- data.frame(term = c("Outcome mean", "Bandwidth"),
                        rbind(sprintf("%.3f", means), sprintf("%.3f", bw)), 
                        check.names = FALSE) |>
        setNames(c("term", names(models)))

    tab <- modelsummary(models,
                        output = "data.frame",
                        coef_map = c("i94" = "Islamic mayor in 1994"),
                        stars   = c("*" = .1, "**" = 0.05, "***" = 0.01),
                        fmt      = 3, 
                        gof_map  = list(list(raw = "r.squared", clean = "R2", fmt = 2),
                                        list(raw = "nobs",      clean = "Observations", fmt = 0)),
                        add_rows = extra)

    tab$term[tab$statistic == "std.error"] <- ""
    tab <- tab |> select(-part, -statistic)
    tab[c(5, 1, 2, 6, 3, 4), ]
}

panel_a <- make_panel(women, h_f, h_stud_f)
panel_b <- make_panel(men,   h_m, h_stud_m)

p_row <- data.frame(term = "p_value", t(sprintf("%.3f", p_values)), check.names = FALSE) |>
    setNames(names(panel_a))

spec <- data.frame(
    term = c("Control function", "Bandwidth", "Covariates"),
    rbind(c("None", "None", rep("Linear", 4), "Quadratic", "Cubic", "Linear"),
        c("Global", "Global", "h", "h", "h/2", "2h", "h", "h", "h"),
        c("No", "Yes", "No", rep("Yes", 6))),
  check.names = FALSE) |>
  setNames(names(panel_a))

table2 <- bind_rows(spec, panel_a, panel_b, p_row)
names(table2)[1] <- " "

t2_tt <- tt(table2, notes = "Standard errors clustered by province in parentheses. * p<0.10, ** p<0.05, *** p<0.01.") |>
  group_tt(j = list("High school completion, age 15-20 (2000)" = 2:9,
                    "Enrollment, 15-30" = 10),
           i = list("Panel A: Women" = 4,
                    "Panel B: Men"   = 10,
                    "Panel C: Test of equality, women = men" = 16))

t2_tt                                                   

save_tt(t2_tt, here("output", "tables", "table2.tex"), overwrite = TRUE)
save_tt(t2_tt, here("output", "tables", "table2.html"), overwrite = TRUE)
