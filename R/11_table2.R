

#This will be moved to the helpers

covs <- c("vshr_islam1994", "partycount", "shhs", "lpop1994", "ageshr19", "ageshr60", "sexr", "merkezi", "merkezp", "subbuyuk", "buyuk")

pd <- grep("^pd_", names(df), value = TRUE)
controls <- c(covs, pd)

t2_data <- df |>
    drop_na(all_of(c("hischshr1520f", "hischshr1520m", "i94", "iwm94", controls)))
nrow(t2_data)


#This is exclusively for table2
ik_bandwidth <- function(y, x) {
    N <- length(x)
    h1 <- 1.84 * sd(x) * N^(-1/5)
    left <- x >= -h1 & x < 0
    right <- x >= 0 & x <= h1
    f0 <- (sum(left) + sum(right)) / (2 * N * h1)
    sigma2 <- (sum((y[left] - mean(y[left]))^2) + 
            sum((y[right] - mean(y[right]))^2)) / 
            (sum(left) + sum(right))

    med_left <- median(x[x < 0])
    med_right <- median(x[x >= 0])
    mid <- x >= med_left & x <= med_right
    fit3 <- lm(y ~I(x >= 0) + x + I(x^2) + I(x^3), subset = mid)
    m3 <- 6 * coef(fit3)[5]
    N_right <- sum(x >= 0)
    N_left <- sum(x < 0)
    base <- 3.56 * (sigma2 / (f0 * max(m3^2, 0.01)))^(1/7)
    h2_right <- base * N_right^(-1/7)
    h2_left <- base * N_left^(-1/7)

    in_right <- x >= 0 & x <= h2_right
    in_left <- x >= -h2_left & x < 0 
    m2_right <- 2 * coef(lm(y ~ x + I(x^2), subset = in_right))[3]
    m2_left <- 2 * coef(lm(y ~ x + I(x^2), subset = in_left))[3]
    r_right <- 720 * sigma2 / (sum(in_right) * h2_right^4)
    r_left <- 720 * sigma2 / (sum(in_left) * h2_left^4)
    h_ik <- 3.4375 * (2 * sigma2 / (f0 * ((m2_right - m2_left)^2 + r_right + r_left)))^(1/5) * N^(-1/5)

    unname(h_ik)
}

lin <- c("vi1", "vi1_i")
quad <- c(lin, "vi2", "vi2_i")
cub <- c(quad, "vi3", "vi3_i")

t2_reg <- function(outcome, poly = NULL, use_controls= TRUE, h = Inf){
    rhs <- c("i94", poly, if (use_controls) controls)
    feols(reformulate(rhs, response = outcome),
    data = t2_data |> filter(abs(iwm94) < h),
    cluster = ~pcode)
}

table2_models <- function(g) {
    y      <- paste0("hischshr1520", g)
    y_stud <- paste0("studshr1530", g)
    h      <- ik_bandwidth(t2_data[[y]],    t2_data$iwm94)
    h_stud <- ik_bandwidth(t2_data[[y_stud]], t2_data$iwm94)

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
        t2_data |> filter(abs(iwm94) < h_f) |> mutate(y = .data[[paste0(stub, "f")]], female = 1),
        t2_data |> filter(abs(iwm94) < h_m) |> mutate(y = .data[[paste0(stub, "m")]], female = 0)
        )
    rhs <- c("i94", poly, if (use_controls) controls)
    fml <- reformulate(paste0("female * (", paste(rhs, collapse = " + "), ")"), response =  "y")
    fit <- feols(fml, data = stacked, cluster = ~pcode, ssc = ssc(K.adj = FALSE))
    2 * pnorm(-abs(coef(fit)[["female:i94"]] / se(fit)[["female:i94"]]))
}

h_f      <- ik_bandwidth(t2_data$hischshr1520f, t2_data$iwm94)
h_m      <- ik_bandwidth(t2_data$hischshr1520m, t2_data$iwm94)
h_stud_f <- ik_bandwidth(t2_data$studshr1530f, t2_data$iwm94)
h_stud_m <- ik_bandwidth(t2_data$studshr1530m, t2_data$iwm94)

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

dir.create(here("output", "tables"), recursive = TRUE, showWarnings = FALSE)
save_tt(t2_tt, here("output", "tables", "table2.tex"), overwrite = TRUE)
save_tt(t2_tt, here("output", "tables", "table2.html"), overwrite = TRUE)
