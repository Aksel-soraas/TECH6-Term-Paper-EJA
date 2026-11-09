# Data used for table 1, 2, 3 and figure 1. 
# It is the baseline sample of 2629 municipalities with 
# 1994 election and 2000 census data.

covs <- c("vshr_islam1994", "partycount", "shhs", "lpop1994", "ageshr19", "ageshr60", "sexr", "merkezi", "merkezp", "subbuyuk", "buyuk")

pd <- grep("^pd_", names(df), value = TRUE)
controls <- c(covs, pd)

matched_sample <- df |>
    drop_na(all_of(c("hischshr1520f", "hischshr1520m", "i94", "iwm94", controls)))
nrow(matched_sample)

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

t1_stars <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ""))))
stars <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))

dir.create(here("output", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figures"), recursive = TRUE, showWarnings = FALSE)