#Controls: 11 covariates from 1994 election and 2000 census, plus 81 province dummies
covs <- c("vshr_islam1994", "partycount", "shhs", "lpop1994", "ageshr19", "ageshr60", "sexr", "merkezi", "merkezp", "subbuyuk", "buyuk")

pd <- grep("^pd_", names(df), value = TRUE)
controls <- c(covs, pd)

#Baseline sample: 2629 municipalities with no missing values, used for tables 1, 2, 3 and Figure 1
matched_sample <- df |>
    drop_na(all_of(c("hischshr1520f", "hischshr1520m", "i94", "iwm94", controls)))

#Imbens-Kalyanaraman (2012) optimal bandwidth, translated from rdob.ado in the online replication package
#Chooses h to balance bias against noise (wide against narrow window)
ik_bandwidth <- function(y, x) {
    #Step 1: pilot bandwidth, used to measure the data near the cutoff
    N <- length(x)
    h1 <- 1.84 * sd(x) * N^(-1/5)
    #Municipalities within the pilot bandwidth on each side
    left <- x >= -h1 & x < 0
    right <- x >= 0 & x <= h1
    #Density of x at the cutoff: how many municipalities lie close to 0
    f0 <- (sum(left) + sum(right)) / (2 * N * h1)
    #Variance of y near the cutoff, around each side's own mean
    sigma2 <- (sum((y[left] - mean(y[left]))^2) +
            sum((y[right] - mean(y[right]))^2)) /
            (sum(left) + sum(right))

    #Step 2: third derivative from a cubic fit on the middle half of the data, used to choose the step 3 windows
    med_left <- median(x[x < 0])
    med_right <- median(x[x >= 0])
    mid <- x >= med_left & x <= med_right
    fit3 <- lm(y ~I(x >= 0) + x + I(x^2) + I(x^3), subset = mid)
    m3 <- 6 * coef(fit3)[5]
    #Windows for estimating the curvature on each side
    N_right <- sum(x >= 0)
    N_left <- sum(x < 0)
    base <- 3.56 * (sigma2 / (f0 * max(m3^2, 0.01)))^(1/7)
    h2_right <- base * N_right^(-1/7)
    h2_left <- base * N_left^(-1/7)

    #Step 3: Second derivative, finding curvature on each side from quadratic fits
    in_right <- x >= 0 & x <= h2_right
    in_left <- x >= -h2_left & x < 0
    m2_right <- 2 * coef(lm(y ~ x + I(x^2), subset = in_right))[3]
    m2_left <- 2 * coef(lm(y ~ x + I(x^2), subset = in_left))[3]
    #Regularization terms, prevent h from becoming very large when the curvatures are similar
    r_right <- 720 * sigma2 / (sum(in_right) * h2_right^4)
    r_left <- 720 * sigma2 / (sum(in_left) * h2_left^4)
    #Optimal bandwidth: wider with more noise, narrower with more data near the cutoff and more curvature
    #3.4375 is IK's constant for a triangular kernel (the paper's choice; a uniform kernel would use 5.4)
    h_ik <- 3.4375 * (2 * sigma2 / (f0 * ((m2_right - m2_left)^2 + r_right + r_left)))^(1/5) * N^(-1/5)

    #Return the bandwidth as a plain number
    unname(h_ik)
}

#Polynomial terms on each side of the cutoff: linear, quadratic and cubic
lin <- c("vi1", "vi1_i")
quad <- c(lin, "vi2", "vi2_i")
cub <- c(quad, "vi3", "vi3_i")

#Significance stars at 10/5/1 %
stars <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))

#Create output folders if they do not exist
dir.create(here("output", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figures"), recursive = TRUE, showWarnings = FALSE)