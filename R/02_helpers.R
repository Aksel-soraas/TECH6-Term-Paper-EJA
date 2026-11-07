# Data used for table 1, 2, and figure 1. 
# It is the baseline sample of 2629 municipalities with 
# 1994 election and 2000 consensus data.

covs <- c("vshr_islam1994", "partycount", "shhs", "lpop1994", "ageshr19", "ageshr60", "sexr", "merkezi", "merkezp", "subbuyuk", "buyuk")

pd <- grep("^pd_", names(df), value = TRUE)
controls <- c(covs, pd)

matched_sample <- df |>
    drop_na(all_of(c("hischshr1520f", "hischshr1520m", "i94", "iwm94", controls)))
nrow(matched_sample)

