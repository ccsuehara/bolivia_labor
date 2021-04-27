source("EDA.R")

adults_r <- adults %>% filter(area == "Rural")
adults_u <- adults %>% filter(area == "Urbana")

# Wage premium in rural & urban areas for people with higher ed -----------------
## percent adults with higher ed, by area
pct_he <- function(df) {
  tot <- nrow(df)
  he <- sum(df$education == "Tertiary")
  he / tot
}
pct_he_r <- pct_he(adults_r)   #0.097
pct_he_u <- pct_he(adults_u)   #0.370

### as a contrast, pct adults with secondary ed has a much smaller gap
pct_se <- function(df) {
  tot <- nrow(df)
  se <- sum(df$education == "Secondary")
  se / tot
}
pct_se_r <- pct_se(adults_r)   #0.350
pct_se_u <- pct_se(adults_u)   #0.427


## LFP rates among adults with higher ed, by area
pct_lfp <- function(df) {
  tot <- nrow(df)
  lfp <- sum(!is.na(df$primary_job))
  lfp / tot
}
pct_lfp_r <- pct_lfp(adults_r %>% filter(education == "Tertiary"))   #0.932
pct_lfp_u <- pct_lfp(adults_u %>% filter(education == "Tertiary"))   #0.776    # Much higher joblessness rate in urban


## Average labor earning (across employed and unemployed) by higher ed status and area
avg_lab_all <- function(df) {
  tot_earn <- sum(df$lab_monthly_inc, na.rm = T)
  tot_earn / nrow(df)
}
avg_lab_all_he_r <- avg_lab_all(adults_r %>% filter(education == "Tertiary"))    #4116
avg_lab_all_nohe_r <- avg_lab_all(adults_r %>% filter(education != "Tertiary"))  #1943
avg_lab_all_he_u <- avg_lab_all(adults_u %>% filter(education == "Tertiary"))    #3913
avg_lab_all_nohe_u <- avg_lab_all(adults_u %>% filter(education != "Tertiary"))  #3476

wage_prem_r <- avg_lab_all_he_r - avg_lab_all_nohe_r    #2173
wage_prem_u <- avg_lab_all_he_u - avg_lab_all_nohe_u    #437


## Average labor earning (only among employed) by higher ed status and area
avg_lab <- function(df) {
  df <- df %>% filter(!is.na(primary_job))
  tot_earn <- sum(df$lab_monthly_inc, na.rm = T)
  tot_earn / nrow(df)
}
avg_lab_he_r <- avg_lab(adults_r %>% filter(education == "Tertiary"))    #4419
avg_lab_nohe_r <- avg_lab(adults_r %>% filter(education != "Tertiary"))  #2315
avg_lab_he_u <- avg_lab(adults_u %>% filter(education == "Tertiary"))    #5045
avg_lab_nohe_u <- avg_lab(adults_u %>% filter(education != "Tertiary"))  #4582


# Multi-generational households --------------------------



# Rural households with and without domestic remittances ----------------------




# Formal vs. informal employment? -----------------------------