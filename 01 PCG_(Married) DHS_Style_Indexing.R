# Clean Up last Session
rm(list = ls())
cat("\014")

# dplyr::glimpse(cg_full)
# table(cg_full$a4ii_1, useNA = "ifany")

#==================================================================
# Load Libraries
#==================================================================
library(haven)
library(dplyr)
library(stringr)
library(psych)
library(purrr)
library(openxlsx)
library(tidyverse)
library(gtsummary)
library(GPArotation) # For oblimin rotation
library(flextable)
library(flexlsx)
library(ftExtra)
library(lavaan)
library(officer)


# Group the items as per how they appear, then place them in one factor
# Extract the mean for that factor
#==================================================================
# Read Data
#==================================================================
path <- "C:/Users/Public/Documents/APHRC/K-YAGNS/National Survey/Final Cleaned DataSets/Analyzed Datasets/Weights/K-YAGNS Dyad Analytic.dta"
df <- read_dta(path)

#==========================================================
# Drop sub-scale items
#==========================================================
drop_vars <- c('cii_fi', 'cii_fii', 'cii_gi', 'cii_gii', 'cii_hi', 'cii_hii')

cg_dropped <- df %>% select(-any_of(drop_vars))


cg_married <- cg_dropped %>%
  filter(df$cg_a4iv == 2)


#==================================================================
# Apply all variable labels
#==================================================================
library(labelled)

var_label(cg_married) <- list(
  
  # note_ci = "Men’s rights/privileges: response options note",
  
  ci_a = "Men’s rights/privileges: Sons deserve more education than daughters",
  ci_b = "Men’s rights/privileges: Girls’ schooling only if not needed at home",
  ci_c = "Men’s rights/privileges: Sons educated to care for parents later",
  ci_d = "Men’s rights/privileges: Spend limited school money on sons first",
  ci_e = "Men’s rights/privileges: Women should leave politics to men",
  ci_f = "Men’s rights/privileges: Women need male protection",
  ci_g = "Men’s rights/privileges: Women rely on sons in old age",
  ci_h = "Men’s rights/privileges: Good wife never questions husband",
  ci_i = "Men’s rights/privileges: Father should decide child health matters",
  
  # Women’s Empowerment
  cii_a = "Household decision-making: Use of respondent’s earnings",
  # cii_a_spy = "Household decision-making: Use of respondent’s earnings: Other (specify)",
  cii_b = "Household decision-making: Use of partner’s earnings",
  # cii_b_spy = "Household decision-making: Use of partner’s earnings: Other (specify)",
  cii_c = "Household decision-making: Respondent’s health care",
  # cii_c_spy = "Household decision-making: Respondent’s health care: Other (specify)",
  cii_d = "Household decision-making: Major household purchases",
  # cii_d_spy = "Household decision-making: Major household purchases: Other (specify)",
  cii_e = "Household decision-making: Visits to family/relatives",
  # cii_e_spy = "Household decision-making: Visits to family/relatives: Other (specify)",
  
  cii_f = "Asset ownership: House ownership status",
  # cii_fi = "Asset ownership: House title deed/document available",
  # cii_fii = "Asset ownership: Respondent named on house document",
  
  cii_g = "Asset ownership: Agricultural land ownership status",
  # cii_gi = "Asset ownership: Agricultural land title deed/document available",
  # cii_gii = "Asset ownership: Respondent named on agricultural land document",
  
  cii_h = "Asset ownership: Non-agricultural land ownership status",
  # cii_hi = "Asset ownership: Non-agricultural land title deed/document available",
  # cii_hii = "Asset ownership: Respondent named on non-agricultural land document",
  
  # note_ciiia = "Gender belief: response options note A",
  
  ciii_a = "Gender belief: Men naturally have many lovers",
  ciii_b = "Gender belief: Women need more than one sex partner",
  ciii_c = "Gender belief: Men ashamed of wives; prefer young lovers to show friends",
  ciii_d = "Gender belief: Friends laugh at men without lovers",
  ciii_e = "Gender belief: Financially independent women avoid commitment",
  ciii_f = "Gender belief: Men often pressure women into sex",
  ciii_g = "Gender belief: Men always ready for sex",
  ciii_h = "Gender belief: Men need sex more than women",
  ciii_i = "Gender belief: Sex is not discussed, just done",
  
  # note_ciiib = "Gender belief: response options note B",
  
  ciii_j = "Gender belief: Woman should not initiate sex",
  ciii_k = "Gender belief: Sex before marriage means woman deserves no respect",
  ciii_l = "Gender belief: Women carrying condoms are 'easy'",
  ciii_m = "Gender belief: Men should be outraged if wife asks to use condom",
  ciii_n = "Gender belief: Woman responsible to avoid pregnancy",
  ciii_o = "Gender belief: Woman is 'real' only after having a child",
  ciii_p = "Gender belief: 'Real man' produces a male child",
  ciii_q = "Comfort discussing family planning with partner",
  ciii_r = "Comfort discussing HIV with partner",
  
  # note_civ = "Gender attitudes towards violence: response options note",
  
  civ_a = "Violence attitude: Woman should tolerate violence to keep family together",
  civ_b = "Violence attitude: Man should defend reputation with force",
  civ_c = "Violence attitude: Partner violence is private and should not be discussed",
  
  # note_civ_d = "Wife-beating justification: response options note",
  
  civ_d_i = "Wife-beating justification: Goes out without telling husband",
  civ_d_ii = "Wife-beating justification: Neglects the children",
  civ_d_iii = "Wife-beating justification: Argues with husband",
  civ_d_iv = "Wife-beating justification: Refuses sex",
  civ_d_v = "Wife-beating justification: Burns the food",
  civ_d_vi = "Wife-beating justification: Refuses household chores",
  civ_d_vii = "Wife-beating justification: Comes home late",
  civ_d_viii = "Wife-beating justification: Unfaithful",
  
  # note_cv = "Domestic chore norm: response options note",
  
  cv_a = "Domestic chore norm: Childcare is mother’s responsibility",
  cv_b = "Domestic chore norm: Woman’s role is home and family care",
  cv_c = "Domestic chore norm: Man’s role is providing for family",
  cv_d = "Decision-making norm: Husband decides major household items",
  cv_e = "Decision-making norm: Man has final word at home",
  cv_f = "Decision-making norm: Woman should obey husband",
  cv_g = "Domestic chore norm: Men and women should share chores",
  
  # note_cvi = "Equity belief: response options note",
  
  cvi_a = "Equity belief: Couple should decide together about children",
  cvi_b = "Equity belief: Woman can suggest condom use like a man",
  cvi_c = "Equity belief: Man should know what partner likes during sex",
  cvi_d = "Equity belief: Couple should decide together on contraception",
  cvi_e = "Power dynamic: Partner gets their way in disagreements",
  cvi_f = "Power dynamic: Partner has more say on important decisions",
  cvi_g = "Power dynamic: Mostly do what partner wants",
  
  # note_cvii = "Relationship control: response options note",
  
  cvii_a = "Relationship control: Respondent is quiet when with partner",
  cvii_b = "Relationship control: Partner controls who respondent spends time with",
  cvii_c = "Relationship control: Partner would suspect infidelity if asked to use condom",
  cvii_d = "Relationship control: Respondent feels trapped in relationship",
  cvii_e = "Relationship control: Partner does what they want despite respondent",
  cvii_f = "Relationship control: Respondent more committed than partner",
  cvii_g = "Relationship control: Partner gets more out of relationship",
  cvii_h = "Relationship control: Partner wants to know respondent’s whereabouts",
  cvii_i = "Relationship control: Partner might be having sex with someone else"
  
)


var_label(cg_married$ci_a)
# look_for(df)


#==========================================================
# DEFINE SECTION GROUPS (DHS STYLE)
#==========================================================

cg_groups <- list(
  
  # CI: Patriarchal norms (men’s rights)
  rights_privileges = c(
    "ci_a","ci_b","ci_c","ci_d","ci_e",
    "ci_f","ci_g","ci_h","ci_i"
  ),
  
  # CII: Women empowerment (decision-making + assets)
  decision_making = c(
    "cii_a","cii_b","cii_c","cii_d","cii_e"
  ),
  
  asset_ownership = c(
    "cii_f","cii_g","cii_h"
  ),
  
  # CIII: Gender beliefs (relationships + sexuality)
  gender_beliefs = c(
    "ciii_a","ciii_b","ciii_c","ciii_d","ciii_e",
    "ciii_f","ciii_g","ciii_h","ciii_i",
    "ciii_j","ciii_k","ciii_l","ciii_m",
    "ciii_n","ciii_o","ciii_p"
  ),
  
  communication = c(
    "ciii_q","ciii_r"
  ),
  
  # CIV: Violence attitudes
  violence_attitudes = c(
    "civ_a","civ_b","civ_c"
  ),
  
  wife_beating = c(
    "civ_d_i","civ_d_ii","civ_d_iii","civ_d_iv",
    "civ_d_v","civ_d_vi","civ_d_vii","civ_d_viii"
  ),
  
  # CV: Domestic roles
  domestic_roles = c(
    "cv_a","cv_b","cv_c","cv_d","cv_e","cv_f","cv_g"
  ),
  
  # CVI: Equity and power
  equity_power = c(
    "cvi_a","cvi_b","cvi_c","cvi_d","cvi_e","cvi_f","cvi_g"
  ),
  
  # CVII: Relationship control
  relationship_control = c(
    "cvii_a","cvii_b","cvii_c","cvii_d","cvii_e",
    "cvii_f","cvii_g","cvii_h","cvii_i"
  )
)

#==========================================================
#  LABEL EACH DOMAIN
#==========================================================

cg_group_labels <- list(
  rights_privileges = "Patriarchal norms: Men’s rights and privileges",
  decision_making   = "Women’s empowerment: Household decision-making",
  asset_ownership   = "Women’s empowerment: Asset ownership",
  gender_beliefs    = "Gender norms: Relationships and sexuality beliefs",
  communication     = "Partner communication (married only)",
  violence_attitudes= "Gender attitudes towards violence",
  wife_beating      = "Justification of wife beating",
  domestic_roles    = "Household gender roles and responsibilities",
  equity_power      = "Equity and power in relationships",
  relationship_control = "Relationship control dynamics (married only)"
)



#==========================================================
# CREATE DATASETS PER DOMAIN
#==========================================================

cg_domain_data <- map(cg_groups, ~ cg_dropped %>% select(all_of(.x)))

# Check one domain
names(cg_domain_data$rights_privileges)

# Count items per domain
map_int(cg_domain_data, ncol)


#==========================================================
# FILTER MARRIED / PARTNERED
#==========================================================

# df_married <- cg_dropped %>%
#   filter(c == 2)   # adjust if coding differs
# 
# 
# # Check size
# nrow(df_married)

#==========================================================
# SPLIT DATA
#==========================================================

# cg_full <- cg_dropped
# 
# cg_married <- cg_dropped %>%
#   filter(df$cg_a4iv == 2)
# 
# nrow(cg_married)
# 
# nrow(cg_full)


#==================================================================
# A) CI → EFA/CFA ->  Rights and privileges of men (PATRIARCHAL NORMS) 
#==================================================================
ci_items <- cg_groups$rights_privileges
ci_data <- cg_married %>% select(all_of(ci_items))


cat("Items selected:", ncol(ci_data), "\n")

# Clean codes
ci_data <- ci_data %>% 
  mutate(across(everything(), ~ as.numeric(.))) %>% 
  mutate(across(everything(), ~{
    x <- .
    x[x == 97] <- NA
    x[x == 98] <- 2
    x
  }))

ci_num <- ci_data %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ factor(.x, levels = c(1, 2, 3), ordered = TRUE)
    )
  )

#-----------------------------------------------------------
# Check factorability
#-----------------------------------------------------------

ci_num <- ci_data %>%
  mutate(across(everything(), as.numeric))

psych::alpha(ci_num)

KMO(ci_num)
cortest.bartlett(ci_num)


#-----------------------------------------------------------
# EFA
#-----------------------------------------------------------
fa.parallel(ci_num, fm = "pa", fa = "fa")

ci_efa <- fa(ci_num, nfactors = 1, fm = "pa", rotate = "promax")

print(ci_efa$loadings, cutoff = 0.4)


# Reliability 
# alpha(ci_num)
# omega(ci_num)




#-----------------------------------------------------------
# CFA
#-----------------------------------------------------------
ci_model <- '
Patriarchal =~ ci_a + ci_b + ci_c + ci_d + ci_e +
               ci_f + ci_g + ci_h + ci_i
'

ci_fit <- lavaan::cfa(ci_model,
              data = ci_num,
              ordered = ci_items,
              estimator = "WLSMV")

# summary(ci_fit, fit.measures = TRUE, standardized = TRUE)

lavaan::fitMeasures(ci_fit, c("cfi","tli","rmsea","srmr"))

# ci_scores <- fa(ci_data, nfactors=1, fm="pa", scores="regression")$scores


#-----------------------------------------------------------
# Descriptives (All items)
#-----------------------------------------------------------
ci_descriptives <- data.frame(
  Item = colnames(ci_num),
  N = sapply(ci_num, function(x) sum(!is.na(x))),
  Mean = sapply(ci_num, function(x) mean(x, na.rm = TRUE)),
  SD = sapply(ci_num, function(x) sd(x, na.rm = TRUE)),
  Min = sapply(ci_num, function(x) min(x, na.rm = TRUE)),
  Max = sapply(ci_num, function(x) max(x, na.rm = TRUE)),
  Median = sapply(ci_num, function(x) median(x, na.rm = TRUE))
)

ci_descriptives <- ci_descriptives %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

#-----------------------------------------------------------
# Descriptives for Patriachial Norms
#-----------------------------------------------------------
ci_efa <- fa(ci_num, nfactors = 1, fm = "pa", rotate = "promax", scores = "regression")

factor_scores_df <- as.data.frame(ci_efa$scores)
colnames(factor_scores_df) <- "Patriarchal"

# factor_scores_df <- as.data.frame(lavPredict(ci_fit))
# colnames(factor_scores_df) <- "Patriarchal"

ci_descriptives2 <- data.frame(
  Factor = colnames(factor_scores_df),
  N = sapply(factor_scores_df, function(x) sum(!is.na(x))),
  Mean = sapply(factor_scores_df, function(x) mean(x, na.rm = TRUE)),
  SD = sapply(factor_scores_df, function(x) sd(x, na.rm = TRUE)),
  Min = sapply(factor_scores_df, function(x) min(x, na.rm = TRUE)),
  Max = sapply(factor_scores_df, function(x) max(x, na.rm = TRUE)),
  Median = sapply(factor_scores_df, function(x) median(x, na.rm = TRUE))
) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

#==========================================================
# B). CIII → EFA/CFA (GENDER BELIEFS)
#==========================================================
# Split:
    # Core beliefs → full sample
    # Communication (q,r) → married only

#------------------------------------
# I) Core beliefs
#------------------------------------
ciii_items <- cg_groups$gender_beliefs

ciii_data <- cg_married %>% select(all_of(ciii_items))

cat("Items selected:", ncol(ciii_data), "\n")


# Clean codes
ciii_num <- ciii_data %>% 
  mutate(across(everything(), ~ as.numeric(.))) %>% 
  mutate(across(everything(), ~{
    x <- .
    x[x == 97] <- NA
    x[x == 98] <- 2
    x
  }))


# Convert items to ordered factors 

ciii_num <- ciii_num %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ factor(.x, levels = c(1, 2, 3), ordered = TRUE)
    )
  )



#-----------------------------------------------------------
# Check factorability
#-----------------------------------------------------------

ciii_num <- ciii_num %>% mutate(across(everything(), as.numeric))

# Reliability 
psych::alpha(ciii_num)
psych::omega(ciii_num)

KMO(ciii_num)
cortest.bartlett(ciii_num)


#-----------------------------------------------------------
# EFA
#-----------------------------------------------------------
fa.parallel(ciii_num)

ciii_efa <- fa(ciii_num, nfactors = 1, fm = "pa", rotate = "promax")

print(ciii_efa$loadings, cutoff = 0.4)





#-----------------------------------------------------------
# CFA
#-----------------------------------------------------------
ciii_model <- '
GENDER BELIEFS  =~ ciii_a + ciii_b + ciii_c + ciii_d + ciii_e +
                ciii_f + ciii_g + ciii_h + ciii_i + ciii_j + ciii_k + ciii_l + ciii_m +
                ciii_n + ciii_o + ciii_p
'
ciii_fit <- lavaan::cfa(ciii_model,
                data = ciii_num,
                ordered = ciii_items,
                estimator = "WLSMV")

# summary(ciii_fit, fit.measures = TRUE, standardized = TRUE)
lavaan::fitMeasures(ciii_fit, c("cfi","tli","rmsea","srmr"))


#-----------------------------------------------------------
# Descriptives for Core beliefs
#-----------------------------------------------------------
ciii_efa <- fa(ciii_num, nfactors = 1, fm = "pa", rotate = "promax", scores = "regression")

factor_scores_df2 <- as.data.frame(ciii_efa$scores)
colnames(factor_scores_df2) <- "GENDER BELIEFS"

# factor_scores_df <- as.data.frame(lavPredict(ci_fit))
# colnames(factor_scores_df) <- "Patriarchal"

ciii_descriptives <- data.frame(
  Factor = colnames(factor_scores_df2),
  N = sapply(factor_scores_df2, function(x) sum(!is.na(x))),
  Mean = sapply(factor_scores_df2, function(x) mean(x, na.rm = TRUE)),
  SD = sapply(factor_scores_df2, function(x) sd(x, na.rm = TRUE)),
  Min = sapply(factor_scores_df2, function(x) min(x, na.rm = TRUE)),
  Max = sapply(factor_scores_df2, function(x) max(x, na.rm = TRUE)),
  Median = sapply(factor_scores_df2, function(x) median(x, na.rm = TRUE))
) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))


#-----------------------------------------------------------
# II) Communication (married only) -> No CFA needed. Two items only.
#-----------------------------------------------------------
# correlation + alpha + composite score

comm_items <- cg_groups$communication

comm_data <- cg_married %>% select(all_of(comm_items))
cat("Items selected:", ncol(comm_data), "\n")

nrow(cg_married)

# Clean codes
comm_data <- comm_data %>% 
  mutate(across(everything(), ~ as.numeric(.))) %>% 
  mutate(across(everything(), ~{
    x <- .
    x[x == 97] <- NA
    x[x == 98] <- 2
    x
  }))

# Drop zero variance 
# comm_data <- comm_data[, sapply(comm_data, function(x) var(x, na.rm = TRUE) > 0)]

comm_data <- comm_data %>% mutate(across(everything(), as.numeric))

# Manual alpha computation
# comm_data_clean <- na.omit(comm_data)
# r <- cor(comm_data_clean$ciii_q, comm_data_clean$ciii_r)
# alpha_manual <- (2 * r) / (1 + r)
# alpha_manual

# Create a composite score (communication score)
comm_index <- rowMeans(comm_data, na.rm = TRUE)

# Descriptives
summary(comm_index)

# Standard Deviation
sd(comm_index, na.rm = TRUE)

# polychoric correlation
pc <- psych::polychoric(comm_data)
r_poly <- pc$rho[1,2]
r_poly

# Polychoric alpha
alpha_poly <- (2 * 0.9149713) / (1 + 0.9149713)
alpha_poly

#==========================================================
# CV → EFA/CFA (DOMESTIC ROLES)
#==========================================================
cv_items <- cg_groups$domestic_roles

cv_data <- cg_married %>% select(all_of(cv_items))

cat("Items selected:", ncol(cv_data), "\n")

# Clean codes
cv_data <- cv_data %>% 
  mutate(across(everything(), ~ as.numeric(.))) %>% 
  mutate(across(everything(), ~{
    x <- .
    x[x == 97] <- NA
    x[x == 98] <- 2
    x
  }))


cv_data <- cv_data %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ factor(.x, levels = c(1, 2, 3), ordered = TRUE)
    )
  )

cv_num <- cv_data %>% mutate(across(everything(), as.numeric))
fa.parallel(cv_num)

cv_efa <- fa(cv_num, nfactors = 1, fm = "pa", rotate = "promax")

print(cv_efa$loadings, cutoff = 0.4)


# # alpha(cv_num)
# omega(cv_num)

psych::alpha(cv_num)
psych::omega(cv_num)

cv_model <- '
Domestic =~ cv_a + cv_b + cv_c + cv_d + cv_e + cv_f + cv_g
'

cv_fit <- lavaan::cfa(cv_model,
              data = cv_num,
              ordered = cv_items,
              estimator = "WLSMV")

# summary(cv_fit, fit.measures = TRUE, standardized = TRUE)
lavaan::fitMeasures(cv_fit, c("cfi","tli","rmsea","srmr"))


#-----------------------------------------------------------
# Descriptives for Core beliefs
#-----------------------------------------------------------
cv_efa <- fa(cv_num, nfactors = 1, fm = "pa", rotate = "promax", scores = "regression")

factor_scores_df3 <- as.data.frame(cv_efa$scores)
colnames(factor_scores_df3) <- "Domestic Roles"


cv_descriptives <- data.frame(
  Factor = colnames(factor_scores_df3),
  N = sapply(factor_scores_df3, function(x) sum(!is.na(x))),
  Mean = sapply(factor_scores_df3, function(x) mean(x, na.rm = TRUE)),
  SD = sapply(factor_scores_df3, function(x) sd(x, na.rm = TRUE)),
  Min = sapply(factor_scores_df3, function(x) min(x, na.rm = TRUE)),
  Max = sapply(factor_scores_df3, function(x) max(x, na.rm = TRUE)),
  Median = sapply(factor_scores_df3, function(x) median(x, na.rm = TRUE))
) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))



#==========================================================
# CVI → EFA/CFA (EQUITY & POWER)
#==========================================================
# Split:
    # Equity items → full
    # Power items → married

#-----------------------------------------------------------
# I) Equity Items
#-----------------------------------------------------------
cvi_equity <- c("cvi_a","cvi_b","cvi_c","cvi_d")

eq_data <- cg_married %>% select(all_of(cvi_equity))


# Clean codes
eq_data <- eq_data %>% 
  mutate(across(everything(), ~ as.numeric(.))) %>% 
  mutate(across(everything(), ~{
    x <- .
    x[x == 97] <- NA
    x[x == 98] <- 2
    x
  }))

eq_data <- eq_data %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ factor(.x, levels = c(1, 2, 3), ordered = TRUE)
    )
  )

eq_num <- eq_data %>% mutate(across(everything(), as.numeric))
fa.parallel(eq_num)


eq_efa <- fa(eq_num, nfactors = 1, fm = "pa", rotate = "promax")

print(eq_efa$loadings, cutoff = 0.4)


# alpha(eq_num)
# omega(eq_num)
psych::alpha(eq_num)
psych::omega(eq_num)

eq_model <- '
Equity =~ cvi_a + cvi_b + cvi_c + cvi_d
'

eq_fit <-lavaan::cfa(eq_model,
              data = eq_num,
              ordered = cvi_equity,
              estimator = "WLSMV")

# summary(cv_fit, fit.measures = TRUE, standardized = TRUE)
lavaan::fitMeasures(eq_fit, c("cfi","tli","rmsea","srmr"))


#-----------------------------------------------------------
# Descriptives for Core beliefs
#-----------------------------------------------------------
eq_efa <- fa(eq_num, nfactors = 1, fm = "pa", rotate = "promax", scores = "regression")

factor_scores_df4 <- as.data.frame(eq_efa$scores)
colnames(factor_scores_df4) <- "Equity"


eq_descriptives <- data.frame(
  Factor = colnames(factor_scores_df4),
  N = sapply(factor_scores_df4, function(x) sum(!is.na(x))),
  Mean = sapply(factor_scores_df4, function(x) mean(x, na.rm = TRUE)),
  SD = sapply(factor_scores_df4, function(x) sd(x, na.rm = TRUE)),
  Min = sapply(factor_scores_df4, function(x) min(x, na.rm = TRUE)),
  Max = sapply(factor_scores_df4, function(x) max(x, na.rm = TRUE)),
  Median = sapply(factor_scores_df4, function(x) median(x, na.rm = TRUE))
) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

#-----------------------------------------------------------
# II) Power (married)
#-----------------------------------------------------------
cvi_power <- c("cvi_e","cvi_f","cvi_g")
pow_data <- cg_married %>% select(all_of(cvi_power))

# Clean codes
pow_num <- pow_data %>% 
  mutate(across(everything(), ~ as.numeric(.))) %>% 
  mutate(across(everything(), ~{
    x <- .
    x[x == 97] <- NA
    x[x == 98] <- 2
    x
  }))


# Convert items to ordered factors 

pow_num <- pow_num %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ factor(.x, levels = c(1, 2, 3), ordered = TRUE)
    )
  )


pow_num <- pow_num %>% mutate(across(everything(), as.numeric))

# Reliablity
psych::alpha(pow_num)

# Check correlation
# r > 0.30 → acceptable
# r > 0.50 → strong

cor(pow_num, use = "pairwise.complete.obs")

# Run EFA
fa.parallel(pow_num, fm = "pa", fa = "fa")
pow_efa <- fa(pow_num, nfactors = 1, fm = "pa", rotate = "none")

print(pow_efa$loadings, cutoff = 0.4)

# Run CFA
model <- 'Power =~ cvi_e + cvi_f + cvi_g'
cfa(model, data = cg_married, estimator = "WLSMV", ordered = cvi_power)

# pow_descriptives <- data.frame(
#   Factor = colnames(factor_scores_df),
#   N = sapply(factor_scores_df4, function(x) sum(!is.na(x))),
#   Mean = sapply(factor_scores_df4, function(x) mean(x, na.rm = TRUE)),
#   SD = sapply(factor_scores_df4, function(x) sd(x, na.rm = TRUE)),
#   Min = sapply(factor_scores_df4, function(x) min(x, na.rm = TRUE)),
#   Max = sapply(factor_scores_df4, function(x) max(x, na.rm = TRUE)),
#   Median = sapply(factor_scores_df4, function(x) median(x, na.rm = TRUE))
# ) %>%
#   mutate(across(where(is.numeric), ~ round(., 3)))


#==========================================================
# CVII → EFA/CFA (RELATIONSHIP CONTROL)
#==========================================================

# Select items
cvii_items <- cg_groups$relationship_control
cvii_data <- cg_married %>% dplyr::select(all_of(cvii_items))

cat("Items selected:", ncol(cvii_data), "\n")


#----------------------------------------------------------
# CLEAN → numeric version (for EFA, alpha, omega)
#----------------------------------------------------------
cvii_num <- cvii_data %>% 
  dplyr::mutate(dplyr::across(everything(), as.numeric)) %>% 
  dplyr::mutate(dplyr::across(everything(), ~{
    x <- .
    x[x == 97] <- NA
    x[x == 98] <- 2
    x
  }))


#----------------------------------------------------------
# ORDERED version (for CFA with WLSMV)
#----------------------------------------------------------
cvii_ord <- cvii_num %>%
  dplyr::mutate(
    dplyr::across(
      everything(),
      ~ factor(.x, levels = c(1, 2, 3), ordered = TRUE)
    )
  )


#----------------------------------------------------------
# EFA + reliability (use numeric)
#----------------------------------------------------------
fa.parallel(cvii_num)

cvii_efa <- psych::fa(
  cvii_num,
  nfactors = 1,
  fm = "pa",
  rotate = "promax",
  scores = "regression"
)

print(cvii_efa$loadings, cutoff = 0.4)

psych::alpha(cvii_num)
psych::omega(cvii_num)


#----------------------------------------------------------
# CFA (use ordered data)
#----------------------------------------------------------
cvii_model <- '
Control =~ cvii_a + cvii_b + cvii_c + cvii_d +
           cvii_e + cvii_f + cvii_g + cvii_h + cvii_i
'

cvii_fit <- lavaan::cfa(
  cvii_model,
  data = cvii_ord,
  ordered = cvii_items,
  estimator = "WLSMV"
)

# Fit indices
lavaan::fitMeasures(cvii_fit, c("cfi","tli","rmsea","srmr"))


#----------------------------------------------------------
# Factor scores (from EFA)
#----------------------------------------------------------
factor_scores_df5 <- as.data.frame(cvii_efa$scores)
colnames(factor_scores_df5) <- "Relationship_control"


#----------------------------------------------------------
# Descriptive statistics
#----------------------------------------------------------
cvii_descriptives <- data.frame(
  Factor = colnames(factor_scores_df5),
  N = sapply(factor_scores_df5, function(x) sum(!is.na(x))),
  Mean = sapply(factor_scores_df5, function(x) mean(x, na.rm = TRUE)),
  SD = sapply(factor_scores_df5, function(x) sd(x, na.rm = TRUE)),
  Min = sapply(factor_scores_df5, function(x) min(x, na.rm = TRUE)),
  Max = sapply(factor_scores_df5, function(x) max(x, na.rm = TRUE)),
  Median = sapply(factor_scores_df5, function(x) median(x, na.rm = TRUE))
) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))






#==========================================================
# Decision index, Asset Index, Wife Beating
#==========================================================

#==========================================================
# CIV_d → INDEX (NOT FACTOR)
#==========================================================
# Binary Scoring
#==========================================================
# Wife beating index
#==========================================================
# Measure acceptance of wife beating. Higher score means higher acceptance.

#  0 = rejects all violence
#  1 = accepts all scenarios

civd_items <- cg_groups$wife_beating

civd_clean <- cg_married %>%
  dplyr::select(all_of(civd_items)) %>%
  dplyr::mutate(
    dplyr::across(
      everything(),
      ~ dplyr::case_when(
        .x == 1 ~ 1,   # Accept
        .x == 2 ~ 0,   # Reject
        TRUE ~ NA_real_
      )
    )
  )

cg_married <- cg_married %>%
  dplyr::mutate(
    wife_beating_index = rowMeans(civd_clean, na.rm = TRUE)
  )




#==========================================================
# CII → Decision-Making Index (DHS Standard)
#==========================================================
# Measure respondent participation in decisions.

#==========================================================
# DECISION-MAKING INDEX
#==========================================================

dec_items <- cg_groups$decision_making

cg_married <- cg_married %>%
  dplyr::mutate(
    decision_index = dplyr::across(
      dplyr::all_of(dec_items),
      ~ ifelse(
        a4ii_1 == 2,
        dplyr::case_when(
          .x %in% c(1, 3) ~ 1,
          .x %in% c(2, 4, 5) ~ 0,
          TRUE ~ NA_real_
        ),
        NA_real_
      )
    ) %>%
      rowMeans(na.rm = TRUE)
  )

# Interpretation
    # 0 → no participation
    # 1 → full participation
    # 0.5 → participates in half of decisions

#==========================================================
# Asset Ownership Index
#==========================================================
# Simple proxy for economic empowerment.

#==========================================================
# ASSET OWNERSHIP INDEX
#==========================================================

cg_married <- cg_married %>%
  dplyr::mutate(
    owns_house = dplyr::case_when(
      is.na(cii_f) ~ NA_real_,
      as.numeric(cii_f) %in% c(1, 2,3, 4, 5) ~ 1,
      TRUE ~ 0
    ),
    owns_agriland = dplyr::case_when(
      is.na(cii_g) ~ NA_real_,
      as.numeric(cii_g) %in% c(1, 2,3, 4, 5) ~ 1,
      TRUE ~ 0
    ),
    owns_nonagriland = dplyr::case_when(
      is.na(cii_h) ~ NA_real_,
      as.numeric(cii_h) %in% c(1, 2,3, 4, 5) ~ 1,
      TRUE ~ 0
    ),
    asset_index = rowMeans(
      dplyr::across(c(owns_house, owns_agriland, owns_nonagriland)),
      na.rm = TRUE
    )
  )


#==================================================================
# sanity checks
#==================================================================
range(cg_married$wife_beating_index, na.rm = TRUE)
range(cg_married$decision_index, na.rm = TRUE)
range(cg_married$asset_index, na.rm = TRUE)



#==================================================================
# 15. DESCRIPTIVES TABLES
#==================================================================

get_desc <- function(x, name, sample) {
  data.frame(
    Index = name,
    Sample = sample,
    N = sum(!is.na(x)),
    Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE)
  )
}

desc_dec <- get_desc(cg_married$decision_index, "decision_index", "Married sample")
desc_ast <- get_desc(cg_married$asset_index, "asset_index", "Married sample")
desc_wb  <- get_desc(cg_married$wife_beating_index, "wife_beating", "Married sample")

final_table <- dplyr::bind_rows(desc_dec, desc_ast, desc_wb)



# Interpretation rules to include
    # Higher decision index → more autonomy
    # Higher asset index → more economic empowerment
    # Higher wife-beating index → higher acceptance of violence



#==================================================================
# Publication tables
#==================================================================
ft <- flextable(final_table) %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  autofit() %>%
  set_caption("Descriptive statistics for composite indices") %>%
  add_footer_lines(
    values = c(
      "Decision-making index: 0 = no participation, 1 = full participation.",
      "Asset index: 0 = owns none, 1 = owns all assets.",
      "Wife-beating index: 0 = rejects all scenarios, 1 = accepts all scenarios."
    )
  ) %>%
  italic(part = "footer")

ft


#===========================================================================================
# DISAGGREGATION + TABLE 1 PIPELINE (MARITAL SAMPLE)
#===========================================================================================
#------------------------------------------------------
# AGE CLEANING
#------------------------------------------------------
cg_married <- cg_married %>%
  mutate(
    a4iii_cg_1 = na_if(a4iii_cg_1, 997),
    a4iii_cg_1 = na_if(a4iii_cg_1, 998),
    
    age_group = case_when(
      a4iii_cg_1 %in% c(1, 2) ~ "18-30",
      a4iii_cg_1 == 3 ~ "30-39",
      a4iii_cg_1 == 4 ~ "40-49",
      a4iii_cg_1 == 5 ~ "50+",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(age_group))

#------------------------------------------------------
# GENDER CLEANING
#------------------------------------------------------
cg_married <- cg_married %>%
  rename(gender = a4ii_1) %>%
  mutate(
    gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(gender))

#------------------------------------------------------
# RESIDENCE
#------------------------------------------------------
cg_married <- cg_married %>%
  mutate(
    residence = case_when(
      residence_pcg == 1 ~ "Rural",
      residence_pcg == 2 ~ "Urban",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(residence))

#------------------------------------------------------
# FORCE NUMERIC INDICES
#------------------------------------------------------
cg_married <- cg_married %>%
  mutate(
    wife_beating_index = as.numeric(wife_beating_index),
    decision_index     = as.numeric(decision_index),
    asset_index        = as.numeric(asset_index)
  )

#------------------------------------------------------
# OPTIONAL: REMOVE ZERO-VARIANCE GROUP ISSUES SAFELY
#------------------------------------------------------
# cg_married <- cg_married %>%
#   filter(
#     !is.na(wife_beating_index),
#     !is.na(decision_index),
#     !is.na(asset_index)
#   )



#------------------------------------------------------
# TABLE 1: OVERALL
#------------------------------------------------------
tbl_overall <-
  cg_married %>%
  select(wife_beating_index, decision_index, asset_index) %>%
  tbl_summary(
    type = list(
      wife_beating_index ~ "continuous",
      decision_index ~ "continuous",
      asset_index ~ "continuous"
    ),
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = all_continuous() ~ 2,
    missing = "no"
  )

#------------------------------------------------------
# TABLE 1: AGE
#------------------------------------------------------
tbl_age <-
  cg_married %>%
  select(age_group, wife_beating_index, decision_index, asset_index) %>%
  tbl_summary(
    by = age_group,
    type = list(
      wife_beating_index ~ "continuous",
      decision_index ~ "continuous",
      asset_index ~ "continuous"
    ),
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = all_continuous() ~ 2,
    missing = "no"
  )
#------------------------------------------------------
# TABLE 1: GENDER
#------------------------------------------------------
tbl_sex <-
  cg_married %>%
  select(gender, wife_beating_index, decision_index, asset_index) %>%
  tbl_summary(
    by = gender,
    type = list(
      wife_beating_index ~ "continuous",
      decision_index ~ "continuous",
      asset_index ~ "continuous"
    ),
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = all_continuous() ~ 2,
    missing = "no"
  )
#------------------------------------------------------
# TABLE 1: RESIDENCE
#------------------------------------------------------
tbl_res <-
  cg_married %>%
  select(residence, wife_beating_index, decision_index, asset_index) %>%
  tbl_summary(
    by = residence,
    type = list(
      wife_beating_index ~ "continuous",
      decision_index ~ "continuous",
      asset_index ~ "continuous"
    ),
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = all_continuous() ~ 2,
    missing = "no"
  )

#------------------------------------------------------
# FINAL TABLE 1 MERGE
#------------------------------------------------------
table1_indices <- tbl_merge(
  tbls = list(tbl_overall, tbl_age, tbl_sex, tbl_res),
  tab_spanner = c("Overall", "Age (Years)", "Sex", "Residence")
) %>%
  bold_labels() %>%
  modify_caption(
    "**Table 1. Sample characteristics and indices (unweighted)**"
  ) %>%
  modify_footnote(
    all_stat_cols() ~ "Values are mean (SD). N = unweighted sample size."
  )





