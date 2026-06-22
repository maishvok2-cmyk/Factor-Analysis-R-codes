# Clean Up last Session
rm(list = ls())
cat("\014")

# dplyr::glimpse(cg_full)
# table(cg_full$a4ii_1, useNA = "ifany")

# Do not use EFA scores if CFA is your final model.

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


cg_not_married <- cg_dropped %>%
  filter(cg_a4iv %in% c(1, 3, 4))

nrow(cg_not_married)

#==================================================================
# Apply all variable labels
#==================================================================
library(labelled)

var_label(cg_not_married) <- list(
  
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


var_label(cg_not_married$ci_a)
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



#==================================================================
# A) CI → EFA/CFA ->  Rights and privileges of men (PATRIARCHAL NORMS) 
#==================================================================
ci_items <- cg_groups$rights_privileges


weight_var <- "d_pspweight_scaled"

ci_data <- cg_not_married %>%
  dplyr::select(all_of(c(ci_items, weight_var))) %>%
  dplyr::mutate(
    dplyr::across(
      -all_of(weight_var),   # exclude weight from ALL transformations
      ~ {
        x <- as.numeric(.x)
        dplyr::case_when(
          x == 97 ~ NA_real_,   # Don't know → NA
          x == 98 ~ 2,          # Recode 98 → 2
          TRUE ~ x
        )
      }
    )
  )


cat("Items selected:", ncol(ci_data), "\n")


ci_num <- ci_data

ci_ord <- ci_data %>%
  dplyr::mutate(
    dplyr::across(
      -all_of(weight_var),
      ~ factor(.x, levels = c(1, 2, 3), ordered = TRUE)
    ),
    !!weight_var := as.numeric(.data[[weight_var]])
  )

#-----------------------------------------------------------
# Check factorability
#-----------------------------------------------------------

psych::alpha(ci_num)

psych::KMO(ci_num)

psych::cortest.bartlett(ci_num)


#-----------------------------------------------------------
# EFA
#-----------------------------------------------------------
fa.parallel(ci_num, fm = "pa", fa = "fa")

ci_efa <- psych::fa(ci_num, nfactors = 1, fm = "pa", rotate = "promax")

print(ci_efa$loadings, cutoff = 0.4)


#-----------------------------------------------------------
# CFA -> Apply weights
#-----------------------------------------------------------
ci_model <- '
Patriarchal =~ ci_a + ci_b + ci_c + ci_d + ci_e +
               ci_f + ci_g + ci_h + ci_i
'

ci_fit <- lavaan::cfa(
  ci_model,
  data = ci_ord,
  ordered = ci_items,
  estimator = "WLSMV",
  sampling.weights = "d_pspweight_scaled"   # replace with your weight
)

lavaan::fitMeasures(ci_fit, c("cfi","tli","rmsea","srmr"))


#-----------------------------------------------------------
# Factor scores -> Use CFA scores, not EFA, if you ran weighted CFA.
#-----------------------------------------------------------
factor_scores_df <- as.data.frame(lavaan::lavPredict(ci_fit))
colnames(factor_scores_df) <- "Patriarchal"

# #-----------------------------------------------------------
# # Weighted Descriptives for factor
# #-----------------------------------------------------------
# library(survey)
# 
# # Add an ID before any processing.
# # Check uniqueness -> If this returns 0 rows, can be used as join key
# cg_not_married %>%
#   dplyr::count(cg_hh_caregiver_id) %>%
#   dplyr::filter(n > 1)
# 
# 
# design <- svydesign(
#   ids = ~1,
#   weights = ~weight_var,
#   data = cbind(cg_not_married, factor_scores_df)
# )
# 
# svymean(~Patriarchal, design, na.rm = TRUE)
# svyvar(~Patriarchal, design, na.rm = TRUE)

#-----------------------------------------------------------
# Item level descriptives(unweighted)
#-----------------------------------------------------------

ci_descriptives <- data.frame(
  Item = colnames(ci_num),
  N = sapply(ci_num, function(x) sum(!is.na(x))),
  Mean = sapply(ci_num, function(x) mean(x, na.rm = TRUE)),
  SD = sapply(ci_num, function(x) sd(x, na.rm = TRUE)),
  Min = sapply(ci_num, function(x) min(x, na.rm = TRUE)),
  Max = sapply(ci_num, function(x) max(x, na.rm = TRUE)),
  Median = sapply(ci_num, function(x) median(x, na.rm = TRUE))
) %>%
  dplyr::mutate(across(where(is.numeric), ~ round(., 3)))


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


weight_var <- "d_pspweight_scaled"

ciii_data <- cg_not_married %>%
  dplyr::select(all_of(c(ciii_items, weight_var))) %>%
  dplyr::mutate(
    dplyr::across(
      -all_of(weight_var),   # exclude weight from ALL transformations
      ~ {
        x <- as.numeric(.x)
        dplyr::case_when(
          x == 97 ~ NA_real_,   # Don't know → NA
          x == 98 ~ 2,          # Recode 98 → 2
          TRUE ~ x
        )
      }
    )
  )



cat("Items selected:", ncol(ciii_data), "\n")


ciii_num <- ciii_data



ciii_ord <- ciii_data %>%
  dplyr::mutate(
    dplyr::across(
      -all_of(weight_var),
      ~ factor(.x, levels = c(1, 2, 3), ordered = TRUE)
    ),
    !!weight_var := as.numeric(.data[[weight_var]])
  )


#-----------------------------------------------------------
# Check factorability
#-----------------------------------------------------------

psych::alpha(ciii_num)
psych::omega(ciii_num)

psych::KMO(ciii_num)
psych::cortest.bartlett(ciii_num)

#-----------------------------------------------------------
# EFA
#-----------------------------------------------------------
fa.parallel(ciii_num, fm = "pa", fa = "fa")

ciii_efa <- psych::fa(ciii_num, nfactors = 1, fm = "pa", rotate = "promax")

print(ciii_efa$loadings, cutoff = 0.4)





#-----------------------------------------------------------
# CFA -> Weighted
#-----------------------------------------------------------
ciii_model <- '
GenderBeliefs =~ ciii_a + ciii_b + ciii_c + ciii_d + ciii_e +
                 ciii_f + ciii_g + ciii_h + ciii_i + ciii_j +
                 ciii_k + ciii_l + ciii_m + ciii_n + ciii_o + ciii_p
'

ciii_fit <- lavaan::cfa(
  ciii_model,
  data = ciii_ord,
  ordered = ciii_items,
  estimator = "WLSMV",
  sampling.weights = "d_pspweight_scaled"   
)

lavaan::fitMeasures(ciii_fit, c("cfi","tli","rmsea","srmr"))


#-----------------------------------------------------------
# Factor scores
#-----------------------------------------------------------
factor_scores_df2 <- as.data.frame(lavaan::lavPredict(ciii_fit))
colnames(factor_scores_df2) <- "GenderBeliefs"


#-----------------------------------------------------------
# Weighted descriptives
#-----------------------------------------------------------
# library(survey)
# 
# design <- svydesign(
#   ids = ~1,
#   weights = ~weight_var,
#   data = cbind(cg_not_married, factor_scores_df2)
# )
# 
# svymean(~GenderBeliefs, design, na.rm = TRUE)
# svyvar(~GenderBeliefs, design, na.rm = TRUE)


#-----------------------------------------------------------
# Item-Level Descriptives
#-----------------------------------------------------------
ciii_item_desc <- data.frame(
  Item = colnames(ciii_num),
  N = sapply(ciii_num, function(x) sum(!is.na(x))),
  Mean = sapply(ciii_num, function(x) mean(x, na.rm = TRUE)),
  SD = sapply(ciii_num, function(x) sd(x, na.rm = TRUE)),
  Min = sapply(ciii_num, function(x) min(x, na.rm = TRUE)),
  Max = sapply(ciii_num, function(x) max(x, na.rm = TRUE)),
  Median = sapply(ciii_num, function(x) median(x, na.rm = TRUE))
) %>%
  dplyr::mutate(across(where(is.numeric), ~ round(., 3)))




#==========================================================
# CV → EFA/CFA (DOMESTIC ROLES)
#==========================================================
cv_items <- cg_groups$domestic_roles

weight_var <- "d_pspweight_scaled"

cv_data  <- cg_not_married %>%
  dplyr::select(all_of(c(cv_items, weight_var))) %>%
  dplyr::mutate(
    dplyr::across(
      -all_of(weight_var),   # exclude weight from ALL transformations
      ~ {
        x <- as.numeric(.x)
        dplyr::case_when(
          x == 97 ~ NA_real_,   # Don't know → NA
          x == 98 ~ 2,          # Recode 98 → 2
          TRUE ~ x
        )
      }
    )
  )


cat("Items selected:", ncol(ci_data), "\n")

cv_num <- cv_data

cv_ord  <- cv_data %>%
  dplyr::mutate(
    dplyr::across(
      -all_of(weight_var),
      ~ factor(.x, levels = c(1, 2, 3), ordered = TRUE)
    ),
    !!weight_var := as.numeric(.data[[weight_var]])
  )



# Factorability
fa.parallel(cv_num)

psych::alpha(cv_num)
psych::omega(cv_num)



# EFA
cv_efa <- psych::fa(
  cv_num,
  nfactors = 1,
  fm = "pa",
  rotate = "promax"
)

print(cv_efa$loadings, cutoff = 0.4)


# CFA
cv_model <- '
Domestic =~ cv_a + cv_b + cv_c + cv_d + cv_e + cv_f + cv_g
'

cv_fit <- lavaan::cfa(
  cv_model,
  data = cv_ord,
  ordered = cv_items,
  estimator = "WLSMV"
)

lavaan::fitMeasures(cv_fit, c("cfi","tli","rmsea","srmr"))

# Factor Scores
factor_scores_df3 <- as.data.frame(lavaan::lavPredict(cv_fit))
colnames(factor_scores_df3) <- "DomesticRoles"


# Item descriptives
cv_descriptives <- data.frame(
  Item = colnames(cv_num),
  N = sapply(cv_num, function(x) sum(!is.na(x))),
  Mean = sapply(cv_num, function(x) mean(x, na.rm = TRUE)),
  SD = sapply(cv_num, function(x) sd(x, na.rm = TRUE)),
  Min = sapply(cv_num, function(x) min(x, na.rm = TRUE)),
  Max = sapply(cv_num, function(x) max(x, na.rm = TRUE)),
  Median = sapply(cv_num, function(x) median(x, na.rm = TRUE))
) %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))


#==========================================================
# CVI → EFA/CFA (EQUITY & POWER)
#==========================================================

#-----------------------------------------------------------
# I) Equity Items
#-----------------------------------------------------------
cvi_equity <- c("cvi_a","cvi_b","cvi_c","cvi_d")

weight_var <- "d_pspweight_scaled"

eq_data  <- cg_not_married %>%
  dplyr::select(all_of(c(cvi_equity, weight_var))) %>%
  dplyr::mutate(
    dplyr::across(
      -all_of(weight_var),   # exclude weight from ALL transformations
      ~ {
        x <- as.numeric(.x)
        dplyr::case_when(
          x == 97 ~ NA_real_,   # Don't know → NA
          x == 98 ~ 2,          # Recode 98 → 2
          TRUE ~ x
        )
      }
    )
  )



cat("Items selected:", ncol(eq_data), "\n")

eq_num <- eq_data


eq_ord  <- eq_data  %>%
  dplyr::mutate(
    dplyr::across(
      -all_of(weight_var),
      ~ factor(.x, levels = c(1, 2, 3), ordered = TRUE)
    ),
    !!weight_var := as.numeric(.data[[weight_var]])
  )


# Factorability
fa.parallel(eq_num)

psych::alpha(eq_num)
psych::omega(eq_num)


# EFA
eq_efa <- psych::fa(
  eq_num,
  nfactors = 1,
  fm = "pa",
  rotate = "promax"
)

print(eq_efa$loadings, cutoff = 0.4)


# CFA
eq_model <- '
Equity =~ cvi_a + cvi_b + cvi_c + cvi_d
'

eq_fit <- lavaan::cfa(
  eq_model,
  data = eq_ord,
  ordered = cvi_equity,
  estimator = "WLSMV"
)

lavaan::fitMeasures(eq_fit, c("cfi","tli","rmsea","srmr"))

# Equity scores

factor_scores_df4 <- as.data.frame(lavaan::lavPredict(eq_fit))
colnames(factor_scores_df4) <- "Equity"

# Descriptives
eq_descriptives <- data.frame(
  Factor = "Equity",
  N = sum(!is.na(factor_scores_df4$Equity)),
  Mean = mean(factor_scores_df4$Equity, na.rm = TRUE),
  SD = sd(factor_scores_df4$Equity, na.rm = TRUE),
  Min = min(factor_scores_df4$Equity, na.rm = TRUE),
  Max = max(factor_scores_df4$Equity, na.rm = TRUE),
  Median = median(factor_scores_df4$Equity, na.rm = TRUE)
) %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))


#-----------------------------------------------------------
# II. POWER ITEMS
#-----------------------------------------------------------
cvi_power <- c("cvi_e","cvi_f","cvi_g")

weight_var <- "d_pspweight_scaled"

pow_data   <- cg_not_married %>%
  dplyr::select(all_of(c(cvi_power, weight_var))) %>%
  dplyr::mutate(
    dplyr::across(
      -all_of(weight_var),   # exclude weight from ALL transformations
      ~ {
        x <- as.numeric(.x)
        dplyr::case_when(
          x == 97 ~ NA_real_,   # Don't know → NA
          x == 98 ~ 2,          # Recode 98 → 2
          TRUE ~ x
        )
      }
    )
  )



cat("Items selected:", ncol(eq_data), "\n")

pow_num <- pow_data

pow_ord   <- pow_data  %>%
  dplyr::mutate(
    dplyr::across(
      -all_of(weight_var),
      ~ factor(.x, levels = c(1, 2, 3), ordered = TRUE)
    ),
    !!weight_var := as.numeric(.data[[weight_var]])
  )



# RELIABILITY + CORRELATION
psych::alpha(pow_num)

cor(pow_num, use = "pairwise.complete.obs")



#  EFA
fa.parallel(pow_num, fm = "pa", fa = "fa")

pow_efa <- psych::fa(
  pow_num,
  nfactors = 1,
  fm = "pa",
  rotate = "none"
)

print(pow_efa$loadings, cutoff = 0.4)


# CFA
power_model <- '
Power =~ cvi_e + cvi_f + cvi_g
'

pow_fit <- lavaan::cfa(
  power_model,
  data = pow_ord,
  ordered = cvi_power,
  estimator = "WLSMV"
)

lavaan::fitMeasures(pow_fit, c("cfi","tli","rmsea","srmr"))

# Power Scores
pow_scores <- as.data.frame(lavaan::lavPredict(pow_fit))
colnames(pow_scores) <- "Power"

# Descriptives

pow_descriptives <- data.frame(
  Factor = "Power",
  N = sum(!is.na(pow_scores$Power)),
  Mean = mean(pow_scores$Power, na.rm = TRUE),
  SD = sd(pow_scores$Power, na.rm = TRUE),
  Min = min(pow_scores$Power, na.rm = TRUE),
  Max = max(pow_scores$Power, na.rm = TRUE),
  Median = median(pow_scores$Power, na.rm = TRUE)
) %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))


#==========================================================
# CVII → EFA/CFA (RELATIONSHIP CONTROL)
#==========================================================

cvii_items <- cg_groups$relationship_control
weight_var <- "d_pspweight_scaled"


cvii_data    <- cg_not_married %>%
  dplyr::select(all_of(c(cvii_items, weight_var))) %>%
  dplyr::mutate(
    dplyr::across(
      -all_of(weight_var),   # exclude weight from ALL transformations
      ~ {
        x <- as.numeric(.x)
        dplyr::case_when(
          x == 97 ~ NA_real_,   # Don't know → NA
          x == 98 ~ 2,          # Recode 98 → 2
          TRUE ~ x
        )
      }
    )
  )



cat("Items selected:", ncol(cvii_data), "\n")

cvii_num <- cvii_data

cvii_ord     <- cvii_data   %>%
  dplyr::mutate(
    dplyr::across(
      -all_of(weight_var),
      ~ factor(.x, levels = c(1, 2, 3), ordered = TRUE)
    ),
    !!weight_var := as.numeric(.data[[weight_var]])
  )

# EFA + RELIABILITY (UNWEIGHTED)
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

# CFA (WEIGHTED)
cvii_model <- '
Control =~ cvii_a + cvii_b + cvii_c + cvii_d +
           cvii_e + cvii_f + cvii_g + cvii_h + cvii_i
'

cvii_fit <- lavaan::cfa(
  cvii_model,
  data = cvii_ord,
  ordered = cvii_items,
  estimator = "WLSMV",
  sampling.weights = "d_pspweight_scaled"   # replace with your weight variable
)

lavaan::fitMeasures(cvii_fit, c("cfi","tli","rmsea","srmr"))


# FACTOR SCORES (CFA CONSISTENT)
factor_scores_df5 <- as.data.frame(lavaan::lavPredict(cvii_fit))
colnames(factor_scores_df5) <- "RelationshipControl"


# WEIGHTED DESCRIPTIVES (FACTOR LEVEL

# library(survey)
# 
# design_cvii <- svydesign(
#   ids = ~1,
#   weights = ~weight_var,
#   data = cbind(cg_not_married, factor_scores_df5)
# )
# 
# svymean(~RelationshipControl, design_cvii, na.rm = TRUE)
# svyvar(~RelationshipControl, design_cvii, na.rm = TRUE)

# Item Descriptives 

cvii_descriptives <- data.frame(
  Factor = "RelationshipControl",
  N = sum(!is.na(factor_scores_df5$RelationshipControl)),
  Mean = mean(factor_scores_df5$RelationshipControl, na.rm = TRUE),
  SD = sd(factor_scores_df5$RelationshipControl, na.rm = TRUE),
  Min = min(factor_scores_df5$RelationshipControl, na.rm = TRUE),
  Max = max(factor_scores_df5$RelationshipControl, na.rm = TRUE),
  Median = median(factor_scores_df5$RelationshipControl, na.rm = TRUE)
) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))







#==========================================================
# Decision index, Asset Index, Wife Beating
#==========================================================



#==================================================================
# WEIGHT VARIABLE + SURVEY DESIGN
#==================================================================
weight_var <- "d_pspweight_scaled"




design <- svydesign(
  ids = ~1,
  weights = as.formula(paste0("~", weight_var)),
  data = cg_not_married
)

#==================================================================
# INDEX CONSTRUCTION
#==================================================================

#--------------------------------------------------
# Wife beating index
#--------------------------------------------------
civd_items <- cg_groups$wife_beating

# Drop only people with zero information

civd_clean <- cg_not_married %>%
  select(all_of(civd_items)) %>%
  mutate(across(everything(), ~ case_when(
    .x == 1 ~ 1,
    .x == 2 ~ 0,
    TRUE ~ NA_real_
  )))

cg_not_married$wife_beating_index <- rowMeans(civd_clean, na.rm = TRUE)

range(cg_not_married$wife_beating_index, na.rm = TRUE)



type = list(
  wife_beating_index ~ "continuous"
)

nrow(cg_not_married)

# Dropped rows count
sum(is.na(cg_not_married$wife_beating_index))

# how many respondents had no valid answers
civd_clean %>%
  mutate(all_missing = rowSums(!is.na(.)) == 0) %>%
  count(all_missing)

#==================================================================
# CLEAN GROUP VARIABLES
#==================================================================
cg_not_married <- cg_not_married %>%
  mutate(
    age_group = case_when(
      a4iii_cg_1 %in% c(1,2) ~ "18-30",
      a4iii_cg_1 == 3 ~ "30-39",
      a4iii_cg_1 == 4 ~ "40-49",
      a4iii_cg_1 == 5 ~ "50+",
      TRUE ~ NA_character_
    ),
    
    gender = case_when(
      a4ii_1 == 1 ~ "Male",
      a4ii_1 == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    
    residence = case_when(
      residence_pcg == 1 ~ "Rural",
      residence_pcg == 2 ~ "Urban",
      TRUE ~ NA_character_
    )
  )

#==================================================================
# FORCE NUMERIC
#==================================================================
cg_not_married <- cg_not_married %>%
  mutate(
    wife_beating_index = as.numeric(wife_beating_index)
  )

# sapply(cg_not_married[, c("asset_index","decision_index","wife_beating_index")], class)

# update survey design with indices + groups
design <- update(design,
                 wife_beating_index = cg_not_married$wife_beating_index,
                 age_group = cg_not_married$age_group,
                 gender = cg_not_married$gender,
                 residence = cg_not_married$residence
)

# options(gtsummary.default_type = "continuous")

#==================================================================
# WEIGHTED DESCRIPTIVE TABLE FUNCTION
#==================================================================
get_wdesc <- function(design, var, name) {
  
  f <- as.formula(paste0("~", var))
  
  mean_val <- svymean(f, design, na.rm = TRUE)
  var_val  <- svyvar(f, design, na.rm = TRUE)
  
  data.frame(
    Index = name,
    N = sum(!is.na(design$variables[[var]])),
    Mean = as.numeric(coef(mean_val)),
    SD = sqrt(as.numeric(var_val)),
    Min = min(design$variables[[var]], na.rm = TRUE),
    Max = max(design$variables[[var]], na.rm = TRUE)
  )
}

final_table_w <- bind_rows(
  get_wdesc(design, "wife_beating_index", "Wife beating index"),
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))


#==================================================================
# TABLE 1 (WEIGHTED)
#==================================================================
# Weighted N
# tbl_overall <- tbl_svysummary(
#   design,
#   include = c(wife_beating_index),
#   type = list(
#     wife_beating_index ~ "continuous"
#   ),
#   statistic = all_continuous() ~ "{mean} ({sd})",
#   digits = all_continuous() ~ 2,
#   missing = "no"
# )

# BOTH WEIGHTED AND UNWEIGHTED
# tbl_overall <- tbl_svysummary(
#   design,
#   include = wife_beating_index,
#   statistic = all_continuous() ~ "{mean} ({sd})",
#   digits = all_continuous() ~ 2,
#   missing = "no"
# ) %>%
#   add_n()

# Unweighted N

tbl_overall <- tbl_svysummary(
  design,
  include = wife_beating_index,
  statistic = all_continuous() ~ "{mean} ({sd})",
  digits = all_continuous() ~ 2,
  missing = "no"
) %>%
  modify_header(
    all_stat_cols() ~ "**N = {n_unweighted}**"
  )

tbl_age <- tbl_svysummary(
  design,
  by = age_group,
  include = c(wife_beating_index),
  type = list(
    wife_beating_index ~ "continuous"
  ),
  statistic = all_continuous() ~ "{mean} ({sd})",
  digits = all_continuous() ~ 2,
  missing = "no"
)


tbl_sex <- tbl_svysummary(
  design,
  by = gender,
  include = c(wife_beating_index),
  type = list(
    wife_beating_index ~ "continuous"
  ),
  statistic = all_continuous() ~ "{mean} ({sd})",
  digits = all_continuous() ~ 2,
  missing = "no"
)

tbl_res <- tbl_svysummary(
  design,
  by = residence,
  include = c(wife_beating_index),
  type = list(
    wife_beating_index ~ "continuous"
  ),
  statistic = all_continuous() ~ "{mean} ({sd})",
  digits = all_continuous() ~ 2,
  missing = "no"
)


table1_indices <- tbl_merge(
  tbls = list(tbl_overall, tbl_age, tbl_sex, tbl_res),
  tab_spanner = c("Overall (Weighted)", "Age", "Sex", "Residence")
) %>%
  bold_labels() %>%
  modify_caption("Table (Not-married). Weighted sample characteristics and indices") %>%
  modify_footnote(
    all_stat_cols() ~ "Survey-weighted mean (SD)."
  )





# Checks
# sum(!is.na(design$variables$wife_beating_index))
# 
# design$variables %>%
#   dplyr::select(wife_beating_index) %>%
#   tidyr::drop_na() %>%
#   nrow()
# 
# 
# nrow(model.frame(~wife_beating_index, design$variables, na.action = na.omit))




