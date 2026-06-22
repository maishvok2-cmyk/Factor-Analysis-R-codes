# Clean Up last Session
rm(list = ls())
cat("\014")

#==================================================================
# Load Libraries
#==================================================================
library(sjmisc)
library(summarytools)
library(haven)
library(stringr)
library(purrr)
library(openxlsx)
library(tidyverse)
library(GPArotation)
library(flextable)
library(flexlsx)
library(ftExtra)
library(officer)
library(psych)
library(gtsummary)
library(survey)
library(dplyr)
library(lavaan)



#==================================================================
# Read Data
#==================================================================
path <- "C:/Users/Public/Documents/APHRC/K-YAGNS/National Survey/Final Cleaned DataSets/Analyzed Datasets/Weights/K-YAGNS Dyad Analytic.dta"
df <- read_dta(path)

#==================================================================
# Select GN Items
#==================================================================
gn_cols <- names(df) %>% 
  keep(~str_starts(.x, "gn")&
         !str_detect(.x, regex("note", ignore_case = TRUE))&
         !str_detect(.x, "_spy"))

gn_df <- df %>% select(all_of(gn_cols))
cat("Selected items:", ncol(gn_df), "\n")

#==================================================================
# Drop Items
#==================================================================
drop_vars <- c('gn19a', 'gn20a', 'gn21a', 'gn22a', 'gn23a',
               'gn24a', 'gn25a', 'gn26a', 'gn27a', 'gn28a', 'gn46')
gn_dropped <- gn_df %>% select(-any_of(drop_vars))

#==================================================================
 # Refuse to answer check
#==================================================================

# gn_df0 <- gn_df |>
#   dplyr::mutate(
#     any_refused = dplyr::if_any(
#       dplyr::all_of(gn_items),
#       ~ . == 97
#     )
#   )

# refused_df <- gn_analysis |>
#   dplyr::mutate(
#     n_refused = rowSums(dplyr::across(dplyr::everything(), ~ . == 97), na.rm = TRUE)
#   ) |>
#   dplyr::filter(n_refused > 0)



#==================================================================
# Clean Codes
#==================================================================
# gn_df2 <- gn_reversed %>% 
#   mutate(across(everything(), ~ as.numeric(.))) %>% 
#   mutate(across(everything(), ~{
#     x <- .
#     x[x == 97] <- NA
#     x[x == 98] <- 2
#     x
#   }))

gn_cleaned <- gn_dropped %>% 
  mutate(across(everything(), ~ as.numeric(.))) %>% 
  mutate(across(everything(), ~{
    x <- .
    x[x == 97] <- NA
    x[x == 98] <- 2
    x
  }))

#==================================================================
# Convert GN items to ordered factors (Ordinal imputation)
#==================================================================

gn_ordered <- gn_cleaned %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ factor(.x, levels = c(1, 2, 3), ordered = TRUE)
    )
  )

# for (v in gn_df) {
#   gn_df[[v]] <- factor(
#     gn_df[[v]],
#     levels = c(1, 2, 3),
#     ordered = TRUE
#   )
# }


#==================================================================
# Reverse Code
#==================================================================
reverse_items <- c('gn2', 'gn4', 'gn6', 'gn9', 'gn17', 'gn34', 'gn46', 'gn48')

gn_reversed <- gn_ordered %>%
  mutate(across(any_of(reverse_items), ~ ifelse(!is.na(.), 
                                                4 - ., NA)))


# library(dplyr)
# 
# # Step 1: convert ordered factors → numeric (preserve values 1,2,3)
# gn_numeric <- gn_ordered %>%
#   mutate(across(all_of(reverse_items), ~ as.numeric(as.character(.))))
# 
# # Step 2: reverse code (1↔3, 2 stays)
# gn_reversed <- gn_numeric %>%
#   mutate(across(all_of(reverse_items), ~ ifelse(!is.na(.), 4 - ., NA)))





#==================================================================
# Filter Items
#==================================================================
# Missingness
valid_pct <- colMeans(!is.na(gn_reversed))
gn_df4 <- gn_reversed[, valid_pct >= 0.70]

# Variance
gn_df4 <- gn_df4[, apply(gn_df4, 2, sd, na.rm = TRUE) >0.01]

cat("Remaining items:", ncol(gn_df4), "\n")  

#==================================================================
# IMpute (Median)
#==================================================================
gn_analysis <- gn_df4
gn_analysis[] <- lapply(gn_analysis, function(x) {
  x[is.na(x)] <- median(x, na.rm = TRUE)
  x
})

# gn_analysis <- gn_df4 %>% drop_na() 
# 
dim(gn_analysis)


#------------------------------------------------------------------------------
# 4. item text labels
#------------------------------------------------------------------------------

gns <- c(
  gn1   = "A girl will lose interest in studying if she has a boyfriend",
  gn2   = "A boy and a girl your age should be able to spend time together alone if they want to",
  gn3   = "Girls your age often get into trouble when they have boyfriends",
  gn4   = "A boy your age should be able to have a girlfriend if he wants to",
  gn5   = "Boys have girlfriends for fun more than love",
  gn6   = "It is normal for a boy your age to want a girlfriend",
  gn7   = "Girls who have boyfriends are irresponsible",
  gn8   = "Boys like girls who wear revealing clothes",
  gn9   = "A girl should be able to have a boyfriend if she wants to",
  gn10  = "Girls are gossiped about if they have boyfriends",
  gn11  = "Boys tell girls they love them when they do not",
  gn12  = "Adolescent girls should avoid boys because they trick them into having sex",
  gn13  = "Boys have girlfriends to show off to their friends",
  gn14  = "Boys generally compete for the prettiest girls",
  gn15  = "Boys feel they should have girlfriends because their friends do",
  gn16  = "Adolescent boys lose interest in a girl after they have sex with her",
  gn17  = "It is normal for a girl to want a boyfriend at your age",
  gn18  = "Adolescent boys fool girls into having sex",
  gn19b = "A woman should not work outside the home to keep peace in her marriage",
  gn20b = "Only men should make decisions about income and expenses",
  gn21b = "Men should make the final decision about their wife using family planning",
  gn22b = "Husbands should make the final decision about how many children to have",
  gn23b = "If a woman disobeys her husband, she should be sent back to her parents or sent away",
  gn24b = "Only women should do the cooking, cleaning, and caring of children",
  gn25b = "Women should stop working when they get married",
  gn26b = "Girls should stop going to school if they get pregnant",
  gn27b = "Husbands should make the final decisions about buying major household items",
  gn28b = "If there is only enough money for one cell phone for the household, the husband should own it",
  gn29  = "Boys should be raised tough so they can overcome any difficulty in life",
  gn30  = "Girls should avoid raising their voice to be lady like",
  gn31  = "Boys should always defend themselves even if it means fighting",
  gn32  = "Girls are expected to be humble",
  gn33  = "Girls need their parents protection more than boys",
  gn34  = "Boys should be able to show their feelings without fear of being teased",
  gn35  = "Boys who behave like girls are considered weak",
  gn36  = "A boy should always have the final say about decisions with his girlfriend",
  gn37  = "It is important for boys to show they are tough even if they are nervous inside",
  gn38  = "It is okay for an adolescent girl to have sex as long as she avoids getting pregnant",
  gn39  = "In general, a girl should only have sex with someone she loves",
  gn40  = "Most of the time, if an adolescent girl says no to sex her boyfriend will dump her",
  gn41  = "It is okay for an adolescent boy to have sex as long as he avoids getting a girl pregnant",
  gn42  = "In general, a boy should only have sex with someone he loves",
  gn43  = "It is okay to tease a girl who acts like a boy",
  gn44  = "It is the girl’s responsibility to prevent pregnancy",
  gn45  = "Girls who carry condoms on them are loose",
  gn46  = "Girls should be proud of their bodies as they become women",
  gn47  = "It is okay to tease a boy who acts like a girl",
  gn48  = "Boys and girls should be equally responsible for household chores",
  gn49  = "A woman’s role is taking care of her home and family",
  gn50  = "A man should have the final word about decisions in the home",
  gn51  = "A woman should obey her husband in all matters",
  gn52  = "A real man should have as many female partners as he can",
  gn53  = "Men are always ready for sex",
  gn54  = "Men should be the ones who bring money home for the family, not women"
)

variables <- tibble::tibble(
  varname = names(gns),
  variable_label = unname(gns)
)

#------------------------------------------------------------------------------
# 5. Polychoric correlation matrix
#------------------------------------------------------------------------------
# This is the key step for ordered/ordinal items
poly_out <- psych::polychoric(gn_analysis)

R_poly <- poly_out$rhoR_poly <- poly_out$rhoR_poly <- poly_out$rho

# Check matrix dimension
dim(R_poly)

#------------------------------------------------------------------------------
# 6. Smooth the matrix if needed
#------------------------------------------------------------------------------
# This helps if the polychoric matrix is not positive definite
R_poly_smooth <- psych::cor.smooth(R_poly)

#------------------------------------------------------------------------------
# 7. Bartlett test and KMO
#------------------------------------------------------------------------------
bart_out <- psych::cortest.bartlett(
  R = R_poly_smooth,
  n = nrow(gn_analysis)
)
bart_out

kmo_out <- psych::KMO(R_poly_smooth)
kmo_out

# Item-level KMO
kmo_items <- tibble::tibble(
  varname = names(kmo_out$MSAi),
  MSAi = as.numeric(kmo_out$MSAi)
)

kmo_items



#------------------------------------------------------------------------------
# 8. Parallel analysis
#------------------------------------------------------------------------------
# Use this plot and output to guide the number of factors
psych::fa.parallel(
  R_poly_smooth,
  n.obs = nrow(gn_analysis),
  fa = "fa",
  fm = "pa"
)

#------------------------------------------------------------------------------
# 9. Fit candidate EFA models
#    Compare 4-, 5-, and 6-factor solutions if needed
#------------------------------------------------------------------------------
# efa4_promax <- psych::fa(
#   r = R_poly_smooth,
#   nfactors = 4,
#   n.obs = nrow(gn_analysis),
#   rotate = "promax",
#   fm = "pa",
#   SMC = TRUE
# )

efa4_promax <- psych::fa(
  r = R_poly_smooth,
  nfactors = 4,
  n.obs = nrow(gn_analysis),
  rotate = "promax",
  fm = "pa",
  SMC = TRUE
)

# efa4_promax <- psych::fa(
#   r = R_poly_smooth,
#   nfactors = 6,
#   n.obs = nrow(gn_analysis),
#   rotate = "promax",
#   fm = "pa",
#   SMC = TRUE
# )

efa4_promax
#------------------------------------------------------------------------------
# 10. Inspect the chosen solution
#     We can change here if we prefer efa4_promax or efa6_promax
#------------------------------------------------------------------------------
# efa_final <- efa5_promax
# efa_final2 <- efa6_promax
ef_final3 <- efa4_promax

print(ef_final3$loadings, cutoff = 0.40, digits = 2)

# Pattern matrix as data frame
loadings_df <- as.data.frame(unclass(ef_final3$loadings)) |>
  tibble::rownames_to_column("varname")

loadings_df

#------------------------------------------------------------------------------
# 11. Factor correlations
#------------------------------------------------------------------------------
ef_final3$Phi

#------------------------------------------------------------------------------
# 12. Item-level KMO and diagnostics
#------------------------------------------------------------------------------
kmo_items <- tibble::tibble(
  varname = loadings_df$varname,
  MSAi = as.numeric(kmo_out$MSAi[loadings_df$varname])
)

item_diagnostics <- tibble::tibble(
  varname = loadings_df$varname,
  h2 = as.numeric(ef_final3$communality[loadings_df$varname]),
  u2 = as.numeric(ef_final3$uniquenesses[loadings_df$varname]),
  com = as.numeric(ef_final3$complexity[loadings_df$varname])
)

kmo_items
item_diagnostics


#------------------------------------------------------------------------------
# 13. Join loadings + labels + KMO + diagnostics
#------------------------------------------------------------------------------
efa_table <- loadings_df |>
  dplyr::left_join(variables, by = "varname") |>
  dplyr::left_join(kmo_items, by = "varname") |>
  dplyr::left_join(item_diagnostics, by = "varname") |>
  dplyr::relocate(variable_label, .before = varname)

efa_table

#------------------------------------------------------------------------------
# 14. Sort factors for easier reading
#------------------------------------------------------------------------------
efa_sorted_loadings <- as.data.frame(unclass(psych::fa.sort(ef_final3$loadings))) |>
  tibble::rownames_to_column("varname")

efa_table_sorted <- efa_sorted_loadings |>
  dplyr::left_join(variables, by = "varname") |>
  dplyr::left_join(kmo_items, by = "varname") |>
  dplyr::left_join(item_diagnostics, by = "varname") |>
  dplyr::relocate(variable_label, .before = varname)

efa_table_sorted

#------------------------------------------------------------------------------
# 15. Flag potentially problematic items
#------------------------------------------------------------------------------
# Rules of thumb:
# - low communality: h2 < 0.30
# - high uniqueness: u2 > 0.70
# - high complexity: com > 1.50
problem_items <- efa_table_sorted |>
  dplyr::mutate(
    low_h2   = h2 < 0.30,
    high_u2  = u2 > 0.70,
    high_com = com > 1.50
  ) |>
  dplyr::filter(low_h2 | high_u2 | high_com)

problem_items

#------------------------------------------------------------------------------
# 16. Create a cleaner loading table with rounded values
#------------------------------------------------------------------------------
efa_table_display <- efa_table_sorted |>
  dplyr::mutate(
    dplyr::across(where(is.numeric), ~ round(., 3))
  )

efa_table_display

#------------------------------------------------------------------------------
# 17. Flextable output
#------------------------------------------------------------------------------
ft_efa <- efa_table_display |>
  flextable::flextable() |>
  flextable::theme_vanilla() |>
  flextable::set_caption(
    caption = "Exploratory factor analysis factor loading matrix for gender norms and SRH-related items (polychoric correlations, promax rotation)",
    align_with_table = FALSE
  ) |>
  flextable::bold(part = "header", bold = TRUE) |>
  flextable::line_spacing(space = 0.9, part = "body") |>
  flextable::fontsize(part = "footer", size = 9) |>
  flextable::add_footer_lines(
    values = c(
      "Items were treated as ordered/ordinal and analyzed using polychoric correlations.",
      "Promax rotation was used to allow factors to correlate.",
      "Extraction method: principal axis factoring (fm = 'pa').",
      "h2 = communality; u2 = uniqueness; com = item complexity; MSAi = item-level KMO."
    )
  ) |>
  flextable::autofit()

ft_efa

#------------------------------------------------------------------------------
# 18. Factor diagram
#------------------------------------------------------------------------------
psych::fa.diagram(ef_final3)

#------------------------------------------------------------------------------
# 19. Assign each item to its strongest factor
#------------------------------------------------------------------------------
loading_only <- as.data.frame(unclass(ef_final3$loadings))
loading_only$varname <- rownames(loading_only)

factor_names <- setdiff(names(loading_only), "varname")

item_assignment <- loading_only |>
  dplyr::rowwise() |>
  dplyr::mutate(
    strongest_factor = factor_names[which.max(abs(c_across(dplyr::all_of(factor_names))))],
    strongest_loading = max(abs(c_across(dplyr::all_of(factor_names))), na.rm = TRUE)
  ) |>
  dplyr::ungroup() |>
  dplyr::left_join(variables, by = "varname") |>
  dplyr::arrange(strongest_factor, dplyr::desc(strongest_loading))

item_assignment

#------------------------------------------------------------------------------
# 20. Keep only reasonably strong primary-loading items
#------------------------------------------------------------------------------
primary_items <- item_assignment |>
  dplyr::filter(strongest_loading >= 0.40)

primary_items

#------------------------------------------------------------------------------
# 21. list items by factor
#------------------------------------------------------------------------------
items_by_factor <- split(primary_items$varname, primary_items$strongest_factor)
items_by_factor

# Convert list
items_by_factor_df <- imap_dfr(items_by_factor, ~ 
                                 tibble(
                                   Factor = .y,
                                   Item = .x
                                 ))


# Create flextable
items_by_factor_table <- flextable(items_by_factor_df) %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  autofit() %>%
  set_caption("Item Assignment to Factors") %>%
  add_footer_lines(
    values = c(
      "Note. Items are assigned based on highest absolute factor loading.",
      "Cross-loading items have been reviewed for conceptual fit."
    )
  ) %>%
  italic(part = "footer")

items_by_factor_table

#------------------------------------------------------------------------------
# 22. alpha by factor
#------------------------------------------------------------------------------
alpha_results <- lapply(items_by_factor, function(vars) {
  dat_tmp <- gn_analysis |>
    dplyr::select(dplyr::all_of(vars)) |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ dplyr::na_if(., 97)),
      dplyr::across(dplyr::everything(), ~ dplyr::na_if(., 98))
    )
  
  psych::alpha(dat_tmp)
})

factor_alphas <- lapply(alpha_results, function(x) x$total)

# Convert to a data frame

factor_alphas_df <- factor_alphas %>%
  imap_dfr(~ as.data.frame(.x) %>%
             rownames_to_column("metric") %>%
             mutate(Factor = .y))

# Clean structure
factor_alphas_df <- factor_alphas_df %>%
  select(Factor, everything())


# Create flextable
factor_alphas_table <- flextable(factor_alphas_df) %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  autofit() %>%
  set_caption("Internal Consistency Statistics by Factor") %>%
  add_footer_lines(
    values = c(
      "Note. Cronbach’s alpha ≥ 0.70 indicates acceptable reliability.",
      "Values between 0.60 and 0.69 indicate marginal reliability.",
      "Values < 0.60 indicate low internal consistency."
    )
  ) %>%
  italic(part = "footer")

factor_alphas_table

#------------------------------------------------------------------------------
# 23. omega by factor
#------------------------------------------------------------------------------
omega_results <- lapply(items_by_factor, function(vars) {
  dat_tmp <- gn_analysis |>
    dplyr::select(dplyr::all_of(vars)) |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ dplyr::na_if(., 97)),
      dplyr::across(dplyr::everything(), ~ dplyr::na_if(., 98))
    )
  
  psych::omega(dat_tmp, plot = FALSE)
})

#------------------------------------------------------------------------------
# 24. Save outputs
#------------------------------------------------------------------------------
saveRDS(ef_final3, "efa_final_gn_polychoric_promax.rds")
saveRDS(efa_table_display, "efa_table_gn_polychoric_promax.rds")
saveRDS(problem_items, "efa_problem_items_gn.rds")
saveRDS(item_assignment, "efa_item_assignment_gn.rds")

#------------------------------------------------------------------------------
# 25. export to CSV
#------------------------------------------------------------------------------
write.csv(efa_table_display, "efa_table_gn_polychoric_promax.csv", row.names = FALSE)
write.csv(problem_items, "efa_problem_items_gn.csv", row.names = FALSE)
write.csv(item_assignment, "efa_item_assignment_gn.csv", row.names = FALSE)


#==================================================================
# 26. FACTOR SCORE CONSTRUCTION
#==================================================================

#----------------------------------------------------------
# FACTOR SCORES (ROW MEAN METHOD)
#----------------------------------------------------------

factor_scores <- lapply(items_by_factor, function(vars) {
  
  gn_analysis %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    mutate(across(everything(), ~ as.numeric(.))) %>%
    rowMeans(na.rm = TRUE)
})

factor_scores <- as.data.frame(factor_scores)

colnames(factor_scores) <- paste0("Factor", seq_along(factor_scores), "_Score")

#----------------------------------------------------------
# MERGE SAFELY WITH ORIGINAL DATA
#----------------------------------------------------------

df_with_scores <- dplyr::bind_cols(
  df,
  factor_scores
)




#==================================================================
# 27. DESCRIPTIVES + ITEM COUNTS
#==================================================================

# Count items per factor
item_counts <- sapply(items_by_factor, length)

item_counts_df <- data.frame(
  Factor = paste0("F", seq_along(item_counts)),
  Items = item_counts
)

# Descriptives
factor_descriptives <- data.frame(
  Factor = colnames(factor_scores),
  N = sapply(factor_scores, function(x) sum(!is.na(x))),
  Mean = sapply(factor_scores, mean, na.rm = TRUE),
  SD = sapply(factor_scores, sd, na.rm = TRUE),
  Min = sapply(factor_scores, min, na.rm = TRUE),
  Max = sapply(factor_scores, max, na.rm = TRUE),
  Median = sapply(factor_scores, median, na.rm = TRUE)
)

factor_descriptives <- factor_descriptives %>%
  mutate(across(where(is.numeric), ~ round(., 3)))


#==================================================================
# 28. ALPHA 
#==================================================================

alpha_only <- lapply(alpha_results, function(x) x$total$raw_alpha) %>%
  unlist()

alpha_df <- data.frame(
  Factor = paste0("F", seq_along(alpha_only)),
  Alpha = round(alpha_only, 3)
)


#==================================================================
# 29. MERGED TABLE
#==================================================================


factor_summary <- factor_descriptives %>%
  # Standardize key to match item_counts_df and alpha_df
  mutate(Factor = paste0("F", seq_len(n()))) %>%
  left_join(item_counts_df, by = "Factor") %>%
  left_join(alpha_df, by = "Factor") %>%
  select(Factor, Items, N, Mean, SD, Median, Min, Max, Alpha)

factor_summary




#==================================================================
# 30. PUBLICATION TABLE
#==================================================================

factor_desc_table <- flextable(factor_summary) %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  autofit() %>%
  set_caption("Descriptive Statistics and Reliability of Derived Factors") %>%
  add_footer_lines(
    values = c(
      "Note. Factor scores computed as mean of constituent items.",
      "Items = number of items loading on each factor.",
      "Cronbach’s alpha ≥ 0.60 indicates acceptable reliability."
    )
  ) %>%
  italic(part = "footer")

factor_desc_table


#==================================================================
# 31. EXPORT TO EXCEL
#==================================================================

outfile_desc <- "C:/Users/Public/Documents/APHRC/K-YAGNS/Factor_Descriptives.xlsx"

wb_desc <- createWorkbook()

addWorksheet(wb_desc, "Factor_Descriptives")
writeData(wb_desc, "Factor_Descriptives", factor_summary)

saveWorkbook(wb_desc, outfile_desc, overwrite = TRUE)

cat("\nFactor descriptives exported to:\n", outfile_desc, "\n")

#==========================================================
# CONFIRMATORY FACTOR ANALYSIS
#==========================================================

cfa_data <- gn_analysis



build_cfa_model <- function(items_by_factor) {
  
  lines <- c()
  
  for (i in seq_along(items_by_factor)) {
    
    factor_name <- paste0("F", i)
    items <- items_by_factor[[i]]
    
    if (length(items) >= 2) {
      line <- paste0(factor_name, " =~ ", paste(items, collapse = " + "))
      lines <- c(lines, line)
    }
  }
  
  model <- paste(lines, collapse = "\n")
  return(model)
}


cfa_model <- build_cfa_model(items_by_factor)
cat(cfa_model)




fit <- cfa(
  cfa_model,
  data = gn_analysis,
  ordered = names(gn_analysis),   # tells lavaan items are ordinal
  estimator = "WLSMV"
)


#==========================================================
## EXTRACT FIT INDICES
#==========================================================
lavaan::fitMeasures(fit, c("cfi","tli","rmsea","srmr"))

#==========================================================
# Standardized Loading check
#==========================================================
summary(fit, standardized = TRUE)


#==========================================================
# Factor Correlation
#==========================================================
inspect(fit, "std")$psi


#==========================================================
# CHECK MODIFICATION INDICES
#==========================================================
modindices(fit, sort = TRUE, minimum.value = 10)


#==========================================================
#  Final Report
#==========================================================
standardizedSolution(fit)


#==========================================================
#  PUBLICATION TABLES
#==========================================================

#----------------------------------------------------------
# CFA Model Fit Table (Main Table)
#----------------------------------------------------------

apa_cfa_fit <- data.frame(
  Model = "CFA model",
  CFI = round(fitMeasures(fit, "cfi"), 3),
  TLI = round(fitMeasures(fit, "tli"), 3),
  RMSEA = round(fitMeasures(fit, "rmsea"), 3),
  SRMR = round(fitMeasures(fit, "srmr"), 3)
)

apa_cfa_fit


epa_fit2 <- flextable(apa_cfa_fit) %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  autofit() %>%
  set_caption(
    "Confirmatory factor analysis model fit indices for competing factor solutions (WLSMV estimator)"
  ) %>%
  add_footer_lines(
    values = c(
      "Note. CFI and TLI ≥ 0.90 indicate acceptable fit and ≥ 0.95 indicate good fit.",
      "RMSEA ≤ 0.08 and SRMR ≤ 0.08 indicate acceptable model fit.",
      "Estimation used WLSMV due to ordinal indicators."
    )
  ) %>%
  italic(part = "footer")

epa_fit2

#----------------------------------------------------------
# Standardized factor loading table
#----------------------------------------------------------

apa_loadings <- standardizedSolution(fit) %>%
  filter(op == "=~") %>%
  mutate(
    est.std = round(est.std, 3),
    pvalue = ifelse(pvalue < 0.001, "< .001", round(pvalue, 3))
  ) %>%
  select(Factor = lhs, Item = rhs, Loading = est.std, p = pvalue)

apa_loadings

apa_loadings_table <- flextable(apa_loadings) %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  autofit() %>%
  set_caption(
    "Standardized Factor Loadings (CFA)"
  ) %>%
  add_footer_lines(
    values = c(
      "Note. Standardized loadings reflect the strength of the relationship between observed items and latent constructs.",
      "Loadings ≥ 0.70 indicate strong indicators.",
      "Loadings between 0.40 and 0.69 indicate acceptable indicators.",
      "NB: Accepted minimum threshold equals to 0.40.",
      "p < .001 means strong evidence the loading is not zero."
    )
  ) %>%
  italic(part = "footer")

apa_loadings_table

#----------------------------------------------------------
# Factor correlation table
#----------------------------------------------------------
apa_cor <- as.data.frame(inspect(fit, "std")$psi)
apa_cor

apa_cor_table <- flextable(apa_cor) %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  autofit() %>%
  set_caption("Factor Correlations") %>%
  add_footer_lines(
    values = c(
      "Interpretation guide:",
      "Correlations < 0.30 indicate weak relationships.",
      "0.30 to 0.70 indicate moderate relationships.",
      "> 0.70 suggests construct overlap and weak discriminant validity."
    )
  ) %>%
  italic(part = "footer")

apa_cor_table




#==========================================================
# EXPORT ALL OUTPUTS TO EXCEL (MULTIPLE SHEETS)
#==========================================================

outfile <- "C:/Users/Public/Documents/APHRC/K-YAGNS/National Survey/Final Cleaned DataSets/Summaries/Factor Analysis/Full_EFA_CFA_Output.xlsx"

wb <- createWorkbook()

#----------------------------------------------------------
# 1. EFA LOADINGS TABLE
#----------------------------------------------------------
addWorksheet(wb, "EFA_Loadings")
writeData(wb, "EFA_Loadings", efa_table_display)

#----------------------------------------------------------
# 2. SORTED LOADINGS
#----------------------------------------------------------
addWorksheet(wb, "EFA_Sorted_Loadings")
writeData(wb, "EFA_Sorted_Loadings", efa_table_sorted)

#----------------------------------------------------------
# 3. PROBLEM ITEMS
#----------------------------------------------------------
addWorksheet(wb, "Problem_Items")
writeData(wb, "Problem_Items", problem_items)

#----------------------------------------------------------
# 4. ITEM ASSIGNMENT
#----------------------------------------------------------
addWorksheet(wb, "Item_Assignment")
writeData(wb, "Item_Assignment", item_assignment)

#----------------------------------------------------------
# 5. PRIMARY ITEMS ONLY
#----------------------------------------------------------
addWorksheet(wb, "Primary_Items")
writeData(wb, "Primary_Items", primary_items)

#----------------------------------------------------------
# 6. ITEMS BY FACTOR
#----------------------------------------------------------
addWorksheet(wb, "Items_by_Factor")
writeData(wb, "Items_by_Factor", items_by_factor_df)

#----------------------------------------------------------
# 7. FACTOR DESCRIPTIVES + ALPHA
#----------------------------------------------------------
addWorksheet(wb, "Factor_Descriptives")
writeData(wb, "Factor_Descriptives", factor_summary)

#----------------------------------------------------------
# 8. FACTOR SCORES
#----------------------------------------------------------
addWorksheet(wb, "Factor_Scores")
writeData(wb, "Factor_Scores", df_with_scores)

#----------------------------------------------------------
# 9. ALPHA TABLE (RAW OUTPUT)
#----------------------------------------------------------
addWorksheet(wb, "Alpha_Details")
writeData(wb, "Alpha_Details", factor_alphas_df)

#----------------------------------------------------------
# 10. OMEGA (SUMMARY ONLY)
#----------------------------------------------------------
omega_summary <- map_dfr(omega_results, function(x) {
  data.frame(
    omega_total = x$omega.tot,
    omega_h = x$omega_h
  )
}, .id = "Factor")

addWorksheet(wb, "Omega")
writeData(wb, "Omega", omega_summary)

#----------------------------------------------------------
# 11. CFA FIT INDICES
#----------------------------------------------------------
addWorksheet(wb, "CFA_Fit")
writeData(wb, "CFA_Fit", apa_cfa_fit)

#----------------------------------------------------------
# 12. CFA LOADINGS
#----------------------------------------------------------
addWorksheet(wb, "CFA_Loadings")
writeData(wb, "CFA_Loadings", apa_loadings)

#----------------------------------------------------------
# 13. CFA FACTOR CORRELATIONS
#----------------------------------------------------------
addWorksheet(wb, "CFA_Correlations")
writeData(wb, "CFA_Correlations", apa_cor)

#----------------------------------------------------------
# 14. MODIFICATION INDICES
#----------------------------------------------------------
mi <- modindices(fit, sort = TRUE, minimum.value = 10)

addWorksheet(wb, "CFA_ModIndices")
writeData(wb, "CFA_ModIndices", mi)

#----------------------------------------------------------
# 15. KMO (ITEM LEVEL)
#----------------------------------------------------------
addWorksheet(wb, "KMO_Items")
writeData(wb, "KMO_Items", kmo_items)

#----------------------------------------------------------
# 16. BARTLETT TEST
#----------------------------------------------------------
bart_df <- data.frame(
  chisq = bart_out$chisq,
  df = bart_out$df,
  p_value = bart_out$p.value
)

addWorksheet(wb, "Bartlett_Test")
writeData(wb, "Bartlett_Test", bart_df)

#----------------------------------------------------------
# SAVE WORKBOOK
#----------------------------------------------------------
saveWorkbook(wb, outfile, overwrite = TRUE)

cat("\nAll outputs exported to:\n", outfile, "\n")














#==========================================================================
# BASED ON THE ORIGINAL EFA PROCESS NOT THE THEORETICAL APPROACH
#==========================================================================

#==========================================================
# DISAGGREGATION: AGE + SEX + RESIDENCE (UNWEIGHTED)
#==========================================================

#==========================================================
# 1. CREATE VARIABLES
#==========================================================
df_with_scores <- df_with_scores %>%
  mutate(
    age_group = case_when(
      d_adol_age %in% c(10, 11, 12) ~ "10–12",
      d_adol_age %in% c(13, 14) ~ "13–14",
      TRUE ~ NA_character_
    ),
    sex_label = case_when(
      as.numeric(d_adolsex) == 1 ~ "Boy",
      as.numeric(d_adolsex) == 2 ~ "Girl",
      TRUE ~ NA_character_
    ),
    residence = case_when(
      residence_vya == 1 ~ "Rural",
      residence_vya == 2 ~ "Urban",
      TRUE ~ NA_character_
    )
  )

#==========================================================
# 2. FACTOR VARIABLES
#==========================================================
factor_vars <- grep("^Factor", names(df_with_scores), value = TRUE)

#==========================================================
# 3. FUNCTION: GET N
#==========================================================
get_n_unw <- function(data) {
  paste0("N = ", nrow(data))
}

get_n_group <- function(data, var) {
  data %>%
    filter(!is.na(.data[[var]])) %>%
    nrow() %>%
    paste0("N = ", .)
}

#==========================================================
# 4. BASE TABLE FUNCTION
#==========================================================
make_tbl_unw <- function(data, by_var = NULL, label = "") {
  
  if (is.null(by_var)) {
    
    tbl <- data %>%
      tbl_summary(
        include = all_of(factor_vars),
        statistic = all_continuous() ~ "{mean} ({sd})",
        missing = "no"
      )
    
  } else {
    
    tbl <- data %>%
      tbl_summary(
        include = all_of(factor_vars),
        by = !!rlang::sym(by_var),   # key fix
        statistic = all_continuous() ~ "{mean} ({sd})",
        missing = "no"
      )
  }
  
  tbl %>%
    modify_header(label = paste0("**", label, "**"))
}
#==========================================================
# 5. BUILD TABLE BLOCKS
#==========================================================

tbl_overall <- make_tbl_unw(
  df_with_scores,
  NULL,
  paste0("Overall\n", get_n_unw(df_with_scores))
)

tbl_age <- make_tbl_unw(
  df_with_scores,
  "age_group",
  paste0("Age (Years)\n", get_n_group(df_with_scores, "age_group"))
)

tbl_sex <- make_tbl_unw(
  df_with_scores,
  "sex_label",
  paste0("Sex\n", get_n_group(df_with_scores, "sex_label"))
)

tbl_res <- make_tbl_unw(
  df_with_scores,
  "residence",
  paste0("Residence\n", get_n_group(df_with_scores, "residence"))
)

#==========================================================
# 6. MERGE INTO FINAL TABLE
#==========================================================
table1_unweighted <- tbl_merge(
  tbls = list(tbl_overall, tbl_age, tbl_sex, tbl_res),
  tab_spanner = c("Overall", "Age (Years)", "Sex", "Residence")
) %>%
  
  bold_labels() %>%
  
  modify_caption(
    "**Table 1. Sample characteristics and factor scores (unweighted)**"
  ) %>%
  
  modify_footnote(
    all_stat_cols() ~
      "Values are mean (SD). N = unweighted sample size."
  )

table1_unweighted


# Standard Deviation:-> It shows how spread out the data is around the mean.
    # Helps judge consistency within a group
    # Two groups can have the same mean but different SD
    # Lower SD often means more agreement among respondents





#==================================================================
# THEORETICAL SPECIFICATION OF FACTORS
#==================================================================
items_forced <- list(
  
  F1 = c("gn19b","gn20b","gn22b","gn23b","gn24b",
         "gn25b","gn27b","gn28b","gn49","gn50","gn51","gn54"),
  
  F2 = c("gn18","gn14","gn13","gn11","gn12","gn10","gn15",
         "gn3","gn7","gn8","gn1","gn5","gn52","gn53", "gn26b"),
  
  F3 = c("gn6","gn4","gn9","gn17","gn2"),
  
  F4 = c("gn37","gn31","gn35","gn30","gn29","gn32","gn33"),
  
  F5 = c("gn39","gn42","gn41","gn38","gn36","gn43")
)


#----------------------------------------------------------
# Ensure items exist
#----------------------------------------------------------
all_items <- unlist(items_forced)

missing_items <- setdiff(all_items, names(gn_analysis))
missing_items


#----------------------------------------------------------
# Build CFA model
#----------------------------------------------------------

build_forced_model <- function(items_list) {
  
  lines <- c()
  
  for (i in seq_along(items_list)) {
    
    factor_name <- names(items_list)[i]
    items <- items_list[[i]]
    
    line <- paste0(factor_name, " =~ ", paste(items, collapse = " + "))
    lines <- c(lines, line)
  }
  
  paste(lines, collapse = "\n")
}

cfa_model_forced <- build_forced_model(items_forced)

cat(cfa_model_forced)

#----------------------------------------------------------
# Run CFA (ORDINAL)
#----------------------------------------------------------
fit_forced <- lavaan::cfa(
  model = cfa_model_forced,
  data = gn_analysis,
  ordered = names(gn_analysis),
  estimator = "WLSMV",
  std.lv = TRUE
)

#----------------------------------------------------------
# MODEL FIT
#----------------------------------------------------------

fit_indices <- lavaan::fitMeasures(
  fit_forced,
  c("cfi","tli","rmsea","srmr")
)

fit_indices


#----------------------------------------------------------
# Standardized loadings
#----------------------------------------------------------
cfa_loadings <- lavaan::standardizedSolution(fit_forced) %>%
  dplyr::filter(op == "=~") %>%
  dplyr::mutate(
    est.std = round(est.std, 3),
    pvalue = ifelse(pvalue < 0.001, "< .001", round(pvalue, 3))
  ) %>%
  dplyr::select(
    Factor = lhs,
    Item = rhs,
    Loading = est.std,
    p = pvalue
  )

cfa_loadings


#----------------------------------------------------------
# Flag weak items
#----------------------------------------------------------
weak_items <- cfa_loadings %>%
  dplyr::filter(Loading < 0.40)

weak_items


#----------------------------------------------------------
# Factor correlations
#----------------------------------------------------------
factor_cor <- as.data.frame(lavaan::inspect(fit_forced, "std")$psi)

factor_cor


#----------------------------------------------------------
# Modification Indices
#----------------------------------------------------------
mi <- lavaan::modindices(fit_forced, sort = TRUE, minimum.value = 10)

mi



#----------------------------------------------------------
# RELIABLITY (ALPHA PER FACTOR)
#----------------------------------------------------------
alpha_forced <- lapply(items_forced, function(vars) {
  
  dat_tmp <- gn_analysis %>%
    dplyr::select(dplyr::all_of(vars))
  
  psych::alpha(dat_tmp)$total
})

alpha_forced_df <- purrr::imap_dfr(alpha_forced, ~
                                     as.data.frame(.x) %>%
                                     tibble::rownames_to_column("metric") %>%
                                     dplyr::mutate(Factor = .y)
)

alpha_forced_df


#----------------------------------------------------------
# FACTOR SCORES
#----------------------------------------------------------
factor_scores_cfa <- lavaan::lavPredict(fit_forced)

factor_scores_cfa <- as.data.frame(factor_scores_cfa)

colnames(factor_scores_cfa) <- names(items_forced)



#----------------------------------------------------------
# DESCRIPTIVES
#----------------------------------------------------------

cfa_descriptives <- data.frame(
  Factor = colnames(factor_scores_cfa),
  N = sapply(factor_scores_cfa, function(x) sum(!is.na(x))),
  Mean = sapply(factor_scores_cfa, mean),
  SD = sapply(factor_scores_cfa, sd),
  Min = sapply(factor_scores_cfa, min),
  Max = sapply(factor_scores_cfa, max),
  Median = sapply(factor_scores_cfa, median)
) %>%
  dplyr::mutate(across(where(is.numeric), ~ round(., 3)))

cfa_descriptives



#----------------------------------------------------------
# PUBLICATION TABLES
#----------------------------------------------------------
apa_cfa_fit_forced <- data.frame(
  Model = "Forced CFA model",
  CFI = round(fitMeasures(fit_forced, "cfi"), 3),
  TLI = round(fitMeasures(fit_forced, "tli"), 3),
  RMSEA = round(fitMeasures(fit_forced, "rmsea"), 3),
  SRMR = round(fitMeasures(fit_forced, "srmr"), 3)
)


apa_cfa_fit_forced2 <- flextable(apa_cfa_fit_forced) %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  autofit() %>%
  set_caption(
    "Confirmatory factor analysis model fit indices for competing factor solutions (WLSMV estimator)"
  ) %>%
  add_footer_lines(
    values = c(
      "Note. CFI and TLI ≥ 0.90 indicate acceptable fit and ≥ 0.95 indicate good fit.",
      "RMSEA ≤ 0.08 and SRMR ≤ 0.08 indicate acceptable model fit.",
      "Estimation used WLSMV due to ordinal indicators."
    )
  ) %>%
  italic(part = "footer")

apa_cfa_fit_forced2

#----------------------------------------------------------
# SAVE OUTPUTS
#----------------------------------------------------------
saveRDS(fit_forced, "cfa_forced_model.rds")
write.csv(cfa_loadings, "cfa_forced_loadings.csv", row.names = FALSE)
write.csv(factor_cor, "cfa_forced_correlations.csv", row.names = TRUE)
write.csv(cfa_descriptives, "cfa_forced_descriptives.csv", row.names = FALSE)








