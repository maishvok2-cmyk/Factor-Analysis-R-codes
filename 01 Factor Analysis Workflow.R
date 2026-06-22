# Clean Up last Session
rm(list = ls())
cat("\014")

# Load libraries
library(haven)     # read .dta
library(dplyr)     # data manipulation
library(stringr)   # string filtering
#===============================================================================
# Read Data
#===============================================================================
path <- "C:/Users/Public/Documents/APHRC/K-YAGNS/National Survey/Final Cleaned DataSets/Analyzed Datasets/Weights/K-YAGNS Dyad Analytic.dta"


df <- read_dta(path)

#===============================================================================
# Select gender norm items
#===============================================================================
gn_cols <- names(df)[
  str_starts(names(df), "gn") &
    !str_detect(names(df), regex("note", ignore_case = TRUE)) &
    !str_detect(names(df), "_spy")
]

gn_df <- df %>%
  select(all_of(gn_cols))

cat("\nSelected", length(gn_cols), "gender norms items\n")

#===============================================================================
# Drop specific items
#===============================================================================
drop_vars <- c('gn19a','gn20a','gn21a','gn22a','gn23a',
               'gn24a','gn25a','gn26a','gn27a','gn28a')

# drop_vars <- c('gn19a','gn20a','gn21a','gn22a','gn23a',
#                'gn24a','gn25a','gn26a','gn27a','gn28a',
#                'gn29', 'gn30', 'gn2', 'gn40', 'gn44', 'gn46',
#                'gn32', 'gn33', 'gn34', 'gn36', 'gn45', 'gn48',
#                'gn31', 'gn35', 'gn37', 'gn43', 'gn47')

# 'gn34', 'gn44', 'gn46', 'gn48'

gn_df <- gn_df %>%
  select(-any_of(drop_vars))

dim(gn_df)

#===============================================================================
# Encode to numeric
#===============================================================================
# Rechecking before mapping

unique(gn_df[[1]])

gn_df <- gn_df %>%
  mutate(across(everything(), ~ {
    x <- as.numeric(.)
    x[x == 97] <- NA
    x[x == 98] <- 2
    x
  }))

# code 98 to 2 (neutral)


# Recheck if mapped
summary(gn_df)

#===============================================================================
# Reverse code
#===============================================================================
reverse_items <- c('gn2','gn4','gn6','gn9','gn17','gn34','gn46','gn48')

# reverse_vars <- c("gn1","gn3","gn5","gn7","gn10","gn11","gn12","gn13",
#                   "gn14","gn15","gn16","gn18","gn19b","gn20b","gn22b",
#                   "gn23b","gn24b","gn25b","gn26b","gn27b","gn28b",
#                   "gn29","gn30","gn31","gn32","gn33","gn35","gn36",
#                   "gn37","gn43","gn45","gn49","gn50","gn51","gn52",
#                   "gn53","gn54")

# data[reverse_vars] <- lapply(data[reverse_vars], function(x) 4 - x)

gn_df <- gn_df %>%
  mutate(across(any_of(reverse_items), ~ 4 - .))

#===============================================================================
# Check for missingness
#===============================================================================
colMeans(is.na(gn_df)) |> round(4)

#===============================================================================
# Check distributions (first 10 items)
#===============================================================================
for (col in names(gn_df)[1:10]) {
  cat("\n", col, "\n")
  print(table(gn_df[[col]], useNA = "ifany"))
}


# Final checks
cat("\nItems after filtering:", ncol(gn_df), "\n")

cat("\nMissing data per item:\n")
missing_prop <- colMeans(is.na(gn_df))
print(round(missing_prop, 4))

#===============================================================================
# keep items with ≥70% valid responses
#===============================================================================
valid_pct <- colMeans(!is.na(gn_df))

gn_df <- gn_df[, valid_pct >= 0.70]

#===============================================================================
# Drop zero or near-zero variance items
#===============================================================================
gn_df <- gn_df[, apply(gn_df, 2, sd, na.rm = TRUE) > 0.01]

# Quick Check
cat("Remaining items:", ncol(gn_df), "\n")
summary(apply(gn_df, 2, sd, na.rm = TRUE))

#===============================================================================
# Drop NaNs -> Avoid dropping use pairwise instead
# Or KNNImputer
# Or Median
#===============================================================================
# gn_analysis <- na.omit(gn_df)

# Pairwise
# gn_analysis <- gn_df

# KNNImputer
# install.packages("VIM")
# library(VIM)
# Apply KNNImputer
# gn_analysis <- kNN(gn_df, k = 5)
# 
# # Remove helper functions added by KNNImputer 
# gn_analysis <- gn_analysis[, !grepl("_imp$", names(gn_analysis))]

# Using Median
gn_analysis <- gn_df

gn_analysis[] <- lapply(gn_analysis, function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
  }
  x
})

# Checks
colSums(is.na(gn_analysis))
cat("Rows before:", nrow(gn_df), "\n")
cat("Rows after:", nrow(gn_analysis), "\n")

#===============================================================================
# Cronbach’s Alpha
#===============================================================================
cronbach_alpha <- function(data) {
  k <- ncol(data)
  if (k < 2) return(NA)
  
  item_var_sum <- sum(apply(data, 2, var, na.rm = TRUE))
  total_score <- rowSums(data, na.rm = TRUE)
  total_var <- var(total_score, na.rm = TRUE)
  
  if (total_var == 0) return(NA)
  
  (k / (k-1)) * (1 - item_var_sum/total_var)
}

alpha <- cronbach_alpha(gn_analysis)
print(alpha)
#===============================================================================
# OPTION 2:
#===============================================================================
# 
# library(psych)
# 
# alpha_result <- alpha(gn_analysis)
# 
# alpha_result$total$raw_alpha

#===============================================================================
# Item Total Statistics
#===============================================================================
n_vars <- ncol(gn_analysis)

item_total_stats <- lapply(names(gn_analysis), function(col) {
  
  rest <- gn_analysis[, setdiff(names(gn_analysis), col), drop = FALSE]
  rest_total <- rowSums(rest, na.rm = TRUE)
  
  corrected_r <- cor(gn_analysis[[col]], rest_total, use = "complete.obs")
  
  k2 <- ncol(rest)
  iv2 <- sum(apply(rest, 2, var, na.rm = TRUE))
  tv2 <- var(rowSums(rest, na.rm = TRUE), na.rm = TRUE)
  
  alpha_del <- if (!is.na(tv2) && tv2 > 0 && k2 > 1) {
    (k2 / (k2 - 1)) * (1 - iv2 / tv2)
  } else {
    NA
  }
  
  data.frame(
    Item = col,
    Mean = mean(gn_analysis[[col]], na.rm = TRUE),
    Std = sd(gn_analysis[[col]], na.rm = TRUE),
    Corrected_Item_Total_r = corrected_r,
    Alpha_if_Deleted = alpha_del
  )
})

alpha_df <- do.call(rbind, item_total_stats)

# Overall Cronbach alpha
alpha_val <- cronbach_alpha(gn_analysis)

# Interpretation
interp <- if (alpha_val >= 0.9) {
  "Excellent"
} else if (alpha_val >= 0.8) {
  "Good"
} else if (alpha_val >= 0.7) {
  "Acceptable"
} else if (alpha_val >= 0.6) {
  "Questionable"
} else if (alpha_val >= 0.5) {
  "Poor"
} else {
  "Unacceptable"
}

# Print output
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("CRONBACH'S ALPHA — RELIABILITY\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat(sprintf("Overall Alpha: %.4f (%s)\n", alpha_val, interp))
cat(sprintf("Number of items: %d\n", n_vars))

cat("\nItem-Total Statistics:\n")
alpha_df_display <- alpha_df %>%
  mutate(across(where(is.numeric), ~ format(., nsmall = 4)))

print(alpha_df_display, row.names = FALSE)


#===============================================================================
 # KMO Kaiser-Meyer-Olkin Measure of Sampling Adequacy
#===============================================================================
library(psych)

# Overrall KMO test
kmo_result <- KMO(gn_analysis)

# Extract Values
kmo_per_item <- kmo_result$MSAi
kmo_overall <- kmo_result$MSA

# Interpretation
kmo_interp <- if (kmo_overall >= 0.9) {
  "Marvelous"
} else if (kmo_overall >= 0.8) {
  "Meritorious"
} else if (kmo_overall >= 0.7) {
  "Middling"
} else if (kmo_overall >= 0.6) {
  "Mediocre"
} else if (kmo_overall >= 0.5) {
  "Miserable"
} else {
  "Unacceptable"
}

# Build per-item table
kmo_df <- data.frame(
  Item = names(kmo_per_item),
  KMO = as.numeric(kmo_per_item)
)

# Print results
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("KMO TEST — SAMPLING ADEQUACY\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat(sprintf("KMO Overall: %.4f (%s)\n", kmo_overall, kmo_interp))

cat("\nPer-Item KMO:\n")
kmo_display <- kmo_df %>%
  mutate(across(where(is.numeric), ~ format(., nsmall = 4)))

print(kmo_display, row.names = FALSE)

#===============================================================================
 # BARTLETT'S TEST OF SPHERICITY
#===============================================================================
# Pairwise formula
# bart_result <- cortest.bartlett(cor(gn_analysis, use = "pairwise.complete.obs"), n = nrow(gn_analysis))

# KNNImputed formula
# bart_result <- cortest.bartlett(cor(gn_analysis), n = nrow(gn_analysis))
# Median imputation
bart_result <- cortest.bartlett(cor(gn_analysis), n = nrow(gn_analysis))

chi2_bart <- bart_result$chisq
p_bart <- bart_result$p.value

n_vars <- ncol(gn_analysis)
dof_bart <- n_vars * (n_vars - 1) / 2

# Interpretation
bart_interp <- if (p_bart < 0.05) {
  "Significant — suitable for factor analysis"
} else {
  "Not significant"
}

# Print results
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("BARTLETT'S TEST OF SPHERICITY\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat(sprintf("Chi-Square: %.4f\n", chi2_bart))
cat(sprintf("Degrees of Freedom: %d\n", dof_bart))
cat(sprintf("p-value: %.2e\n", p_bart))
cat(sprintf("Interpretation: %s\n", bart_interp))

#===============================================================================
 # DETERMINE NUMBER OF FACTORS
#===============================================================================

# PCA and eigenvalues
data_std <- scale(gn_analysis)

# pca <- prcomp(data_std, center = TRUE, scale. = TRUE)
# Remove Inf
data_std[is.infinite(as.matrix(data_std))] <- NA

# Remove zero variance columns
data_std <- data_std[, apply(data_std, 2, function(x) var(x, na.rm = TRUE) > 0)]

# Impute or drop
data_std_clean <- na.omit(data_std)

# Run PCA
pca <- prcomp(data_std_clean, center = TRUE, scale. = TRUE)

eigenvalues <- (pca$sdev)^2
var_ratio <- eigenvalues / sum(eigenvalues)
cum_var <- cumsum(var_ratio)

n_vars <- ncol(gn_analysis)
n_obs <- nrow(gn_analysis)

# Kaiser Criterion (eigenvalue > 1)
n_kaiser <- sum(eigenvalues > 1)

# Parallel Analysis (Horn Method)
set.seed(123)

n_iter <- 1000
random_eigenvalues <- matrix(NA, nrow = n_iter, ncol = n_vars)

for (i in 1:n_iter) {
  random_data <- matrix(rnorm(n_obs * n_vars), nrow = n_obs)
  random_corr <- cor(random_data)
  random_eigenvalues[i, ] <- sort(eigen(random_corr)$values, decreasing = TRUE)
}

mean_random_eig <- colMeans(random_eigenvalues)
p95_random_eig <- apply(random_eigenvalues, 2, quantile, 0.95)

n_parallel <- sum(eigenvalues > p95_random_eig)

# Output Summary
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("NUMBER OF FACTORS — EXTRACTION CRITERIA\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat(sprintf("Kaiser criterion (eigenvalue > 1): %d factors\n", n_kaiser))
cat(sprintf("Parallel analysis (95th pctl):     %d factors\n", n_parallel))


# Eigen table
eigen_df <- data.frame(
  Factor = paste0("Factor ", 1:n_vars),
  Eigenvalue = eigenvalues,
  Pct_Variance = var_ratio * 100,
  Cumulative_Pct = cum_var * 100,
  Random_95th = p95_random_eig,
  Retain_Parallel = ifelse(eigenvalues > p95_random_eig, "Yes", "No")
)

eigen_df_print <- eigen_df %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

print(eigen_df_print, row.names = FALSE)

#===============================================================================
# SCREE PLOT WITH PARALLEL ANALYSIS
#===============================================================================
# Prepare the data
library(ggplot2)
library(plotly)


factor_id <- 1:n_vars

# Build data frame
scree_df <- data.frame(
  Factor = factor_id,
  Eigenvalue = eigenvalues,
  Parallel_95 = p95_random_eig
)

# Scree plot (ggplot2)
p <- ggplot(scree_df, aes(x = Factor)) +
  
  # actual eigenvalues
  geom_line(aes(y = Eigenvalue, color = "Actual Eigenvalues"), linewidth = 1.2) +
  geom_point(aes(y = Eigenvalue, color = "Actual Eigenvalues"), size = 2) +
  
  # parallel analysis
  geom_line(aes(y = Parallel_95, color = "Parallel Analysis (95th %)"), 
            linetype = "dashed", linewidth = 1.1) +
  geom_point(aes(y = Parallel_95, color = "Parallel Analysis (95th %)"), size = 2) +
  
  # Kaiser line
  geom_hline(yintercept = 1, linetype = "dotted", color = "gray40") +
  
  labs(
    title = "Scree Plot with Parallel Analysis",
    x = "Factor Number",
    y = "Eigenvalue",
    color = ""
  ) +
  
  scale_x_continuous(breaks = 1:n_vars) +
  
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(p)

#===============================================================================
# EXPLORATORY FACTOR ANALYSIS (EFA)
#===============================================================================
# Set number of factors
n_factors <- n_parallel  # change based on parallel analysis

if (n_factors < 1) {
  n_factors <- n_kaiser
}

cat("\nExtracting", n_factors, "factors...\n")

# Run EFA (Maximum Likelihood + Varimax)
fa_model <- fa(
  gn_analysis,
  nfactors = n_factors,
  rotate = "varimax",
  fm = "ml",
  # use = "pairwise"
)


# Loadings
loadings_df <- as.data.frame(unclass(fa_model$loadings))
colnames(loadings_df) <- paste0("Factor", 1:n_factors)

# fa_variance <- fa_model$Vaccounted
# 
# # Factor loadings table
# loadings_df <- as.data.frame(unclass(fa_model$loadings))

loadings_df$Communality <- fa_model$communality
loadings_df$Uniqueness <- fa_model$uniquenesses

# Clean formatting
loadings_df <- round(loadings_df, 4)

# # Variance explained
# variance <- fa_model$Vaccounted

# Build base table
variance_df <- data.frame(
  Metric = c("SS Loadings", "Proportion Variance", "Cumulative Variance")
)

# Add each factor dynamically # sum of squared(ss) factor loadings
for (i in 1:n_factors) {
  variance_df[[paste0("Factor", i)]] <- c(
    fa_model$Vaccounted["SS loadings", i],
    fa_model$Vaccounted["Proportion Var", i],
    fa_model$Vaccounted["Cumulative Var", i]
  )
}


# Print results
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("EFA RESULTS —", n_factors, "FACTORS (Varimax Rotation, ML)\n")
cat(paste(rep("=", 60), collapse = ""), "\n")


cat("\nFactor Loadings:\n")
print(loadings_df)

cat("\nVariance Explained:\n")
variance_df_print <- variance_df %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

print(variance_df_print, row.names = FALSE)

# print(n_parallel)
# print(n_kaiser)

#===============================================================================
# FACTOR LOADINGS HEATMAP
#===============================================================================
# Prepare loading matrix
library(ggplot2)
library(reshape2)
library(plotly)

loading_mat <- as.matrix(loadings_df[, paste0("Factor", 1:n_factors)])
# names(loadings_df)
# loading_mat <- as.matrix(loadings_df[, paste0("ML", 1:n_factors)])



# Convert to long format
heat_df <- melt(loading_mat)

colnames(heat_df) <- c("Item", "Factor", "Loading")

# Static heatmap (ggplot2)
p <- ggplot(heat_df, aes(x = Factor, y = Item, fill = Loading)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    limits = c(-1, 1)
  ) +
  labs(
    title = "Factor Loadings Heatmap (Varimax Rotation)",
    x = "Factors",
    y = "Items",
    fill = "Loading"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 10)
  )

# Interactive version (plotly)
ggplotly(p)


#===============================================================================
# ITEM–FACTOR ASSIGNMENT & SUMMARY
#===============================================================================
# Extract loadings matrix
loading_mat <- as.matrix(unclass(fa_model$loadings))
loading_mat <- loading_mat[, 1:n_factors]

# Build item–factor assignment table
library(dplyr)

assignment <- data.frame(
  Item = rownames(loading_mat),
  Best_Factor = apply(loading_mat, 1, function(x) {
    paste0("Factor ", which.max(abs(x)))
  }),
  Loading = apply(loading_mat, 1, function(x) {
    x[which.max(abs(x))]
  }),
  Communality = fa_model$communality
)

# Print structured output
cat("\n", strrep("=", 60), "\n")
cat("ITEM–FACTOR ASSIGNMENT\n")
cat(strrep("=", 60), "\n")

for (f in 1:n_factors) {
  
  factor_items <- assignment %>%
    filter(Best_Factor == paste0("Factor ", f))
  
  cat("\nFactor", f, "(", nrow(factor_items), "items )\n")
  
  for (i in 1:nrow(factor_items)) {
    
    row <- factor_items[i, ]
    
    marker <- ifelse(abs(row$Loading) >= 0.4, "*", "~")
    
    cat(
      marker,
      sprintf("%-20s", row$Item),
      " loading = ",
      sprintf("%.4f", row$Loading),
      " h2 = ",
      sprintf("%.4f", row$Communality),
      "\n"
    )
  }
}



#===============================================================================
# CRONBACH'S ALPHA PER FACTOR (SUB-SCALE RELIABILITY)
#===============================================================================
# Ensure numeric dataset
gn_analysis <- as.data.frame(lapply(gn_analysis, as.numeric))

# Re-define Cronbach alpha function 
cronbach_alpha <- function(df) {
  
  k <- ncol(df)
  
  item_vars <- apply(df, 2, var, na.rm = TRUE)
  total_var <- var(rowSums(df, na.rm = TRUE), na.rm = TRUE)
  
  alpha <- (k / (k - 1)) * (1 - sum(item_vars) / total_var)
  
  return(alpha)
}

# Subscale reliability loop
cat("\n", strrep("=", 60), "\n")
cat("SUB-SCALE RELIABILITY (Alpha per Factor)\n")
cat(strrep("=", 60), "\n")

for (f in 1:n_factors) {
  
  factor_items <- assignment$Item[assignment$Best_Factor == paste0("Factor ", f)]
  
  valid_items <- intersect(factor_items, names(gn_analysis))
  
  if (length(valid_items) >= 2) {
    
    sub_alpha <- cronbach_alpha(gn_analysis[, valid_items, drop = FALSE])
    
    cat("Factor", f, "(", length(valid_items), "items ) α =",
        sprintf("%.4f", sub_alpha), "\n")
    
  } else {
    cat("Factor", f, "(", length(valid_items), "items ) Too few items\n")
  }
}

#===============================================================================
# COMPUTE FACTOR SCORES
#===============================================================================
factor_scores <- as.data.frame(fa_model$scores)

colnames(factor_scores) <- paste0("Factor", 1:n_factors, "_Score")

# Merge with original dataset
df_with_scores <- cbind(df, factor_scores)

# # If there is a row mismatch 4230 v/s 1416
# df_with_scores <- cbind(
#   df[rownames(factor_scores), ],
#   factor_scores
# )

# summary_stats <- data.frame(
#   Count = sapply(factor_scores, function(x) sum(!is.na(x))),
#   Mean  = sapply(factor_scores, mean, na.rm = TRUE),
#   SD    = sapply(factor_scores, sd, na.rm = TRUE),
#   Min   = sapply(factor_scores, min, na.rm = TRUE),
#   Max   = sapply(factor_scores, max, na.rm = TRUE)
# )

round(summary_stats, 4)





#===============================================================================
 # Export results to excel
#===============================================================================
library(openxlsx)

# Set file path
outfile <- "C:/Users/Public/Documents/APHRC/K-YAGNS/National Survey/Final Cleaned DataSets/Summaries/Factor Analysis/EIGHT_Factor_KNNImputer_Generated_Factor_Analysis_Results.xlsx"

# Create Workbook
wb <- createWorkbook()

# Add sheets + write data
addWorksheet(wb, "Cronbach_Alpha")
writeData(wb, "Cronbach_Alpha", alpha_df)


addWorksheet(wb, "KMO")
writeData(wb, "KMO", kmo_df)

addWorksheet(wb, "Eigenvalues")
writeData(wb, "Eigenvalues", eigen_df)

addWorksheet(wb, "Factor_Loadings")
writeData(wb, "Factor_Loadings", loadings_df)

addWorksheet(wb, "Item_Assignment")
writeData(wb, "Item_Assignment", assignment)

addWorksheet(wb, "Variance_Explained")

writeData(wb, "Variance_Explained", variance_df)

addWorksheet(wb, "Score_Descriptives")
writeData(wb, "Score_Descriptives", summary(factor_scores))

addWorksheet(wb, "Data_With_Scores")
writeData(wb, "Data_With_Scores", df_with_scores)

# Save file
saveWorkbook(wb, outfile, overwrite = TRUE)

cat("\nResults exported to:\n", outfile, "\n")









#===============================================================================
 # Checking Weak Factors
#===============================================================================
  # 1. Identify items weakening Factor 5
#===============================================================================
#Interpretation rule
# Remove items that:
#   
  # have low loading (< 0.40)
  # increase alpha when removed
  # show low communality (< 0.20)
#===============================================================================
# Step 1: Isolate the items

factor5_items <- loadings_df %>%
  tibble::rownames_to_column("Item") %>%
  select(Item, ML5, Communality) %>%
  arrange(abs(ML5))

# Step 2: flag weak items
# Use a clear rule:
  # weak loading < 0.40
  # very weak < 0.30

factor5_items <- factor5_items %>%
  mutate(
    Weak = case_when(
      abs(ML5) < 0.30 ~ "Very Weak",
      abs(ML5) < 0.40 ~ "Weak",
      TRUE ~ "Acceptable"
    )
  )

factor5_items

# Step 3: identify items reducing alpha
factor5_vars <- rownames(loadings_df)[abs(loadings_df$ML5) >= 0.30]

alpha(gn_analysis[,factor5_vars, drop = FALSE])

# Then iterate:
for (v in factor5_vars) {
  
  test_set <- setdiff(factor5_vars, v)
  
  # skip invalid cases
  if (length(test_set) < 2) {
    cat(v, ": skipped (too few items)\n")
    next
  }
  
  temp_data <- gn_analysis[, test_set, drop = FALSE]
  
  # drop zero variance columns
  temp_data <- temp_data[, apply(temp_data, 2, sd, na.rm = TRUE) > 0]
  
  if (ncol(temp_data) < 2) {
    cat(v, ": skipped (no variance)\n")
    next
  }
  
  result <- tryCatch({
    alpha(temp_data)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(result)) {
    cat(v, ": failed\n")
  } else {
    cat(v, ":", result$total$raw_alpha, "\n")
  }
}

#===============================================================================
# 2. Merge Factor 6 and Factor 7 logically
#===============================================================================
# Step 1: check correlation between factors
# Interpretation:
 
  # 0.50 → strong link → merge
  # 0.30 to 0.50 → consider merging
  # < 0.30 → keep separate

cor(factor_scores$ML6,
    factor_scores$ML7,
    use = "complete.obs")

# Step 2: Test combined reliablity:
merge_items <- c(
  rownames(loadings_df)[loadings_df$ML6 >= 0.30],
           row.names(loadings_df)[loadings_df$ML7 >= 0.30]
           )

alpha(,merge_items, drop = FALSE)

library(psych)

temp_data <- gn_analysis[, merge_items, drop = FALSE]

# remove zero variance columns
temp_data <- temp_data[, apply(temp_data, 2, sd, na.rm = TRUE) > 0]

if (ncol(temp_data) >= 2) {
  alpha(temp_data)
} else {
  cat("Too few valid items for alpha\n")
}

# Step 3: decision rule
# Merge if:
  # Correlation between factor scores ≥ 0.40
  # Combined alpha ≥ 0.70
  # Conceptually similar items



