#####
# AIC
#####
 
rm(list = ls())

# Working directory
setwd("C:/Users/jijia/Desktop/Jijiaxin/VŠ/02_Master/03_Zweite_WS_25-26/DSMA/seminar paper/new_code/more_relaxed")

# Libraries
library(dplyr)
library(readr)
library(MASS) 
library(car)
library(tidyverse)

# Load data
df <- read_csv("data12a_df_ml.csv", show_col_types = FALSE)
df_no_out <- read_csv("data12b_df_ml_no_out.csv", show_col_types = FALSE)

colnames(df)

# AIC-based selection
run_aic <- function(data, target, seed = 123) {
  set.seed(seed)
  if("price_range" %in% names(data)) {
    data$price_range <- as.factor(data$price_range)}
  
  data <- data %>%
    mutate(across(all_of(target), as.numeric))
  var_leaveout <- setdiff(c("satisfied_stars", "satisfied_sentiment","is_weekend",
                            "Q2","Q3","Q4","is_covid"), target)
  data <- data %>%
    dplyr::select(-dplyr::any_of(var_leaveout))
  
  train_idx <- sample(seq_len(nrow(data)), size = floor(0.7 * nrow(data)))
  train_data <- data[train_idx, ]
  
  full_formula <- as.formula(paste(target, "~ ."))
  full_model <- glm(full_formula, data = train_data, family = binomial(), 
                    control = glm.control(maxit = 50))
  
  aic_model <- stepAIC(full_model, direction = "both", trace = FALSE)
  
  list(model = aic_model, variables = names(coef(aic_model))[-1])}

# clean dummy names
clean_names <- function(var_list) {
  unique(gsub("[0-9]+$", "", var_list))}

# run AIC selection
aic_sent <- run_aic(df, "satisfied_sentiment")
aic_star <- run_aic(df, "satisfied_stars")

vars_sent_clean <- unique(gsub("^price_range[0-9]+$", "price_range", aic_sent$variables))
vars_star_clean <- unique(gsub("^price_range[0-9]+$", "price_range", aic_star$variables))

# Final datasets 
# df_final_sent <- df %>%
#   dplyr::select(satisfied_sentiment, dplyr::all_of(vars_sent_clean))
# 
# df_final_star <- df %>%
#   dplyr::select(satisfied_stars, dplyr::all_of(vars_star_clean))


# ------------------------------------------------------------
# Multicollinearity diagnostics
cat("\nVIF – Sentiment model\n")
print(vif(aic_sent$model))

cat("\nVIF – Stars model\n")
print(vif(aic_star$model))


# ------------------------------------------------------------
# Repeat for No-Outliers dataset 
aic_sent_no_out <- run_aic(df_no_out, "satisfied_sentiment")
aic_star_no_out <- run_aic(df_no_out, "satisfied_stars")

vars_sent_no_out_clean <- unique(gsub("^price_range[0-9]+$", "price_range", aic_sent_no_out$variables))
vars_star_no_out_clean <- unique(gsub("^price_range[0-9]+$", "price_range", aic_star_no_out$variables))

# df_no_out_final_sent <- df_no_out %>%
#   dplyr::select(satisfied_sentiment, dplyr::all_of(vars_sent_no_out_clean))
# 
# df_no_out_final_star <- df_no_out %>%
#   dplyr::select(satisfied_stars, dplyr::all_of(vars_star_no_out_clean))

# LEFT out variables
original_vars <- colnames(df)[colnames(df) != "satisfied_sentiment"]
kept_vars <- vars_sent_clean
dropped_vars <- setdiff(original_vars, kept_vars)
cat("Variables AIC decided to LEAVE OUT:\n")
print(dropped_vars)


colnames(df_final_sent)
colnames(df_final_star)

colnames(df_no_out_final_sent)
colnames(df_no_out_final_star)

# ------------------------------------------------------------
# Save outputs - not taking the AIC diagnostic into account - instead it will be compared to later 

df_ml_clean_final_sent <- df %>% 
  dplyr::select(-satisfied_stars)

df_ml_clean_final_star <- df %>% 
  dplyr::select(-satisfied_sentiment)

df_ml_no_outliers_clean_final_sent <- df_no_out %>% 
  dplyr::select(-satisfied_stars)

df_ml_no_outliers_clean_final_star <- df_no_out %>% 
  dplyr::select(-satisfied_sentiment)

colnames(df_ml_clean_final_sent)
colnames(df_ml_no_outliers_clean_final_sent)

colnames(df_ml_clean_final_star)
colnames(df_ml_no_outliers_clean_final_star)

summary(df_ml_clean_final_sent)
summary(df_ml_clean_final_star)

write_csv(df_ml_clean_final_sent, "data13_final_ml_sent.csv")
write_csv(df_ml_clean_final_star, "data13_final_ml_star.csv")

write_csv(df_ml_no_outliers_clean_final_sent, "data13_final_ml_no_out_sent.csv")
write_csv(df_ml_no_outliers_clean_final_star, "data13_final_ml_no_out_star.csv")


