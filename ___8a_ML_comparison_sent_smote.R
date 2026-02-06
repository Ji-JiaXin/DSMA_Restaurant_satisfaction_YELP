# ==============================================================================
# ML comparison: Normal vs. SMOTE
# Target: satisfied_sentiment
# ==============================================================================

rm(list = ls())
set.seed(123)


library(caret)
library(dplyr)
library(readr)
library(pROC)
library(recipes)
library(themis)      
library(doParallel)
library(foreach)
library(gains)
library(tidyr)
library(ggplot2)
library(nnet)
library(rpart)
library(ipred)
library(ranger)
library(xgboost)
library(e1071)
library(LiblineaR)
library(stargazer)
library(car)
library(scales)


n_cores <- max(1, parallel::detectCores() - 2)
cl <- makePSOCKcluster(n_cores)
registerDoParallel(cl)


# Load Data
setwd("C:/Users/jijia/Desktop/Jijiaxin/VŠ/02_Master/03_Zweite_WS_25-26/DSMA/seminar paper/new_code/more_relaxed")

df <- read_csv("data13_final_ml_sent.csv", show_col_types = FALSE)

# Target variable
target_var <- "satisfied_sentiment"

df[[target_var]] <- factor(
  ifelse(df[[target_var]] == 1, "Yes", "No"),
  levels = c("No", "Yes"))

# Reduced dataset for Logit/NB (multicollinearity handling)
df_reduced <- df %>%
  dplyr::select(-avg_temp_lag1, -avg_temp_roll3, -PRCP_avg_lag1, -PRCP_avg_roll3)

# Quick Logistic regression check
logit_final <- glm(as.formula(paste(target_var, "~ .")), data = df_reduced, family = binomial)
stargazer(logit_final, type = "text", title = "Logistic Regression Results", single.row = TRUE, 
          star.cutoffs = c(0.05, 0.01, 0.001), digits = 3)
vif(logit_final)

# train/test Split
idx <- createDataPartition(df[[target_var]], p = 0.7, list = FALSE)

train_full_raw <- df[idx, ]
test_full_raw  <- df[-idx, ]
train_red_raw  <- df_reduced[idx, ]
test_red_raw   <- df_reduced[-idx, ]

# Preprocessing 
recipe_full <- recipe(as.formula(paste(target_var, "~ .")), data = train_full_raw) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_zv(all_predictors())

recipe_red <- recipe(as.formula(paste(target_var, "~ .")), data = train_red_raw) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_zv(all_predictors())

# Prepare Normal datasets
train_pp_full_norm <- juice(prep(recipe_full))
train_pp_red_norm  <- juice(prep(recipe_red))
test_pp_full       <- bake(prep(recipe_full), new_data = test_full_raw)
test_pp_red        <- bake(prep(recipe_red), new_data = test_red_raw)

# Prepare SMOTE datasets (50/50)
train_pp_full_smote <- recipe_full %>% step_smote(!!sym(target_var), over_ratio = 0.5) %>% prep() %>% juice()
train_pp_red_smote  <- recipe_red %>% step_smote(!!sym(target_var), over_ratio = 0.5) %>% prep() %>% juice()

# Helper functions
optimal_threshold <- function(y_true, prob) {
  roc_obj <- roc(y_true, prob, quiet = TRUE)
  coords(roc_obj, "best", best.method = "youden", transpose = FALSE)$threshold
}

compute_metrics <- function(y_true, prob, threshold, m_name, s_name) {
  pred <- factor(ifelse(prob >= threshold, "Yes", "No"), levels = c("No","Yes"))
  cm <- confusionMatrix(pred, y_true, positive = "Yes")
  roc_obj <- roc(y_true, prob, quiet = TRUE)
  top_n <- ceiling(0.10 * length(prob))
  tdl <- mean(y_true[order(prob, decreasing = TRUE)][1:top_n] == "Yes") / mean(y_true == "Yes")
  
  data.frame(
    Model = m_name, Sampling = s_name,
    Accuracy = cm$overall["Accuracy"], Precision = cm$byClass["Precision"],
    Recall = cm$byClass["Recall"], F1 = cm$byClass["F1"],
    Gini = 2 * as.numeric(auc(roc_obj)) - 1, Top_Decile_Lift = tdl
  )
}

# Unified Model Runner
train_predict <- function(model, train_df, test_df, target) {
  X_train <- as.matrix(dplyr::select(train_df, -all_of(target)))
  X_test  <- as.matrix(dplyr::select(test_df, -all_of(target)))
  y_train <- train_df[[target]]
  
  if (model == "Logit") {
    fit <- glm(as.formula(paste(target, "~ .")), data = train_df, family = binomial)
    p_tr <- predict(fit, train_df, type = "response")
    p_ts <- predict(fit, test_df, type = "response")
    
  } else if (model == "Tree") {
    tbl <- table(y_train)
    weights <- ifelse(y_train == "Yes", as.numeric(tbl["No"]/tbl["Yes"]), 1)
    
    fit <- rpart(
      as.formula(paste(target, "~ .")), 
      data = train_df,
      method = "class",
      parms = list(split = "gini"),
      weights = weights,
      control = rpart.control(cp = 1e-6, minsplit = 2, maxdepth = 30, xval = 5)
    )
    
    p_tr <- predict(fit, train_df, type = "prob")[,"Yes"]
    p_ts <- predict(fit, test_df,  type = "prob")[,"Yes"]
    
  } else if (model == "Bagging") {
    fit <- ipred::bagging(as.formula(paste(target, "~ .")), data = train_df, nbagg = 25)
    p_tr <- predict(fit, train_df, type = "prob")[,"Yes"]
    p_ts <- predict(fit, test_df, type = "prob")[,"Yes"]
    
  } else if (model == "RF") {
    fit <- ranger(x = X_train, y = y_train, probability = TRUE, num.trees = 100, num.threads = 1)
    p_tr <- predict(fit, X_train)$predictions[,"Yes"]
    p_ts <- predict(fit, X_test)$predictions[,"Yes"]
    
  } else if (model == "XGB") {
    fit <- xgboost(data = X_train, label = as.numeric(y_train=="Yes"), nrounds = 30,
                   objective = "binary:logistic", verbose=0, nthread=1)
    p_tr <- predict(fit, X_train)
    p_ts <- predict(fit, X_test)
    
  } else if (model == "SVM") {
    fit <- LiblineaR(X_train, y_train, type = 7)
    p_tr <- predict(fit, X_train, proba = TRUE)$probabilities[,"Yes"]
    p_ts <- predict(fit, X_test, proba = TRUE)$probabilities[,"Yes"]
    
  } else if (model == "kNN") {
    set.seed(123)
    train_small <- train_df %>% slice_sample(prop = 0.10)
    fit <- caret::knn3(as.formula(paste(target, "~ .")), data = train_small, k = 15)
    p_tr <- predict(fit, train_df)[,"Yes"]
    p_ts <- predict(fit, test_df)[,"Yes"]
    
  } else if (model == "NB") {
    fit <- e1071::naiveBayes(as.formula(paste(target, "~ .")), data = train_df)
    p_tr <- predict(fit, train_df, type = "raw")[,"Yes"]
    p_ts <- predict(fit, test_df, type = "raw")[,"Yes"]
    
  } else if (model == "NN") {
    fit <- nnet::nnet(as.formula(paste(target, "~ .")), data = train_df, size = 3, maxit = 50, trace = FALSE)
    p_tr <- as.vector(predict(fit, train_df, type = "raw"))
    p_ts <- as.vector(predict(fit, test_df, type = "raw"))
  }
  
  thr <- optimal_threshold(y_train, p_tr)
  return(list(p_test = p_ts, threshold = thr))
}

# Benchmark Loop
model_list <- c("Logit", "Tree", "Bagging", "RF", "XGB", "SVM", "kNN", "NB", "NN")

results_all <- foreach(m = model_list, .packages = c("caret","pROC","ranger","xgboost","rpart","ipred","LiblineaR","e1071","nnet","dplyr")) %dopar% {
  is_red <- m %in% c("Logit", "NB")
  
  # Normal Sampling
  res_n <- train_predict(m,
                         if(is_red) train_pp_red_norm else train_pp_full_norm,
                         if(is_red) test_pp_red else test_pp_full,
                         target_var)
  
  met_n <- compute_metrics(if(is_red) test_pp_red[[target_var]] else test_pp_full[[target_var]],
                           res_n$p_test, res_n$threshold, m, "Normal")
  
  # SMOTE Sampling
  res_s <- train_predict(m,
                         if(is_red) train_pp_red_smote else train_pp_full_smote,
                         if(is_red) test_pp_red else test_pp_full,
                         target_var)
  
  met_s <- compute_metrics(if(is_red) test_pp_red[[target_var]] else test_pp_full[[target_var]],
                           res_s$p_test, res_s$threshold, m, "SMOTE")
  
  rbind(met_n, met_s)
}

results_final <- bind_rows(results_all) %>% mutate(across(where(is.numeric), round, 3))
print(results_final)


# -------------------------
# NORMAL predictions for plotting 

predictions_normal <- lapply(model_list, function(m) {
  is_red <- m %in% c("Logit", "NB")
  
  tr_data <- if(is_red) train_pp_red_norm else train_pp_full_norm
  ts_data <- if(is_red) test_pp_red else test_pp_full
  
  res <- train_predict(m, tr_data, ts_data, target_var)
  return(res$p_test)
})

names(predictions_normal) <- model_list


# ------------------------
# Graphs 

# Model ordering
model_order <- results_final %>%
  filter(Sampling == "Normal") %>%
  arrange(desc(Gini)) %>%
  pull(Model)

# Plotting data with fixed ordering
plot_data <- results_final %>%
  mutate(
    Model = factor(Model, levels = model_order),
    Sampling = factor(Sampling, levels = c("Normal", "SMOTE"))  )

# -------------------------------------------------------------
# Gini Comparison Plot

p_gini <- ggplot(
  plot_data,
  aes(x = Model, y = Gini, fill = Sampling)
) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = round(Gini, 3)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  scale_fill_manual(
    values = c("Normal" = "#377eb8", "SMOTE" = "#e41a1c"),
    labels = c("Normal" = "Original", "SMOTE" = "SMOTE")
  ) +
  labs(
    title = "Gini Coefficient Comparison: Original vs. SMOTE",
    x = "Algorithm",
    y = "Gini Score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

print(p_gini)

# -------------------------------------------------------------
# Top Decile Lift Comparison Plot
p_tdl <- ggplot(
  plot_data,   
  aes(x = Model, y = Top_Decile_Lift, fill = Sampling)
) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = round(Top_Decile_Lift, 2)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  scale_fill_manual(
    values = c("Normal" = "#4daf4a", "SMOTE" = "#ff7f00"),
    labels = c("Normal" = "Original", "SMOTE" = "SMOTE")
  ) +
  labs(
    title = "Top Decile Lift Comparison: Original vs. SMOTE",
    x = "Algorithm",
    y = "Lift Factor"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

print(p_tdl)

# -------------------------------------------------------------
# F1 Score Comparison Plot

p_f1 <- ggplot(
  plot_data, 
  aes(x = Model, y = F1, fill = Sampling)
) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = round(F1, 3)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  scale_fill_manual(
    values = c("Normal" = "#984ea3", "SMOTE" = "#ffff33"),
    labels = c("Normal" = "Original", "SMOTE" = "SMOTE")
  ) +
  labs(
    title = "F1 Score Comparison: Original vs. SMOTE",
    subtitle = "Harmonic mean of Precision and Recall",
    x = "Algorithm",
    y = "F1 Score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

print(p_f1)


# Having all in one plot 
plot_data_long <- plot_data %>%
  pivot_longer(
    cols = c(Gini, Top_Decile_Lift, F1), 
    names_to = "Metric", 
    values_to = "Value"
  ) %>%
  mutate(
    Metric = case_when(
      Metric == "Gini" ~ "Gini Coefficient",
      Metric == "Top_Decile_Lift" ~ "Top Decile Lift",
      Metric == "F1" ~ "F1 Score"
    ),
    Metric = factor(Metric, levels = c("Gini Coefficient", "F1 Score", "Top Decile Lift")),
    Model = factor(Model, levels = model_order)
  )

p_final <- ggplot(
  plot_data_long, 
  aes(x = Model, y = Value, fill = Sampling)
) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.85)) +
  
  geom_text(
    aes(label = sprintf("%.3f", Value)),
    position = position_dodge(width = 0.85),
    vjust = -1.2,   
    size = 3.2,    
    fontface = "bold"
  ) +
  
  facet_wrap(~Metric, scales = "free_y", ncol = 1) + 
  
  scale_fill_manual(
    values = c("Normal" = "#377eb8", "SMOTE" = "#e41a1c"),
    labels = c("Normal" = "Original", "SMOTE" = "SMOTE")
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + 
  
  labs(
    title = "Model Performance Comparison: Original vs. SMOTE",
    x = "Algorithm",
    y = "Metric Value",
    fill = "Sampling Method"
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey90", color = NA), 
    strip.text = element_text(face = "bold", size = 11),
    panel.spacing = unit(2, "lines"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "grey30")
  )

print(p_final)

# -------------------------
# ROC curves 
roc_df <- bind_rows(lapply(names(predictions_normal), function(m) {
  r <- roc(test_full_raw[[target_var]], predictions_normal[[m]], quiet = TRUE)
  data.frame(
    FPR = 1 - r$specificities,
    TPR = r$sensitivities,
    Model = m
  )
}))

ggplot(roc_df, aes(FPR, TPR, color = Model)) +
  geom_line(linewidth = 1) +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  labs(title = paste("ROC Curves - Original Sampling:", target_var))

# -------------------------
# Lift curves

lift_df <- bind_rows(lapply(names(predictions_normal), function(m) {
  g <- gains(
    actual = as.numeric(test_full_raw[[target_var]] == "Yes"),
    predicted = predictions_normal[[m]],
    groups = 10
  )
  data.frame(Depth = g$depth, Lift = g$lift, Model = m)
}))

ggplot(lift_df, aes(Depth, Lift, color = Model)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() +
  labs(title = paste("Lift Curves - Original Sampling:", target_var))



# -------------------------
# Refit Best Model 

# Find best model
df_norm <- results_final[results_final$Sampling == "Normal", ]
best_model <- df_norm$Model[which.max(df_norm$Gini)]
print(paste("Best Model Identified:", best_model))

is_red_best <- best_model %in% c("Logit", "NB")
tr_final <- if(is_red_best) train_pp_red_norm else train_pp_full_norm

if (best_model == "XGB") {
  message("Starting Hyperparameter Tuning for XGBoost...")
  
  # the tuning grid
  xgb_grid <- expand.grid(
    nrounds = c(50, 100),           
    max_depth = c(3, 6, 9),         
    eta = c(0.01, 0.1),             
    gamma = 0,                     
    colsample_bytree = 0.8,         
    min_child_weight = 1,          
    subsample = 0.8                
  )
  
  fit_control <- trainControl(
    method = "cv", 
    number = 5,                     
    classProbs = TRUE, 
    summaryFunction = twoClassSummary,
    verboseIter = TRUE             
  )
  
  xgb_tuned <- train(
    as.formula(paste(target_var, "~ .")),
    data = tr_final,
    method = "xgbTree",
    trControl = fit_control,
    tuneGrid = xgb_grid,
    metric = "ROC",
    verbosity = 0
  )
  
  print(xgb_tuned$bestTune)
  
  # Variable Importance for XGBoost
  importance_matrix <- xgb.importance(
    feature_names = colnames(dplyr::select(tr_final, -all_of(target_var))), 
    model = xgb_tuned$finalModel)
  
  xgb_imp <- as.data.frame(importance_matrix) %>%
    dplyr::select(Variable = Feature, Importance = Gain)
  
  print(ggplot(head(xgb_imp, 15), aes(x = reorder(Variable, Importance), y = Importance)) +
          geom_col(fill = "#FF6600") + 
          coord_flip() +
          theme_minimal() +
          labs(title = paste("Top Drivers (Tuned XGBoost):", target_var),
               subtitle = "Importance measured by Gain (Contribution to Model Accuracy)"))
} else if (best_model == "RF") {
  rf_final <- ranger(
    dependent.variable.name = target_var,
    data = tr_final,
    probability = TRUE,
    num.trees = 500, 
    importance = "permutation",
    num.threads = n_cores
  )
  
  rf_imp <- data.frame(Importance = rf_final$variable.importance) %>%
    tibble::rownames_to_column(var = "Variable") %>% 
    arrange(desc(Importance))
  
}

# comparing the two models: base vs tunned model
p_tuned <- predict(xgb_tuned, test_pp_full, type = "prob")[,"Yes"]

p_train_tuned <- predict(xgb_tuned, train_pp_full_norm, type = "prob")[,"Yes"]
thr_tuned <- optimal_threshold(train_pp_full_norm[[target_var]], p_train_tuned)

metrics_initial <- results_final %>% 
  filter(Model == "XGB", Sampling == "Normal") %>%
  mutate(Version = "Initial (Defaults)")

metrics_tuned <- compute_metrics(test_full_raw[[target_var]], p_tuned, thr_tuned, "XGB", "Normal") %>%
  mutate(Version = "Final (Tuned)")

# Combine for comparison
comp_df <- bind_rows(metrics_initial, metrics_tuned) %>%
  dplyr::select(Version, Accuracy, Precision, Recall, F1, Gini, Top_Decile_Lift)

print(comp_df)



# visualisations

# Pivot for plotting
plot_comp <- comp_df %>%
  pivot_longer(cols = -Version, names_to = "Metric", values_to = "Value")

ggplot(plot_comp, aes(x = Metric, y = Value, fill = Version)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(Value, 3)), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Initial (Defaults)" = "#BDC3C7", "Final (Tuned)" = "#E67E22")) +
  theme_minimal() +
  labs(title = "Multi-Metric Performance Lift: Initial vs. Tuned XGBoost",
       subtitle = "Comparison across classification and ranking metrics",
       y = "Score (0-1 Range)", x = "")


# ROC CURVE
p_tuned <- predict(xgb_tuned, test_pp_full, type = "prob")[,"Yes"]

roc_initial <- roc(test_full_raw[[target_var]], predictions_normal[["XGB"]], quiet = TRUE)
roc_tuned <- roc(test_full_raw[[target_var]], p_tuned, quiet = TRUE)

plot(roc_initial, col = "grey", lty = 2, main = "ROC Comparison: Initial vs. Tuned XGBoost")
plot(roc_tuned, col = "orange", lwd = 2, add = TRUE)
legend("bottomright", legend = c("Initial (Defaults)", "Final (Tuned)"), 
       col = c("grey", "orange"), lty = c(2, 1), lwd = c(1, 2))


# Importance of variables in the tunned model 
var_imp <- varImp(xgb_tuned, scale = TRUE)

imp_df <- var_imp$importance %>%
  tibble::rownames_to_column("Variable") %>%
  rename(Importance = Overall) %>%
  arrange(desc(Importance))

# Plot top 15
ggplot(head(imp_df, 15),
       aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Importance, 3)), 
            hjust = -0.2, size = 3.5) + 
  coord_flip() +
  expand_limits(y = max(head(imp_df, 15)$Importance) * 1.1) + 
  theme_minimal() +
  labs(
    title = paste("Variable Importance – Tuned", best_model),
    x = "",
    y = "Importance"
  )

# getting confusion matrix
pred_tuned_class <- factor(
  ifelse(p_tuned >= thr_tuned, "Yes", "No"),
  levels = c("No", "Yes"))

cm_tuned <- confusionMatrix(
  pred_tuned_class,
  test_full_raw[[target_var]],
  positive = "Yes")

cm_tuned
saveRDS(
  list(
    model = xgb_tuned,
    threshold = thr_tuned,
    confusion_matrix = cm_tuned),
  file = "final_xgb_tuned_results.rds")
cm_tuned$byClass[c("Precision", "Recall", "F1")]
cm_tuned$overall["Accuracy"]

cm_table <- as.data.frame(cm_tuned$table)

ggplot(data = cm_table, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 8, fontface = "bold") +
  scale_fill_gradient(low = "#e8f1f8", high = "#337ab7") +
  labs(
    title = "Confusion Matrix: Tuned XGBoost Model",
    subtitle = paste0("Accuracy: ", round(cm_tuned$overall["Accuracy"] * 100, 2), "%"),
    x = "Actual",
    y = "Predicted",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold")
  )


# -------------------------
# Close parallel
stopCluster(cl)
registerDoSEQ()