#####
# Descriptive stats
#####

rm(list = ls())
setwd("C:/Users/jijia/Desktop/Jijiaxin/VŠ/02_Master/03_Zweite_WS_25-26/DSMA/seminar paper/new_code/more_relaxed")

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(naniar)
library(sf)
library(stringr)
library(slider)
library(ggcorrplot)
library(vtable)
library(psych)
library(factoextra)
library(cluster)
library(mice)
library(corrplot)


# load data 
df <- read_csv("data11_panel_final.csv", show_col_types = FALSE)
summary(df)

# threshold - global mean of reviews 
global_mean_val <- mean(df$review_stars, na.rm = TRUE)

# adding variables
df_ml <- df %>%
  mutate(
    # Target 1: Binary Stars (Behavioral Satisfaction)
    # Threshold global mean of reviews
    satisfied_stars = ifelse(review_stars > global_mean_val, 1, 0),
    
    # Target 2: Binary Sentiment (Textual Satisfaction)
    # Threshold 0.05 is the standard VADER 'positive' cutoff
    satisfied_sentiment = ifelse(vader_compound >= 0.05, 1, 0),
    word_count = str_count(review_text, "\\S+"))


sent_counts <- table(df_ml$satisfied_sentiment)
sent_pct    <- prop.table(sent_counts) * 100
print(round(sent_pct, 2))

star_counts <- table(df_ml$satisfied_stars)
star_pct    <- prop.table(star_counts) * 100
print(round(star_pct, 2))


#----------------------------------
##INPUTATION of business attributes

# Variables to be imputed
mice_vars <- c(
  "outdoor_seating", "delivery", "takeout",
  "reservations", "good_for_groups",
  "table_service", "credit_cards", "price_range")

predictor_vars <- c("business_avg_stars", "business_review_count", "latitude", "longitude")

mice_block <- df_ml %>%
  select(all_of(mice_vars), all_of(predictor_vars)) %>%
  mutate(across(all_of(mice_vars), as.factor)) 

cat("Missing values before MICE:\n")
print(colSums(is.na(mice_block[mice_vars])))

if (any(is.na(mice_block[mice_vars]))) {
  
  set.seed(123)
  
  mice_mod <- mice(
    mice_block,
    m = 3,           
    maxit = 3,      
    method = "rf",
    printFlag = TRUE)
  
  plot(mice_mod) 
  mice_complete <- complete(mice_mod, 1)
  mice_complete <- mice_complete %>%
    mutate(across(all_of(mice_vars), ~as.numeric(as.character(.))))
  df_ml[mice_vars] <- mice_complete[mice_vars]
  
  cat("\nImputation Complete. Missing values now:\n")
  print(colSums(is.na(df_ml[mice_vars])))
}


densityplot(mice_mod, ~outdoor_seating)
densityplot(mice_mod, ~delivery)
densityplot(mice_mod, ~takeout)
densityplot(mice_mod, ~reservations)
densityplot(mice_mod, ~good_for_groups)
densityplot(mice_mod, ~table_service)
densityplot(mice_mod, ~credit_cards)
densityplot(mice_mod, ~price_range)


write.csv(df_ml,"data11a_inputed.csv")

#-----------
# MULTICOLLINEARITY 
#-----------
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(naniar)
library(sf)
library(stringr)
library(slider)
library(ggcorrplot)
library(vtable)
library(psych)
library(factoextra)
library(cluster)
library(mice)
library(corrplot)

setwd("C:/Users/jijia/Desktop/Jijiaxin/VŠ/02_Master/03_Zweite_WS_25-26/DSMA/seminar paper/new_code/more_relaxed")
df_ml  <- read_csv("data11a_inputed.csv",  show_col_types = FALSE)


vars_to_drop_1 <- c(
  "review_id", "user_id", "name", "city", "state",
  "categories", "review_date", "review_text", 
  "postal_code","latitude","longitude",
  
  # raw categorical
  "wifi", "alcohol", "noise_level",
  
  # multicollinearity removals
  "business_tips_pre_review", "business_checkins_pre_review",
  "TMAX", "TMIN", "d_wifi_bin",
  
  # pairwise-zero-variance offenders
  "TOBS",
  
  # redundant seasonal
  "Quarter","Q1",
  
  # too many missing values
  "SNWD_avg_lag1","SNWD_avg",
  
  # dropping not useful outliers
  "outlier_business_checkins_pre_review", "outlier_TMAX", "outlier_TMIN",
  
  # dropping vader related
  "vader_compound", "vader_pos", "vader_neg", "vader_neu", "outlier_vader_compound",
  
  # dropping other not used variables according to the group discussion
  "review_stars", "business_avg_stars","user_avg_stars","business_reviews_pre_review",
  "takeout","good_for_groups","credit_cards","miss_wifi","miss_alcohol","miss_noise",
  "user_review_count","d_alcohol_beer_and_wine", "TMAX_lag1","TMIN_lag1","TMAX_roll3",
  "TMAX_roll5","TMIN_roll3","TMIN_roll5","d_alcohol_none","SNOW_avg","SNOW_avg_lag1",
  "avg_temp_roll5","PRCP_avg_roll5","business_review_count","...1")


df_ml_final <- df_ml[, !(names(df_ml) %in% vars_to_drop_1)]
summary(df_ml_final)

cols_to_check <- colnames(df_ml_final)
df_corr <- df_ml_final %>% dplyr::select(all_of(cols_to_check))

# only numeric variables
df_corr_numeric <- df_corr %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(where(~ sd(.x, na.rm = TRUE) > 0))

colnames(df_corr_numeric)

# inspect only base columns
base_vars <- c(
  "satisfied_stars","satisfied_sentiment",
  "word_count","user_fans", "user_num_friends", 
  "outdoor_seating", "delivery", "reservations", "table_service", 
  "d_wifi_free", "d_alcohol_full_bar",  
  "d_noise_quiet", "d_noise_very_loud", "price_range",
  "PRCP_avg", "avg_temp", "is_weekend", "is_covid")

df_base <- df_corr_numeric[, base_vars]

summary(df_base)

# Correlation matrix
corr_matrix <- cor(df_base,use = "pairwise.complete.obs")

corrplot(corr_matrix, 
         method = "color", 
         type = "upper", 
         tl.col = "black", 
         tl.srt = 45, 
         addCoef.col = "black", 
         number.cex = 0.6)

# identify high multicollinearity (r > 0.8)
high_corr_list <- as.data.frame(as.table(corr_matrix)) %>%
  filter(Freq > 0.8 & Var1 != Var2) %>%
  arrange(desc(Freq))

print("Variables with High Correlation (r > 0.8):")
print(high_corr_list)

sapply(df_ml_final, is.numeric)

gg_miss_var(df_ml_final) + labs(title = "Missing Values per Variable")

df_ml_final_clean <- df_ml_final %>%
  na.omit()

colnames(df_ml_final_clean)

summary(df_ml_final_clean)

#-------------------
# no outliers 
df_ml_no_outliers_clean <- df_ml_final_clean %>%
  filter(
    outlier_business_review_count == 0,
    outlier_user_review_count     == 0,
    outlier_user_fans             == 0,
    outlier_user_num_friends      == 0,
    outlier_PRCP_avg              == 0,
    outlier_avg_temp              == 0,
    inactive_flag                 == 1
  )%>%
  dplyr::select(-starts_with("outlier_"), -inactive_flag)


df_ml_final_clean <- df_ml_final_clean %>%
  dplyr::select(-starts_with("outlier_"), -inactive_flag)

colnames(df_ml_final_clean)
colnames(df_ml_no_outliers_clean)


#-------
# summary stats 
summary(df_ml_final_clean)
summary(df_ml_no_outliers_clean)


st(df_ml_final_clean, 
   title = "Summary Statistics: Full Dataset",
   out = "viewer") 
st(df_ml_no_outliers_clean, 
   title = "Summary Statistics: No Outliers Dataset",
   out = "viewer")


stats_full <- describe(df_ml_final_clean, quant = c(.25, .5, .75))
print(stats_full)
stats_clean <- describe(df_ml_no_outliers_clean, quant = c(.25, .5, .75))
print(stats_clean)

colnames(df_ml_final_clean)


#--------------------
# UNSUPERVISED ML
#--------------------
# business level agrregation
business_feat <- df_ml_final_clean %>%
  group_by(business_id) %>%
  summarise(
    review_volume    = n(),
    avg_word_count   = mean(word_count, na.rm = TRUE),
    avg_price        = mean(price_range, na.rm = TRUE),
    wifi_rate        = mean(d_wifi_free, na.rm = TRUE),
    delivery_rate    = mean(delivery, na.rm = TRUE),
    outdoor_rate     = mean(outdoor_seating, na.rm = TRUE),
    full_bar_rate    = mean(d_alcohol_full_bar, na.rm = TRUE),
    reservation_rate = mean(reservations, na.rm = TRUE),
    quiet_rate       = mean(d_noise_quiet, na.rm = TRUE),
    very_loud_rate   = mean(d_noise_very_loud, na.rm = TRUE)
  ) %>%
  ungroup()

# scaling
numeric_ready <- business_feat %>% 
  dplyr::select(-business_id)

scaled_features <- scale(numeric_ready)


# elbow
p_elbow <- fviz_nbclust(scaled_features, kmeans, method = "wss", linecolor = 'black') +
  geom_vline(xintercept = 5, linetype = "dashed", color = 'red', linewidth = 1) +
  labs(
    title = "Optimal Cluster Selection: Elbow Method",
    x = "Number of Clusters (k)",
    y = "Total Within-Cluster Sum of Squares"
  ) +
  theme_minimal()
print(p_elbow)


p_sil <- fviz_nbclust(scaled_features, kmeans, method = "silhouette", linecolor = 'black') +
  geom_vline(xintercept = 2, linetype = "dashed", color = "red", linewidth = 1) +
  labs(title = "Optimal Cluster Selection: Silhouette Analysis",
       x = "Number of Clusters (k)", y = "Average Silhouette Width") +
  theme_minimal()

print(p_sil)

#------
# clusters 
set.seed(123)
final_km_2 <- kmeans(scaled_features, centers = 2, nstart = 25)
business_feat$business_cluster <- as.factor(final_km_2$cluster)

df_ml_final_clean <- df_ml_final_clean %>%
  left_join(business_feat %>% dplyr::select(business_id, business_cluster), 
            by = "business_id")

# charasterics of clusters
cluster_exploration <- df_ml_final_clean %>%
  group_by(business_cluster) %>%
  summarise(
    N              = n(),
    Mean_Price     = mean(price_range, na.rm = TRUE),
    Avg_Word_Length= mean(word_count, na.rm = TRUE), 
    WiFi_Rate      = mean(d_wifi_free, na.rm = TRUE),
    Full_Bar_Rate  = mean(d_alcohol_full_bar, na.rm = TRUE),
    Satisfied_Rate = mean(satisfied_stars, na.rm = TRUE)
  ) %>%
  arrange(desc(Satisfied_Rate))

print(cluster_exploration)

#---------------------
# no outliers
#---------------------
#---------------------------------------------------------
# UNSUPERVISED ML: NO OUTLIER DATASET
#---------------------------------------------------------

business_feat_no_out <- df_ml_no_outliers_clean %>%
  group_by(business_id) %>%
  summarise(
    review_volume    = n(),
    avg_word_count   = mean(word_count, na.rm = TRUE),
    avg_price        = mean(price_range, na.rm = TRUE),
    
    wifi_rate        = mean(d_wifi_free, na.rm = TRUE),
    delivery_rate    = mean(delivery, na.rm = TRUE),
    outdoor_rate     = mean(outdoor_seating, na.rm = TRUE),
    full_bar_rate    = mean(d_alcohol_full_bar, na.rm = TRUE),
    reservation_rate = mean(reservations, na.rm = TRUE),
    
    quiet_rate       = mean(d_noise_quiet, na.rm = TRUE),
    very_loud_rate   = mean(d_noise_very_loud, na.rm = TRUE)
  ) %>%
  ungroup()

# Scaling
numeric_ready_no_out <- business_feat_no_out %>% 
  dplyr::select(-business_id)

scaled_features_no_out <- scale(numeric_ready_no_out)

# Elbow Method
p_elbow_no_out <- fviz_nbclust(scaled_features_no_out, kmeans, method = "wss", linecolor = 'black') +
  geom_vline(xintercept = 3, linetype = "dashed", color = 'red', linewidth = 1) +
  labs(
    title = "Optimal Cluster Selection: Elbow Method (No Outliers)",
    x = "Number of Clusters (k)",
    y = "Total Within-Cluster Sum of Squares"
  ) +
  theme_minimal()

print(p_elbow_no_out)

# Silhouette Analysis
p_sil_no_out <- fviz_nbclust(scaled_features_no_out, kmeans, method = "silhouette", linecolor = 'black') +
  geom_vline(xintercept = 3, linetype = "dashed", color = "red", linewidth = 1) +
  labs(title = "Optimal Cluster Selection: Silhouette Analysis (No Outliers)",
       x = "Number of Clusters (k)", y = "Average Silhouette Width") +
  theme_minimal()

print(p_sil_no_out)

# Fitting K-means
set.seed(123)
final_km_3_no_out <- kmeans(scaled_features_no_out, centers = 3, nstart = 25)
business_feat_no_out$business_cluster <- as.factor(final_km_3_no_out$cluster)

# Join clusters back to the main clean dataframe
df_ml_no_outliers_clean <- df_ml_no_outliers_clean %>%
  left_join(business_feat_no_out %>% 
              dplyr::select(business_id, business_cluster), 
            by = "business_id")


cluster_validation_no_out <- df_ml_no_outliers_clean %>%
  group_by(business_cluster) %>%
  summarise(
    N               = n(),
    Mean_Price      = mean(price_range, na.rm = TRUE),
    WiFi_Rate       = mean(d_wifi_free, na.rm = TRUE),
    Full_Bar_Rate   = mean(d_alcohol_full_bar, na.rm = TRUE),
    Avg_Word_Count  = mean(word_count, na.rm = TRUE),
    Satisfied_Rate  = mean(satisfied_stars, na.rm = TRUE)
  ) %>%
  arrange(desc(Satisfied_Rate))

print(cluster_validation_no_out)

#--------------
# PCA visualisation 
#--------------
pca_plot <- fviz_cluster(
  list(data = scaled_features, cluster = business_feat$business_cluster),
  geom = "point",
  pointsize = 1.4,
  alpha = 0.6,
  ellipse = TRUE,
  ellipse.type = "convex",
  palette = "jco",
  ggtheme = theme_minimal(base_size = 14)
) +
  labs(
    title = "PCA Projection of Business Clusters",
    x = "PC1",
    y = "PC2"
  )

print(pca_plot)

pca_plot_out <- fviz_cluster(
  list(data = scaled_features_no_out, cluster = business_feat_no_out$business_cluster),
  geom = "point",
  pointsize = 1.4,
  alpha = 0.6,
  ellipse = TRUE,
  ellipse.type = "convex",
  palette = "jco",
  ggtheme = theme_minimal(base_size = 14)
) +
  labs(
    title = "PCA Projection of Business Clusters (no outliers)",
    x = "PC1",
    y = "PC2")


print(pca_plot_out)

pca <- prcomp(scaled_features)
pca$rotation

pca_no <- prcomp(scaled_features_no_out)
pca_no$rotation


#-----------------
# SAVE datasets 

df_ml_final_clean <- df_ml_final_clean %>%
  select(-business_id,-business_cluster)

df_ml_no_outliers_clean <- df_ml_no_outliers_clean %>%
  select(-business_id,-business_cluster)

summary(df_ml_final_clean)
colnames(df_ml_final_clean)
colnames(df_ml_no_outliers_clean)

write_csv(df_ml_final_clean, "data12a_df_ml.csv")
write_csv(df_ml_no_outliers_clean, "data12b_df_ml_no_out.csv")

#----------------
# time variation
df_time <- df_ml %>%
  mutate(month = floor_date(as.Date(review_date), "month")) %>%
  group_by(month) %>%
  summarise(
    mean_star_satisfaction = mean(satisfied_stars, na.rm = TRUE),
    mean_sent_satisfaction = mean(satisfied_sentiment, na.rm = TRUE))

df_time$month <- as.Date(df_time$month)

ggplot(df_time, aes(x = month, y = mean_star_satisfaction)) + 
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-12-31"), 
           ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "black") +
  
  geom_line(aes(color = "Star review"), size = 1.2) + 
  
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "solid", size = 0.8) +
  
  geom_vline(xintercept = c(as.Date("2020-03-01"), as.Date("2021-12-31")), 
             linetype = "dotted", color = "grey40", size = 0.6) +
  
  annotate("text", x = as.Date("2020-12-01"), y = Inf, 
           label = "COVID-19 period", color = "grey40", vjust = 2, fontface = "italic") +
  
  scale_color_manual(values = c("Star review" = "#1F78B4")) + 
  theme_minimal() +
  labs(
    title = "Evolution of satisfaction over time (star review)",
    y = "Average satisfaction",
    x = "Date",
    color = "Indicator")

ggplot(df_time, aes(x = month, y = mean_sent_satisfaction)) + 
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-12-31"), 
           ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "black") +
  
  geom_line(aes(color = "Sentiment review"), size = 1.2) + 
  
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "solid", size = 0.8) +
  
  geom_vline(xintercept = c(as.Date("2020-03-01"), as.Date("2021-12-31")), 
             linetype = "dotted", color = "grey40", size = 0.6) +
  
  annotate("text", x = as.Date("2020-12-01"), y = Inf, 
           label = "COVID-19 period", color = "grey40", vjust = 2, fontface = "italic") +
  
  scale_color_manual(values = c("Sentiment review" = "darkgreen")) + 
  theme_minimal() +
  labs(
    title = "Evolution of satisfaction over time (sentiment review)",
    y = "Average satisfaction",
    x = "Date",
    color = "Indicator")

# covid inspection
df_ml_final_clean %>%
  group_by(is_covid) %>%
  summarise(
    mean_star = mean(satisfied_stars),
    mean_sent = mean(satisfied_sentiment),
    n = n())

# seasonality in quarters
df_ml %>%
  group_by(Quarter) %>%
  summarise(
    mean_star = mean(satisfied_stars),
    mean_sent = mean(satisfied_sentiment),
    n = n())

ggplot(df_ml, aes(factor(Quarter), satisfied_stars)) +
  stat_summary(fun = mean, geom = "bar") +
  theme_minimal() +
  labs(title = "Seasonal variation in satisfaction")

ggplot(df_ml, aes(factor(Quarter), satisfied_sentiment)) +
  stat_summary(fun = mean, geom = "bar") +
  theme_minimal() +
  labs(title = "Seasonal Variation in Satisfaction")






