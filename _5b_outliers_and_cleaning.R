#####
# Panel dataset
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
library(slider)

#load data 
df_weather <- read_csv("data10_df_weather_merged_fixed.csv", show_col_types = FALSE)
df_review_senti <- read_csv("data8_df_review_with_sentiment.csv", show_col_types = FALSE)

df_review_senti_crop <- df_review_senti[c("review_id","vader_compound","vader_pos","vader_neg","vader_neu")]

# merge data into one
big_panel <- df_weather %>%
  left_join(df_review_senti_crop, by = "review_id")

colnames(big_panel)

# ---------------------------------------------------------
# MISSINGS

gg_miss_var(big_panel) + labs(title = "Missing Values per Variable")

## missing weather data - imputation

weather_valid <- big_panel %>% 
  filter(!is.na(PRCP_avg) | !is.na(TMAX))
  
weather_missing <- big_panel %>% 
  filter(is.na(PRCP_avg) & is.na(TMAX))
  
# For each missing row, find the average of the data for that day
city_daily_avg <- weather_valid %>%
    group_by(review_date) %>%
    summarize(
      PRCP_fill = mean(PRCP_avg, na.rm = TRUE),
      TMAX_fill = mean(TMAX, na.rm = TRUE),
      TMIN_fill = mean(TMIN, na.rm = TRUE),
      .groups = "drop")
  
big_panel_imputed <- big_panel %>%
    left_join(city_daily_avg, by = "review_date") %>%
    mutate(
      PRCP_avg = ifelse(is.na(PRCP_avg), PRCP_fill, PRCP_avg),
      TMAX = ifelse(is.na(TMAX), TMAX_fill, TMAX),
      TMIN = ifelse(is.na(TMIN), TMIN_fill, TMIN)
    ) %>%
    select(-ends_with("_fill"))

gg_miss_var(big_panel_imputed) + labs(title = "Missing Values per Variable")

# ---------------------------------------------------------
# ADDING VARIABLES
### weekdays, weekends, quartal 

big_panel_imputed <- big_panel_imputed %>% 
  mutate( 
    is_weekend = if_else(wday(review_date) %in% c(1, 7), 1, 0),
    # quarter factors 
    Quarter = as.factor(quarters(review_date)), 
    Q1 = if_else(Quarter == "Q1", 1, 0),
    Q2 = if_else(Quarter == "Q2", 1, 0), 
    Q3 = if_else(Quarter == "Q3", 1, 0), 
    Q4 = if_else(Quarter == "Q4", 1, 0) )

#### average temperature & laggs

big_panel_imputed <- big_panel_imputed %>% 
  arrange(business_id, review_date) %>% 
  group_by(business_id) %>% 
  mutate( 
    # average temperature 
    avg_temp = (TMAX + TMIN) / 2, 
    # lagged variables 
    PRCP_avg_lag1 = lag(PRCP_avg, 1), 
    SNOW_avg_lag1 = lag(SNOW_avg, 1), 
    SNWD_avg_lag1 = lag(SNWD_avg, 1), 
    TMAX_lag1 = lag(TMAX, 1), 
    TMIN_lag1 = lag(TMIN, 1), 
    avg_temp_lag1 = lag(avg_temp, 1) ) %>% 
  ungroup()

# rolling averages
big_panel_imputed <- big_panel_imputed %>%
  arrange(business_id, review_date) %>%   
  group_by(business_id) %>%
  mutate(
    # Rolling averages for avg_temp
    avg_temp_roll3 = slide_dbl(avg_temp, mean, .before = 2, .complete = TRUE, na.rm = TRUE),
    avg_temp_roll5 = slide_dbl(avg_temp, mean, .before = 4, .complete = TRUE, na.rm = TRUE),
    
    # Rolling averages for precipitation
    PRCP_avg_roll3 = slide_dbl(PRCP_avg, mean, .before = 2, .complete = TRUE, na.rm = TRUE),
    PRCP_avg_roll5 = slide_dbl(PRCP_avg, mean, .before = 4, .complete = TRUE, na.rm = TRUE),
    
    # Rolling averages for temperature extremes
    TMAX_roll3 = slide_dbl(TMAX, mean, .before = 2, .complete = TRUE, na.rm = TRUE),
    TMAX_roll5 = slide_dbl(TMAX, mean, .before = 4, .complete = TRUE, na.rm = TRUE),
    
    TMIN_roll3 = slide_dbl(TMIN, mean, .before = 2, .complete = TRUE, na.rm = TRUE),
    TMIN_roll5 = slide_dbl(TMIN, mean, .before = 4, .complete = TRUE, na.rm = TRUE)
  ) %>%
  ungroup()

# covid effect (first case at Tampa - till end of year after Omicron)
big_panel_imputed <- big_panel_imputed %>%
  mutate(is_covid = ifelse(review_date >= as.Date("2020-03-01") & 
                             review_date <= as.Date("2021-12-31"), 1, 0))

# categorical business attributes 
big_panel_imputed <- big_panel_imputed %>%
  mutate(
    # Specific WiFi types
    d_wifi_free = ifelse(!is.na(wifi) & wifi == "free", 1, 0),
    d_wifi_bin  = ifelse(!is.na(wifi) & wifi %in% c("free", "paid"), 1, 0),
    # Specific Alcohol types
    d_alcohol_full_bar      = ifelse(!is.na(alcohol) & alcohol == "full_bar", 1, 0),
    d_alcohol_beer_and_wine = ifelse(!is.na(alcohol) & alcohol == "beer_and_wine", 1, 0),
    d_alcohol_none          = ifelse(!is.na(alcohol) & alcohol == "none", 1, 0),
    # Noise Levels
    d_noise_quiet     = ifelse(!is.na(noise_level) & noise_level == "quiet", 1, 0),
    d_noise_very_loud = ifelse(!is.na(noise_level) & noise_level == "very_loud", 1, 0))

# ---------------------------------------------------------
# OUTLIER ANALYSIS
colnames(big_panel_imputed)

# IQR outlier function
outlier_iqr_flag <- function(df, var) {
  x <- df[[var]]
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_fence <- Q1 - 1.5 * IQR_val
  upper_fence <- Q3 + 1.5 * IQR_val
  df[[paste0("outlier_", var)]] <- ifelse(!is.na(x) & (x < lower_fence | x > upper_fence), 1, 0)
  return(df)}

# priority variables for flagging
vars_to_flag <- c(
  "business_review_count", "user_review_count", 
  "user_fans", "user_num_friends",
  "business_checkins_pre_review", 
  "PRCP_avg", "TMAX", "TMIN", "avg_temp",
  "vader_compound")


for (v in vars_to_flag) {
  big_panel_imputed <- outlier_iqr_flag(big_panel_imputed, v)}

# Percentage of outliers per variable
outlier_cols <- grep("outlier_", names(big_panel_imputed), value = TRUE)
outlier_summary <- colMeans(big_panel_imputed[outlier_cols]) * 100
print(round(outlier_summary, 2))


# ---------------------------------------------------------
# INACTIVITY

# Find the maximum consecutive days of zero check-ins
inactivity_summary <- big_panel_imputed %>%
  arrange(business_id, review_date) %>%
  group_by(business_id) %>%
  mutate(
    run_id = rleid(business_checkins_pre_review == 0),
    run_length = ave(business_checkins_pre_review == 0, run_id, FUN = length)
  ) %>%
  summarise(max_zero_run = max(run_length, na.rm = TRUE))


Q1_z <- quantile(inactivity_summary$max_zero_run, 0.25)
Q3_z <- quantile(inactivity_summary$max_zero_run, 0.75)
IQR_z <- Q3_z - Q1_z
dynamic_thr <- Q3_z + 1.5 * IQR_z

final_inactivity_threshold <- max(dynamic_thr, 30)

# Flag inactive restaurants
inactive_ids <- inactivity_summary %>%
  filter(max_zero_run >= final_inactivity_threshold) %>%
  pull(business_id)

big_panel_imputed$inactive_flag <- ifelse(big_panel_imputed$business_id %in% inactive_ids, 1, 0)

# percentage of inactive
num_inactive <- length(inactive_ids) 
num_total <- n_distinct(big_panel_imputed$business_id) 
pct_inactive <- round(100 * num_inactive / num_total, 2)
cat("Inactive restaurants:", num_inactive, "of", num_total, "(", pct_inactive, "% )\n")

# ---------------------------------------------------------
# HISTOGRAMS

for (v in vars_to_flag) {
  df <- big_panel_imputed %>% filter(!is.na(.data[[v]]))
  
  p <- ggplot(df, aes(x = .data[[v]])) +
    geom_histogram(bins = 100, fill = "orange", alpha = 0.8, color = "white") +
    labs(
      title = paste("Distribution of", v),
      x = v,y = "Count") +
    theme_minimal(base_size = 14)
ggsave(paste0("distri_var_plots_norm/FULL_", v, ".png"), p, width = 8, height = 5)
}

for (v in vars_to_flag) {
  df <- big_panel_imputed %>% filter(!is.na(.data[[v]]))
  # Calculate 99th percentile to 'zoom' 
  x_limit <- quantile(df[[v]], 0.99, na.rm = TRUE)
  p <- ggplot(df, aes(x = .data[[v]])) +
    geom_histogram(bins = 60, fill = "blue", alpha = 0.8, color = "white") +
    coord_cartesian(xlim = c(min(df[[v]]), x_limit)) +
    labs(
      title = paste("Linear Distribution of", v),
      subtitle = paste("Zoomed to 99th percentile"),
      x = v,y = "Frequency (Count)") +
    theme_minimal(base_size = 14)
  ggsave(paste0("distri_var_plots_norm/99_zoom", v, ".png"), p, width = 8, height = 5)
}

# ---------------------------------------------------------
big_panel_final <- big_panel_imputed %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.) | is.infinite(.), NA, .)))

# Save final dataset
write_csv(big_panel_final, "data11_panel_final.csv")
