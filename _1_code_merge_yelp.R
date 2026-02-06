#############
# YELP DATASET Merging 
#############
rm(list = ls())


# setting working directory
setwd("C:/Users/jijia/Desktop/Jijiaxin/VŠ/02_Master/03_Zweite_WS_25-26/DSMA/seminar paper/new_code/more_relaxed")

# libraries
library(dplyr)
library(readr)
library(tidyr)

# Load the datasets
chunk1_core <- read_csv("data1_business_user_table.csv",na = c("", "NA", "NULL"), show_col_types = FALSE)

chunk2_reviews  <- read_csv("data2_prior_review_counts.csv",  show_col_types = FALSE)
chunk3_tips     <- read_csv("data3_tips_pre.csv",  show_col_types = FALSE)
chunk4_checkins <- read_csv("data4_checkin.csv",  show_col_types = FALSE)

# Merge the data
df_final <- chunk1_core %>%
  left_join(chunk2_reviews,  by = "review_id") %>%
  left_join(chunk3_tips,     by = "review_id") %>%
  left_join(chunk4_checkins, by = "review_id")

# Cleaning
df_final <- df_final %>%
  mutate(
    business_reviews_pre_review = replace_na(business_reviews_pre_review, 0),
    business_tips_pre_review    = replace_na(business_tips_pre_review, 0),
    business_checkins_pre_review = replace_na(business_checkins_pre_review, 0)
  ) %>%
  mutate(review_date = as.Date(review_date))

summary(df_final)

df_clean <- df_final %>%
  mutate(across(c(user_fans, user_review_count, user_num_friends), ~replace_na(.x, 0)))

colnames(df_clean)


# Save
write_csv(df_clean, "data5_tampa_restaurants_master_yelp.csv")

