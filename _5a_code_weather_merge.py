import os
import pandas as pd
import numpy as np
from scipy.spatial import KDTree

os.chdir("C:\\Users\\jijia\\Desktop\\Jijiaxin\\VŠ\\02_Master\\03_Zweite_WS_25-26\\DSMA\\seminar paper\\new_code\\more_relaxed")

df_restaurants = pd.read_csv("data6_Yelp_tampa_restaurants_clean_.csv")
df_weather = pd.read_csv("data9_weather_df_tampa.csv")

# 1. Prepare Dates
df_restaurants['review_date'] = pd.to_datetime(df_restaurants['review_date'])
df_weather['DATE'] = pd.to_datetime(df_weather['DATE'])

# 2. Get Unique Weather Grid Centers
# We find the center of each weather square to calculate distance
df_grid = df_weather[['lat_min', 'lat_max', 'long_min', 'long_max']].drop_duplicates()
df_grid['center_lat'] = (df_grid['lat_min'] + df_grid['lat_max']) / 2
df_grid['center_long'] = (df_grid['long_min'] + df_grid['long_max']) / 2

# 3. Use KDTree to find the closest grid center for every restaurant
tree = KDTree(df_grid[['center_lat', 'center_long']].values)
distances, indices = tree.query(df_restaurants[['latitude', 'longitude']].values)

# Assign the nearest grid bounds to each restaurant
df_restaurants['matched_lat_min'] = df_grid.iloc[indices]['lat_min'].values
df_restaurants['matched_long_min'] = df_grid.iloc[indices]['long_min'].values

# 4. Merge on Date AND the matched grid coordinates
df_final = pd.merge(
    df_restaurants, 
    df_weather, 
    left_on=['review_date', 'matched_lat_min', 'matched_long_min'], 
    right_on=['DATE', 'lat_min', 'long_min'], 
    how='left'
)

# 5. Cleanup
drop_cols = ['DATE', 'lat_min', 'lat_max', 'long_min', 'long_max', 'matched_lat_min', 'matched_long_min']
df_final = df_final.drop(columns=drop_cols)

df_final.to_csv("data10_df_weather_merged_fixed.csv", index=False)
print(f"Done! Final rows: {len(df_final)}. Check for NAs now: {df_final['PRCP_avg'].isna().sum()}")