import pandas as pd
import ast
import os


os.chdir("C:\\Users\\jijia\\Desktop\\Jijiaxin\\VŠ\\02_Master\\03_Zweite_WS_25-26\\DSMA\\seminar paper\\new_code\\more_relaxed")

df_restaurants = pd.read_csv("data5_tampa_restaurants_master_yelp.csv")

print("Loaded dataset with", len(df_restaurants), "rows")

# ============================================================
# 1. ATTRIBUTE PARSING 

def parse_outer_dict(attr):
    if pd.isna(attr):
        return {}
    try:
        return ast.literal_eval(attr)
    except Exception:
        return {}


def maybe_parse_nested(value):
    if not isinstance(value, str):
        return value
    value = value.strip()
    if value.startswith("{") and value.endswith("}"):
        try:
            return ast.literal_eval(value)
        except Exception:
            return value
    return value


def unnest_attributes(attr):
    outer = parse_outer_dict(attr)
    flat = {}

    for k, v in outer.items():
        v = maybe_parse_nested(v)

        if isinstance(v, dict):
            for sk, sv in v.items():
                flat[f"{k}_{sk}"] = sv
        else:
            flat[k] = v

    return flat


parsed_attrs = df_restaurants["attributes"].apply(unnest_attributes)
attributes_df = pd.json_normalize(parsed_attrs)

df = pd.concat(
    [df_restaurants.drop(columns=["attributes"]), attributes_df],
    axis=1)

print("Attributes successfully unnested")


# ============================================================
# 2. UNIVERSAL STRING CLEANER

def clean_string(x):
    if pd.isna(x):
        return pd.NA

    x = str(x).lower()
    x = x.replace("u'", "").replace("'", "").strip()

    if x in ("", "na", "null", "none", "none_selected"):
        return pd.NA

    return x


# ============================================================
# 3. ATTRIBUTE-SPECIFIC CLEANERS

def clean_wifi(x):
    x = clean_string(x)
    if pd.isna(x):
        return pd.NA
    if "free" in x:
        return "free"
    if "paid" in x:
        return "paid"
    if x == "no":
        return "no"
    return pd.NA


def clean_alcohol(x):
    x = clean_string(x)
    if pd.isna(x):
        return pd.NA
    if x == "none":
        return "none"
    if "wine" in x:
        return "beer_and_wine"
    if "full" in x:
        return "full_bar"
    return pd.NA


def clean_noise(x):
    x = clean_string(x)
    if pd.isna(x):
        return pd.NA
    if "quiet" in x:
        return "quiet"
    if "average" in x:
        return "average"
    if x == "loud":
        return "loud"
    if "very" in x:
        return "very_loud"
    return pd.NA


def clean_bool(x):
    x = clean_string(x)
    if pd.isna(x):
        return pd.NA
    if x in ("true", "yes", "1", "t"):
        return 1
    if x in ("false", "no", "0", "f"):
        return 0
    return pd.NA


# ============================================================
# 4. APPLY CLEANING 

df["wifi"]        = df.get("WiFi").apply(clean_wifi)
df["alcohol"]     = df.get("Alcohol").apply(clean_alcohol)
df["noise_level"] = df.get("NoiseLevel").apply(clean_noise)

df["outdoor_seating"] = df.get("OutdoorSeating").apply(clean_bool)
df["delivery"]        = df.get("RestaurantsDelivery").apply(clean_bool)
df["takeout"]         = df.get("RestaurantsTakeOut").apply(clean_bool)
df["reservations"]    = df.get("RestaurantsReservations").apply(clean_bool)
df["good_for_groups"] = df.get("RestaurantsGoodForGroups").apply(clean_bool)
df["table_service"]   = df.get("RestaurantsTableService").apply(clean_bool)
df["credit_cards"]    = df.get("BusinessAcceptsCreditCards").apply(clean_bool)

df["price_range"] = pd.to_numeric(
    df.get("RestaurantsPriceRange2"), errors="coerce"
)


# ============================================================
# 5. MISSINGNESS FLAGS 
df["miss_wifi"]    = df["wifi"].isna().astype(int)
df["miss_alcohol"] = df["alcohol"].isna().astype(int)
df["miss_noise"]   = df["noise_level"].isna().astype(int)


# ============================================================
# 6. FINAL COLUMN SELECTION

base_cols = ["review_id","business_id","user_id","review_stars", "review_text",
             "review_date", "name","city", "state", "postal_code","latitude",
             "longitude","business_avg_stars", "business_review_count",
            "categories","user_review_count","user_fans", "user_avg_stars","user_num_friends",
            "business_reviews_pre_review", "business_tips_pre_review","business_checkins_pre_review"]

attributes_col = ["wifi", "alcohol", "noise_level","outdoor_seating", 
                  "delivery", "takeout", "reservations","good_for_groups", 
                  "table_service", "credit_cards","price_range",
                  "miss_wifi", "miss_alcohol", "miss_noise"]

review_col = ["review_id","business_id","user_id","review_stars","review_text","review_date" ]

final_cols = base_cols + attributes_col
restaurants_clean = df[final_cols]
df_review = df[review_col]

print("Final dataset shape:", restaurants_clean.shape)


# ============================================================
# 7. SAVE OUTPUT

restaurants_clean.to_csv(
    "data6_Yelp_tampa_restaurants_clean_.csv",
    index=False)

df_review.to_csv(
    "data7_df_review.csv",
    index=False)

print("Saved Yelp_tampa_restaurants_clean_.csv")

