import os
import pandas as pd
import numpy as np
import re
from multiprocessing import Pool, cpu_count, freeze_support
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
from tqdm import tqdm


def clean_text(text):
    if pd.isna(text):
        return ""
    text = re.sub(r"[\r\n]+", " ", text)
    return re.sub(r"\s+", " ", text).strip()


def fix_utf8(text):
    try:
        text.encode("utf-8")
        return text
    except UnicodeEncodeError:
        return text.encode("utf-8", errors="ignore").decode("utf-8")


def vader_worker(batch):
    analyzer = SentimentIntensityAnalyzer()
    results = []

    for review_id, text in batch:
        scores = analyzer.polarity_scores(text)
        results.append({
            "review_id": review_id,
            "vader_compound": scores["compound"],
            "vader_pos": scores["pos"],
            "vader_neg": scores["neg"],
            "vader_neu": scores["neu"]
        })
    return results


if __name__ == "__main__":
    freeze_support()

    # Set working directory
    os.chdir(
        "C:\\Users\\jijia\\Desktop\\Jijiaxin\\VŠ\\02_Master\\03_Zweite_WS_25-26\\DSMA\\seminar paper\\new_code\\more_relaxed"
    )

    # Load review data
    reviews = pd.read_csv("data7_df_review.csv")

    reviews["review_date"] = pd.to_datetime(reviews["review_date"], errors="coerce")
    reviews = reviews.drop_duplicates(subset="review_id")

    reviews["text_clean"] = reviews["review_text"].apply(clean_text)
    reviews["text_clean"] = reviews["text_clean"].apply(fix_utf8)

    # Prepare multiprocessing input
    chunk_size = 2000
    data = list(zip(reviews["review_id"], reviews["text_clean"]))

    chunks = [
        data[i:i + chunk_size]
        for i in range(0, len(data), chunk_size)
    ]

    n_cores = min(4, cpu_count())
    print(f"Running VADER in {len(chunks)} chunks using {n_cores} cores.")

    with Pool(processes=n_cores) as pool:
        sentiment_chunks = list(
            tqdm(pool.imap(vader_worker, chunks), total=len(chunks))
        )

    # Flatten results
    sentiment_results = [row for chunk in sentiment_chunks for row in chunk]
    sentiment_df = pd.DataFrame(sentiment_results)

    reviews_final = reviews.merge(
        sentiment_df,
        on="review_id",
        how="left"
    )

    reviews_final.to_csv("data8_df_review_with_sentiment.csv", index=False)
    print("VADER sentiment analysis complete.")
