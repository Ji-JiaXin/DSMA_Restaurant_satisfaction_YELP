# Wordclouds

rm(list = ls())
set.seed(123) 


library(reticulate)

use_python("C:/Users/jijia/AppData/Local/Programs/Python/Python312/python.exe", required = TRUE)

python_code <- "
import os
import re
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from wordcloud import WordCloud
import nltk
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize

nltk.download('punkt')
nltk.download('stopwords')
stop_words = set(stopwords.words('english'))

os.chdir('C:/Users/jijia/Desktop/Jijiaxin/VŠ/02_Master/03_Zweite_WS_25-26/DSMA/seminar paper/new_code/more_relaxed')

df = pd.read_csv('data8_df_review_with_sentiment.csv')

# sentiment labels
df['sentiment_label'] = np.where(
    df['vader_compound'] >= 0.05, 'positive',
    np.where(df['vader_compound'] <= -0.05, 'negative', 'neutral'))

# words that are associated with common reviews
domain_stopwords = {
    'restaurant', 'food', 'place', 'table', 'menu', 'server', 'servers',
    'staff', 'waiter', 'waitress', 'service','customer',
    'order', 'ordered', 'ordering','told','got','came',
    'meal', 'dinner', 'lunch', 'breakfast',
    'dish', 'plate', 'plates','one','asked','said',
    'bar', 'drink', 'drinks','sushi',
    'pizza', 'burger', 'chicken', 'steak',
    'eat', 'eating','location', 'time', 'day'}

def preprocess_for_wordcloud(text):
    if pd.isna(text):
        return ''
    text = text.lower()
    text = re.sub(r'[^a-z\\s]', '', text)
    tokens = word_tokenize(text)
    tokens = [t for t in tokens if t not in stop_words and t not in domain_stopwords and len(t) > 2]
    return ' '.join(tokens)

df['wc_text'] = df['text_clean'].apply(preprocess_for_wordcloud)

positive_text = ' '.join(df.loc[df['sentiment_label'] == 'positive', 'wc_text'])
negative_text = ' '.join(df.loc[df['sentiment_label'] == 'negative', 'wc_text'])

# Word clouds
wc_positive = WordCloud(width=900, height=450, background_color='white', max_words=150, colormap='Greens').generate(positive_text)
wc_negative = WordCloud(width=900, height=450, background_color='white', max_words=150, colormap='Reds').generate(negative_text)

plt.figure(figsize=(12, 6))
plt.imshow(wc_positive, interpolation='bilinear')
plt.axis('off')
plt.title('Positive Reviews', fontsize=16)
plt.savefig('wordcloud_positive.png', dpi=300)

plt.figure(figsize=(12, 6))
plt.imshow(wc_negative, interpolation='bilinear')
plt.axis('off')
plt.title('Negative Reviews', fontsize=16)
plt.savefig('wordcloud_negative.png', dpi=300)

print('Word clouds generated and saved successfully.')
"

py_run_string(python_code)