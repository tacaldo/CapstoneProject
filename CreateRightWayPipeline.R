# ============ REGENERATE CORRECT N-GRAMS (KEEP STOPWORDS) ============
library(dplyr)
library(tidytext)
library(readr)

# Function to generate top 5000 n-grams from cleaned text (NO stopword removal)
generate_ngrams <- function(clean_text_file, prefix) {
  clean_lines <- readLines(clean_text_file, warn = FALSE)
  
  # Unigrams (full, no stopwords removed)
  tibble(text = clean_lines) %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) %>%
    write_csv(paste0(prefix, "_unigrams_full.csv"))
  
  # Bigrams
  tibble(text = clean_lines) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    count(bigram, sort = TRUE) %>%
    head(5000) %>%
    write_csv(paste0(prefix, "_bigrams_top5000_full.csv"))
  
  # Trigrams
  tibble(text = clean_lines) %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
    count(trigram, sort = TRUE) %>%
    head(5000) %>%
    write_csv(paste0(prefix, "_trigrams_top5000_full.csv"))
}

# Run for each source (adjust paths if needed)
generate_ngrams("blogs_cleaned.txt", "blogs")
generate_ngrams("news_cleaned.txt", "news")
generate_ngrams("twitter_cleaned.txt", "twitter")

cat("New files created! Now re-run the predictor script with these.\n")