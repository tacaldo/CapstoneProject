# ============ FINAL FIX — REBUILD N-GRAMS CORRECTLY ONCE AND FOR ALL ============
library(dplyr)
library(tidytext)
library(stringr)

cat("Loading text...\n")
blogs   <- readLines("blogs_fixed.txt",   warn = FALSE)
news    <- readLines("news_fixed.txt",    warn = FALSE)
twitter <- readLines("twitter_fixed.txt", warn = FALSE)

set.seed(123)
sample_text <- c(
  sample(blogs,   min(80000, length(blogs))),
  sample(news,    min(80000, length(news))),
  sample(twitter, min(160000, length(twitter)))
)

cat("Training on", length(sample_text), "lines\n")

# BUILD PERFECT N-GRAMS — THIS IS THE CORRECT WAY
for (n in 5:1) {
  cat("Building", n, "-gram...\n")
  
  # Create n-grams
  ngrams <- tibble(text = sample_text) %>%
    unnest_tokens(ngram, text, token = "ngrams", n = n) %>%
    filter(!is.na(ngram)) %>%
    count(ngram, sort = TRUE)
  
  keep <- switch(n, `5`=10000, `4`=20000, `3`=50000, `2`=80000, 50000)
  ngrams <- head(ngrams, keep)
  
  # THIS IS THE KEY FIX — SPLIT CORRECTLY
  split_ngram <- str_split_fixed(ngrams$ngram, " ", n)
  ngrams$prefix <- apply(split_ngram[, 1:(n-1), drop = FALSE], 1, paste, collapse = " ")
  ngrams$word   <- split_ngram[, n]
  
  # For unigrams, prefix is empty
  if (n == 1) ngrams$prefix <- ""
  
  final_df <- ngrams[c("prefix", "word", "n")]
  saveRDS(final_df, paste0("ngram_", n, ".rds"))
  cat(n, "-gram saved:", nrow(final_df), "rows | Example: '", final_df$prefix[1], "' → '", final_df$word[1], "'\n")
}

print("Check this one — should show real phrases:")
print(head(readRDS("ngram_3.rds")))


print(head(readRDS("ngram_2.rds")))
