# ============ BLOCK 2 — BUILD N-GRAMS (SAFE SAMPLING — NO ERRORS) ============
library(dplyr)
library(tidytext)

cat("Loading text files...\n")
blogs   <- readLines("blogs_fixed.txt",   warn = FALSE)
news    <- readLines("news_fixed.txt",    warn = FALSE)
twitter <- readLines("twitter_fixed.txt", warn = FALSE)

cat("Lines: blogs =", length(blogs), ", news =", length(news), ", twitter =", length(twitter), "\n")

# SAFE SAMPLING — NEVER CRASHES
set.seed(123)
safe_sample <- function(x, n) {
  if (length(x) == 0) return(character(0))
  sample(x, min(n, length(x)))
}

sample_text <- c(
  safe_sample(blogs,   100000),
  safe_sample(news,    100000),
  safe_sample(twitter, 200000)
)

cat("Training on", length(sample_text), "lines\n")

for (n in 5:1) {
  cat("Building", n, "-gram...\n")
  df <- tibble(text = sample_text) %>%
    unnest_tokens(ngram, text, token = "ngrams", n = n) %>%
    filter(!is.na(ngram)) %>%
    count(ngram, sort = TRUE) %>%
    head(ifelse(n==5,15000,ifelse(n==4,30000,ifelse(n==3,80000,ifelse(n==2,150000,100000)))))
  
  names(df)[2] <- "count"
  df$prefix <- ifelse(n == 1, "", sub(" [^ ]+$", "", df$ngram))
  df$word   <- sub("^.* ", "", df$ngram)
  
  saveRDS(df[c("prefix", "word", "count")], paste0("ngram_", n, ".rds"))
  cat(n, "-gram saved\n")
}

cat("MODEL BUILT — NO ERRORS!\n")

