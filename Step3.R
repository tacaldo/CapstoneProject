# ============ STEP 3 — BUILD PERFECT N-GRAMS (SAFE SAMPLING) ============
library(dplyr)
library(tidytext)

# SAFE SAMPLE FUNCTION
safe_sample <- function(x, n) {
  if (length(x) == 0) return(character(0))
  sample(x, min(n, length(x)))
}

set.seed(123)
sample_text <- c(
  safe_sample(readLines("blogs_fixed.txt",   warn = FALSE), 100000),
  safe_sample(readLines("news_fixed.txt",    warn = FALSE), 100000),
  safe_sample(readLines("twitter_fixed.txt", warn = FALSE), 200000)
)

cat("Training on", length(sample_text), "lines\n")

for (n in 5:1) {
  cat("Building", n, "-gram...\n")
  df <- tibble(text = sample_text) %>%
    unnest_tokens(ngram, text, token = "ngrams", n = n) %>%
    filter(!is.na(ngram)) %>%
    count(ngram, sort = TRUE) %>%
    head(switch(n, `5`=15000, `4`=30000, `3`=80000, `2`=150000, 100000))
  
  df$prefix <- ifelse(n == 1, "", sub(" [a-z0-9']+$", "", df$ngram))
  df$word   <- sub("^.* ", "", df$ngram)
  
  saveRDS(df[c("prefix", "word", "n")], paste0("ngram_", n, ".rds"))
  cat(n, "-gram saved:", nrow(df), "rows\n")
}