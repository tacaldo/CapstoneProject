# ================== LOAD LIBRARIES ==================
library(tm)          # for VCorpus & classic preprocessing
library(tidytext)    # for unnest_tokens
library(dplyr)
library(stringr)
library(quanteda)    # fastest for raw counts (optional but recommended)
library(ggplot2)     # optional plotting
library(tidyr)    # <-- this gives you drop_na(), pivot_longer(), etc.

# ================== 1. READ & SAMPLE THE DATA ==================
dir_path <- "C:/R_ProjectsWorkspace/CapstoneFinalProject/Coursera-SwiftKey/final/en_US"
files <- list.files(dir_path, pattern = "\\.txt$", full.names = TRUE)

# Example: work with all three files together (or just one)
# raw_text <- unlist(lapply(files, function(f) {
#   con <- file(f, "r", encoding = "UTF-8")
#   lines <- readLines(con, encoding = "UTF-8", warn = FALSE)
#   close(con)
#   return(lines)
# }))

# Example: work with all three files together (or just one)
raw_text <- unlist(lapply(files[1], function(f) {
  con <- file(f, "r", encoding = "UTF-8")
  lines <- readLines(con, encoding = "UTF-8", warn = FALSE)
  close(con)
  return(lines)
}))


# Optional: take a random sample if the full data is too big
# set.seed(123)
# raw_text <- sample(raw_text, size = 200000)   # adjust size as needed

cat("=== BASIC RAW STATISTICS ===\n")
cat("Lines          :", length(raw_text), "\n")
cat("Characters     :", sum(nchar(raw_text)), "\n")
cat("Words (approx) :", sum(str_count(raw_text, "\\w+")), "\n\n")

# ================== 2. CLEAN TEXT USING tm (classic way) ==================
corpus <- VCorpus(VectorSource(raw_text))

clean_corpus <- corpus %>%
  tm_map(content_transformer(function(x) iconv(x, to = "ASCII//TRANSLIT"))) %>%  # remove curly quotes
  tm_map(content_transformer(tolower)) %>%
  tm_map(content_transformer(function(x) gsub("[^[:alnum:][:space:]']", " ", x))) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, c(stopwords("en"), "t", "s")) %>%   # add your custom stopwords
  tm_map(stripWhitespace)

# Convert back to plain character vector (one document per original line)
clean_lines <- sapply(clean_corpus, as.character)

# ================== 3. BASIC COUNTS & TABLES (multiple ways) ==================

# ---- 3a. Using base R / stringr (fast & transparent) ----
data_summary <- data.frame(
  Metric = c("Lines", "Words (raw)", "Words (cleaned)", "Characters (cleaned)",
             "Unique words", "Average words per line"),
  Value = c(
    length(raw_text),
    sum(str_count(raw_text, "\\w+")),
    sum(str_count(clean_lines, "\\w+")),
    sum(nchar(clean_lines)),
    length(unique(unlist(str_split(clean_lines, "\\s+")))),
    round(mean(str_count(clean_lines, "\\w+")), 1)
  ),
  stringsAsFactors = FALSE
)

print(data_summary)

# ---- 3b. Word frequency table (tidytext) ----
tidy_words <- tibble(line = 1:length(clean_lines), text = clean_lines) %>%
  unnest_tokens(word, text)

word_freq <- tidy_words %>%
  count(word, sort = TRUE) %>%
  rename(Frequency = n)

cat("\nTop 20 most frequent words (cleaned):\n")
print(head(word_freq, 20))

# ---- 3c. Bigram & Trigram tables ----
big_freq <- tibble(text = clean_lines) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE) %>%
  drop_na()

tri_freq <- tibble(text = clean_lines) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  count(trigram, sort = TRUE) %>%
  drop_na()

cat("\nTop 15 bigrams:\n")
print(head(big_freq, 15))

cat("\nTop 15 trigrams:\n")
print(head(tri_freq, 15))

# ---- 3d. Using quanteda (super fast for huge corpora) ----
# If you prefer even faster counts:
# qcorp <- corpus(clean_lines)
# word_freq_q   <- dfm(qcorp) %>% colSums() %>% sort(decreasing = TRUE) %>% head(20)
# bigram_dfm    <- dfm(tokens_ngrams(tokens(qcorp), n = 2)) %>% colSums() %>% sort(decreasing = TRUE)

# ================== 4. EXPORT EVERYTHING NICELY ==================
write.csv(data_summary, "summary_statistics.csv", row.names = FALSE)
write.csv(word_freq,   "word_frequencies.csv",   row.names = FALSE)
write.csv(head(big_freq, 1000), "top1000_bigrams.csv", row.names = FALSE)
write.csv(head(tri_freq, 1000), "top1000_trigrams.csv", row.names = FALSE)

# Optional: save the cleaned text for later modeling
writeLines(clean_lines, "cleaned_sample.txt")

cat("\nAll tables exported successfully!\n")