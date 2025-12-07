# ================== SAFELY READ AND COMBINE N-GRAMS ==================
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(forcats)
library(gridExtra)
library(scales)

csv_files <- list.files(pattern = "\\.csv$", full.names = TRUE)

# --- Helper function to read a type safely ---
read_ngrams <- function(pattern, ngram_col) {
  files <- csv_files[grepl(pattern, csv_files)]
  if (length(files) == 0) {
    message("No ", pattern, " files found → skipping plot")
    return(NULL)
  }
  message("Found ", length(files), " ", pattern, " files")
  
  do.call(rbind, lapply(files, function(f) {
    df <- read_csv(f, show_col_types = FALSE)
    src <- sub("^(blogs|news|twitter).*", "\\1", basename(f))
    df$source <- src
    # Make sure the column has the expected name
    if (!ngram_col %in% names(df)) {
      possible <- names(df)[grepl("gram|word", names(df), ignore.case = TRUE)]
      if (length(possible) > 0) {
        df[[ngram_col]] <- df[[possible[1]]]
      }
    }
    df
  })) %>% select(any_of(c(ngram_col, "frequency", "source")))
}

# --- Now safely extract each type ---
unigrams <- read_ngrams("_unigrams\\.csv", "word")
bigrams  <- read_ngrams("_bigrams", "bigram")
trigrams <- read_ngrams("_trigrams", "trigram")

# --- Only create plots if data exists ---
if (!is.null(unigrams) && nrow(unigrams) > 0) {
  top10_words <- unigrams %>%
    group_by(source) %>%
    slice_max(frequency, n = 10) %>%
    ungroup() %>%
    mutate(word = fct_reorder(word, frequency))
  
  p1 <- ggplot(top10_words, aes(x = frequency, y = word, fill = source)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~source, scales = "free_y") +
    labs(title = "Top 10 Words by Source", x = "Frequency", y = NULL) +
    theme_minimal(base_size = 13)
  print(p1)
  ggsave("top10_words.png", p1, width = 11, height = 7, dpi = 300)
} else {
  message("Skipping word plot – no unigram data")
}

if (!is.null(bigrams) && nrow(bigrams) > 0) {
  top10_bigrams <- bigrams %>%
    group_by(source) %>%
    slice_max(frequency, n = 10) %>%
    ungroup() %>%
    mutate(bigram = fct_reorder(bigram, frequency))
  
  p2 <- ggplot(top10_bigrams, aes(x = frequency, y = bigram, fill = source)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~source, scales = "free_y") +
    labs(title = "Top 10 Bigrams by Source", x = "Frequency", y = NULL) +
    theme_minimal(base_size = 13)
  print(p2)
  ggsave("top10_bigrams.png", p2, width = 11, height = 7, dpi = 300)
} else {
  message("Skipping bigram plot – no bigram data found")
}

if (!is.null(trigrams) && nrow(trigrams) > 0) {
  top10_trigrams <- trigrams %>%
    group_by(source) %>%
    slice_max(frequency, n = 10) %>%
    ungroup() %>%
    mutate(trigram = fct_reorder(trigram, frequency))
  
  p3 <- ggplot(top10_trigrams, aes(x = frequency, y = trigram, fill = source)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~source, scales = "free_y") +
    labs(title = "Top 10 Trigrams by Source", x = "Frequency", y = NULL) +
    theme_minimal(base_size = 13)
  print(p3)
  ggsave("top10_trigrams.png", p3, width = 11, height = 7, dpi = 300)
}

