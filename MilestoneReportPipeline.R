# ================== LOAD LIBRARIES ==================
library(tm)
library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
library(quanteda)   # optional
library(ggplot2)    # optional

# ================== SET DIRECTORY & FIND FILES ==================
dir_path <- "C:/R_ProjectsWorkspace/CapstoneFinalProject/Coursera-SwiftKey/final/en_US"
files <- list.files(dir_path, pattern = "\\.txt$", full.names = TRUE)

# Check we actually found the three files
if (length(files) == 0) stop("No .txt files found in directory!")

# Optional: set seed once for reproducible sampling across files
set.seed(123)

# ================== MAIN LOOP: ONE FILE AT A TIME ==================
for (file_path in files) {
  
  # Extract nice filename (e.g., "en_US.blogs.txt")
  file_name <- basename(file_path)
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("PROCESSING FILE: ", file_name, "\n")
  cat(rep("=", 60), "\n\n", sep = "")
  
  # ------------------- 1. READ THE FILE -------------------
  con <- file(file_path, "r", encoding = "UTF-8")
  raw_text <- readLines(con, encoding = "UTF-8", warn = FALSE)
  close(con)
  
  cat("Raw lines read     :", length(raw_text), "\n")
  cat("Raw characters     :", sum(nchar(raw_text)), "\n")
  cat("Approx raw words   :", sum(str_count(raw_text, "\\w+")), "\n\n")
  
  # Optional: sample down if file is huge (especially twitter)
  # Comment out or adjust per file if you want full data
  # if (length(raw_text) > 300000) {
  #   raw_text <- sample(raw_text, size = 200000)
  #   cat("Sampled down to    : 200,000 lines (for speed)\n\n")
  # }
  
  # ------------------- 2. CLEAN USING tm -------------------
  corpus <- VCorpus(VectorSource(raw_text))
  
  clean_corpus <- corpus %>%
    tm_map(content_transformer(function(x) iconv(x, to = "ASCII//TRANSLIT"))) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(content_transformer(function(x) gsub("[^[:alnum:][:space:]']", " ", x))) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, c(stopwords("en"), "t", "s")) %>%
    tm_map(stripWhitespace)
  
  clean_lines <- sapply(clean_corpus, as.character)
  
  # ------------------- 3. BASIC STATISTICS -------------------
  data_summary <- data.frame(
    Metric = c("Source File", "Lines (raw)", "Lines (sampled)", "Words (raw)", "Words (cleaned)",
               "Characters (cleaned)", "Unique words", "Avg words per line"),
    Value = c(
      file_name,
      length(readLines(file_path, warn = FALSE)),  # true original line count
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
  
  # ------------------- 4. N-GRAM TABLES -------------------
  # Unigrams
  word_freq <- tibble(text = clean_lines) %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) %>%
    rename(frequency = n) %>%
    mutate(source = file_name)
  
  # Bigrams
  big_freq <- tibble(text = clean_lines) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    filter(!is.na(bigram)) %>%
    count(bigram, sort = TRUE) %>%
    rename(frequency = n) %>%
    mutate(source = file_name)
  
  # Trigrams
  tri_freq <- tibble(text = clean_lines) %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
    filter(!is.na(trigram)) %>%
    count(trigram, sort = TRUE) %>%
    rename(frequency = n) %>%
    mutate(source = file_name)
  
  cat("\nTop 10 words:\n")
  print(head(word_freq, 10))
  cat("\nTop 10 bigrams:\n")
  print(head(big_freq, 10))
  
  # ------------------- 5. EXPORT WITH CLEAR NAMES -------------------
  # Create a short prefix: blogs, news, twitter
  prefix <- sub("en_US\\.(.*)\\.txt", "\\1", file_name)
  
  write.csv(data_summary,            paste0(prefix, "_summary.csv"),           row.names = FALSE)
  write.csv(word_freq,               paste0(prefix, "_unigrams.csv"),          row.names = FALSE)
  write.csv(head(big_freq, 5000),    paste0(prefix, "_bigrams_top5000.csv"),   row.names = FALSE)
  write.csv(head(tri_freq, 5000),    paste0(prefix, "_trigrams_top5000.csv"),  row.names = FALSE)
  
  # write.csv(head(big_freq, 50),    paste0(prefix, "_bigrams_top50.csv"),   row.names = FALSE)
  # write.csv(head(tri_freq, 50),    paste0(prefix, "_trigrams_top50.csv"),  row.names = FALSE)
  
  
  writeLines(clean_lines,            paste0(prefix, "_cleaned.txt"))
  
  cat("\nAll files for", prefix, "saved successfully!\n")
  cat(rep("=", 60), "\n\n", sep = "")
}

cat("ALL THREE FILES PROCESSED SUCCESSFULLY!\n")

