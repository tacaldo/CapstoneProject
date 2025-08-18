install.packages("tidytext")
install.packages(c("tm", "ggplot2", "wordcloud", "dplyr"))
install.packages("stringr")
install.packages("quanteda")  # If not already installed



# Load required libraries
library(tm)
library(tidytext)
library(dplyr)
library(stringr)
library(lexicon)  # For profanity_alvarez

dir_path <- "C:/R_ProjectsWorkspace/CapstoneFinalProject/Coursera-SwiftKey/final/en_US"
doc_index <- 1

# Get the file path for the selected document
file_path <- list.files(dir_path, full.names = TRUE)[doc_index]

# Read a random sample of lines to reduce memory usage
set.seed(123)  # For reproducibility
sample_size <- 10000  # Adjust as needed (e.g., 1% of ~1M lines)
# Count total lines using readLines (cross-platform)
total_lines <- length(readLines(file_path, encoding = "UTF-8", n = -1, warn = FALSE))
if (total_lines == 0) {
  stop("File is empty or cannot be read.")
}
sample_indices <- sample(1:total_lines, size = min(sample_size, total_lines), replace = FALSE)
sample_lines <- readLines(file_path, encoding = "UTF-8", n = total_lines)[sample_indices]

# Create VCorpus from sampled lines
single_doc_corpus <- VCorpus(VectorSource(sample_lines))

# Preprocess with profanity filtering
system.time({
  print("Starting preprocessing...")
  
  # Custom transformer to clean SwiftKey-specific noise
  clean_text <- content_transformer(function(x) {
    x <- gsub("#[[:alnum:]]+", " ", x)
    x <- gsub("@[[:alnum:]]+", " ", x)
    x <- gsub("http[s]?://[[:alnum:]]+\\.[[:alnum:]]+[[:alnum:]/]*", " ", x)
    x <- gsub("[[:punct:]]+(?<!')", " ", x, perl = TRUE)
    x <- gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", x)
    x <- gsub("\\s+", " ", x)
    return(x)
  })
  
  # Apply transformations
  single_doc_corpus <- tm_map(single_doc_corpus, clean_text)
  single_doc_corpus <- tm_map(single_doc_corpus, content_transformer(tolower))
  single_doc_corpus <- tm_map(single_doc_corpus, removeNumbers)
  custom_stopwords <- c(stopwords("en"), "t")
  single_doc_corpus <- tm_map(single_doc_corpus, removeWords, custom_stopwords)
  #single_doc_corpus <- tm_map(single_doc_corpus, removeWords, profanity_alvarez)  # Profanity filtering
  single_doc_corpus <- tm_map(single_doc_corpus, stripWhitespace)
  single_doc_corpus <- tm_map(single_doc_corpus, content_transformer(trimws))
  
  # Combine processed lines into a single string
  doc_content_processed <- paste(sapply(single_doc_corpus, content), collapse = "\n")
  if (is.null(doc_content_processed) || nchar(doc_content_processed) == 0) {
    stop("Document content is empty or NULL after preprocessing")
  }
  
  # Debug: Inspect first few lines
  lines <- unlist(strsplit(doc_content_processed, "\n"))
  cat("Sample of first 5 lines after preprocessing:\n")
  print(head(lines, 5))
  
  # Debug: Check word boundaries
  sample_words <- unlist(strsplit(lines[1], "\\s+"))
  cat("First 10 words of first line:\n")
  print(head(sample_words, 10))
  
  # Debug: Check for profanities
  has_profanity <- any(sapply(profanity_alvarez[1:5], function(p) grepl(p, doc_content_processed, ignore.case = TRUE)))
  cat("Profanities found in processed text (should be FALSE):\n")
  print(has_profanity)
  
  # Save processed content
  writeLines(doc_content_processed, "processed_sample.txt")
  
  print("Finished preprocessing.")
})

# Generate bigram table with custom tokenization
text_df <- data.frame(text = doc_content_processed, stringsAsFactors = FALSE)
bigrams <- text_df %>%
  unnest_tokens(bigram, text, token = "regex", pattern = "\\b[[:alnum:]]+('[[:alnum:]]+)?\\b") %>%
  count(bigram, sort = TRUE)
cat("Top 10 bigrams:\n")
print(head(bigrams, 10))




text_df <- data.frame(text = doc_content_processed, stringsAsFactors = FALSE)
bigrams <- text_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)
print(head(bigrams, 10))



# Testing -----
library(tidytext)
library(dplyr)

sentence <- tibble(text = "the cat sat on the mat")
bigrams <- sentence %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams %>%
  count(bigram) %>%
  mutate(prob = n / sum(n))


install.packages("text2vec")
library(text2vec)

tokens <- word_tokenizer("the cat sat on the mat")
it <- itoken(tokens, progress_bar = FALSE)
vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(it, vectorizer)

# ------------





# Explore: Word count
doc_text <- content(single_doc_corpus[[1]])
words <- unlist(strsplit(doc_text, "\\s+"))
words <- words[words != ""]
cat("Total word count:", length(words), "\n")
cat("Unique word count:", length(unique(words)), "\n")










