install.packages("tidytext")
install.packages(c("tm", "ggplot2", "wordcloud", "dplyr"))
install.packages("stringr")
install.packages("quanteda")  # If not already installed

library(tidytext)
library(dplyr)
library(tm)
library(ggplot2)
library(wordcloud)
library(stringr)
library(quanteda)

dir_path <- "C:/R_ProjectsWorkspace/CapstoneFinalProject/Coursera-SwiftKey/final/en_US"

# Create corpus from directory with explicit UTF-8 encoding
corpus <- Corpus(DirSource(dir_path, encoding = "UTF-8"))

# Create a corpus from all text files in the directory
#corpus <- Corpus(DirSource(dir_path))

# Select the first document
doc_index <- 1
# Use VCorpus to avoid transformation warnings..

single_doc_corpus <- VCorpus(VectorSource(content(corpus[[doc_index]])))

# Preprocess with explicit word boundary preservation
system.time({
  print("Starting preprocessing...")
  
  # Custom transformer to clean SwiftKey-specific noise and preserve word boundaries
  clean_text <- content_transformer(function(x) {
    # Replace hashtags, usernames, and URLs with space or remove
    x <- gsub("#[[:alnum:]]+", " ", x)  # Remove hashtags, replace with space
    x <- gsub("@[[:alnum:]]+", " ", x)  # Remove usernames, replace with space
    x <- gsub("http[s]?://[[:alnum:]]+\\.[[:alnum:]]+[[:alnum:]/]*", " ", x)  # Remove URLs
    # Replace punctuation (e.g., hyphens, apostrophes) with space to avoid concatenation
    x <- gsub("[[:punct:]]", " ", x)
    # Split camelCase (e.g., SouthNorwalk -> South Norwalk)
    x <- gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", x)
    # Replace multiple spaces with single space
    x <- gsub("\\s+", " ", x)
    return(x)
  })
  
  # Apply transformations
  single_doc_corpus <- tm_map(single_doc_corpus, clean_text)
  single_doc_corpus <- tm_map(single_doc_corpus, content_transformer(tolower))
  single_doc_corpus <- tm_map(single_doc_corpus, removeNumbers)
  single_doc_corpus <- tm_map(single_doc_corpus, removeWords, stopwords("en"))
  single_doc_corpus <- tm_map(single_doc_corpus, stripWhitespace)
  single_doc_corpus <- tm_map(single_doc_corpus, content_transformer(trimws))
  
  # Extract content to verify
  doc_content_processed <- content(single_doc_corpus[[1]])
  if (is.null(doc_content_processed) || nchar(doc_content_processed) == 0) {
    stop("Document content is empty or NULL after preprocessing")
  }
  
  # Debug: Inspect first few lines
  lines <- unlist(strsplit(doc_content_processed, "\n"))
  #cat("Sample of first 5 lines after preprocessing:\n")
  #print(head(lines, 5))
  
  # Debug: Check word boundaries in a sample
  sample_words <- unlist(strsplit(lines[1], "\\s+"))
  #cat("First 10 words of first line (to check spacing):\n")
  #print(head(sample_words, 10))
  
  # Save processed content for manual inspection
  writeLines(doc_content_processed, "processed_sample.txt")
  
  print("Finished preprocessing.")
})



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










