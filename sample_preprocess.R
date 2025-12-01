
# Load required libraries
library(tm)
library(tidytext)
library(dplyr)
library(stringr)
library(lexicon)  # For profanity_alvarez

# Custom profanity removal function using fixed matching
remove_profanities <- content_transformer(function(x) {
  for (profanity in profanity_alvarez) {
    x <- gsub(profanity, " ", x, fixed = TRUE, ignore.case = TRUE)
  }
  return(x)
})

dir_path <- "C:/R_ProjectsWorkspace/CapstoneFinalProject/Coursera-SwiftKey/final/en_US"
doc_index <- 1

# Get the file path for the selected document
file_path <- list.files(dir_path, full.names = TRUE)[doc_index]

# Parameters for chunked reading and sampling
set.seed(123)  # For reproducibility
total_sample_size <- 10000  # Total lines to sample
chunk_size <- 10000  # Lines to read per chunk
lines_per_chunk <- 1000  # Lines to sample from each chunk
sampled_lines <- character()  # Store sampled lines

# Open file connection
con <- file(file_path, "r", encoding = "UTF-8")

# Read file in chunks and sample
system.time({
  print("Starting chunked reading and sampling...")
  while (length(sampled_lines) < total_sample_size) {
    chunk_lines <- readLines(con, n = chunk_size, warn = FALSE)
    if (length(chunk_lines) == 0) break  # End of file
    chunk_sample_size <- min(lines_per_chunk, length(chunk_lines), total_sample_size - length(sampled_lines))
    if (chunk_sample_size > 0) {
      chunk_sample <- sample(chunk_lines, size = chunk_sample_size)
      sampled_lines <- c(sampled_lines, chunk_sample)
    }
  }
  close(con)  # Close the connection
  print("Finished reading and sampling.")
  
  # Check if enough lines were sampled
  if (length(sampled_lines) == 0) {
    stop("No lines sampled. Check file path or content.")
  }
  
  # Create VCorpus from sampled lines
  single_doc_corpus <- VCorpus(VectorSource(sampled_lines))
  
  # Preprocess with custom profanity filtering
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
  single_doc_corpus <- tm_map(single_doc_corpus, remove_profanities)  # Custom profanity filtering
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
  has_profanity <- any(sapply(profanity_alvarez[1:5], function(p) grepl(p, doc_content_processed, fixed = TRUE, ignore.case = TRUE)))
  cat("Profanities found in processed text (should be FALSE):\n")
  print(has_profanity)
  
  # Save processed content
  writeLines(doc_content_processed, "processed_sample.txt")
  
  print("Finished preprocessing.")
})




# After all your excellent preprocessing...

# Final cleanup of curly quotes and newlines
doc_content_processed <- sapply(single_doc_corpus, content)
doc_content_processed <- iconv(doc_content_processed, to = "ASCII//TRANSLIT")
doc_content_processed <- gsub("[“”]", "\"", doc_content_processed)
doc_content_processed <- gsub("[‘’]", "'", doc_content_processed)
doc_content_processed <- gsub("[-–—]", " - ", doc_content_processed)
doc_content_processed <- gsub("…", "...", doc_content_processed)
doc_content_processed <- gsub("[\n\r\t]", " ", doc_content_processed)
doc_content_processed <- gsub("\\s+", " ", doc_content_processed)
doc_content_processed <- trimws(doc_content_processed)
doc_content_processed <- paste(doc_content_processed, collapse = " ")

# Now generate REAL bigrams
text_df <- data.frame(text = doc_content_processed, stringsAsFactors = FALSE)

bigrams <- text_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE) %>%
  filter(nchar(bigram) > 2)  # Optional: remove very short garbage

cat("Top 20 actual bigrams:\n")
print(head(bigrams, 20))


