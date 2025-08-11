install.packages("tidytext")

install.packages(c("tm", "ggplot2", "wordcloud", "dplyr"))
install.packages("stringr")


library(tidytext)
library(dplyr)
library(tm)
library(ggplot2)
library(wordcloud)
library(stringr)




#setwd("C:/R_ProjectsWorkspace/CapstoneFinalProject/Coursera-SwiftKey/final/en_US")

# Specify the directory containing text files
dir_path <- "C:/R_ProjectsWorkspace/CapstoneFinalProject/Coursera-SwiftKey/final/en_US"  # Replace with your directory path

# Create a corpus from all text files in the directory
corpus <- Corpus(DirSource(dir_path))

# Inspect the corpus..
summary(corpus)
# inspect(corpus)



cat(head(corpus[[1]]$content, 10))

cat(head(corpus[[2]]$content, 10))

 
file_path <- "C:/R_ProjectsWorkspace/CapstoneFinalProject/Coursera-SwiftKey/final/en_US/en_US.twitter.txt"

line_count <- length(readLines(file_path))
cat(sprintf("en_US.twitter.txt has %d lines\n", line_count))


# Initialize variable to track the longest line
max_length <- 0

# Iterate through each document in the corpus
for (i in 1:length(corpus)) {
  # Get content and split into lines
  doc_content <- corpus[[i]]$content
  lines <- strsplit(doc_content, "\n")[[1]]
  
  # Calculate length of each line and find the maximum
  line_lengths <- nchar(lines)
  doc_max_length <- max(line_lengths, na.rm = TRUE)
  
  # Update global maximum if current document's max is larger
  if (doc_max_length > max_length) {
    max_length <- doc_max_length
    cat(sprintf("Value of i on last update %d \n", i))
    corpus[[i]]$meta
  }
}

# Print the result
cat(sprintf("The longest line in the corpus has %d characters\n", max_length))

corpus["en_US.twitter.txt"]$content



setwd("C:/R_ProjectsWorkspace/CapstoneFinalProject/Coursera-SwiftKey/final/en_US")
# Read lines directly
lines <- readLines("en_US.twitter.txt")

# Split into words and count "love"
words <- unlist(strsplit(lines, "\\s+"))

love_count <- sum(words == "love", na.rm = TRUE)
hate_count <- sum(words == "hate", na.rm = TRUE)


# Print result
cat(sprintf("The word 'love' (lowercase) appears %d times in en_US.twitter.txt\n", love_count))
cat(sprintf("The word 'hate' (lowercase) appears %d times in en_US.twitter.txt\n", hate_count))

cat(sprintf("The words 'love' divided by 'hate' (lowercase) appears %d times in en_US.twitter.txt\n", love_count/hate_count))


# Read lines directly
tweets <- readLines("en_US.twitter.txt")

# Find the first tweet containing "biostats" (case-sensitive)
biostats_index <- grep("\\bbiostats\\b", tweets)[1]

# Print the result
if (!is.na(biostats_index)) {
  biostats_tweet <- tweets[biostats_index]
  cat(sprintf("First tweet containing 'biostats':\n%s\n", biostats_tweet))
} else {
  cat("No tweet containing 'biostats' found in en_US.twitter.txt\n")
}



# Read lines directly
tweets <- readLines("en_US.twitter.txt")

# Define the exact phrase
phrase <- "A computer once beat me at chess, but it was no match for me at kickboxing"

# Count occurrences of the exact phrase
phrase_count <- sum(tweets == phrase, na.rm = TRUE)

# Print the result
cat(sprintf("The phrase '%s' appears %d times in en_US.twitter.txt\n", phrase, phrase_count))


# Create a data frame (if loading from a file, e.g., CSV)
data <- data.frame(doc_id = 1:length(tweets), text = tweets)


dir_path <- "C:/R_ProjectsWorkspace/CapstoneFinalProject/Coursera-SwiftKey/final/en_US"


# Apply the tolower transformation to this new corpus, takes too long
# single_doc_corpus <- tm_map(single_doc_corpus, content_transformer(tolower))


# Example: Split document into smaller parts (if applicable)
# doc_content <- content(single_doc_corpus[[1]])
# chunks <- str_split(doc_content, "\\n", n=100)[[1]]  # Split by newline, adjust as needed
# chunk_lower <- lapply(chunks, tolower)  # Process each chunk
# doc_content_lower <- paste(chunk_lower, collapse="\n")
# content(single_doc_corpus[[1]]) <- doc_content_lower


library(tidytext)
library(dplyr)
library(tm)
library(ggplot2)
library(wordcloud)
library(stringr)
# Install and load required packages
install.packages("quanteda")  # If not already installed
library(quanteda)

dir_path <- "C:/R_ProjectsWorkspace/CapstoneFinalProject/Coursera-SwiftKey/final/en_US"

# Create corpus from directory
corpus <- Corpus(DirSource(dir_path))
# Assuming 'corpus' is your tm Corpus object and you want the first document
doc_index <- 1  # Change this to the index of the document you want
# Extract the single document into a new corpus
single_doc_corpus <- Corpus(VectorSource(content(corpus[[doc_index]])))

# Preprocess
system.time({
  print("Starting preprocessing...")
  doc_content <- content(single_doc_corpus[[1]])
  if (is.null(doc_content) || nchar(doc_content) == 0) {
    stop("Document content is empty or NULL")
  }
  chunks <- unlist(strsplit(doc_content, "\n"))[1:min(100, length(unlist(strsplit(doc_content, "\n"))))]
  chunk_processed <- lapply(chunks, function(text) {
    text <- tolower(text)
    text <- gsub("[[:punct:]]", "", text)
    text <- gsub("[[:digit:]]", "", text)
    text <- removeWords(text, stopwords("en"))
    text <- gsub("\\s+", " ", trimws(text))
    return(text)
  })
  doc_content_processed <- paste(chunk_processed, collapse="\n")
  content(single_doc_corpus[[1]]) <- doc_content_processed
  print("Finished preprocessing.")
})

# Explore: Word count
doc_text <- content(single_doc_corpus[[1]])
words <- unlist(strsplit(doc_text, "\\s+"))
words <- words[words != ""]
cat("Total word count:", length(words), "\n")
cat("Unique word count:", length(unique(words)), "\n")

# Explore: Term frequency
term_freq <- table(words)
term_freq_df <- data.frame(Term=names(term_freq), Frequency=as.integer(term_freq), row.names=NULL)
term_freq_df <- term_freq_df[order(-term_freq_df$Frequency), ]
head(term_freq_df, 10)

# Explore: Word cloud
wordcloud(words=term_freq_df$Term, freq=term_freq_df$Frequency, max.words=100, colors=brewer.pal(8, "Dark2"))

# Save preprocessed corpus
saveRDS(single_doc_corpus, "preprocessed_corpus.rds")


# Check the corpus
length(single_doc_corpus)  # Should be 1
inspect(single_doc_corpus)  # Preview the document
cat("Document size (characters):", nchar(content(single_doc_corpus[[1]])), "\n")


# Convert tm corpus to quanteda corpus
doc_text <- content(single_doc_corpus[[1]])
quanteda_corpus <- corpus(doc_text)

# Tokenize and create 2-grams and 3-grams
tokens <- tokens(quanteda_corpus, what="word", remove_punct=TRUE, remove_numbers=TRUE)

# 2-grams (bigrams)
bigrams <- tokens_ngrams(tokens, n=2)
bigram_dfm <- dfm(bigrams)
bigram_freq <- colSums(bigram_dfm)
bigram_freq_df <- data.frame(
  Bigram = names(bigram_freq),
  Frequency = as.integer(bigram_freq),
  row.names = NULL
)
bigram_freq_df <- bigram_freq_df[order(-bigram_freq_df$Frequency), ]

# 3-grams (trigrams)
trigrams <- tokens_ngrams(tokens, n=3)
trigram_dfm <- dfm(trigrams)
trigram_freq <- colSums(trigram_dfm)
trigram_freq_df <- data.frame(
  Trigram = names(trigram_freq),
  Frequency = as.integer(trigram_freq),
  row.names = NULL
)
trigram_freq_df <- trigram_freq_df[order(-trigram_freq_df$Frequency), ]

# View top 10 2-grams and 3-grams
cat("Top 10 2-grams:\n")
print(head(bigram_freq_df, 10))
cat("\nTop 10 3-grams:\n")
print(head(trigram_freq_df, 10))

saveRDS(bigram_freq_df, "bigram_freq.rds")
saveRDS(trigram_freq_df, "trigram_freq.rds")

write.csv(bigram_freq_df, "bigram_frequencies.csv", row.names=FALSE)
write.csv(trigram_freq_df, "trigram_frequencies.csv", row.names=FALSE)

# 2-gram word cloud
wordcloud(
  words = bigram_freq_df$Bigram,
  freq = bigram_freq_df$Frequency,
  max.words = 50,
  colors = brewer.pal(8, "Dark2")
)
# 3-gram word cloud
wordcloud(
  words = trigram_freq_df$Trigram,
  freq = trigram_freq_df$Frequency,
  max.words = 50,
  colors = brewer.pal(8, "Dark2")
)

# Bar plot for top 10 2-grams
barplot(
  height = head(bigram_freq_df$Frequency, 10),
  names.arg = head(bigram_freq_df$Bigram, 10),
  las = 2,  # Rotate labels
  main = "Top 10 2-grams",
  ylab = "Frequency",
  col = "skyblue"
)
# Bar plot for top 10 3-grams
barplot(
  height = head(trigram_freq_df$Frequency, 10),
  names.arg = head(trigram_freq_df$Trigram, 10),
  las = 2,
  main = "Top 10 3-grams",
  ylab = "Frequency",
  col = "lightgreen"
)




# Preprocess
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)




















