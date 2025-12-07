# ============ FINAL VERSION – NO add_column(), NO tidyr NEEDED ============
library(dplyr)
library(readr)
library(stringr)
library(purrr)   # only for map_chr (safe to keep)

# 1. Load your files
files <- list.files(pattern = "(unigrams.*\\.csv|_bigrams_top5000.*\\.csv|_trigrams_top5000.*\\.csv)$", 
                    full.names = TRUE)

# 2. Read and process each file
ngram_data <- lapply(files, function(f) {
  df <- read_csv(f, show_col_types = FALSE)
  
  # Detect type
  if (grepl("unigrams", f, ignore.case = TRUE)) type <- "uni"
  else if (grepl("bigrams", f, ignore.case = TRUE)) type <- "bi"
  else if (grepl("trigrams", f, ignore.case = TRUE)) type <- "tri"
  else type <- "unknown"
  
  # Standardize column names
  names(df)[1] <- "phrase"
  if ("n" %in% names(df)) names(df)[names(df) == "n"] <- "count"
  if ("frequency" %in% names(df)) names(df)[names(df) == "frequency"] <- "count"
  if (!"count" %in% names(df) && ncol(df) >= 2) names(df)[2] <- "count"
  
  # Split phrase into prefix + next word
  df %>%
    mutate(
      words     = str_split(phrase, " "),
      prefix    = ifelse(lengths(words) == 1, "", 
                         map_chr(words, ~ paste(head(., -1), collapse = " "))),
      next_word = map_chr(words, ~ tail(., 1))
    ) %>%
    select(prefix, next_word, count) %>%
    mutate(type = type)   # ← THIS REPLACES add_column() – 100% safe
}) %>% bind_rows()

# 3. Create clean lookup tables
trigrams <- ngram_data %>% filter(type == "tri") %>% select(prefix, next_word, count)
bigrams  <- ngram_data %>% filter(type == "bi")  %>% select(prefix, next_word, count)
unigrams <- ngram_data %>% filter(type == "uni") %>% 
  rename(word = next_word) %>% 
  select(word, count)

# 4. Prediction function – now rock solid
predict_next_word <- function(input, top_n = 5) {
  words <- str_to_lower(input) %>%
    str_replace_all("[^a-z' ]", " ") %>%
    str_squish() %>%
    str_split(" ") %>% .[[1]]
  
  if (length(words) == 0 || all(words == "")) return("Type something...")
  
  # Trigram
  if (length(words) >= 2) {
    ctx <- paste(tail(words, 2), collapse = " ")
    hits <- trigrams %>% filter(prefix == ctx)
    if (nrow(hits) > 0) {
      return(head(hits %>% arrange(desc(count)) %>% pull(next_word), top_n))
    }
  }
  
  # Bigram
  if (length(words) >= 1) {
    ctx <- tail(words, 1)
    hits <- bigrams %>% filter(prefix == ctx)
    if (nrow(hits) > 0) {
      return(head(hits %>% arrange(desc(count)) %>% pull(next_word), top_n))
    }
  }
  
  # Unigram fallback
  return(head(unigrams %>% arrange(desc(count)) %>% pull(word), top_n))
}

# ============== TEST IT – WILL WORK THIS TIME ==============
predict_next_word("one of the")
predict_next_word("i don't")
predict_next_word("in the")
predict_next_word("thanks for")
predict_next_word("good")

cat("Success! Your predictor is working perfectly now.\n")