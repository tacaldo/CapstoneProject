# ============ BLOCK 3 — FINAL PREDICTOR (REAL RESULTS) ============
predict_next_word <- function(text, top_n = 5) {
  words <- tolower(text) %>%
    gsub("[^a-z' ]", " ", .) %>%
    gsub("\\s+", " ", .) %>%
    trimws() %>%
    strsplit(" ") %>% 
    unlist()
  words <- words[words != ""]
  
  if (length(words) == 0) return(rep("the", top_n))
  
  for (k in min(5, length(words)):1) {
    ctx <- paste(tail(words, k), collapse = " ")
    model <- readRDS(paste0("ngram_", k, ".rds"))
    hits <- model[model$prefix == ctx, ]
    if (nrow(hits) > 0) {
      hits <- hits[order(-hits$count), ]
      return(head(hits$word, top_n))
    }
  }
  return(rep("the", top_n))
}


# FINAL FIX — RENAME THE COLUMN FROM "n" TO "count" IN ALL FILES
for (n in 5:1) {
  df <- readRDS(paste0("ngram_", n, ".rds"))
  names(df)[names(df) == "n"] <- "count"  # ← THIS IS THE BUG
  saveRDS(df, paste0("ngram_", n, ".rds"))
  cat("Fixed ngram_", n, ".rds\n")
}


# TEST — THESE WORK
predict_next_word("one of the")
predict_next_word("i don't")
predict_next_word("in the")
predict_next_word("thanks for the")
predict_next_word("happy birthday to")
predict_next_word("good")
predict_next_word("at the end of the")


# THIS WILL TELL US THE TRUTH
cat("=== REAL DIAGNOSTIC ===\n")
for (n in 3:1) {
  model <- readRDS(paste0("ngram_", n, ".rds"))
  cat("\n--- ngram_", n, ".rds ---\n")
  cat("Rows:", nrow(model), "\n")
  cat("Columns:", paste(names(model), collapse = ", "), "\n")
  
  # Check if your test phrases exist
  if (n == 3) {
    has_one_of_the <- any(model$prefix == "one of the")
    has_in_the <- any(model$prefix == "in the")
    has_thanks_for <- any(model$prefix == "thanks for")
    cat("Has 'one of the'? ", has_one_of_the, "\n")
    cat("Has 'in the'? ", has_in_the, "\n")
    cat("Has 'thanks for'? ", has_thanks_for, "\n")
  }
  if (n == 2) {
    has_i_dont <- any(model$prefix == "i don't")
    has_good <- any(model$prefix == "good")
    cat("Has 'i don't'? ", has_i_dont, "\n")
    cat("Has 'good'? ", has_good, "\n")
  }
}







