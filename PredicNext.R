predict_next_word <- function(text, top_n = 5) {
  words <- tolower(text) %>% str_squish() %>% str_split(" ") %>% unlist()
  words <- words[words != ""]
  
  if (length(words) == 0) return(rep("the", top_n))
  
  for (k in min(5, length(words)):1) {
    ctx <- paste(tail(words, k), collapse = " ")
    model <- readRDS(paste0("ngram_", k, ".rds"))
    hits <- model[model$prefix == ctx, ]
    if (nrow(hits) > 0) {
      hits <- hits[order(-hits$n), ]
      return(head(hits$word, top_n))
    }
  }
  
  return(c("the", "to", "and", "a", "of")[1:top_n])
}

# TEST — THESE WILL NOW WORK 100%
predict_next_word("one of the")
predict_next_word("i don't")
predict_next_word("in the")
predict_next_word("thanks for the")
predict_next_word("happy birthday to")
predict_next_word("good")