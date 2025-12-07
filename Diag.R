# DIAGNOSTIC — THIS TELLS US EVERYTHING
cat("=== DIAGNOSTIC ===\n")
for (n in 5:1) {
  file <- paste0("ngram_", n, ".rds")
  if (!file.exists(file)) {
    cat(file, "DOES NOT EXIST\n")
  } else {
    df <- readRDS(file)
    cat(file, "has", nrow(df), "rows\n")
    if (nrow(df) > 0) {
      cat("  First prefix:", df$prefix[1], "\n")
      cat("  First word:  ", df$word[1], "\n")
      cat("  Column names:", paste(names(df), collapse = ", "), "\n")
    }
  }
}
