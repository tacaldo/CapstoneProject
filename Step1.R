# ============ STEP 1 — DELETE ALL OLD FILES ============
file.remove(list.files(pattern = "ngram_.*\\.rds|char_5gram.rds|blogs_fixed.txt|news_fixed.txt|twitter_fixed.txt", full.names = TRUE))
cat("Old files deleted\n")

