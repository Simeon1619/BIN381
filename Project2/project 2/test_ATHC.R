setwd("C:/SCRIPT/Project2/Datasets")
df <- read.csv("access-to-health-care_national_zaf.csv", stringsAsFactors = FALSE)
for (col in names(df)) {
  df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
}
numeric_cols <- names(df)[sapply(df, is.numeric)]
for (i in 1:(length(numeric_cols) - 1)) {
  for (j in (i + 1):length(numeric_cols)) {
    col1 <- numeric_cols[i]
    col2 <- numeric_cols[j]
    
    x <- df[[col1]]
    y <- df[[col2]]
    
    valid <- complete.cases(x, y)
    x <- x[valid]
    y <- y[valid]
    
    if (length(x) >= 10) {
      result <- cor.test(x, y)
      cat("\n----------------------------------------\n")
      cat("Correlation test between:", col1, "and", col2, "\n")
      cat("Correlation coefficient:", round(result$estimate, 4), "\n")
      cat("p-value:", format(result$p.value, scientific = TRUE), "\n")
      cat("95% CI:", paste(round(result$conf.int[1], 4), "to", round(result$conf.int[2], 4)), "\n")
    }
  }
}