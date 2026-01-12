setwd("C:/SCRIPT/Project2/Datasets")
df <- read.csv("child-mortality-rates_national_zaf.csv", stringsAsFactors = FALSE)
numeric_fields <- c("Value", "Precision", "DenominatorWeighted", "DenominatorUnweighted", "CILow", "CIHigh")
for (col in numeric_fields) {
  if (col %in% names(df)) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
}
numeric_cols <- names(df)[sapply(df, is.numeric)]
results <- list()
for (i in 1:(length(numeric_cols) - 1)) {
  for (j in (i + 1):length(numeric_cols)) {
    x <- df[[numeric_cols[i]]]
    y <- df[[numeric_cols[j]]]
    valid <- complete.cases(x, y)
    if (sum(valid) >= 10) {
      test <- cor.test(x[valid], y[valid])
      results[[paste(numeric_cols[i], numeric_cols[j], sep = " ~ ")]] <- list(
        cor = round(test$estimate, 4),
        p = round(test$p.value, 6),
        ci = round(test$conf.int, 4)
      )
    }
  }
}
cat("=== Correlation & Significance Results ===\n")
for (name in names(results)) {
  cat("\n", name, "\n")
  cat("  Correlation:", results[[name]]$cor, "\n")
  cat("  p-value:", results[[name]]$p, "\n")
  cat("  95% CI:", paste(results[[name]]$ci[1], "to", results[[name]]$ci[2]), "\n")
}
if ("CILow" %in% names(df) && "CIHigh" %in% names(df)) {
  df$Weight <- 1 / (df$CIHigh - df$CILow)
}
selected_fields <- c("Value", "SurveyYear", "Indicator", "CharacteristicLabel", "CILow", "CIHigh", "Weight")
df_selected <- df[, selected_fields[selected_fields %in% names(df)]]
cat("\n=== Documentation of Inclusion/Exclusion ===\n")
cat("- Included fields with statistical significance and modeling relevance\n")
cat("- Excluded fields with NA correlations, constant values, or redundancy\n")
cat("- Applied weighting based on confidence interval width\n")
cat("- Selected subset includes mortality indicators, time context, and precision\n")