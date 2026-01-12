setwd("C:/SCRIPT/Project2/Datasets")
df <- read.csv("iycf_national_zaf.csv", stringsAsFactors = FALSE)

all_fields <- c("ISO3","DataId","Indicator","Value","Precision","DHS_CountryCode","CountryName",
                "SurveyYear","SurveyId","IndicatorId","IndicatorOrder","IndicatorType",
                "CharacteristicId","CharacteristicOrder","CharacteristicCategory","CharacteristicLabel",
                "ByVariableId","ByVariableLabel","IsTotal","IsPreferred","SDRID","RegionId",
                "SurveyYearLabel","SurveyType","DenominatorWeighted","DenominatorUnweighted",
                "CILow","CIHigh","LevelRank")

for (col in all_fields) {
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