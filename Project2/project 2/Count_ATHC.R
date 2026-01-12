setwd("C:/SCRIPT/Project2/Datasets")
df <- read.csv("access-to-health-care_national_zaf.csv")
missing_per_column <- colSums(is.na(df))
total_missing <- sum(is.na(df))
cat("Missing values per column:\n")
print(missing_per_column)
cat("\nTotal missing values:", total_missing, "\n\n")
num_duplicates <- sum(duplicated(df))
cat("Number of duplicate rows:", num_duplicates, "\n\n")
outlier_count <- 0
outliers_per_column <- numeric(length = ncol(df))
names(outliers_per_column) <- names(df)
for (col in names(df)) {
  if (is.numeric(df[[col]])) {
    q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    outliers <- df[[col]] < lower | df[[col]] > upper
    outliers_per_column[col] <- sum(outliers, na.rm = TRUE)
    outlier_count <- outlier_count + sum(outliers, na.rm = TRUE)
  }
}
cat("Outliers per numeric column:\n")
print(outliers_per_column)
cat("\nTotal outliers:", outlier_count, "\n\n")
noise_count <- 0
if ("Value" %in% names(df) && "Indicator" %in% names(df)) {
  noise_flags <- df$Value > 100 & !grepl("Total", df$Indicator)
  noise_count <- sum(noise_flags, na.rm = TRUE)
}

cat("Number of potential noise values (Value > 100 and not 'Total'):", noise_count, "\n")