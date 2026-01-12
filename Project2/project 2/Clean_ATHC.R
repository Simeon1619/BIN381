setwd("C:/SCRIPT/Project2/Datasets")
df <- read.csv("access-to-health-care_national_zaf.csv")
df <- df[!duplicated(df),]
#make columns name standard
names(df) <- gsub("[^[:alnum:]_]","_",names(df))

#missing values???
missing_counts <- colSums(is.na(df))
print(missing_counts)

for (col in names(df)){
  if (is.numeric(df[[col]])){
    df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
  }
}

for (col in names(df)){
  if (is.numeric(df[[col]])){
    q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    df[[col]][df[[col]] < lower | df[[col]] > upper] <- NA
    df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
  }
}

if ("Value" %in% names(df)){
  df <- df[!(df$Value > 100 & !grepl("Total", df$Indicator)), ]
}

for (col in names(df)) {
  df[[col]] <- gsub("Missing|Don't know|N/A|na", "Unknown", df[[col]])
}

if ("SurveyYear" %in% names(df)) {
  df <- df[df$SurveyYear == 2016, ]
}

# write.csv(df, "cleaned_healthcare_data.csv", row.names = FALSE)
