# Install once if needed
install.packages(c("dplyr", "tidyr", "janitor", "stringr", "readr"))

# Load libraries
library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)


# Load the data
df <- read_csv("C:/Users/faase/OneDrive/Desktop/BIN/Project/Datasets/access-to-health-care_national_zaf.csv", skip = 2)
#Use clean_names() to convert to snake_case and remove special characters.
df <- df %>% clean_names()
#Eliminate duplicated rows to avoid bias and redundancy using distinct(.keep_all = TRUE)
df <- df %>% distinct(.keep_all = TRUE)
#Remove columns that are entirely missing.
df <- df %>% select(where(~ !all(is.na(.))))
#Drop any row containing at least one NA using drop_na().
df <- df %>% drop_na()
#Replace NA in numeric columns with the column median, preserving distribution shape and robustness to outliers
df <- df %>% mutate(across(where(is.numeric),
                           ~ replace_na(., median(., na.rm = TRUE))))
#Remove accidental leading and trailing spaces.

df <- df %>% mutate(across(where(is.character),
                           ~ str_trim(.)))
                         

#Filter out values outside 1.5×IQR bounds for each numeric column to reduce distortion in analysis.
numeric_cols <- df %>% select(where(is.numeric))
limits <- numeric_cols %>%
  summarise(across(everything(),
                   list(lower = ~ quantile(., 0.25) - 1.5 * IQR(.),
                        upper = ~ quantile(., 0.75) + 1.5 * IQR(.))))
for(col in names(numeric_cols)) {
  low  <- limits[[paste0(col, "_lower")]]
  high <- limits[[paste0(col, "_upper")]]
  df   <- df %>% filter(between(.data[[col]], low, high))
}

#Inspect and Save
glimpse(df)  
write_csv(df, "C:/Users/faase/Downloads/Project Datasets-20250906/Project Datasetsaccess-to-health-care_national_zaf_clean.csv")
                           