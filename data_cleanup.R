
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
if (!requireNamespace("zoo", quietly = TRUE)) {
  install.packages("zoo")
}
if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}
library(tidyverse)
library(zoo)
library(stringr)

data <- read.csv("data/sales_raw.csv")


#  Missing Values
cols_with_na <- colnames(data)[colSums(is.na(data)) > 0]
data_no_missing <- data %>%
  filter(!apply(is.na(.), 1, any))

median_age <- median(data$Age, na.rm = TRUE)
data$Age[is.na(data$Age)] <- median_age

mode_gender <- names(sort(table(data$Gender), decreasing = TRUE))[1]
data$Gender[is.na(data$Gender)] <- mode_gender

data$Email[is.na(data$Email)] <- "unknown@email.com"

mean_revenue <- mean(data$revenue, na.rm = TRUE)
data$revenue[is.na(data$revenue)] <- mean_revenue


revenue_iqr <- IQR(data$revenue)
revenue_upper_bound <- quantile(data$revenue, 0.75) + 1.5 * revenue_iqr
revenue_lower_bound <- quantile(data$revenue, 0.25) - 1.5 * revenue_iqr
outliers <- data$revenue[data$revenue > revenue_upper_bound | data$revenue < revenue_lower_bound]

# outliers
data$revenue[data$revenue > revenue_upper_bound] <- revenue_upper_bound
data$revenue[data$revenue < revenue_lower_bound] <- revenue_lower_bound

data$Age <- as.integer(data$Age)

data <- data %>%
  mutate(Email = ifelse(!str_detect(Email, "^.+@.+\\..+$"), NA, Email))

#dupes
data <- data[!duplicated(data), ]

# Normalize
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
data$revenue <- normalize(data$revenue)


data$Date <- sample(seq(as.Date('1999/01/01'), as.Date('2022/01/01'), by="day"), 100)
data$Date <- as.Date(ifelse(str_sub(data$Date, start = -2, end = -3) == "-", 
                            paste0("19", str_sub(data$Date, start = -2)), as.character(data$Date)))

#Convert into factor
data$Gender <- as.factor(data$Gender)

data$Age[data$Age < 0] <- NA

# dump
write.csv(data, "data/sales_processed.csv", row.names = FALSE)

