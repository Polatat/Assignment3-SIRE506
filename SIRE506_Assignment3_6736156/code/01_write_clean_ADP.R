library(tidyverse)
library(data.table)
library(ggplot2)
library(e1071)
library(readr)
data <- read_tsv("raw_data/PlateletHW.tsv") # reading table
# Overview of the data
summary(data)

# Checking ADP summary
summary(data$ADP)
# we found that there the minimum value is negative, meaning that it should not be there we should remove it or correct it.

#Visual inspection
# only ADP
ggplot(data, aes(x = ADP)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of ADP Levels", x = "ADP Level", y = "Frequency")

ggplot(data_,aes(y= ADP)) + geom_boxplot()


# Checking distribution of ADP 

skewness_value <- skewness(data$ADP) # the data is right skewness mean > median. However, It can be assumed that the data is normal.
# Moderate right skewness (positive skewness)


# Cleaning ADP values(Tranform negative values in to positve by using abs)
data_clean <- data %>%
  mutate(ADP_abs = abs(ADP))
# Set variables 
data_clean$ADP_abs <- unlist(data_clean$ADP_abs)
data_clean$ADP <- NULL
names(data_clean)[names(data_clean) == "ADP_abs"] <- "ADP"
data_clean <- data_clean[, c("IID", "ADP", setdiff(names(data_clean), c("IID", "ADP")))]

#Write a new clean data
write_tsv(data_clean, "clean_data/PlateletHW_clean.tsv")












