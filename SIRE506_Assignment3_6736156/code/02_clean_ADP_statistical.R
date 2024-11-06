library(car)
# Load the data with specified column types
data_clean <- read_tsv("clean_data/PlateletHW_clean.tsv")

# Summary clean_ADP
summary(data_clean$ADP)

# Checking clean data histogram
ggplot(data_clean, aes(x =ADP)) + 
  geom_histogram(binwidth = 5, fill = "cyan", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of CLEAN_ADP Levels", x = "ADP Level", y = "Frequency")

# Check a boxplot graph

ggplot(data_clean,aes(y= ADP)) + geom_boxplot()

#Statistical methods

# Check skewness
clean_skewness_value <- skewness(data_clean$ADP) #  right skewness # not appropriate to do Z score checking outliers

# Identify outliers using IQR method  # appropriate way to Check outliers
Q1 <- quantile(data_clean$ADP, 0.25)
Q3 <- quantile(data_clean$ADP, 0.75)
IQR_value <- IQR(data_clean$ADP)
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value
outliers_iqr <- data_clean %>%
  filter(ADP < lower_bound | ADP > upper_bound)   # There is no outliers that less than Q1 - 1.5 * IQR value or more than Q3 + 1.5 * IQR value.

cat("Number of outliers by IQR method:", nrow(outliers_iqr), "\n")  # IQR method is appropriate at this case because ADP has little right skewness.


# Checking The Relationship Between ADP and Resistance
names(data_clean)

scatterplot(Resistance ~ ADP, data=data_clean, reg.line
            = lm, smooth=FALSE)
