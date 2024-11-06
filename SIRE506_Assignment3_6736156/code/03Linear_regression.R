library(ggplot2)
# Normalize ADP by take log
data_clean$ADP_log <- log(data_clean$ADP)

# rs42244285
boxplot(data_clean$ADP ~ data_clean$rs4244285,
        xlab = "rs4244285",
        ylab = "ADP",
        main = "Boxplot of ADP Levels by rs4244285 Genotype",
        col = "blue")

#rs4986893
boxplot(data_clean$ADP ~ data_clean$rs4986893,
        xlab = "rs4986893",
        ylab = "ADP",
        main = "Boxplot of ADP Levels by rs4986893 Genotype",
        col = "red")

# rs662
boxplot(data_clean$ADP ~ data_clean$rs662,
        xlab = "rs662",
        ylab = "ADP",
        main = "Boxplot of ADP Levels by rs662 Genotype",
        col = "green")

# Test association 
linear_A <- lm(ADP ~ rs4244285, data = data_clean)
linear_B <- lm(ADP ~ rs4986893, data = data_clean)
linear_C <- lm(ADP ~ rs662, data = data_clean)

liner_logA <- lm(ADP_log ~ rs4244285, data = data_clean)
liner_logB <- lm(ADP_log ~ rs4986893, data = data_clean)
linear_logC <- lm(ADP_log ~ rs662, data = data_clean)



# Association linear graph in rs42244285'
# old ADP
summary(linear_A)

ggplot(data_clean, aes(x = rs4244285, y = ADP)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Association between ADP and rs4244285",
       x = "rs4244285 Genotype (0, 1, 2)",
       y = "ADP-Induced Platelet Aggregation") +
  theme_minimal()

plot(linear_A$fitted.values, linear_A$residuals,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "blue")

qqnorm(linear_A$residuals)
qqline(linear_A$residuals, col = "blue")

hist(linear_A$residuals, main = "Histogram of Residuals linear_A", xlab = "Residuals linear_A")

# log ADP
summary(liner_logA)

ggplot(data_clean, aes(x = rs4244285, y = ADP_log)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Association between log ADP and rs4244285",
       x = "rs4244285 Genotype (0, 1, 2)",
       y = "ADP-Induced Platelet Aggregation") +
  theme_minimal()


plot(liner_logA$fitted.values, liner_logA$residuals,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "blue")

qqnorm(liner_logA$residuals)
qqline(liner_logA$residuals, col = "blue")

hist(liner_logA$residuals, main = "Histogram of Residuals modelogA", xlab = "Residuals model logA")

# Test more coefficients.


# Association linear graph in rs4986893

summary(linear_B)

ggplot(data_clean, aes(x = rs4986893 , y = ADP)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Association between ADP and rs4986893",
       x = "rs4986893 Genotype (0, 1, 2)",
       y = "ADP-Induced Platelet Aggregation") +
  theme_minimal()

plot(linear_B$fitted.values, linear_B$residuals,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red")

qqnorm(linear_B$residuals)
qqline(linear_B$residuals, col = "red")

#log ADP 
summary(liner_logB)

qqnorm(liner_logB$residuals)
qqline(liner_logB$residuals, col = "red")

ggplot(data_clean, aes(x = rs4986893 , y = ADP_log)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Association between log ADP and rs4986893",
       x = "rs4986893 Genotype (0, 1, 2)",
       y = "ADP-Induced Platelet Aggregation") +
  theme_minimal()


# Association linear graph in rs662

ggplot(data_clean, aes(x = rs662 , y = ADP)) +
  geom_point() +
  geom_smooth(method = "lm", color = "green") +
  labs(title = "Association between ADP and rs4244285",
       x = "rs662 Genotype (0, 1, 2)",
       y = "ADP-Induced Platelet Aggregation") +
  theme_minimal()

plot(linear_C$fitted.values, linear_C$residuals,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "green")

qqnorm(linear_C$residuals)
qqline(linear_C$residuals, col = "green")

# log ADP

summary(linear_logC)

ggplot(data_clean, aes(x = rs662 , y = ADP_log)) +
  geom_point() +
  geom_smooth(method = "lm", color = "green") + 
  labs(title = "Association between log ADP and rs662",
       x = "rs662 Genotype (0, 1, 2)",
       y = "ADP-Induced Platelet Aggregation") +
  theme_minimal()

qqnorm(linear_logC$residuals)
qqline(linear_logC$residuals, col = "green")


# Check SEX and AGE association

linear_gender <-lm(ADP_log ~ SEX, data= data_clean)
linear_age <- lm(ADP_log ~ AGE, data= data_clean)


summary(linear_gender)# no significant
summary(linear_age) # no significant

ggplot(data_clean, aes(x =SEX , y = ADP_log)) +
  geom_point() +
  geom_smooth(method = "lm", color = "purple") + 
  labs(title = "Association between log ADP and SEX",
       x = "SEX 0 =male. 1 =SEX ",
       y = "ADP-Induced Platelet Aggregation") +
  theme_minimal()


ggplot(data_clean, aes(x =AGE , y = ADP_log)) +
  geom_point() +
  geom_smooth(method = "lm", color = "yellow") + 
  labs(title = "Association between log ADP and AGE",
       x = "AGE",
       y = "ADP-Induced Platelet Aggregation") +
  theme_minimal()


# Create complex linear regression
snp_list <- c("rs4244285", "rs4986893", "rs662")

results_list <- list()

for (snp in snp_list) {
  model_sum <- lm(as.formula(paste("ADP_log ~ AGE + SEX +", snp)), data = data_clean)
  results_list[[snp]] <- summary(model_sum)
}
print(results_list[["rs4244285"]]) # significant
print(results_list[["rs4986893"]])# significant
print(results_list[["rs662"]]) # not siginifcant


