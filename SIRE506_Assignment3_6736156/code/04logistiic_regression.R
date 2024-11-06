library(ggplot2) # Clopidogrel resistance = 1  0 = not resistance
# Logistic regression between resistance and SNPs.
logistic_A <- glm(Resistance ~ rs4244285, data= data_clean, family = binomial)

logistic_B <- glm(Resistance ~ rs4986893, data= data_clean, family = binomial)

logistic_C <- glm(Resistance ~ rs662, data= data_clean, family = binomial)


# Summary logistic A
summary(logistic_A)

exp(coef(logistic_A))

# Histogram 
# 0 = GG ,1 = AG ,2 = AA

ggplot(data_clean, aes(x = rs4244285, fill = factor(Resistance))) +
  geom_bar(position = "dodge") +
  labs(x = "rs4244285", y = "Count", fill = "Drug Resistance",
       title = "Distribution of Drug Resistance by rs4244285 Category") +
  theme_minimal()

# Predict probabilities of resistance

data_clean$predicted_probA <- predict(logistic_A, type = "response")

# Plot the predicted probabilities
ggplot(data_clean, aes(x =rs4244285, y = predicted_probA)) +
  geom_point() +
  geom_line(aes(group = 1), color = "blue") +
  labs(x = "rs4244285", y = "Predicted Probability of Resistance",
       title = "Predicted Probability of Drug Resistance by rs4244285 Category") +
  theme_minimal()

# Test q-q plot
residuals_devianceA <- residuals(logistic_A , type ="deviance")

qqnorm(residuals_devianceA, main = "QQ Plot of Deviance Residuals")
qqline(residuals_devianceA, col = "blue", lwd = 2)



#Summary logistic B 

# GG = 2 AG =1 AA =0

summary(logistic_B)

exp(coef(logistic_B))

#Histogram
ggplot(data_clean, aes(x = rs4986893, fill = factor(Resistance))) +
  geom_bar(position = "dodge") +
  labs(x = "rs4986893", y = "Count", fill = "Drug Resistance",
       title = "Distribution of Drug Resistance by rs4986893 Category") +
  theme_minimal()

#Predict probabilities of resistance

data_clean$predicted_probB <- predict(logistic_B, type = "response")

ggplot(data_clean, aes(x =rs4986893, y = predicted_probB)) +
  geom_point() +
  geom_line(aes(group = 1), color = "red") +
  labs(x = "rs4986893", y = "Predicted Probability of Resistance",
       title = "Predicted Probability of Drug Resistance by rs4986893 Category") +
  theme_minimal()

# Test q-q plot

residuals_devianceB <- residuals(logistic_B , type ="deviance")

qqnorm(residuals_devianceB, main = "QQ Plot of Deviance Residuals")
qqline(residuals_devianceB, col = "red", lwd = 2)

    

#Summary logistic C
# GG=2 AG =1 AA =0
summary(logistic_C)

exp(coef(logistic_C))

#Histogram
ggplot(data_clean, aes(x = rs662, fill = factor(Resistance))) +
  geom_bar(position = "dodge") +
  labs(x = "rs662", y = "Count", fill = "Drug Resistance",
       title = "Distribution of Drug Resistance by rs662 Category") +
  theme_minimal()

#Predict probabilities of resistance

data_clean$predicted_probC <- predict(logistic_C, type = "response")

ggplot(data_clean, aes(x =rs662, y = predicted_probC)) +
  geom_point() +
  geom_line(aes(group = 1), color = "green") +
  labs(x = "rs662", y = "Predicted Probability of Resistance",
       title = "Predicted Probability of Drug Resistance by rs662 Category") +
  theme_minimal()

# Test q-q plot

residuals_devianceC <- residuals(logistic_C , type ="deviance")

qqnorm(residuals_devianceC, main = "QQ Plot of Deviance Residuals")
qqline(residuals_devianceC, col = "green", lwd = 2)

# Adding Confounding  for control bias or adjusting for covariates.

fulll_logistic_A <- glm(Resistance ~ rs4244285 + AGE + SEX, data= data_clean, family = binomial) # rs4224285 significant while AGE and SEX are not

fulll_logistic_B <- glm(Resistance ~ rs4986893 + AGE + SEX , data= data_clean, family = binomial) # rs4986893 significant while AGE and SEX are not

fulll_logistic_C <- glm(Resistance ~ rs662 +AGE +SEX , data= data_clean, family = binomial) # not statiscallly significant for all.

# summary each full_logistics

summary(fulll_logistic_A)
summary(fulll_logistic_B)
summary(fulll_logistic_C)