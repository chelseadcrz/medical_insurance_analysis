install.packages("broom")
install.packages("car")

library(tidyverse)
library(broom)
library(car)

insurance <- read_csv("insurance.csv")
head(insurance)


## Exploratory Data Analysis (EDA)

# Distribution of Annual Medical Insurance Cost
ggplot(insurance, aes(x = charges)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Annual Medical Charges", x = "Charges ($)", y = "Count")

# BMI and Smoker interaction
ggplot(insurance, aes(x = bmi, y = charges, color = smoker)) +
  geom_point(alpha = 0.5) +
  labs(title = "Charges vs BMI, colored by Smoking Status")

# Charges by region
ggplot(insurance, aes(x = region, y = charges, fill = region)) +
  geom_boxplot() + labs(title = "Charges by Region")

# Charges by sex
ggplot(insurance, aes(x = sex, y = charges, fill = sex)) +
  geom_boxplot() + labs(title = "Charges by Sex")

# Charges by age
ggplot(insurance, aes(x = age, y = charges)) +
  geom_point(alpha = 0.4) + geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Charges vs Age")



# MLR (using all predictors)
mlr_full <- lm(charges ~ ., data = insurance)

mlr_full_tidy <- tidy(mlr_full, conf.int = 0.95)
mlr_full_tidy

# Full model diagnostic
plot(mlr_full, which = 1)    # residual plot
plot(mlr_full, which = 2)    # QQ plot

vif(mlr_full)     # variance inflation factor



# Multiple Linear Regression (transformed + interaction model)
mlr_log <- lm(sqrt(charges) ~ age + sex + bmi + smoker + region + bmi*smoker + age*smoker,
              data = insurance)

mlr_log_tidy <- tidy(mlr_log, conf.int = 0.95)
mlr_log_tidy


# Transformed model diagnostic
plot(mlr_log, which = 1)    # residual plot
plot(mlr_log, which = 2)    # QQ plot

vif(mlr_log, type = "predictor")



# Logistic Regression  (Additive model)
cutoff_value <- quantile(insurance$charges, 0.75)
high_cost <- ifelse(insurance$charges > cutoff_value, 1, 0)

insurance_logit_add <- glm(high_cost ~ age + sex + bmi + children + smoker + region,
                           data = insurance,
                           family = binomial)

summary(insurance_logit_add)

pearson_residuals <- residuals(insurance_logit_add,
                               type = "pearson")
dispersion_ratio <- sum(pearson_residuals^2) / df.residual(insurance_logit_add)
dispersion_ratio



#Logistic Regression (Interaction model)
insurance_logit_int <- 
  glm(high_cost ~ sex + children + region + smoker * age + smoker * bmi,
      data = insurance,
      family = binomial)

summary(insurance_logit_int)


pearson_residuals_int <- residuals(insurance_logit_int,
                                   type = "pearson")
dispersion_ratio_int <- sum(pearson_residuals_int^2) / df.residual(insurance_logit_int)
dispersion_ratio_int
