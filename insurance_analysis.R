library(tidyverse)
library(broom)

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

