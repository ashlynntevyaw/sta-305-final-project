# Install and load required packages
install.packages("ggplot2")
library(readr)
library(ggplot2)

# Loads the dataset into the variable named coffee
coffee <- read_csv("final/coffee_sales.csv")

# View the dataset
View(coffee)

# Bar chart

# Box plot
ggplot(data = coffee, aes(x = Weekday, y = Time_of_Day)) +
  geom_boxplot(width = 0.3) +
  theme_minimal()
