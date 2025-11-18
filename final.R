# Install and load required packages
install.packages("ggplot2")
library(readr)
library(ggplot2)

# Loads the dataset into the variable named coffee
coffee <- read_csv("coffee_sales.csv")

# Reorder the appearances of Weekdays
coffee$Weekday <- factor(coffee$Weekday, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), ordered = TRUE)
coffee$Month_name <- factor(coffee$Month_name, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), ordered = TRUE)

# View the dataset
View(coffee)

# Bar chart
# Sales by weekday and time of day
ggplot(data = coffee, aes(x = Weekday, fill = Time_of_Day)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c("Afternoon" = "#D6B588", "Morning" = "#705E46", "Night" = "#422701")) +
  labs(title = "Coffee Sales by Weekday and Time of Day", x = "Weekday", y = "Count", fill = "Time of Day")

# Sales by month and time of day
ggplot(data = coffee, aes(x = Month_name, fill = Time_of_Day)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c("Afternoon" = "#D6B588", "Morning" = "#705E46", "Night" = "#422701")) +
  labs(title = "Coffee Sales by Month and Time of Day", x = "Month", y = "Count", fill = "Time of Day")

# Sales by month and season
ggplot(data = coffee, aes(x = Month_name, fill = )) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c("Afternoon" = "#D6B588", "Morning" = "#705E46", "Night" = "#422701")) +
  labs(title = "Coffee Sales by Month and Season")

# Scatter plot
ggplot(data = coffee, aes(x = hour_of_day, y = money, color = coffee_name)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(values = c("#F3E5AB", "#D6B588", "#B9975B", "#8B6B3F", "#705E46", "#59442A", "#422701", "#2C1A0E")) +
  labs(title = "Coffee Sales by Hour of Day", subtitle = "Colored by Coffee Type", x = "Hour of Day", y = "Sale Amount ($)", color = "Coffee Type") +
  theme_minimal()

ggplot(data = coffee, aes(x = Weekdaysort, y = money, color = cash_type)) +
  geom_jitter(alpha = 0.7, width = 0.2, size = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = cash_type), linewidth = 1.1) +
  scale_x_continuous(breaks = 1:7, labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  scale_color_manual(values = c("#D6B588")) +
  labs(title = "Weekly Sales Trends by Payment Type", subtitle = "Average spending pattern across weedays", x = "Day of the Week", y = "Sale Amount ($)", color = "Payment Type") +
  theme_minimal()
