library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(knitr)


coffee <- read_csv("Coffee_sales.csv")

coffee
str(coffee)
summary(coffee)



summary(coffee$money)
summary(coffee$hour_of_day)


full_hours <- data.frame(hour_of_day = 0:23)

hour_summary_full <- full_hours %>%
  left_join(
    coffee %>%
      group_by(hour_of_day) %>%
      summarise(
        avg_sales = mean(money, na.rm = TRUE),
        transactions = n()
      ), by = "hour_of_day"
  ) %>%
  replace_na(list(avg_sales = NA, transactions = 0)) %>%
  mutate(
    time_label = ifelse(hour_of_day == 0, "12 AM",
                        ifelse(hour_of_day < 12, paste0(hour_of_day, " AM"),
                               ifelse(hour_of_day == 12, "12 PM",
                                      paste0(hour_of_day - 12, " PM"))))
  ) %>%
  select(time_label, avg_sales, transactions) %>%
  arrange(desc(avg_sales))


hour_summary_full_df <- as.data.frame(hour_summary_full)


kable(hour_summary_full_df, caption = "Coffee Sales by Hour of Day (AM/PM)")



weekday_summary_simple_df <- coffee %>%
  group_by(Weekday) %>%
  summarise(
    avg_sales = mean(money, na.rm = TRUE),
    transactions = n()
  ) %>%
  arrange(desc(avg_sales)) %>%
  as.data.frame()

kable(weekday_summary_simple_df, caption = "Coffee Sales by Weekday")


month_summary_df <- coffee %>%
  group_by(Month_name) %>%
  summarise(
    avg_sales = mean(money, na.rm = TRUE),
    transactions = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_sales)) %>%
  as.data.frame()

kable(month_summary_df, caption = "Coffee Sales by Month")




hour_summary_plot <- full_hours %>%
  left_join(
    coffee %>%
      group_by(hour_of_day) %>%
      summarise(avg_sales = mean(money, na.rm = TRUE)),
    by = "hour_of_day"
  )

# Plot
ggplot(hour_summary_plot, aes(x = hour_of_day, y = avg_sales)) +
  geom_line(color = "tan", linewidth = 1.2) +
  geom_point(color = "brown", size = 2) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Average Coffee Sales by Hour of Day",
    x = "Hour of Day (0–23)",
    y = "Average Money")



coffee$Date <- as.Date(coffee$Date)
coffee$week <- format(coffee$Date, "%U")
weekly_sales <- coffee %>%
  group_by(week) %>%
  summarise(avg_sales = mean(money, na.rm = TRUE))

ggplot(weekly_sales, aes(x = as.numeric(week), y = avg_sales)) +
  geom_line(color="brown") +
  geom_point() +
  labs(
    title = "Average Coffee Sales by Week",
    x = "Week Number",
    y = "Average Sales"
  ) +
  theme_minimal()


