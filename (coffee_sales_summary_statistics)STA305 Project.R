library(ggplot2)
library(tidyr)
library(dplyr)

coffee <- read.csv("Coffee_sales.csv")

summary(coffee)
summary(coffee$money)
summary(coffee$hour_of_day)


###hour summary###
hour_summary <- coffee %>%
  group_by(hour_of_day) %>%
  summarise(
    avg_sales = mean(money, na.rm = TRUE),
    transactions = n())

###make 0-23###
hour_summary_full <- data.frame(hour_of_day = 0:23) %>%
  left_join(hour_summary, by = "hour_of_day") %>%
  replace_na(list(avg_sales = NA, transactions = 0)) %>%
  mutate(
    time_label = case_when(
      hour_of_day == 0 ~ "12 AM",
      hour_of_day < 12 ~ paste0(hour_of_day, " AM"),
      hour_of_day == 12 ~ "12 PM",
      TRUE ~ paste0(hour_of_day - 12, " PM")) %>%
  select(time_label, avg_sales, transactions) %>%
  arrange(desc(avg_sales))

###weekday summary###
weekday_summary <- coffee %>%
  group_by(Weekday) %>%
  summarise(
    avg_sales = mean(money, na.rm = TRUE),
    transactions = n()) %>% arrange(desc(avg_sales))

###month summary###
month_summary <- coffee %>%
  group_by(Month_name) %>%
  summarise(
    avg_sales = mean(money, na.rm = TRUE),
    transactions = n()) %>% arrange(desc(avg_sales))

###summaries###
print(hour_summary_full)
print(weekday_summary)
print(month_summary)

###hourly###
ggplot(hour_summary_full, aes(x = hour_of_day, y = avg_sales)) +
  geom_line(color = "tan", linewidth = 1.2) +
  geom_point(color = "brown", size = 2) +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Average Coffee Sales by Hour of Day",
    x = "Hour of Day (0–23)", y = "Average Sales") + theme_minimal()

###weekly###
coffee$Date <- as.Date(coffee$Date)
coffee$week <- format(coffee$Date, "%U")

weekly_sales <- coffee %>%
  group_by(week) %>%
  summarise(avg_sales = mean(money, na.rm = TRUE))

ggplot(weekly_sales, aes(x = as.numeric(week), y = avg_sales)) +
  geom_line(color = "brown") +
  geom_point() +
  labs(title = "Average Coffee Sales by Week",
    x = "Week Number", y = "Average Sales") + theme_minimal()




