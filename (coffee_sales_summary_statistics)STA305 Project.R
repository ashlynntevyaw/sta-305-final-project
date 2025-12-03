library(ggplot2)
library(tidyr)
library(dplyr)

coffee <- read.csv("Coffee_sales.csv")

###head data###
options(tibble.width = Inf)
head(coffee, 20) 

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

###hourly line###
ggplot(hour_summary_full, aes(x = hour_of_day, y = avg_sales)) +
  geom_line(color = "tan", linewidth = 1.2) +
  geom_point(color = "brown", size = 2) +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Average Coffee Sales by Hour of Day",
    x = "Hour of Day (0–23)", y = "Average Sales") + theme_minimal()

###weekly line###
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


###month boxplot###
month_order <- month_summary_df$Month_name

ggplot(coffee, aes(x = factor(Month_name, levels = month_order), y = money)) +
  geom_boxplot(fill = "tan") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  labs(title = "Coffee Sales by Month",
       x = "Month",
       y = "Sale Amount") +
  theme_minimal()

###time and season###
coffee_clean <- coffee %>%
  filter(!is.na(hour_of_day)) %>%
  mutate(
    time_block = case_when(
      hour_of_day >= 6  & hour_of_day < 12 ~ "Morning",
      hour_of_day >= 12 & hour_of_day < 17 ~ "Afternoon",
      hour_of_day >= 17 & hour_of_day < 21 ~ "Evening",
      TRUE ~ "Night"),
    season = case_when(
      Month_name %in% c("Dec","Jan","Feb") ~ "Winter",
      Month_name %in% c("Mar","Apr","May") ~ "Spring",
      Month_name %in% c("Jun","Jul","Aug") ~ "Summer",
      Month_name %in% c("Sep","Oct","Nov") ~ "Fall"))

###weekday###
weekday_summary <- coffee_clean %>%
  group_by(Weekday) %>%
  summarise(avg_sales = mean(money, na.rm = TRUE)) %>%
  arrange(desc(avg_sales))
weekday_order <- weekday_summary$Weekday

###timeblocks###
timeblock_summary <- coffee_clean %>%
  group_by(time_block) %>%
  summarise(avg_sales = mean(money, na.rm = TRUE)) %>%
  arrange(desc(avg_sales))
timeblock_order <- timeblock_summary$time_block

###season boxplot###
season_order <- c("Winter", "Spring", "Summer", "Fall")
ggplot(coffee_clean, aes(x = factor(season, levels = season_order), y = money)) +
  geom_boxplot(fill = "tan") +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  labs(title = "Coffee Sales by Season",
       x = "Season", y = "Sale Amount") + theme_minimal()





