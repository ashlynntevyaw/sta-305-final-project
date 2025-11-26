library(ggplot2)
library(dplyr)
library(knitr)

month_order <- month_summary_df$Month_name

ggplot(coffee, aes(x = factor(Month_name, levels = month_order), y = money)) +
  geom_boxplot(fill = "tan") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  labs(title = "Coffee Sales by Month",
       x = "Month",
       y = "Sale Amount") +
  theme_minimal()


coffee_clean <- coffee %>% filter(!is.na(hour_of_day))

coffee_clean <- coffee_clean %>%
  mutate(time_block = case_when(
    hour_of_day >= 6 & hour_of_day < 12 ~ "Morning",
    hour_of_day >= 12 & hour_of_day < 17 ~ "Afternoon",
    hour_of_day >= 17 & hour_of_day < 21 ~ "Evening",
    TRUE ~ "Night"
  ))

coffee_clean <- coffee_clean %>%
  mutate(season = case_when(
    Month_name %in% c("Dec","Jan","Feb") ~ "Winter",
    Month_name %in% c("Mar","Apr","May") ~ "Spring",
    Month_name %in% c("Jun","Jul","Aug") ~ "Summer",
    Month_name %in% c("Sep","Oct","Nov") ~ "Fall"
  ))


weekday_summary <- coffee_clean %>%
  group_by(Weekday) %>%
  summarise(avg_sales = mean(money, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_sales))
weekday_order <- weekday_summary$Weekday

timeblock_summary <- coffee_clean %>%
  group_by(time_block) %>%
  summarise(avg_sales = mean(money, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_sales))
timeblock_order <- timeblock_summary$time_block


season_order <- c("Winter", "Spring", "Summer", "Fall")
ggplot(coffee_clean, aes(x = factor(season, levels = season_order), y = money)) +
  geom_boxplot(fill = "tan") +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  labs(title = "Coffee Sales by Season",
       x = "Season",
       y = "Sale Amount") +
  theme_minimal()








