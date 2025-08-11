library(tidyverse)
library(janitor)
library(lubridate)
library(scales)

data <- read_csv("data.csv") %>%
  clean_names() %>%
  mutate(date = ymd(date)) %>%
  drop_na()

summary_data <- data %>%
  group_by(category) %>%
  summarise(total = sum(value, na.rm = TRUE))

write_csv(data, "cleaned_data.csv")
write_csv(summary_data, "summary_data.csv")

ggplot(summary_data, aes(x = category, y = total)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  labs(title = "Example Summary Plot", x = "Category", y = "Total")

ggsave("summary_plot.png")
