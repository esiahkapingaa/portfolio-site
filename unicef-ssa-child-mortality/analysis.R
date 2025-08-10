
# analysis.R
# UNICEF + World Bank SSA Child Mortality Analysis

# --- 1. Load libraries ---
library(tidyverse)
library(janitor)
library(lubridate)
library(scales)

# --- 2. Read the data ---

# Adjust paths if running from root repo folder
u5mr <- read_csv("unicef-ssa-child-mortality/unicef_u5mr.csv") %>% clean_names()
imr <- read_csv("unicef-ssa-child-mortality/unicef_imr.csv") %>% clean_names()
neonatal <- read_csv("unicef-ssa-child-mortality/unicef_neonatal.csv") %>% clean_names()
gdp <- read_csv("unicef-ssa-child-mortality/worldbank_gdp.csv") %>% clean_names()
fertility <- read_csv("unicef-ssa-child-mortality/worldbank_fertility.csv") %>% clean_names()
region_codes <- read_csv("unicef-ssa-child-mortality/un_region_codes.csv") %>% clean_names()

# --- 3. Filter to Sub-Saharan Africa countries ---

ssa_countries <- region_codes %>%
  filter(region_name == "Sub-Saharan Africa") %>%
  pull(country_name)

u5mr <- u5mr %>% filter(country %in% ssa_countries)
imr <- imr %>% filter(country %in% ssa_countries)
neonatal <- neonatal %>% filter(country %in% ssa_countries)

# --- 4. Reshape data to long format ---

u5mr_long <- u5mr %>%
  pivot_longer(cols = starts_with("y"), names_to = "year", values_to = "u5mr") %>%
  mutate(year = as.integer(str_remove(year, "y")))

imr_long <- imr %>%
  pivot_longer(cols = starts_with("y"), names_to = "year", values_to = "imr") %>%
  mutate(year = as.integer(str_remove(year, "y")))

neonatal_long <- neonatal %>%
  pivot_longer(cols = starts_with("y"), names_to = "year", values_to = "neonatal_mortality") %>%
  mutate(year = as.integer(str_remove(year, "y")))

# --- 5. Merge datasets ---

mortality <- u5mr_long %>%
  left_join(imr_long, by = c("country", "year")) %>%
  left_join(neonatal_long, by = c("country", "year")) %>%
  left_join(gdp, by = c("country", "year")) %>%
  left_join(fertility, by = c("country", "year"))

# --- 6. Clean data ---

mortality <- mortality %>%
  filter(year >= 2000 & year <= 2022) %>%
  drop_na(u5mr, imr, neonatal_mortality)

# --- 7. Calculate rate reductions (example with U5MR) ---

mortality <- mortality %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(u5mr_reduction = (first(u5mr) - last(u5mr)) / first(u5mr) * 100) %>%
  ungroup()

# --- 8. Visualizations ---

# a) Regional Trend Line (Mean U5MR by year)
p1 <- mortality %>%
  group_by(year) %>%
  summarise(mean_u5mr = mean(u5mr, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_u5mr)) +
  geom_line(color = "#1f77b4", size = 1.2) +
  labs(title = "Average Under-5 Mortality Rate in SSA (2000–2022)",
       y = "Deaths per 1,000 live births", x = "Year") +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# b) Country Rankings in 2022
p2 <- mortality %>%
  filter(year == 2022) %>%
  arrange(desc(u5mr)) %>%
  ggplot(aes(x = reorder(country, u5mr), y = u5mr)) +
  geom_col(fill = "#d62728") +
  coord_flip() +
  labs(title = "Under-5 Mortality Rate by Country (2022)",
       y = "U5MR per 1,000", x = "Country") +
  theme_minimal()

# c) GDP vs U5MR Scatter plot in 2022
p3 <- mortality %>%
  filter(year == 2022) %>%
  ggplot(aes(x = gdp_per_capita, y = u5mr)) +
  geom_point(aes(size = fertility_rate), color = "#2ca02c", alpha = 0.7) +
  scale_x_log10(labels = scales::dollar) +
  labs(title = "GDP vs U5MR in SSA (2022)",
       x = "GDP per Capita (log scale)", y = "U5MR per 1,000") +
  theme_minimal()

# --- 9. Save outputs ---

ggsave("unicef-ssa-child-mortality/ssa_trend.png", plot = p1, width = 8, height = 5, dpi = 300)
ggsave("unicef-ssa-child-mortality/ssa_rankings.png", plot = p2, width = 8, height = 5, dpi = 300)
ggsave("unicef-ssa-child-mortality/ssa_gdp_vs_u5mr.png", plot = p3, width = 8, height = 5, dpi = 300)

write_csv(mortality, "unicef-ssa-child-mortality/mortality_cleaned.csv")

