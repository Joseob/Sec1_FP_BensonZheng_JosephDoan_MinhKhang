# === 1️⃣ Set up & merge ===
library(readr)
library(dplyr)
library(ggplot2)

# Load data
stats <- read_csv("season_stats.csv")
salaries <- read_csv("NBA_salaries.csv")

# Clean names + rename salary column for clarity
stats <- stats %>%
  mutate(Player = trimws(Player))

salaries <- salaries %>%
  mutate(Player = trimws(Player)) %>%
  rename(Salary_17_18 = season17_18)

# Filter stats to 2017 (represents the 2017–18 season)
stats_2018 <- stats %>%
  filter(Year == 2017)

# Merge by Player and Team
merged_df <- stats_2018 %>%
  left_join(salaries, by = c("Player", "Tm" = "Tm"))

# Remove "TOT" rows (combined team rows)
merged_clean <- merged_df %>%
  filter(Tm != "TOT")

# === 2️⃣ Scatterplot: Salary vs. Points per Game ===
ggplot(merged_clean, aes(x = PTS / G, y = Salary_17_18)) +
  geom_point() +
  labs(
    title = "Salary vs. Points Per Game (2017–18 Season)",
    x = "Points Per Game (PPG)",
    y = "Salary (USD)"
  ) +
  theme_minimal()

# === 3️⃣ Barplot: Average Salary by Position ===
ggplot(merged_clean, aes(x = Pos, y = Salary_17_18)) +
  stat_summary(fun = mean, geom = "bar") +
  labs(
    title = "Average Salary by Position (2017–18 Season)",
    x = "Position",
    y = "Average Salary (USD)"
  ) +
  theme_minimal()

# === 4️⃣ Table: Best Bang-for-the-Buck Players ===
value_table <- merged_clean %>%
  filter(!is.na(Salary_17_18)) %>%
  mutate(
    PPG = PTS / G,
    Points_per_Million = PPG / (Salary_17_18 / 1e6)
  ) %>%
  arrange(desc(Points_per_Million)) %>%
  select(Player, Pos, Tm, PPG, Salary_17_18, Points_per_Million) %>%
  head(10)

# Print the table in console
print(value_table)