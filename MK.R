# 1) Load libraries
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(knitr)
library(kableExtra)
library(corrplot)
library(ggrepel) 
# 2) Read raw data
stats    <- read_csv("nba_1947-2025_stats.csv", show_col_types = FALSE)
salaries <- read_csv("nba_salaries_22-23.csv", show_col_types = FALSE) %>%
  select(-...1) 

# 3) Standardize player names
stats <- stats %>%
  mutate(player = str_to_lower(str_squish(player)))

salaries <- salaries %>%
  rename(player = `Player Name`, pos = Position) %>%  # rename Position → pos
  mutate(player = str_to_lower(str_squish(player)))

# 4) Filter stats to the 2023 season
stats_2023 <- stats %>%
  filter(season == 2023) %>%       # use the lowercase 'season' column
  select(player, age, experience) %>%
  distinct(player, .keep_all = TRUE)

# 5) Join the two tables
nba_joined <- inner_join(stats_2023, salaries, by = "player")

# 6) Extract the top 10 highest‐paid players
top10 <- nba_joined %>%
  arrange(desc(Salary)) %>%
  slice_head(n = 10)

# 7) Plot 1: Top 10 salaries with Age overlaid
ggplot(top10, aes(x = reorder(player, -Salary))) +
  geom_col(aes(y = Salary/1e6), fill = "steelblue", alpha = 0.8) +
  geom_point(aes(y = age * 1.2), color = "firebrick", size = 3) +
  geom_line(aes(y = age * 1.2, group = 1),
            color = "firebrick", linetype = "dashed") +
  scale_y_continuous(
    name      = "Salary (Million $)",
    labels    = dollar_format(prefix = "$", suffix = "M"),
    sec.axis  = sec_axis(~./1.2, name = "Age (years)")
  ) +
  labs(
    title = "Top 10 NBA 2022–23 Salaries & Player Age",
    x     = "Player (by descending salary)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8) Summary table: Salary by age group
age_summary <- nba_joined %>%
  mutate(age_group = cut(
    age,
    breaks = c(0, 25, 30, 35, Inf),
    labels = c("Under 25", "25–29", "30–34", "35+"),
    right  = FALSE
  )) %>%
  group_by(age_group) %>%
  summarise(
    Count         = n(),
    Mean_Salary   = mean(Salary, na.rm = TRUE),
    Median_Salary = median(Salary, na.rm = TRUE),
    Max_Salary    = max(Salary, na.rm = TRUE),
    Min_Salary    = min(Salary, na.rm = TRUE)
  ) %>%
  arrange(desc(Mean_Salary))

# Print & render the table
print(age_summary)
kable(age_summary,
      col.names = c("Age Group", "Count", "Mean $", "Median $", "Max $", "Min $"),
      digits    = 0,
      format    = "html",
      caption   = "NBA 2022–23: Salary by Age Group") %>%
  kable_styling(full_width = FALSE)

# 9) Plot 2: Age vs. Salary by position
ggplot(nba_joined, aes(x = age, y = Salary/1e6, color = pos)) +
  geom_jitter(width = 0.3, height = 0, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "M")) +
  labs(
    title = "NBA 2022–23: Age vs. Salary by Position",
    x     = "Age (years)",
    y     = "Salary (Million $)",
    color = "Position"
  ) +
  theme_minimal()

# 10) Plot 3: Experience vs. Salary by position
ggplot(nba_joined, aes(x = experience, y = Salary/1e6, color = pos)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "M")) +
  labs(
    title = "NBA 2022–23: Experience vs. Salary by Position",
    x     = "Experience (years)",
    y     = "Salary (Million $)",
    color = "Position"
  ) +
  theme_minimal()




# INSIGHT 1: Team Salary Analysis ----
# Calculate team total salaries
team_analysis <- nba_joined %>%
  group_by(Team) %>%
  summarise(
    total_salary = sum(Salary, na.rm = TRUE),
    avg_salary = mean(Salary, na.rm = TRUE),
    player_count = n()
  ) %>%
  arrange(desc(total_salary))

# Visualize team salaries
ggplot(team_analysis, aes(x = reorder(Team, -total_salary), y = total_salary/1000000)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0("$", round(total_salary/1000000, 0), "M")),
            vjust = -0.3, size = 3) +
  scale_y_continuous(labels = dollar_format(suffix = "M")) +
  labs(
    title = "NBA Team Total Salary Expenditure (2022-2023)",
    x = "Team",
    y = "Total Salary (Millions $)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# INSIGHT 2: Salary Cap Context  ----
# 2022-2023 Salary Cap was $123.7 million
SALARY_CAP <- 123655000

# Calculate team spending relative to cap
team_cap_analysis <- nba_joined %>%
  group_by(Team) %>%
  summarise(
    total_team_salary = sum(Salary, na.rm = TRUE),
    pct_of_cap = total_team_salary / SALARY_CAP * 100,
    number_of_players = n()
  ) %>%
  arrange(desc(pct_of_cap))

# Visualize team spending relative to cap
ggplot(team_cap_analysis, aes(x = reorder(Team, -pct_of_cap), y = pct_of_cap)) +
  geom_bar(stat = "identity", aes(fill = pct_of_cap)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
  geom_text(aes(label = paste0(round(pct_of_cap, 1), "%")), 
            hjust = -0.1, vjust = 0.5, size = 3, angle = 90) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  coord_flip() +
  labs(
    title = "NBA Team Spending Relative to Salary Cap (2022-2023)",
    subtitle = "Red line indicates the salary cap ($123.7M)",
    x = "Team",
    y = "Percentage of Salary Cap (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# INSIGHT 3: Maximum Contract Analysis ----
# In 2022-23, max contracts were:
# 0-6 years: 25% of cap = ~$30.9M
# 7-9 years: 30% of cap = ~$37.1M
# 10+ years: 35% of cap = ~$43.3M

# Create the maximum contract data
max_contract_data <- data.frame(
  experience_tier = c("0-6 years", "7-9 years", "10+ years"),
  max_amount = c(30900000, 37100000, 43300000)
)

# Find players near max contracts

max_contract_players <- nba_joined %>%
  # Create max_tier as a new column
  mutate(
    max_tier = case_when(
      experience <= 6 ~ "0-6 years",
      experience >= 7 & experience <= 9 ~ "7-9 years",
      experience >= 10 ~ "10+ years",
      TRUE ~ NA_character_  # Handle any unexpected values
    )
  ) %>%
  # Create factor with explicit levels to control ordering
  mutate(
    max_tier = factor(max_tier, levels = c("0-6 years", "7-9 years", "10+ years"))
  ) %>%
  # Join with the max contract data
  left_join(max_contract_data, by = c("max_tier" = "experience_tier")) %>%
  # Calculate percentage of max and identify near-max players
  mutate(
    pct_of_max = Salary / max_amount * 100,
    near_max = pct_of_max > 85
  ) %>%
  # Filter to only players near the max
  filter(near_max) %>%
  # Select only relevant columns
  select(player, pos, age, experience, Salary, max_amount, pct_of_max, max_tier) %>%  # Added max_tier here
  # Sort by percentage of max
  arrange(desc(pct_of_max))


# Visualize players on max contracts
ggplot(max_contract_players, aes(x = reorder(player, pct_of_max), y = pct_of_max)) +
  geom_bar(stat = "identity", aes(fill = max_tier)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
  geom_text(aes(label = paste0(round(pct_of_max, 1), "%")), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "NBA Players on Near-Maximum Contracts (2022-2023)",
    subtitle = "Red line indicates 100% of maximum available contract",
    x = "Player",
    y = "Percentage of Maximum Contract (%)",
    fill = "Experience Tier"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
