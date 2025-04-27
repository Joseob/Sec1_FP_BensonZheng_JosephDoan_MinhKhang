# Load in the necessary libraries
library(tidyverse)
library(readr)
library(dplyr)

# Load the datasets
stats <- read_csv("~/GitHub/Sec1_FP_BensonZheng_JosephDoan_MinhKhang/nba_1947-2025_stats.csv")
salaries <- read_csv("~/GitHub/Sec1_FP_BensonZheng_JosephDoan_MinhKhang/nba_salaries_22-23.csv")

# Data Wrangling
## Clean the stats dataset to be 2023 players' stats on age & experience
stats_2023 <- stats %>%
  filter(season == 2022) %>%
  select(player, age, experience) %>%  
  distinct(player, .keep_all = TRUE)

## Clean the salaries dataset to be 2023 salaries of the players
salaries_clean <- salaries %>%
  select("Player Name", Salary) %>% 
  mutate(Salary = as.numeric(gsub("[$,]", "", Salary)))

# Joining the data together
nba_joined <- inner_join(stats_2023, salaries, 
                         by = c("player" = "Player Name"))

## filtering to find player, age, experience, and salary

nba_joined_filtered <- nba_joined %>%
  select(player, Salary, age, experience)

### finding the top 10 highest paid player in the nba 2022-2023

top_paid_players <- nba_joined_filtered %>%
  arrange(desc(Salary)) %>%
  slice_head(n = 10)

# Visualizing the Relationships
## player vs. salary
### Bar graph of top salary players vs. their salary
ggplot(top_paid_players, aes(x = player, y = Salary)) +
  geom_bar(stat = "identity", fill="skyblue") +
  labs(title = "Top 10 Highest Paid NBA Players (2023)",
       x = "Player",
       y = "Salary (dollars)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

## Bar graph of top salary player vs. their age
ggplot(top_paid_players, aes(x = player, y = age)) +
  geom_bar(stat = "identity", fill="skyblue") +
  labs(title = "Players vs. Age",
       x = "Player",
       y = "Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))


## Bar graph of top salary player vs. their experience
ggplot(top_paid_players, aes(x = player, y = experience)) +
  geom_bar(stat = "identity", fill="skyblue") +
  labs(title = "Players vs. Experience",
       x = "Player",
       y = "Experience") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

#------PlaceHolder--------#
