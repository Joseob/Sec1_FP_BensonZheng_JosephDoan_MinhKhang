# NBA Player Salary and Performance Analysis (2022-2023)
# Following feedback to improve visualization clarity and efficiency

# Load necessary libraries -----
library(tidyverse)
library(readr)
library(ggplot2)
library(kableExtra)
library(scales)

# Load datasets -----
stats <- read_csv("nba_1947-2025_stats.csv")
salaries <- read_csv("nba_salaries_22-23.csv")

# Data preparation -----
stats_2023 <- stats %>%
  filter(season == 2023) %>%
  distinct(player, .keep_all = TRUE)

salaries_clean <- salaries %>%
  rename(player = "Player Name") %>%
  mutate(
    player = str_trim(player),
    Salary = ifelse(is.character(Salary), 
                    as.numeric(gsub("[$,]", "", Salary)), 
                    Salary)
  )

# Join datasets -----
nba_joined <- inner_join(stats_2023, salaries_clean, by = "player")

# Get top 10 highest paid players -----
top_paid_players <- nba_joined %>%
  arrange(desc(Salary)) %>%
  slice_head(n = 10)

# IMPROVEMENT 1: Combined age/salary visualization for top 10 players -----
# Create age groups for clearer visualization
age_groups <- nba_joined %>%
  mutate(
    age_group = cut(age, 
                    breaks = c(0, 25, 30, 35, 100),
                    labels = c("Under 25", "25-29", "30-34", "35+"),
                    right = FALSE)
  )

# Create combined visualization for top 10 players
top_10_combined <- ggplot(top_paid_players, aes(x = reorder(player, Salary))) +
  geom_bar(aes(y = Salary/1000000), stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_point(aes(y = age * 2), color = "red", size = 3) +
  geom_line(aes(y = age * 2, group = 1), color = "red", linetype = "dashed") +
  scale_y_continuous(
    name = "Salary (Million $)",
    labels = dollar_format(suffix = "M"),
    sec.axis = sec_axis(~./2, name = "Age (years)")
  ) +
  labs(title = "Top 10 Highest Paid NBA Players: Salary and Age",
       subtitle = "Blue bars show salary, red dots show age",
       x = "Player") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(top_10_combined)

# IMPROVEMENT 2: Combined experience/salary visualization for top 10 players -----
top_10_combined_exp <- ggplot(top_paid_players, aes(x = reorder(player, Salary))) +
  geom_bar(aes(y = Salary/1000000), stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_point(aes(y = experience * 2), color = "darkorange", size = 3) +
  geom_line(aes(y = experience * 2, group = 1), color = "darkorange", linetype = "dashed") +
  scale_y_continuous(
    name = "Salary (Million $)",
    labels = dollar_format(suffix = "M"),
    sec.axis = sec_axis(~./2, name = "Experience (years)")
  ) +
  labs(title = "Top 10 Highest Paid NBA Players: Salary and Experience",
       subtitle = "Blue bars show salary, orange dots show experience",
       x = "Player") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(top_10_combined_exp)

# IMPROVEMENT 3: Summary tables by age groups -----
age_group_summary <- age_groups %>%
  group_by(age_group) %>%
  summarise(
    n_players = n(),
    mean_salary = mean(Salary, na.rm = TRUE),
    median_salary = median(Salary, na.rm = TRUE),
    mean_per = mean(per, na.rm = TRUE),
    mean_ws = mean(ws, na.rm = TRUE)
  )

kable(age_group_summary, 
      col.names = c("Age Group", "Players", "Mean Salary ($)", "Median Salary ($)", 
                    "Mean PER", "Mean Win Shares"),
      format.args = list(big.mark = ","),
      caption = "NBA Player Statistics by Age Group (2022-2023)",
      digits = c(0, 0, 0, 0, 1, 1)) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# IMPROVEMENT 4: Scatter plot of age vs salary for all players -----
age_salary_scatter <- ggplot(nba_joined, aes(x = age, y = Salary/1000000)) +
  geom_point(aes(color = pos, size = per), alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  labs(title = "NBA Player Age vs. Salary (2022-2023)",
       subtitle = "Point size represents Player Efficiency Rating (PER)",
       x = "Age (years)",
       y = "Salary (Million $)",
       color = "Position",
       size = "PER") +
  scale_y_continuous(labels = dollar_format(suffix = "M")) +
  theme_minimal() +
  theme(legend.position = "right")

print(age_salary_scatter)

# IMPROVEMENT 5: Experience vs salary with age groups as color -----
exp_salary_scatter <- ggplot(nba_joined, aes(x = experience, y = Salary/1000000)) +
  geom_point(aes(color = age), alpha = 0.7, size = 3) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "NBA Player Experience vs. Salary (2022-2023)",
       subtitle = "Color intensity represents player age",
       x = "Years of Experience",
       y = "Salary (Million $)",
       color = "Age") +
  scale_y_continuous(labels = dollar_format(suffix = "M")) +
  theme_minimal()

print(exp_salary_scatter)

# IMPROVEMENT 6: Position-specific analysis table -----
position_summary <- nba_joined %>%
  group_by(pos) %>%
  summarise(
    n_players = n(),
    mean_salary = mean(Salary, na.rm = TRUE),
    median_salary = median(Salary, na.rm = TRUE),
    mean_age = mean(age, na.rm = TRUE),
    mean_experience = mean(experience, na.rm = TRUE),
    mean_per = mean(per, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_salary))

kable(position_summary, 
      col.names = c("Position", "Players", "Mean Salary ($)", "Median Salary ($)", 
                    "Mean Age", "Mean Experience", "Mean PER"),
      format.args = list(big.mark = ","),
      caption = "NBA Player Statistics by Position (2022-2023)",
      digits = c(0, 0, 0, 0, 1, 1, 1)) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# IMPROVEMENT 7: Correlation between age, experience, and salary -----
correlation_matrix <- nba_joined %>%
  select(age, experience, Salary, per, ws, vorp) %>%
  cor(use = "complete.obs")

# Create correlation plot
correlation_plot <- ggplot(data = reshape2::melt(correlation_matrix), 
                           aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Matrix: Player Attributes", 
       x = "", y = "") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4)

print(correlation_plot)

# IMPROVEMENT 8: 3D surface plot for age, experience, and salary relationship -----
library(plotly)

age_experience_salary <- nba_joined %>%
  na.omit() %>%
  group_by(age, experience) %>%
  summarise(avg_salary = mean(Salary)/1000000) %>%
  ungroup()

plot_ly(age_experience_salary, 
        x = ~age, 
        y = ~experience, 
        z = ~avg_salary,
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 5, color = ~avg_salary, colorscale = 'Viridis')) %>%
  layout(title = "Age, Experience, and Salary Relationship",
         scene = list(xaxis = list(title = "Age"),
                      yaxis = list(title = "Experience"),
                      zaxis = list(title = "Average Salary ($M)")))

# IMPROVEMENT 9: Value efficiency visualization -----
# Calculate value metric
nba_joined <- nba_joined %>%
  mutate(
    value_score = ifelse(ws > 0 & Salary > 0, 
                         ws / (Salary/1000000), 
                         NA),
    efficiency_group = cut(value_score,
                           breaks = c(-Inf, 0.5, 1, 1.5, Inf),
                           labels = c("Low Value", "Medium Value", "High Value", "Exceptional Value"))
  )

# Create value efficiency plot
value_efficiency_plot <- ggplot(nba_joined %>% filter(!is.na(value_score)), 
                                aes(x = Salary/1000000, y = ws)) +
  geom_point(aes(color = efficiency_group, size = per), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("red", "orange", "lightgreen", "darkgreen")) +
  labs(title = "Player Value Efficiency: Win Shares vs. Salary",
       subtitle = "Point size represents PER",
       x = "Salary (Million $)",
       y = "Win Shares",
       color = "Value Efficiency",
       size = "PER") +
  scale_x_continuous(labels = dollar_format(suffix = "M")) +
  theme_minimal()

print(value_efficiency_plot)

# Create a summary table of player efficiency
efficiency_summary <- nba_joined %>%
  filter(!is.na(efficiency_group)) %>%
  group_by(efficiency_group) %>%
  summarise(
    n_players = n(),
    avg_salary = mean(Salary),
    avg_per = mean(per, na.rm = TRUE),
    avg_value_score = mean(value_score, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_value_score))

kable(efficiency_summary, 
      col.names = c("Value Category", "Players", "Average Salary ($)", 
                    "Average PER", "Average Value Score"),
      format.args = list(big.mark = ","),
      caption = "Player Value Efficiency Summary",
      digits = c(0, 0, 0, 1, 2)) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
