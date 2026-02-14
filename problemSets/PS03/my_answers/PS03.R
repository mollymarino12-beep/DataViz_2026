library(tidyverse)
ces_data <- read_csv("C:/Users/molly/OneDrive/Documents/GitHub/DataViz_2026/problemsets/PS03/my_answers/CES2015.csv")
ces_data <- ces_data |> 
  filter(discard == "Good quality")
names(ces_data)
ces_data <- ces_data |> 
  mutate(
    voted_flag = case_when(
      p_voted == "Yes" ~ "Voted",
      p_voted == "No" ~ "Did not vote",
      p_voted %in% c("Don’t know", "Refused") ~ NA_character_,
      TRUE ~ NA_character_  
    )
  )
table(ces_data$voted_flag)
ces_data <- ces_data |> 
  mutate(
    age_num = 2015 - as.numeric(age)
  )
ces_data <- ces_data |> 
  mutate(
    age_group = cut(
      age_num,
      breaks = c(-Inf, 29, 44, 64, Inf),
      labels = c("<30", "30-44", "45-64", "65+")
    )
  )
table(ces_data$age_group, useNA = "ifany")
#1
library(ggplot2)
library(dplyr)
ces_turnout <- ces_data |> 
  filter(!is.na(voted_flag) & !is.na(age_group))

turnout_by_age <- ces_turnout |> 
  group_by(age_group) |> 
  summarize(
    turnout_rate = mean(voted_flag == "Voted"),
    n = n()
  )
plot1 <- ggplot(turnout_by_age, aes(x = age_group, y = turnout_rate)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Age group",
    y = "Turnout rate",
    title = "Voter Turnout by Age Group (CES 2015)"
  ) +
  theme_minimal()
ggsave("graph1.pdf", plot = plot1, width = 7, height = 5)


#2
library(ggplot2)
library(dplyr)

ces_ideo <- ces_ideo %>%
  mutate(vote_for = factor(vote_for,
                           levels = c("Liberal", "Conservative", "NDP", "Bloc Québécois", "Green")))
graph2 <- ggplot(ces_ideo, aes(x = p_selfplace, fill = vote_for, color = vote_for)) +
  geom_density(alpha = 0.4, size = 1) +
  scale_x_continuous(breaks = 0:10, labels = 0:10) +
  labs(
    x = "Left–Right Ideology (0 = Left, 10 = Right)",
    y = "Density",
    fill = "Intended Vote",
    color = "Intended Vote",
    title = "Ideological Self-Placement by Party (CES 2015)"
  ) +
  theme_minimal(base_size = 12)
ggsave("graph2.pdf", plot = graph2, width = 7, height = 5)


#3
library(tidyverse)
ces_turnout_income <- ces_data |> 
  filter(discard == "Good quality") |> 
  filter(!is.na(voted_flag)) |> 
  filter(!is.na(income_full)) |> 
  filter(!income_full %in% c(".d", ".r"))
graph3 <- ggplot(ces_turnout_income, aes(x = income_full, fill = voted_flag)) +
  geom_bar(position = "stack") +
  facet_wrap(~ province, ncol = 4) +
  labs(
    title = "Voter Turnout by Income and Province (CES 2015)",
    x = "Household Income",
    y = "Number of Respondents",
    fill = "Turnout"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 5),  # extra small
    strip.text = element_text(face = "bold", size = 7),
    legend.position = "top"
  )
ggsave("graph3.pdf", plot = graph3, width = 8, height = 5)
#4
library(tidyverse)
library(ggrepel)
graph4 <- ggplot(age_turnout_summary, aes(x = age_group, y = turnout_rate, fill = turnout_rate)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Older Canadians Are More Likely to Vote",
    subtitle = "Turnout rates by age group among CES 2015 respondents",
    x = "Age Group",
    y = "Turnout Rate",
    caption = "Data: Canadian Election Study 2015. Counts weighted by NatWgt. Turnout coded 'Voted' vs 'Did not vote'."
  ) +
  geom_text_repel(
    data = annotation_point,
    aes(
      x = age_group,
      y = turnout_rate,
      label = paste0(round(turnout_rate*100), "%")
    ),
    nudge_y = 0.03,
    size = 4,
    fontface = "bold",
    segment.color = "red"
  ) +
  theme_minimal(base_size = 12)  
ggsave("graph4.pdf", plot = graph4, width = 7, height = 5)

