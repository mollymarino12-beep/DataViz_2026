library(tidyverse)

ces_data <- read_csv("C:/Users/molly/OneDrive/Documents/GitHub/DataViz_2026/problemsets/PS03/my_answers/CES2015.csv")
# This CSV had parsing issues and I did not have time to clean it. 
# Filter the dataset to include only high quality participants
ces_data <- ces_data |> 
  filter(discard == "Good quality")
names(ces_data)
ces_data <- ces_data |> 
  mutate(
    voted_flag = case_when(
      p_voted == "Yes" ~ "Voted",
      p_voted == "No" ~ "Did not vote",
      p_voted %in% c("Don’t know", "Refused") ~ NA_character_,
      TRUE ~ NA_character_  # catches any other unexpected responses
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


