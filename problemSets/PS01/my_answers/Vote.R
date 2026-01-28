library(readr)  
library(readxl)   
library(dplyr)    
library(tidyr)  
file_path <- "mep_info_26Jul11.xls"
sheets <- c("EP1", "EP2", "EP3", "EP4", "EP5")
mep_list <- lapply(sheets, function(sheet_name) {
  read_excel(file_path, sheet = sheet_name, col_names = TRUE)
}) 
names(mep_list) <- sheets
rcv_ep1 <- read_csv("rcv_ep1.txt")
colnames(rcv_ep1)[1:5] <- c("MEPID", "MEPNAME", "MS", "NP", "EPG")
colnames(rcv_ep1)[6:ncol(rcv_ep1)] <- paste0("V", 1:(ncol(rcv_ep1)-5))
rcv_ep1_long <- rcv_ep1 %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "VoteNumber",
    values_to = "Vote"
  )
rcv_ep1_long <- rcv_ep1_long %>%
  mutate(VoteLabel = case_when(
    Vote == 1 ~ "Yes",
    Vote == 2 ~ "No",
    Vote == 3 ~ "Abstain",
    Vote == 4 ~ "Present but did not vote",
    Vote == 0 ~ "Absent",
    Vote == 5 ~ "Not an MEP",
    TRUE ~ "Unknown"
  ))
vote_summary <- rcv_ep1_long %>%
  group_by(VoteLabel) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
vote_summary
mep_ep1 <- mep_list$EP1 %>%
  rename(MEPID = `MEP id`) %>%
  select(
    MEPID,
    `National Party`,
    `EP Group`,
    `NOM-D1`,
    `NOM-D2`
  )
mep_ep1$MEPID <- as.character(mep_ep1$MEPID)
rcv_ep1_long$MEPID <- as.character(rcv_ep1_long$MEPID)
combined_ep1 <- left_join(
  rcv_ep1_long,
  mep_ep1,
  by = "MEPID"
)
missing_summary <- combined_ep1 %>%
  summarise(across(everything(), ~ sum(is.na(.))))
missing_summary

valid_votes <- combined_ep1 %>%
  filter(VoteLabel %in% c("Yes", "No", "Abstain"))
ep_group_votes <- valid_votes %>%
  group_by(EPG) %>%
  summarise(
    yes_rate = mean(VoteLabel == "Yes"),
    abstention_rate = mean(VoteLabel == "Abstain"),
    .groups = "drop"
  )
ep_group_nom <- combined_ep1 %>%
  filter(EPG != "0") %>%              
  select(EPG, MEPID, `NOM-D1`, `NOM-D2`) %>%
  distinct() %>%                      
  mutate(
    `NOM-D1` = as.numeric(`NOM-D1`),
    `NOM-D2` = as.numeric(`NOM-D2`)
  ) %>%
  group_by(EPG) %>%
  summarise(
    mean_NOM_D1 = mean(`NOM-D1`, na.rm = TRUE),
    mean_NOM_D2 = mean(`NOM-D2`, na.rm = TRUE),
    .groups = "drop"
  )
ep_group_summary <- left_join(ep_group_votes, ep_group_nom, by = "EPG")
ep_group_summary

#Data Visualization 
library(ggplot2)
epg_boxplot <- ggplot(
  combined_ep1 %>% filter(!is.na(`NOM-D1`), EPG != "0"),
  aes(x = reorder(EPG, `NOM-D1`, FUN = median), y = `NOM-D1`)
) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  coord_flip() +  
  labs(
    title = "Distribution of NOMINATE Dimension 1 by EP Group (EP1)",
    x = "EP Group",
    y = "NOMINATE Dimension 1"
  ) +
  theme_minimal(base_size = 14)
ggsave(
  filename = "vote_graph_one.pdf",  
  plot = epg_boxplot,               
  width = 8,                        
  height = 6,                       
  units = "in"                       
)
# What trends do you see? 
library(ggplot2)
plot_data <- combined_ep1 %>%
  filter(!is.na(`NOM-D1`), !is.na(`NOM-D2`), EPG != "0") %>%
  distinct(MEPID, .keep_all = TRUE) %>%
  mutate(
    `NOM-D1` = as.numeric(`NOM-D1`),
    `NOM-D2` = as.numeric(`NOM-D2`)
  )
nominate_scatter <- ggplot(plot_data, aes(x = `NOM-D1`, y = `NOM-D2`, color = EPG)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "MEP Positions on NOMINATE Dimensions (EP1)",
    x = "NOMINATE Dimension 1",
    y = "NOMINATE Dimension 2",
    color = "EP Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")
ggsave(
  filename = "vote_graph_two.pdf",  
  plot = nominate_scatter,         
  width = 8,                       
  height = 6,                      
  units = "in"                      
)
#Graph 3 
library(ggplot2)
valid_votes <- combined_ep1 %>%
  filter(VoteLabel %in% c("Yes", "No", "Abstain"))
mep_yes <- valid_votes %>%
  group_by(MEPID, EPG) %>%
  summarise(
    prop_yes = mean(VoteLabel == "Yes"),
    .groups = "drop"
  )
vote_cohesion_plot <- ggplot(mep_yes, aes(x = reorder(EPG, prop_yes, FUN = median), y = prop_yes, fill = EPG)) +
  geom_boxplot() +
  coord_flip() +  
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Cohesion of EP Groups in EP1 (Proportion of Yes Votes)",
    x = "EP Group",
    y = "Proportion Voting Yes"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
ggsave(
  filename = "vote_graph_three.pdf",
  plot = vote_cohesion_plot,
  width = 8,
  height = 6,
  units = "in"
)
#Graph 4 
party_yes <- combined_ep1 %>%
  filter(VoteLabel %in% c("Yes", "No", "Abstain")) %>%
  group_by(`National Party`) %>%
  summarise(
    prop_yes = mean(VoteLabel == "Yes"),
    n_votes = n(),
    .groups = "drop"
  )
library(ggplot2)

ggplot(
  party_yes,
  aes(x = reorder(`National Party`, prop_yes), y = prop_yes)
) +
  geom_col(width = 0.6, fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Proportion Voting Yes by National Party (EP1)",
    x = "National Party",
    y = "Proportion Voting Yes"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.y = element_text(size = 5),
    plot.title = element_text(size = 12, face = "bold")
  )
ggsave(
  filename = "vote_graph_four.pdf",
  width = 8,
  height = 12,  
  units = "in"
)






