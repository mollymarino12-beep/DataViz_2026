library(readr)
library(dplyr)
library(ggplot2)
library(scales)  
drug_data <- read_csv("drug_data.csv")
drug_clean <- drug_data %>%
  filter(jurisdiction_occurrence == "United States",
         time_period == "12 month-ending",
         !is.na(drug_overdose_deaths),
         drug_involved %in% c("Fentanyl", "Heroin", "Cocaine", "Methamphetamine")) %>%
  mutate(month_ending_date = as.Date(month_ending_date, format = "%m/%d/%Y"))
years_in_data <- range(format(drug_clean$month_ending_date, "%Y"))
years_title <- paste(years_in_data[1], "-", years_in_data[2])
#plot1
drug_total_all_years <- drug_clean %>%
  group_by(drug_involved) %>%
  summarise(total_deaths = sum(drug_overdose_deaths)) %>%
  ungroup()
ggplot(drug_total_all_years, aes(x = drug_involved, y = total_deaths)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = comma) +  
  labs(
    title = paste("Total Overdose Deaths by Drug (", years_title, ")", sep = ""),
    x = "Drug",
    y = "Total Number of Deaths"
  ) +
  theme_minimal()
ggsave("plot1.pdf", width = 8, height = 6)
  # Figure 2
drug_yearly <- drug_clean %>%
  mutate(year = as.numeric(format(month_ending_date, "%Y"))) %>%
  group_by(year, drug_involved) %>%
  summarise(total_deaths = sum(drug_overdose_deaths), .groups = "drop")
year_totals <- drug_yearly %>%
  group_by(year) %>%
  summarise(year_total = sum(total_deaths), .groups = "drop")
drug_yearly <- drug_yearly %>%
  left_join(year_totals, by = "year") %>%
  mutate(percent = round(total_deaths / year_total * 100, 1))
ggplot(drug_yearly, aes(x = factor(year), y = total_deaths, group = drug_involved)) +
  geom_segment(aes(x = factor(year), xend = factor(year), y = 0, yend = total_deaths, color = drug_involved),
               size = 1) +
  geom_point(aes(color = drug_involved), size = 4) +
  geom_text(data = subset(drug_yearly, drug_involved == "Fentanyl"),
            aes(label = paste0(percent, "%")), vjust = -0.5, fontface = "bold") +
  scale_color_manual(values = c("Fentanyl" = "steelblue", 
                                "Heroin" = "darkorange",
                                "Cocaine" = "forestgreen",
                                "Methamphetamine" = "purple")) +
  labs(
    title = "Overdose Deaths by Drug by Year",
    subtitle = "Fentanyl increasingly dominates major drug deaths",
    x = "Year",
    y = "Number of Deaths",
    color = "Drug"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),       
    plot.subtitle = element_text(hjust = 0.5, size = 10)         
  )
ggsave("plot2.pdf", width = 8, height = 6)
  
# Figure 3:
slope_data <- drug_clean %>%
  mutate(year = as.numeric(format(month_ending_date, "%Y"))) %>%
  filter(year %in% c(2024, 2025)) %>%
  group_by(drug_involved, year) %>%
  summarise(total_deaths = sum(drug_overdose_deaths), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = total_deaths, names_prefix = "year_") %>%
  mutate(
    reduction = year_2024 - year_2025,
    reduction_pct_of_total = round(reduction / sum(reduction) * 100, 1)
  ) %>%
  pivot_longer(cols = c(year_2024, year_2025), names_to = "year", values_to = "deaths") %>%
  mutate(year = factor(ifelse(year == "year_2024", 2024, 2025), levels = c(2024, 2025)))
fentanyl_label <- slope_data %>%
  filter(year == "2025" & drug_involved == "Fentanyl") %>%
  mutate(
    label_y = deaths + max(slope_data$deaths) * 0.10  
  )
plot3 <- ggplot(slope_data, aes(x = year, y = deaths, group = drug_involved, color = drug_involved)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
    geom_segment(data = fentanyl_label,
               aes(x = year, xend = year, y = deaths, yend = label_y),
             color = "gray30", linetype = "dashed") +
    geom_text(data = fentanyl_label,
            aes(x = year, y = label_y, label = paste0(reduction_pct_of_total, "%")),
            fontface = "bold",
            color = "black",
            show.legend = FALSE,
            vjust = 0) +
   scale_color_manual(values = c(
    "Fentanyl" = "steelblue",
    "Heroin" = "firebrick",
    "Cocaine" = "darkgreen",
    "Methamphetamine" = "orange"
  )) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.20)), labels = comma) +
  
  labs(
    title = "Reduction of Drug Overdose Deaths 2024 → 2025",
    subtitle = "Fentanyl Leads Largest Reduction in Deaths",
    y = "Number of Overdose Deaths",
    x = "Year",
    color = "Drug"
  ) +
    theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )
plot3
ggsave("plot3.pdf", plot = plot3, width = 8, height = 6)
