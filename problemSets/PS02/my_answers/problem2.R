detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()
pkgTest <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  
  sapply(pkg, require, character.only = TRUE)
}

# Load any necessary packages
lapply(c("tidyverse", "ggplot2"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


NCSS <- read.csv("NCSS.csv")
head(NCSS)
library(dplyr)
NCSS_selected <-select(NCSS,CASEID,YEAR,GDREGION,NUMOFFMBR,TRAD6,TRAD12,INCOME)
Religion <- NCSS_selected %>%
	filter(TRAD6 %in% c("Juives", "Musulmanes", "Chrétiennes"))
head(Religion)
table(Religion$TRAD6)
Religion_classification <- Religion %>% 
	filter(TRAD6 %in% c("Juives", "Musulmanes", "Chrétiennes")) %>%
group_by(YEAR, TRAD6) %>% 
summarise(total_members=sum(NUMOFFMBR, na.rm=TRUE)) %>%
arrange(YEAR,desc(total_members)) 
Religion_classification
last_year <- max(Religion$YEAR, na.rm = TRUE)
Income_last_year <- Religion %>%
  filter(TRAD6 %in% c("Juives", "Musulmanes", "Chrétiennes"),
         YEAR == last_year) %>%
  group_by(TRAD6) %>%
  summarise(
    mean_income = mean(INCOME, na.rm = TRUE),
    median_income = median(INCOME, na.rm = TRUE),
    count = sum(!is.na(INCOME)),  
    .groups = "drop"
  )
Income_last_year
Religion_with_income_cat <- Religion %>%
  group_by(YEAR) %>%
  mutate(
    avg_income_year = mean(INCOME, na.rm = TRUE),
    AVG_INCOME = ifelse(INCOME >= avg_income_year, 1, 0)
  ) %>%
  ungroup()
Religion_with_income_cat$AVG_INCOME

plot_data <- Religion_with_income_cat %>%
  filter(!is.na(TRAD12), !is.na(AVG_INCOME)) %>%
  group_by(YEAR, TRAD12, AVG_INCOME) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(YEAR, TRAD12) %>%
  mutate(prop = n / sum(n))
library(ggplot2)

income_plot <-ggplot(plot_data, aes(x = TRAD12, y = prop, fill = factor(AVG_INCOME))) +
  geom_col(position = "stack") +
  facet_wrap(~ YEAR) +
  labs(
    x = "Religious Classification (TRAD12)",
    y = "Proportion of Congregations",
    fill = "Income Category"
  ) +
  scale_fill_manual(
    values = c("0" = "gray70", "1" = "steelblue"),
    labels = c("Below average income", "Average or above income")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave(
  filename = "avg_income_by_trad12_year.pdf",
  plot = income_plot,
  device = "pdf",
  width = 11,
  height = 8.5
)  
#Second graph
hist_data <- Religion %>%
  filter(YEAR == 2022,
         !is.na(TRAD12),
         !is.na(TRAD6)) %>%
  group_by(TRAD12, TRAD6) %>%
  summarise(
    total_members = sum(NUMOFFMBR, na.rm = TRUE),
    .groups = "drop"
  )
religion_graph2 <-ggplot(hist_data, aes(x = TRAD12, y = total_members, fill = TRAD6)) +
  geom_col(position = "dodge") +
  facet_wrap(~ TRAD6) +
  labs(
    x = "12-level Religious Classification (TRAD12)",
    y = "Number of Official Members",
    fill = "6-level Religious Classification"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave(
  filename = "religion_graph2.pdf",
  width = 11,
  height = 8.5
)
#Graph 3 
ridge_data <- Religion %>%
  filter(
    YEAR == 2022,
    !is.na(INCOME),
    !is.na(GDREGION)
  )
library(ggridges)

religion_graph3 <-ggplot(ridge_data, aes(x = INCOME, y = GDREGION, fill = GDREGION)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2) +
  labs(
    x = "Yearly Income",
    y = "Region",
    title = "Distribution of Congregation Income by Region (2022)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )
ggsave(
  filename = "religion_graph3.pdf",
  width = 11,
  height = 8.5
)
# Graph 4
boxplot_data <- Religion %>%
  filter(
    YEAR == 2022,
    !is.na(NUMOFFMBR),
    !is.na(TRAD6),
    !is.na(GDREGION)
  )
religion_graph4 <-ggplot(boxplot_data, aes(x = TRAD6, y = NUMOFFMBR, fill = TRAD6)) +
  geom_boxplot() +
  facet_wrap(~ GDREGION) +
  labs(
    x = "6-level Religious Classification (TRAD6)",
    y = "Number of Official Members per Congregation",
    title = "Distribution of Congregation Size by Religion and Region",
    fill = "TRAD6"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
ggsave(
  filename = "religion_graph4.pdf",
  width = 11,
  height = 8.5
)
