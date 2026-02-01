detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("tidyverse", "ggplot2", "ggridges", "tradestatistics"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


NCSS <- read.csv("NCSS.csv")
head(NCSS)
# Put the data into R as CSV 
library(dplyr)
NCSS_selected <-select(NCSS,CASEID,YEAR,GDREGION,NUMOFFMBR,TRAD6,TRAD12,INCOME)
# Took out key variables that are needed for analysis
Religion <- NCSS_selected %>%
filter(TRAD6 %in% c("Juives", "Musulmanes", "Chrétiennes"))
head(Religion)
table(Religion$TRAD6)
#Filtered the dataset so that only the three religions are shown in the TRAD6 
Religion_classification <- Religion %>% 
filter(TRAD6 %in% c("Juives", "Musulmanes", "Chrétiennes")) %>%
group_by(YEAR, TRAD6) %>% 
summarise(total_members=sum(NUMOFFMBR, na.rm=TRUE)) %>%
arrange(YEAR,desc(total_members)) 
Religion_classification
#Computed the number of religious classifications by year 
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
# I computed the median and mean total income while excluding the NAs for the last year.

  
  
  



	