
# Libraries and data ------------------------------------------------------

# install.packages(plyr) # Install this package for computing counts
library(tidyverse)
library(readxl)
library(geosphere)
library(zoo)
library(gplots)
library(plm) # Panel data analysis
library(car)
library(stargazer) # For latex tables
library(fastDummies) # Creating dummy variables
library(broom)
library(lmtest)
library(did) # Difference-in-difference
library(dplyr)

# Oil field data
discoveries <- read.csv("Data/giant_fields_2018.csv")

# How many countries in discoveries
plyr::count(discoveries$COUNTRY) # 80 countries

# Conflict data
conflicts <- read_excel("Data/GEDEvent_v23_1.xlsx")

# How many countries in conflicts
plyr::count(conflicts$country) # 124 countries


# Cleaning ----------------------------------------------------------------

# Checking what countries are in each dataset
sort(unique(conflicts$country))
setdiff(discoveries$COUNTRY, conflicts$country)
setdiff(conflicts$country, discoveries$COUNTRY)

# Differences in country names
sort(setdiff(union(unique(conflicts$country), unique(discoveries$COUNTRY)), 
        intersect(unique(conflicts$country), unique(discoveries$COUNTRY))))

# Match country names in the two datasets
discoveries <-discoveries %>%
  mutate(COUNTRY = case_when(COUNTRY == "Sierre Leone" ~ "Sierra Leone", 
                             COUNTRY == "Equatorial Guinea" ~ "Guinea",
                             COUNTRY == "Congo (Brazzaville)" ~ "Congo",
                             COUNTRY == "Norway and United Kingdom" ~ "Norway",
                             COUNTRY == "Divided Neutral Zone: Kuwait/Saudi Arabia" ~ "Kuwait", # for simplicity
                             TRUE ~ COUNTRY))
conflicts <- conflicts %>% 
  mutate(country = case_when(country == "Russia (Soviet Union)" ~ "Russia",
                             country == "Myanmar (Burma)" ~ "Myanmar",
                             country == "Yemen (North Yemen)" ~ "Yemen",
                             country == "United States of America" ~ "United States",
                             country == "DR Congo (Zaire)" ~ "Congo",
                             country == "Guinea-Bissau" ~ "Guinea",
                             country == "United Arab Emirates" ~ "UAE",
                             TRUE ~ country))

# Get distinct conflicts
conflictsuniq <- conflicts %>%
  arrange(date_start) %>%
  distinct(conflict_new_id, .keep_all = TRUE)

# Cleaned dataset
result_data <- expand.grid(year = seq(1980, 2022, 1), 
                           country = unique(c(conflictsuniq$country, discoveries$COUNTRY)))

# Numeric ID for each country
result_data <- result_data %>%
  group_by(country) %>%
  mutate(country_ID = cur_group_id(),
         .before = country)

# Conflict dummy
result_data$conflict_dummy <- 0
result_data <- result_data %>% 
  mutate(country_year = paste(country, year), .after = country)
conflictsuniq <- conflictsuniq %>% 
  mutate(country_year = paste(country, year))
result_data <- result_data %>% 
  mutate(conflict_dummy = case_when(country_year %in% conflictsuniq$country_year ~ 1, T ~ 0))

# Discovery dummy
result_data$discovery_dummy <- 0
discoveries <- discoveries %>% 
  mutate(country_year = paste(COUNTRY, year))
result_data <- result_data %>% 
  mutate(discovery_dummy = case_when(country_year %in% discoveries$country_year ~ 1, T ~ 0))


# Number of conflicts in a given country-year -----------------------------

# Number of starting conflicts and total fatalities
conflictsuniq <- conflictsuniq %>% 
  group_by(country_year) %>% 
  mutate(number_of_conflicts_started = n(),
         total_fatalities = sum(best),
         active_conflicts = sum(active_year))

# From this point, using all violence types -------------------------------

# Create different datasets for different type_of_violence (see other script)
# conflictsuniq_1 <- subset(conflictsuniq, type_of_violence == 1) # state-based conflict
# conflictsuniq_2 <- subset(conflictsuniq, type_of_violence == 2) # non-state conflict
# conflictsuniq_3 <- subset(conflictsuniq, type_of_violence == 3) # one-sided violence

# See other script for event study using other types of violence (type_of_violence)

# Merge number_of_conflicts_started and total_fatalities to result_data
result_data <- left_join(result_data, subset(conflictsuniq, 
                                             select = c("country_year", 
                                                        "number_of_conflicts_started",
                                                        "total_fatalities",
                                                        "active_conflicts")), 
                         by = "country_year")
result_data <- result_data %>% 
  mutate(number_of_conflicts_started = case_when(is.na(number_of_conflicts_started) ~ 0, 
                                                 T ~ number_of_conflicts_started),
         total_fatalities = case_when(is.na(total_fatalities) ~ 0,
                                      T ~ total_fatalities))

# Remove duplicate rows
result_data <- result_data %>% 
  distinct()


# Number of discoveries in a given country-year ---------------------------

# Number of discoveries in a given country
result_data <- result_data %>% 
  group_by(country) %>% 
  mutate(total_discoveries = sum(discovery_dummy))

# What are the countries where there are no discoveries?
plyr::count(subset(result_data, total_discoveries == 0)$country) # 62 countries

# Lag dummies for discoveries in the past 5 and 10 years
result_data <- result_data %>% 
  mutate(discovery_dummy_lag_5 = + rollapplyr(discovery_dummy > 0, 5, any, 
                                              fill = NA), .after = discovery_dummy)

result_data <- result_data %>% 
  mutate(discovery_dummy_lag_10 = + rollapplyr(discovery_dummy > 0, 10, any, 
                                               fill = NA), .after = discovery_dummy_lag_5)


# Remove rows before 1989 -------------------------------------------------

result_data <- subset(result_data, year >= 1989)
result_data <- subset(result_data, select = -c(country_year))
result_data <- result_data %>% relocate(year, .after = country)
result_data$country <- as.character(result_data$country)
# sort(unique(result_data$country))


# Time period indicator ---------------------------------------------------

# Discovery period
result_data <- result_data %>%
  group_by(country) %>% 
  mutate(period = row_number() - which(discovery_dummy == 1)[1],
         .after = discovery_dummy)

# Year of first discovery
result_data <- result_data %>% 
  group_by(country) %>%
  mutate(first_discovery = `year`[period == 0],
         .after = discovery_dummy)
result_data <- result_data %>% 
  mutate(first_discovery = case_when(is.na(first_discovery) ~ 0, T ~ first_discovery))


# Event study -------------------------------------------------------------


## Number of conflicts started ----

# Group-time average treatment effects
out <- att_gt(yname = "number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              xformla = ~ 1,
              data = result_data,
              est_method = "reg",
              allow_unbalanced_panel = T)
# ggdid(out) # Too many groups to see anything

# Aggregate group-time average treatment effects (dynamic event study)
es <- aggte(out, type = "dynamic", na.rm = T)
# summary(es)

# Overall average treatment effect
summary(aggte(out, type = "group"))

# Event study plot
ggdid(es) +
  ggtitle("Average Effect on No. of Conflicts Started (All Conflict Types)") +
  theme_classic(base_size = 12) +
  geom_segment(aes(x = 0, y = -4, xend = 0, yend = 3), lty = 2, col = "black")


## Total fatalities ----

# Group-time average treatment effects
out <- att_gt(yname = "total_fatalities",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              xformla = ~ 1,
              data = result_data,
              est_method = "reg",
              allow_unbalanced_panel = T)

# Aggregate group-time average treatment effects (dynamic event study)
es <- aggte(out, type = "dynamic", na.rm = T)

# Overall average treatment effect
summary(aggte(out, type = "group"))

# Event study plot
ggdid(es) +
  ggtitle("Average Effect on Total No. of Fatalities") +
  theme_classic(base_size = 12)
  
