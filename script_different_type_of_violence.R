
# Different types of violence
# Libraries and data ------------------------------------------------------

library(dplyr)
library(ggplot2)
library(gplots)
library(readxl)
library(zoo)
library(did)

# Data
conflicts <- read.csv("Data/conflicts_raw.csv")
discoveries <- read.csv("Data/discoveries_raw.csv")
pop_data <- read.csv("Data/population_data.csv")


# Getting unique conflicts ------------------------------------------------

conflictsuniq <- conflicts %>%
  filter(type_of_violence == 1) %>% # choose 1 (state based), 2 (non-state), or 3 (one-sided)
  mutate(country_year = paste(country, year))

# Total fatalities at country year level
conflictsuniq <- conflictsuniq %>% 
  group_by(country_year) %>% 
  mutate(total_fatalities = sum(best))

# Number of starting conflict events at country year level
conflictsuniq <- conflictsuniq %>% 
  group_by(country_year) %>% 
  mutate(number_of_conflicts_started = n())

# Aggregate to country year level
conflictsuniq <- conflictsuniq %>% 
  select(country, year, country_year, number_of_conflicts_started, total_fatalities) %>%
  distinct(country_year, .keep_all = T) %>% 
  arrange(country, year)


# Building the panel dataset ----------------------------------------------

# Cleaned dataset
result_data <- expand.grid(year = seq(1989, 2022, 1), 
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
result_data <- result_data %>% 
  mutate(conflict_dummy = case_when(country_year %in% conflictsuniq$country_year ~ 1, T ~ 0))

# Discovery dummy
result_data$discovery_dummy <- 0
discoveries <- discoveries %>% 
  mutate(country_year = paste(COUNTRY, year))
result_data <- result_data %>% 
  mutate(discovery_dummy = case_when(country_year %in% discoveries$country_year ~ 1, T ~ 0))


# Merge conflict data -----------------------------------------------------

# Merge number_of_conflicts_started and total_fatalities to result_data
result_data <- left_join(result_data, subset(conflictsuniq, 
                                             select = c("country_year", 
                                                        "number_of_conflicts_started",
                                                        "total_fatalities")), 
                         by = "country_year")

result_data <- result_data %>% 
  mutate(number_of_conflicts_started = case_when(is.na(number_of_conflicts_started) ~ 0, 
                                                 T ~ number_of_conflicts_started),
         total_fatalities = case_when(is.na(total_fatalities) ~ 0,
                                      T ~ total_fatalities))

result_data <- result_data %>% 
  group_by(country_year) %>% 
  mutate(number_of_conflicts_started = sum(number_of_conflicts_started),
         total_fatalities = sum(total_fatalities))

# Remove duplicate rows
result_data <- result_data %>% 
  distinct()


# Number of discoveries in a given country-year ---------------------------

# Number of discoveries in a given country
result_data <- result_data %>% 
  group_by(country) %>% 
  mutate(total_discoveries = sum(discovery_dummy))

# What are the countries where there are no discoveries?
n_distinct(subset(result_data, total_discoveries == 0)$country) # 78 countries


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


# Merge population data ---------------------------------------------------

result_data <- left_join(result_data, pop_data)
result_data <- subset(result_data, country != "Kyrgyzstan")


# Variable transformations ------------------------------------------------

# Logs
result_data <- result_data %>% 
  mutate(log_number_of_conflicts_started = log(number_of_conflicts_started + 1),
         log_total_fatalities = log(total_fatalities + 1))

# Scale by population
result_data <- result_data %>% 
  mutate(scaled_number_of_conflicts_started = number_of_conflicts_started / pop * 10000,
         scaled_total_fatalities = total_fatalities / pop * 1000)


# Number of conflicts started ---------------------------------------------


## Log number of conflicts ----
out <- att_gt(yname = "log_number_of_conflicts_started",
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
  ggtitle("Average Effect on Log No. of Conflicts Started") +
  theme_classic(base_size = 12) +
  ylim(c(-7, 7))


## Population scaled number of conflicts ----
out <- att_gt(yname = "scaled_number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              xformla = ~ 1,
              data = result_data,
              est_method = "reg",
              allow_unbalanced_panel = T)
es <- aggte(out, type = "dynamic", na.rm = T)
summary(aggte(out, type = "group"))
ggdid(es) +
  ggtitle("Average Effect on No. of Conflicts Started (Scaled by Population and Multiplied by 10,000)") +
  theme_classic(base_size = 12) +
  ylim(c(-0.5, 0.5))


# Total fatalities --------------------------------------------------------


## Log fatalities ----
out <- att_gt(yname = "log_total_fatalities",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              xformla = ~ 1,
              data = result_data,
              est_method = "reg",
              allow_unbalanced_panel = T)
es <- aggte(out, type = "dynamic", na.rm = T)
summary(aggte(out, type = "group"))
ggdid(es) +
  ggtitle("Average Effect on Log No. of Fatalities") +
  theme_classic(base_size = 12) +
  ylim(c(-7, 7))


## Population scaled fatalities ----
out <- att_gt(yname = "scaled_total_fatalities",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              xformla = ~ 1,
              data = result_data,
              est_method = "reg",
              allow_unbalanced_panel = T)
es <- aggte(out, type = "dynamic", na.rm = T)
summary(aggte(out, type = "group"))
ggdid(es) +
  ggtitle("Average Effect on No. of Fatalities (Scaled by Population and Multiplied by 1,000)") +
  theme_classic(base_size = 12) +
  ylim(c(-0.5, 0.5))
