
# Libraries and data ------------------------------------------------------

library(dplyr)
library(ggplot2)
library(gplots)
library(readxl)
library(zoo)
library(did)
library(gtsummary)
library(xtable)

# Oil field data
discoveries <- read.csv("Data/giant_fields_2018.csv")

# How many countries in discoveries
count(discoveries, COUNTRY) # 80 countries

# Conflict data
conflicts <- read_excel("Data/GEDEvent_v23_1.xlsx") %>% 
  dplyr::select(c("id", "conflict_new_id", "country", "year", "type_of_violence", 
                  "best"))

conflicts$conflict_new_id <- as.factor(conflicts$conflict_new_id)

# Population data from world bank
pop_data <- read.csv("Data/population_data.csv")

# GDP Data
gdp <- read.csv("Data/gdp_cleaned.csv")

# Political Stability data
stability_data <- read.csv("Data/stability_data.csv")

# How many countries in conflicts
n_distinct(conflicts$country) # 124 countries


# Country names -----------------------------------------------------------

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

# Write to csv
write.csv(conflicts, "Data/conflicts_raw.csv", row.names = F)
write.csv(discoveries, "Data/discoveries_raw.csv", row.names = F)


# Getting unique conflicts ------------------------------------------------

conflictsuniq <- conflicts %>%
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


# Merge population and GDP data -------------------------------------------

result_data <- left_join(result_data, gdp)
result_data <- left_join(result_data, pop_data)
result_data <- left_join(result_data, stability_data)

# We do not have population and stability data for Kyrgyzstan, so we remove it
# View(result_data[!complete.cases(result_data$stability),])
result_data <- subset(result_data, country != "Kyrgyzstan")


# Summary Statistics ------------------------------------------------------

n_distinct(result_data$country) # Number of countries
n_distinct(subset(result_data, total_discoveries != 0)$country) # Number of countries with discovery
summary(subset(result_data, total_discoveries != 0)$first_discovery) # Year of first discovery, conditional
summary(result_data$total_discoveries) # Number of discoveries
summary(result_data$total_fatalities) # Number of fatalities
summary(result_data$number_of_conflicts_started) # Number of conflicts started
summary(result_data$gdp) # Log GDP per capita
summary(result_data$pop) # Population
summary(result_data$stability) # Political stability

# Heterogeneous groups
summary(subset(result_data, gdp_quartile %in% c(3, 4))$total_discoveries)
summary(subset(result_data, gdp_quartile %in% c(3, 4))$number_of_conflicts_started)

summary(subset(result_data, gdp_quartile %in% c(1, 2))$total_discoveries)
summary(subset(result_data, gdp_quartile %in% c(1, 2))$number_of_conflicts_started)

# Variable transformations ------------------------------------------------

# Logs
result_data <- result_data %>% 
  mutate(log_number_of_conflicts_started = log(number_of_conflicts_started + 1),
         log_total_fatalities = log(total_fatalities + 1))

# Scale by population
result_data <- result_data %>% 
  mutate(scaled_number_of_conflicts_started = number_of_conflicts_started / pop * 10000,
         scaled_total_fatalities = total_fatalities / pop * 100)


# Checking correlation between conflicts and fatalities -------------------

# Set the plotting space
par(mfrow = c(1, 3))

# Scatter plot
with(result_data, plot(number_of_conflicts_started, total_fatalities, cex.lab = 1.5))
abline(lm(total_fatalities ~ number_of_conflicts_started, data = result_data), col = "blue")

# With logs
with(result_data, plot(log_number_of_conflicts_started, log_total_fatalities, cex.lab = 1.5))
abline(lm(log_total_fatalities ~ log_number_of_conflicts_started, data = result_data), col = "blue")

# Population scaled
with(result_data, plot(scaled_number_of_conflicts_started, scaled_total_fatalities, cex.lab = 1.5))
abline(lm(scaled_total_fatalities ~ scaled_number_of_conflicts_started, data = result_data), col = "blue")

# Correlation with population is positive
with(result_data, plot(number_of_conflicts_started, pop, cex.lab = 1.5))
abline(lm(pop ~ number_of_conflicts_started, data = result_data), col = "blue")


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
  ylim(c(-9, 9))


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
  ylim(c(-0.9, 0.9))


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
  ylim(c(-9, 9))


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
  ggtitle("Average Effect on No. of Fatalities (Scaled by Population and Multiplied by 100)") +
  theme_classic(base_size = 12) +
  ylim(c(-0.9, 0.9))


# Adding a conflict dummy -------------------------------------------------

## Log number of conflicts ----
out <- att_gt(yname = "scaled_total_fatalities",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              xformla = ~ conflict_dummy,
              data = result_data,
              est_method = "reg",
              allow_unbalanced_panel = T)
es <- aggte(out, type = "dynamic", na.rm = T)
summary(aggte(out, type = "group"))
ggdid(es) +
  ggtitle("Average Effect on No. of Fatalities (Scaled by Population and Multiplied by 100) (Including Conflict Dummy)") +
  theme_classic(base_size = 12) +
  ylim(c(-0.5, 0.5))


# Heterogeneous effects using GDP and political stability -----------------

## Checking correlation ----
with(result_data, plot(gdp, stability, cex.lab = 1.5))
abline(lm(stability ~ gdp, data = result_data), col = "blue")

## Richer countries ----
quart <- subset(result_data, quartile_stability %in% c(3, 4))

# Number of conflicts
out <- att_gt(yname = "log_number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              xformla = ~ 1,
              data = quart,
              est_method = "reg",
              allow_unbalanced_panel = T)
es <- aggte(out, type = "dynamic", na.rm = T)
summary(aggte(out, type = "group"))
ggdid(es) +
  ggtitle("Average Effect on Log No. of Conflicts Started (Above Median Political Stability)") +
  theme_classic(base_size = 12) +
  ylim(c(-9, 9))






