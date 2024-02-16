
# Libraries and data ------------------------------------------------------

library(dplyr)
library(ggplot2)
library(gplots)
library(readxl)
library(zoo)
library(did)

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



# Merge population and gdp data -------------------------------------------

result_data <- left_join(result_data, gdp)
result_data <- left_join(result_data, pop_data)

View(result_data[!complete.cases(result_data$gdp),])


# Event study -------------------------------------------------------------

## Number of conflicts started ----

# Take logs
result_data <- result_data %>% 
  mutate(number_of_conflicts_started = case_when(number_of_conflicts_started == 0 ~ 1e-50, 
                                                 T ~ number_of_conflicts_started))

result_data <- result_data %>% 
  mutate(log_number_of_conflicts_started = log(number_of_conflicts_started) / 100,
         .after = number_of_conflicts_started)

# Group-time average treatment effects
out <- att_gt(yname = "log_number_of_conflicts_started",
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
  ggtitle("Average Effect on Log No. of Conflicts Started") +
  theme_classic(base_size = 12) +
  geom_segment(aes(x = 0, y = -4, xend = 0, yend = 3), lty = 2, col = "black")


## Total fatalities ----

# Scale the variable
result_data <- result_data %>% 
  mutate(total_fatalities = case_when(total_fatalities == 0 ~ 1e-50, 
                                      T ~ total_fatalities))
result_data <- result_data %>% 
  mutate(scaled_total_fatalities = log(total_fatalities / pop) / 100)

# Group-time average treatment effects
out <- att_gt(yname = "scaled_total_fatalities",
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
  ggtitle("Average Effect on Total No. of Fatalities, Scaled by Population and in Logs") +
  theme_classic(base_size = 12) +
  geom_segment(aes(x = 0, y = -4, xend = 0, yend = 3), lty = 2, col = "black")


# Heterogeneous effects using GDP ------------------------------------------


