
# Libraries ---------------------------------------------------------------

library(tidyr)
library(dplyr)
library(readxl)
library(fabricatr)


# Continents --------------------------------------------------------------
# From https://www.kaggle.com/datasets/hserdaraltan/countries-by-continent

df <- read.csv("Data/Countries by continents.csv")
df <- df %>% rename("country" = "Country", "continent" = "Continent")
df <- df %>% 
  mutate(country = case_when(country == "Bosnia and Herzegovina" ~ "Bosnia-Herzegovina",
                             country == "Burkina" ~ "Burkina Faso",
                             country == "Cambodia" ~ "Cambodia (Kampuchea)",
                             country == "Swaziland" ~ "Kingdom of eSwatini (Swaziland)",
                             country == "Macedonia" ~ "Macedonia, FYR",
                             country == "Madagascar" ~ "Madagascar (Malagasy)",
                             country == "Burma (Myanmar)" ~ "Myanmar",
                             country == "Serbia" ~ "Serbia (Yugoslavia)",
                             country == "United Arab Emirates" ~ "UAE",
                             country == "Zimbabwe" ~ "Zimbabwe (Rhodesia)",
                             T ~ country))
write.csv(df, "Data/continents_cleaned.csv", row.names = F)


# Religion data -----------------------------------------------------------
# From https://datahub.io/sagargg/world-religion-projections

df <- read.csv("Data/by_rounded_percentage_share_csv.csv")
df <- subset(df, select = -c(Year, Region))
df <- df %>% rename("country" = "Country")
df <- subset(df, !duplicated(country))
df <- mutate_if(df, is.numeric, function(x) x / 100)

# Index of fractionalisation
df <- df %>% 
  mutate(religion_index = 1 - (Buddhists ^ 2 + Christians ^ 2 + Folk.Religions ^ 2 + 
           Hindus ^ 2 + Jews ^ 2 + Muslims ^ 2 + Other.Religions ^ 2 + Unaffiliated ^ 2))

dx <- df %>%  
  pivot_longer(-country, names_to = "religion") %>% 
  group_by(country) %>% 
  slice_max(value) %>% 
  ungroup()
dx <- left_join(dx, subset(df, select = c(country, religion_index)))

dx <- dx %>% 
  mutate(country = case_when(country == "Democratic Republic of the Congo" ~ "Congo",
                             country == "Cambodia" ~ "Cambodia (Kampuchea)",
                             country == "Swaziland" ~ "Kingdom of eSwatini (Swaziland)",
                             country == "Republic of Macedonia" ~ "Macedonia, FYR",
                             country == "Madagascar" ~ "Madagascar (Malagasy)",
                             country == "Burma (Myanmar)" ~ "Myanmar",
                             country == "Serbia" ~ "Serbia (Yugoslavia)",
                             country == "United Arab Emirates" ~ "UAE",
                             country == "Zimbabwe" ~ "Zimbabwe (Rhodesia)",
                             T ~ country))

# Religions we want Christians, Muslims, and Other
dx <- dx %>% mutate(religion = case_when(religion %in% c("Buddhists", "Folk.Religions", "Hindus",
                                                         "Jews", "Unaffiliated") ~ "Other",
                                         T ~ religion))

# Write csv
write.csv(subset(dx, select = c(country, religion, religion_index)), "Data/religion_cleaned.csv", row.names = F)


# GDP data ---------------------------------------------------------
# Source?

df <- read.csv("Data/gdp.csv")

df <- df %>% rename("country" = "Country.or.Area",
                    "year" = "Year",
                    "gdp" = "Value")

df <- df %>% 
  mutate(country = case_when(country == "Russian Federation" ~ "Russia",
                             country == "United Republic of Tanzania: Mainland" ~ "Tanzania",
                             country == "United Republic of Tanzania: Zanzibar" ~ "Tanzania",
                             country == "Côte d'Ivoire" ~ "Ivory Coast",
                             country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                             country == "Republic of North Macedonia" ~ "Macedonia, FYR",
                             country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
                             country == "China (mainland)" ~ "China",
                             country == "Bolivia (Plurinational State of)" ~ "Bolivia",
                             country == "Syrian Arab Republic" ~ "Syria",
                             country == "Lao People's Democratic Republic" ~ "Laos",
                             country == "Türkiye" ~ "Turkey",
                             country == "Bosnia and Herzegovina" ~ "Bosnia-Herzegovina",
                             country == "Republic of Moldova" ~ "Moldova",
                             country == "Iran, Islamic Republic of" ~ "Iran",
                             country == "Brunei Darussalam" ~ "Brunei",
                             country == "Viet Nam" ~ "Vietnam",
                             country == "United Arab Emirates" ~ "UAE",
                             country == "Cambodia" ~ "Cambodia (Kampuchea)",
                             country == "Kingdom of Eswatini" ~ "Kingdom of eSwatini (Swaziland)",
                             country == "Madagascar" ~ "Madagascar (Malagasy)",
                             country == "Serbia" ~ "Serbia (Yugoslavia)",
                             country == "Zimbabwe" ~ "Zimbabwe (Rhodesia)",
                             T ~ country))

df <- df %>% 
  group_by(country) %>% 
  mutate(lgdp_1988 = log(gdp[year == 1988][1]))
df <- subset(df, year > 1988)
df <- subset(df, select = -c(Item))

df <- df %>%
  group_by(country)%>%
  summarise(gdp = mean(gdp), lgdp_1988 = mean(lgdp_1988))

df <- df %>% 
  mutate(lgdp = log(gdp))

# Write csv
write.csv(subset(df, select = c(country, lgdp, lgdp_1988)), "Data/gdp_cleaned.csv", row.names = F)


# V-Dem rule of law data --------------------------------------------------
# From https://ourworldindata.org/grapher/rule-of-law-index

df <- read.csv("Data/rule-of-law-index.csv")
df <- df %>% rename("year" = "Year",
                    "country" = "Entity")
df <- subset(df, year > 1988)
df <- df %>% 
  group_by(country) %>% 
  mutate(rule_of_law = mean(rule_of_law_vdem_owid))

df <- subset(df, !duplicated(country))

df <- df %>% 
  mutate(country = case_when(country == "Bosnia and Herzegovina" ~ "Bosnia-Herzegovina",
                             country == "Cambodia" ~ "Cambodia (Kampuchea)",
                             country == "Cote d'Ivoire" ~ "Ivory Coast",
                             country == "Eswatini" ~ "Kingdom of eSwatini (Swaziland)",
                             country == "North Macedonia" ~ "Macedonia, FYR",
                             country == "Madagascar" ~ "Madagascar (Malagasy)",
                             country == "Serbia" ~ "Serbia (Yugoslavia)",
                             country == "United Arab Emirates" ~ "UAE",
                             country == "Zimbabwe" ~ "Zimbabwe (Rhodesia)",
                             T ~ country))

write.csv(subset(df, select = c(country, rule_of_law)), 
          "Data/rule_of_law_cleaned.csv", row.names = F)


# Population data ---------------------------------------------------------
# From https://data.worldbank.org/indicator/SP.POP.TOTL

df <- read.csv("Data/API_SP.POP.TOTL_DS2_en_csv_v2_79.csv", skip = 3)
df <- subset(df, select = -c(2:33, X))

df <- df %>% rename("country" = "Country.Name")

df <- df %>% 
  mutate(country = case_when(country == "Cambodia" ~ "Cambodia (Kampuchea)",
                             country == "Russian Federation" ~ "Russia",
                             country == "Zimbabwe" ~ "Zimbabwe (Rhodesia)",
                             country == "Turkiye" ~ "Turkey",
                             country == "Congo, Dem. Rep." ~ "Congo",
                             country == "Venezuela, RB" ~ "Venezuela",
                             country == "Lao PDR" ~ "Laos",
                             country == "Egypt, Arab Rep." ~ "Egypt",
                             country == "Cote d'Ivoire" ~ "Ivory Coast",
                             country == "Iran, Islamic Rep." ~ "Iran",
                             country == "Serbia" ~ "Serbia (Yugoslavia)",
                             country == "Bosnia and Herzegovina" ~ "Bosnia-Herzegovina",
                             country == "Yemen, Rep." ~ "Yemen",
                             country == "Gambia, The" ~ "Gambia",
                             country == "North Macedonia" ~ "Macedonia, FYR",
                             country == "Madagascar" ~ "Madagascar (Malagasy)",
                             country == "Syrian Arab Republic" ~ "Syria",
                             country == "Eswatini" ~ "Kingdom of eSwatini (Swaziland)",
                             country == "United Arab Emirates" ~ "UAE",
                             country == "Brunei Darussalam" ~ "Brunei",
                             country == "Viet Nam" ~ "Vietnam",
                             T ~ country))

# Wide to long
df <- df %>%
  pivot_longer(-country)

df <- df %>% rename("year" = "name",
                    "pop" = "value")

df <- df %>% 
  mutate(year = as.numeric(gsub("X", "", year)))

# Write csv
write.csv(df, "Data/population_data.csv", row.names = F)


# Building the data for analysis ------------------------------------------

# Oil field data
discoveries <- read.csv("Data/giant_fields_2018.csv")

# How many countries in discoveries
count(discoveries, COUNTRY) # 80 countries

# Conflict data
conflicts <- read_excel("Data/GEDEvent_v23_1.xlsx") %>% 
  dplyr::select(c("id", "conflict_new_id", "country", "year", "type_of_violence", 
                  "best"))

conflicts$conflict_new_id <- as.factor(conflicts$conflict_new_id)

# Continent names
continents <- read.csv("Data/continents_cleaned.csv")

# Population data
pop_data <- read.csv("Data/population_data.csv")

# GDP data
gdp <- read.csv("Data/gdp_cleaned.csv")

# Rule of law data
rule <- read.csv("Data/rule_of_law_cleaned.csv")

# Religion data
religion <- read.csv("Data/religion_cleaned.csv")

# How many countries in conflicts
n_distinct(conflicts$country) # 124 countries


# Country names -----------------------------------------------------------

# Match country names between conflicts and discoveries
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


# Merge other data --------------------------------------------------------

result_data <- left_join(result_data, gdp) # GDP
result_data <- left_join(result_data, pop_data) # Population
result_data <- left_join(result_data, rule) # Rule of law
result_data <- left_join(result_data, religion) # Religion
result_data <- left_join(result_data, continents) # Continent names

# We do not have some data for Kyrgyzstan and Brunei, so we remove them
# View(result_data[!complete.cases(result_data$lgdp),])
# View(result_data[!complete.cases(result_data$pop),])
# View(result_data[!complete.cases(result_data$rule_of_law),])
# View(result_data[!complete.cases(result_data$religion),])
# View(result_data[!complete.cases(result_data$continent),])
result_data <- subset(result_data, !(country %in% c("Kyrgyzstan", "Brunei")))

# GDP tercile
result_data$lgdp_tercile <- split_quantile(result_data$lgdp, 3)

# Rule of law tercile
result_data$rule_of_law_tercile <- split_quantile(result_data$rule_of_law, 3)

# Religion index tercile
result_data$religion_index_tercile <- split_quantile(result_data$religion_index, 3)


# Variable transformations ------------------------------------------------

# Logs
result_data <- result_data %>% 
  mutate(log_number_of_conflicts_started = log(number_of_conflicts_started + 1),
         log_total_fatalities = log(total_fatalities + 1))

# Scale by population
result_data <- result_data %>% 
  mutate(scaled_number_of_conflicts_started = number_of_conflicts_started / pop,
         scaled_total_fatalities = total_fatalities / pop)


# Write to csv ------------------------------------------------------------

write.csv(subset(result_data, select = -c(country_year)), "Data/final_dataset.csv", row.names = F)

