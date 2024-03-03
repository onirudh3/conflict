
# Libraries ---------------------------------------------------------------

library(tidyr)
library(dplyr)
library(readxl)


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

df <- subset(df, year > 1988)
df <- subset(df, select = -c(Item))

df <- df %>%
  group_by(country)%>%
  summarise(gdp = mean(gdp))

df <- df %>% 
  mutate(gdp = log(gdp))

df <- df %>% 
  mutate(quartile_1 = quantile(gdp, probs = 0.25),
         quartile_2 = quantile(gdp, probs = 0.50),
         quartile_3 = quantile(gdp, probs = 0.75),
         quartile_4 = quantile(gdp, probs = 1)) %>% 
  mutate(gdp_quartile = case_when(gdp >= 0 & gdp < quartile_1 ~ 1,
                                  gdp >= quartile_1 & gdp < quartile_2 ~ 2,
                                  gdp >= quartile_2 & gdp < quartile_3 ~ 3, 
                                  T ~ 4))

# Write csv
write.csv(subset(df, select = c(country, gdp, gdp_quartile)), "Data/gdp_cleaned.csv", row.names = F)


# Political stability data ---------------------------------------------------------
# From https://data.worldbank.org/indicator/PV.PER.RNK

df <- read.csv("Data/API_PV.PER.RNK_DS2_en_csv_v2_1195.csv", skip = 3)
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
                    "stability" = "value")

df <- df %>% 
  mutate(year = as.numeric(gsub("X", "", year)))

df <- subset(df, !is.na(stability))

df <- df %>%
  group_by(country)%>%
  summarise(stability = mean(stability))

df <- df %>% 
  mutate(quartile_stability_1 = quantile(stability, probs = 0.25),
         quartile_stability_2 = quantile(stability, probs = 0.50),
         quartile_stability_3 = quantile(stability, probs = 0.75),
         quartile_stability_4 = quantile(stability, probs = 1)) %>% 
  mutate(quartile_stability = case_when(stability >= 0 & stability < quartile_stability_1 ~ 1,
                                        stability >= quartile_stability_1 & stability < quartile_stability_2 ~ 2,
                                        stability >= quartile_stability_2 & stability < quartile_stability_3 ~ 3, 
                                        T ~ 4))

# Write csv
write.csv(subset(df, select = c(country, stability, quartile_stability)), "Data/stability_data.csv", row.names = F)


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

# Population data
pop_data <- read.csv("Data/population_data.csv")

# GDP data
gdp <- read.csv("Data/gdp_cleaned.csv")

# Political stability data
stability_data <- read.csv("Data/stability_data.csv")

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
result_data <- left_join(result_data, stability_data) # Political stability

# We do not have population and political stability data for Kyrgyzstan, so we remove it
# View(result_data[!complete.cases(result_data$stability),])
result_data <- subset(result_data, country != "Kyrgyzstan")


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

write.csv(result_data, "Data/final_dataset.csv", row.names = F)

