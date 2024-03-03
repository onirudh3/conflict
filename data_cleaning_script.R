
# Libraries ---------------------------------------------------------------

library(tidyr)
library(dplyr)
library(readxl)

# GDP data ---------------------------------------------------------

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


