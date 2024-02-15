
# Population data ---------------------------------------------------------

library(tidyr)
library(dplyr)

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
