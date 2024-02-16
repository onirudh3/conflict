
# Population data ---------------------------------------------------------

library(tidyr)
library(dplyr)

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

# Write csv
write.csv(df, "Data/gdp_cleaned.csv", row.names = F)


