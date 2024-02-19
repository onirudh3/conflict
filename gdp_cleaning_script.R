
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


