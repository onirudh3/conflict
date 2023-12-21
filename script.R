
# Libraries and data ------------------------------------------------------

library(tidyverse)
library(readxl)
library(geosphere)
library(zoo)
library(gplots)
library(plm) # Panel data analysis
library(car)
library(stargazer) # For latex tables

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
                             COUNTRY == "Divided Neutral Zone: Kuwait/Saudi Arabia" ~ "Kuwait",
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

# Duration of each conflict
conflicts <- conflicts %>% 
  mutate(duration = difftime(date_end, date_start, units = "days"))

# Get distinct conflicts
conflictsuniq <- conflicts %>%
  arrange(date_start) %>%
  distinct(conflict_new_id, .keep_all = TRUE)

# Cleaned dataset
result_data <- expand.grid(year = seq(1980, 2022, 1), 
                           country = unique(c(conflictsuniq$country, discoveries$COUNTRY)))

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

# Number of starting conflicts
conflictsuniq <- conflictsuniq %>% 
  group_by(country_year) %>% 
  mutate(number_of_conflicts_started = n())

# Merge to result_data
result_data <- left_join(result_data, subset(conflictsuniq, 
                                             select = c("country_year", "number_of_conflicts_started")), by = "country_year")

result_data <- result_data %>% 
  mutate(number_of_conflicts_started = case_when(is.na(number_of_conflicts_started) ~ 0, T ~ number_of_conflicts_started))

result_data <- result_data %>% 
  distinct() # Remove duplicate rows


# Number of discoveries in a given country-year ---------------------------

# Number of discoveries in a given country
result_data <- result_data %>% 
  group_by(country) %>% 
  mutate(sum_disc = sum(discovery_dummy))

# Lags for discoveries in the past 5 and 10 years
result_data <- result_data %>% 
  mutate(discovery_dummy_lag_5 = + rollapplyr(discovery_dummy > 0, 5, any, fill = NA), .after = discovery_dummy)

result_data <- result_data %>% 
  mutate(discovery_dummy_lag_10 = + rollapplyr(discovery_dummy > 0, 10, any, fill = NA), .after = discovery_dummy_lag_5)


# Remove rows before 1989 -------------------------------------------------

result_data <- subset(result_data, year >= 1989)

result_data <- subset(result_data, select = -c(country_year))

result_data <- result_data %>% relocate(year, .after = country)


# Linear model ------------------------------------------------------------

reg1 <- lm(conflict_dummy ~ discovery_dummy + discovery_dummy_lag_5 + discovery_dummy_lag_10, result_data)
summary(reg1)
stargazer(reg1)


# Fixed effects -----------------------------------------------------------

reg2 <- plm(conflict_dummy ~ discovery_dummy + discovery_dummy_lag_5 + 
                discovery_dummy_lag_10 + factor(year) + factor(country), result_data,
              model = "within", effect = "twoways", index = c("country", "year"))
summary(reg2)
stargazer(reg2)


# Exploratory analysis ----------------------------------------------------

# Mean number of conflicts by year
plotmeans(number_of_conflicts_started ~ year, data = result_data, n.label = F)

# Total number of conflicts by year
dx <- result_data %>% 
  subset(sum_disc == 0)
dx <- dx %>% 
  group_by(year) %>% 
  summarise(number_of_conflicts_started = sum(number_of_conflicts_started))
dx %>% 
  ggplot(aes(year, number_of_conflicts_started)) +
  geom_line() +
  theme_classic()


# Parallel trends in neighboring countries --------------------------------

# Somalia, Ethiopia (1 discovery in 2010)
dx <- subset(result_data, country %in% c("Somalia", "Ethiopia"))
dx %>% 
  ggplot(aes(year, number_of_conflicts_started, color = country)) +
  geom_line() +
  geom_segment(aes(x = 2010, y = -0.5, xend = 2010, yend = 10), lty = 2, col = "black") +
  geom_text(size = 5, x = 2010, y = 9, label = "Oil discovery in 2010 in Ethiopia", colour = "black") +
  theme_classic(base_size = 20)

# Kenya, Ethiopia (1 discovery in 2010)
dx <- subset(result_data, country %in% c("Kenya", "Ethiopia"))
dx %>% 
  ggplot(aes(year, number_of_conflicts_started, color = country)) +
  geom_line() +
  geom_segment(aes(x = 2010, y = -0.5, xend = 2010, yend = 10), lty = 2, col = "black") +
  geom_text(size = 5, x = 2010, y = 9, label = "Oil discovery in 2010 in Ethiopia", colour = "black") +
  theme_classic(base_size = 20)

# Zimbabwe, South Africa
dx <- subset(result_data, country %in% c("Zimbabwe", "South Africa"))
dx %>% 
  ggplot(aes(year, number_of_conflicts_started, color = country)) +
  geom_line() +
  geom_segment(aes(x = 2019, y = -0.5, xend = 2019, yend = 10), lty = 2, col = "black") +
  geom_text(size = 5, x = 2010, y = 9, label = "Oil discovery in 2019 in SA", colour = "black") +
  theme_classic(base_size = 20)


# Difference in difference ------------------------------------------------

reg3 <- lm(conflict_dummy ~ factor(country) + factor(year) + discovery_dummy * year, result_data)
summary(reg3)
stargazer(reg3)


# Event study plot --------------------------------------------------------





