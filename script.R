
# Libraries and data ------------------------------------------------------

library(tidyverse)
library(readxl)
library(geosphere)
library(zoo)
library(gplots)
library(plm) # Panel data analysis
library(car)
library(stargazer) # For latex tables
library(fastDummies) # For dummy variables
library(broom)
library(lmtest)

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
                                             select = c("country_year", 
                                                        "number_of_conflicts_started")), 
                         by = "country_year")

result_data <- result_data %>% 
  mutate(number_of_conflicts_started = case_when(is.na(number_of_conflicts_started) ~ 0, 
                                                 T ~ number_of_conflicts_started))

result_data <- result_data %>% 
  distinct() # Remove duplicate rows


# Number of discoveries in a given country-year ---------------------------

# Number of discoveries in a given country
result_data <- result_data %>% 
  group_by(country) %>% 
  mutate(sum_disc = sum(discovery_dummy))

# Lags for discoveries in the past 5 and 10 years
result_data <- result_data %>% 
  mutate(discovery_dummy_lag_5 = + rollapplyr(discovery_dummy > 0, 5, any, 
                                              fill = NA), .after = discovery_dummy)

result_data <- result_data %>% 
  mutate(discovery_dummy_lag_10 = + rollapplyr(discovery_dummy > 0, 10, any, 
                                               fill = NA), .after = discovery_dummy_lag_5)


# Remove rows before 1989 -------------------------------------------------

result_data <- subset(result_data, year >= 1989)

result_data <- subset(result_data, select = -c(country_year))

result_data <- result_data %>% relocate(year, .after = country)

result_data$country <- as.character(result_data$country)

# sort(unique(result_data$country))


# Time period indicator ---------------------------------------------------

result_data <- result_data %>%
  group_by(country) %>% 
  mutate(period = row_number() - which(discovery_dummy == 1)[1], .after = discovery_dummy)

result_data <- dummy_cols(result_data, select_columns = "period", ignore_na = T)

result_data <- result_data %>% 
  mutate_at(c(10:73), ~ replace_na(., 0))

# Rename period columns for clarity
colnames(result_data) <- str_replace(colnames(result_data), "period_-", "lag")
colnames(result_data) <- str_replace(colnames(result_data), "period_", "lead")


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


# Linear model ------------------------------------------------------------

reg1 <- lm(conflict_dummy ~ discovery_dummy + discovery_dummy_lag_5 + 
             discovery_dummy_lag_10, result_data)
summary(reg1)
stargazer(reg1)


# Difference in difference ------------------------------------------------

# It is a long formula
variables <- names(result_data)[10:73]
formula_str <- paste("conflict_dummy ~ factor(year) + factor(country) + discovery_dummy +", 
                     paste(variables, collapse = " + "))
formula <- as.formula(formula_str)

# Regression
reg2 <- plm(formula, result_data, model = "within", effect = "twoways", 
            index = c("country", "year"))
summary(reg2)

# Robust standard errors
G <- length(unique(result_data$country))
c <- G / (G - 1)
robust_se <- coeftest(reg2, c * vcovHC(reg2, type = 'HC1', cluster = 'group'))
reg2 <- summary(reg2)
reg2$coefficients[, 2:4] <- robust_se[, 2:4]

# Confidence intervals
coefs <- data.frame(reg2[["coefficients"]])
coefs$conf.low <- coefs$Estimate + c(-1) * coefs$Std..Error * qt(0.975, 42)
coefs$conf.high <- coefs$Estimate + c(1) * coefs$Std..Error * qt(0.975, 42)

# Some wrangling
interest <- c("lag30", "lag29", "lag28", "lag27", "lag26", "lag25", "lag24", 
              "lag23", "lag22", "lag21", "lag20", "lag19", "lag18", "lag17", 
              "lag16", "lag15", "lag14", "lag13", "lag12", "lag11", "lag10", 
              "lag9", "lag8", "lag7", "lag6", "lag5", "lag4", "lag3", "lag2",
              "lag1", "lead0", "lead1", "lead2", "lead3", "lead4", "lead5", 
              "lead6", "lead7", "lead8", "lead9", "lead10", "lead11", "lead12", 
              "lead13", "lead14", "lead15", "lead16", "lead17", "lead18", 
              "lead19", "lead20", "lead21", "lead22", "lead23", "lead24", 
              "lead25", "lead26", "lead27", "lead28", "lead29", "lead30", 
              "lead31", "lead32")
coefs <- subset(coefs, rownames(coefs) %in% interest)
coefs <- cbind(rownames(coefs), data.frame(coefs, row.names = NULL))
coefs <- coefs %>% rename("coefficient" = "rownames(coefs)")
coefs <- coefs %>%
  mutate(coefficient =  factor(coefficient, levels = interest)) %>%
  arrange(coefficient)  
coefs$time <- c(seq(-30, 32, 1))

# Plot
ggplot(coefs, aes(time, Estimate))+
  geom_line() +
  geom_point()+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = 2)
