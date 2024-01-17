

# Other types of violence -------------------------------------------------

# Merge number_of_conflicts_started to result_data
result_data <- left_join(result_data, subset(conflictsuniq_1, # choose conflictsuniq_1, conflictsuniq_2, or conflictsuniq_3
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
  mutate(total_discoveries = sum(discovery_dummy))

# What are the countries where there are no discoveries?
plyr::count(subset(result_data, total_discoveries == 0)$country) # 62 countries

# Lag dummies for discoveries in the past 5 and 10 years
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


# Event study -------------------------------------------------------------

out <- att_gt(yname = "number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              xformla = ~ 1,
              data = result_data,
              est_method = "reg")

# Dynamic event study
es <- aggte(out, type = "dynamic", na.rm = T)
summary(es)

# Event study plot
ggdid(es) +
  ggtitle("Average Effect on State-Based Conflicts Started") +
  theme_classic(base_size = 12) +
  geom_segment(aes(x = 0, y = -5, xend = 0, yend = 2.5), lty = 2, col = "black")
