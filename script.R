
# Libraries and data ------------------------------------------------------

library(dplyr)
library(ggplot2)
library(gplots)
library(readxl)
library(zoo)
library(did)
library(gtsummary)
library(xtable)

# Read cleaned data
df <- read.csv("Data/final_dataset.csv")


# Summary Statistics ------------------------------------------------------

n_distinct(result_data$country) # Number of countries
n_distinct(subset(result_data, total_discoveries != 0)$country) # Number of countries with discovery
summary(subset(result_data, total_discoveries != 0)$first_discovery) # Year of first discovery, conditional
summary(result_data$total_discoveries) # Number of discoveries
summary(result_data$total_fatalities) # Number of fatalities
summary(result_data$number_of_conflicts_started) # Number of conflicts started
summary(result_data$gdp) # Log GDP per capita
summary(result_data$pop) # Population
summary(result_data$stability) # Political stability

# Heterogeneous groups
summary(subset(result_data, gdp_quartile %in% c(3, 4))$total_discoveries)
summary(subset(result_data, gdp_quartile %in% c(3, 4))$number_of_conflicts_started)

summary(subset(result_data, gdp_quartile %in% c(1, 2))$total_discoveries)
summary(subset(result_data, gdp_quartile %in% c(1, 2))$number_of_conflicts_started)


# Checking correlation between conflicts and fatalities -------------------

# Set the plotting space
par(mfrow = c(1, 3))

# Scatter plot
with(result_data, plot(number_of_conflicts_started, total_fatalities, cex.lab = 1.5))
abline(lm(total_fatalities ~ number_of_conflicts_started, data = result_data), col = "blue")

# With logs
with(result_data, plot(log_number_of_conflicts_started, log_total_fatalities, cex.lab = 1.5))
abline(lm(log_total_fatalities ~ log_number_of_conflicts_started, data = result_data), col = "blue")

# Population scaled
with(result_data, plot(scaled_number_of_conflicts_started, scaled_total_fatalities, cex.lab = 1.5))
abline(lm(scaled_total_fatalities ~ scaled_number_of_conflicts_started, data = result_data), col = "blue")

# Correlation with population is positive
with(result_data, plot(number_of_conflicts_started, pop, cex.lab = 1.5))
abline(lm(pop ~ number_of_conflicts_started, data = result_data), col = "blue")


# Number of conflicts started ---------------------------------------------


## Log number of conflicts ----
out <- att_gt(yname = "log_number_of_conflicts_started",
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
  ggtitle("Average Effect on Log No. of Conflicts Started") +
  theme_classic(base_size = 12) +
  ylim(c(-9, 9))


## Population scaled number of conflicts ----
out <- att_gt(yname = "scaled_number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              xformla = ~ 1,
              data = result_data,
              est_method = "reg",
              allow_unbalanced_panel = T)
es <- aggte(out, type = "dynamic", na.rm = T)
summary(aggte(out, type = "group"))
ggdid(es) +
  ggtitle("Average Effect on No. of Conflicts Started (Scaled by Population and Multiplied by 10,000)") +
  theme_classic(base_size = 12) +
  ylim(c(-0.9, 0.9))


# Total fatalities --------------------------------------------------------


## Log fatalities ----
out <- att_gt(yname = "log_total_fatalities",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              xformla = ~ 1,
              data = result_data,
              est_method = "reg",
              allow_unbalanced_panel = T)
es <- aggte(out, type = "dynamic", na.rm = T)
summary(aggte(out, type = "group"))
ggdid(es) +
  ggtitle("Average Effect on Log No. of Fatalities") +
  theme_classic(base_size = 12) +
  ylim(c(-9, 9))


## Population scaled fatalities ----
out <- att_gt(yname = "scaled_total_fatalities",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              xformla = ~ 1,
              data = result_data,
              est_method = "reg",
              allow_unbalanced_panel = T)
es <- aggte(out, type = "dynamic", na.rm = T)
summary(aggte(out, type = "group"))
ggdid(es) +
  ggtitle("Average Effect on No. of Fatalities (Scaled by Population and Multiplied by 100)") +
  theme_classic(base_size = 12) +
  ylim(c(-0.9, 0.9))


# Adding a conflict dummy -------------------------------------------------

## Log number of conflicts ----
out <- att_gt(yname = "scaled_total_fatalities",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              xformla = ~ conflict_dummy,
              data = result_data,
              est_method = "reg",
              allow_unbalanced_panel = T)
es <- aggte(out, type = "dynamic", na.rm = T)
summary(aggte(out, type = "group"))
ggdid(es) +
  ggtitle("Average Effect on No. of Fatalities (Scaled by Population and Multiplied by 100) (Including Conflict Dummy)") +
  theme_classic(base_size = 12) +
  ylim(c(-0.5, 0.5))


# Heterogeneous effects using GDP and political stability -----------------

## Checking correlation ----
with(result_data, plot(gdp, stability, cex.lab = 1.5))
abline(lm(stability ~ gdp, data = result_data), col = "blue")

## Richer countries ----
quart <- subset(result_data, quartile_stability %in% c(3, 4))

# Number of conflicts
out <- att_gt(yname = "log_number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              xformla = ~ 1,
              data = quart,
              est_method = "reg",
              allow_unbalanced_panel = T)
es <- aggte(out, type = "dynamic", na.rm = T)
summary(aggte(out, type = "group"))
ggdid(es) +
  ggtitle("Average Effect on Log No. of Conflicts Started (Above Median Political Stability)") +
  theme_classic(base_size = 12) +
  ylim(c(-9, 9))






