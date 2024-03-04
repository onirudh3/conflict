
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


# Checking correlation between conflicts and fatalities -------------------

# Set the plotting space
par(mfrow = c(1, 3))

# Scatter plot
with(df, plot(number_of_conflicts_started, total_fatalities, cex.lab = 1.5))
abline(lm(total_fatalities ~ number_of_conflicts_started, data = df), col = "blue")

# With logs
with(df, plot(log_number_of_conflicts_started, log_total_fatalities, cex.lab = 1.5))
abline(lm(log_total_fatalities ~ log_number_of_conflicts_started, data = df), col = "blue")

# Population scaled
with(df, plot(scaled_number_of_conflicts_started, scaled_total_fatalities, cex.lab = 1.5))
abline(lm(scaled_total_fatalities ~ scaled_number_of_conflicts_started, data = df), col = "blue")

# Correlation with population is positive
with(df, plot(number_of_conflicts_started, pop, cex.lab = 1.5))
abline(lm(pop ~ number_of_conflicts_started, data = df), col = "blue")


# Number of conflicts started ---------------------------------------------


## Log number of conflicts started ----
out <- att_gt(yname = "log_number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              data = df)

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
              data = df,
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
              data = df,
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
              data = df,
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
              data = df,
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
with(df, plot(gdp, stability, cex.lab = 1.5))
abline(lm(stability ~ gdp, data = df), col = "blue")

## Richer countries ----
quart <- subset(df, quartile_stability %in% c(3, 4))

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






