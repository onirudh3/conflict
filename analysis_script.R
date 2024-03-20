
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
control_df <- subset(df, total_discoveries == 0)


# Summary statistics heterogeneous groups ---------------------------------
n_distinct(subset(df, religion_index_tercile %in% c(3))$country)
summary(subset(df, religion_index_tercile %in% c(3))$number_of_conflicts_started)
summary(subset(df, religion_index_tercile %in% c(3))$total_discoveries)


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


# Presence of conflict ----------------------------------------------------

# Conflict dummy
out <- att_gt(yname = "conflict_dummy",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              data = df, alp = 0.1)

# Overall average treatment effect
summary(aggte(out, type = "group"))

# Event study plot 
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("Average Effect on Presence of Conflict") +
  theme_classic(base_size = 12) +
  ylim(c(-4, 4))


# Number of conflicts started ---------------------------------------------

# Log number of conflicts started
out <- att_gt(yname = "log_number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              data = df, alp = 0.1)
summary(aggte(out, type = "group"))
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("Average Effect on Log No. of Conflicts Started") +
  theme_classic(base_size = 12) +
  ylim(c(-9, 9))

# Population scaled number of conflicts
df$scaled_number_of_conflicts_started <- df$scaled_number_of_conflicts_started * 10000
out <- att_gt(yname = "scaled_number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              data = df, alp = 0.1)
summary(aggte(out, type = "group"))
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("Average Effect on No. of Conflicts Started (Scaled by Population and Multiplied by 10,000)") +
  theme_classic(base_size = 12) +
  ylim(c(-0.9, 0.9))


# Total fatalities --------------------------------------------------------

# Log fatalities
out <- att_gt(yname = "log_total_fatalities",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              data = df, alp = 0.1)
summary(aggte(out, type = "group"))
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("Average Effect on Log No. of Fatalities") +
  theme_classic(base_size = 12) +
  ylim(c(-9, 9))

# Population scaled fatalities
df$scaled_total_fatalities <- df$scaled_total_fatalities * 100
out <- att_gt(yname = "scaled_total_fatalities",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              data = df, alp = 0.1)
summary(aggte(out, type = "group"))
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("Average Effect on No. of Fatalities (Scaled by Population and Multiplied by 100)") +
  theme_classic(base_size = 12) +
  ylim(c(-0.9, 0.9))


# Heterogeneous effects using log GDP -------------------------------------

# Checking correlation between GDP and rule of law
with(df, plot(lgdp, rule_of_law, cex.lab = 1, pch = 20))
abline(lm(rule_of_law ~ lgdp, data = df), col = "blue")

# Number of conflicts
# Poor countries
out <- att_gt(yname = "log_number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              data = subset(df, lgdp_tercile %in% c(1)), alp = 0.1)
summary(aggte(out, type = "group"))
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("Average Effect on Log No. of Conflicts Started (Poorest Tercile of GDP)") +
  theme_classic(base_size = 12) +
  ylim(c(-9, 9))

# Middle countries
out <- att_gt(yname = "log_number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              data = subset(df, lgdp_tercile %in% c(2)), alp = 0.1)
summary(aggte(out, type = "group"))
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("Average Effect on Log No. of Conflicts Started (Middle Tercile of GDP)") +
  theme_classic(base_size = 12) +
  ylim(c(-9, 9))

# Rich countries
out <- att_gt(yname = "log_number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              data = subset(df, lgdp_tercile %in% c(3)), alp = 0.1)
summary(aggte(out, type = "group"))
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("Average Effect on Log No. of Conflicts Started (Richest Tercile of GDP)") +
  theme_classic(base_size = 12) +
  ylim(c(-9, 9))


# Heterogeneous effects using rule of law ---------------------------------

# Low quality rule of law countries
out <- att_gt(yname = "log_number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              data = subset(df, rule_of_law_tercile %in% c(1)), alp = 0.1)
summary(aggte(out, type = "group"))
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("Average Effect on Log No. of Conflicts Started (Low Quality Rule of Law Countries)") +
  theme_classic(base_size = 12) +
  ylim(c(-9, 9))

# Middle quality rule of law countries
out <- att_gt(yname = "log_number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              data = subset(df, rule_of_law_tercile %in% c(2)), alp = 0.1)
summary(aggte(out, type = "group"))
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("Average Effect on Log No. of Conflicts Started (Middle Quality Rule of Law Countries)") +
  theme_classic(base_size = 12) +
  ylim(c(-9, 9))

# High quality rule of law countries
out <- att_gt(yname = "log_number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              data = subset(df, rule_of_law_tercile %in% c(3)), alp = 0.1)
summary(aggte(out, type = "group"))
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("Average Effect on Log No. of Conflicts Started (High Quality Rule of Law Countries)") +
  theme_classic(base_size = 12) +
  ylim(c(-9, 9))


# Heterogeneous effect using religion -------------------------------------

# Christian
out <- att_gt(yname = "log_number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              data = subset(df, religion %in% c("Christians")), alp = 0.1)
summary(aggte(out, type = "group"))
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("Average Effect on Log No. of Conflicts Started (Predominantly Christian Countries)") +
  theme_classic(base_size = 12) +
  ylim(c(-9, 9))

# Muslim
out <- att_gt(yname = "log_number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              data = subset(df, religion %in% c("Muslims")), alp = 0.1)
summary(aggte(out, type = "group"))
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("Average Effect on Log No. of Conflicts Started (Predominantly Muslim Countries)") +
  theme_classic(base_size = 12) +
  ylim(c(-9, 9))

# Other
out <- att_gt(yname = "log_number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              data = subset(df, religion %in% c("Other")), alp = 0.1)
summary(aggte(out, type = "group"))
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("Average Effect on Log No. of Conflicts Started (Predominantly Other Religion Countries)") +
  theme_classic(base_size = 12) +
  ylim(c(-9, 9))


# Religious fractionalisation ---------------------------------------------

# Checking correlation between GDP and religious fractionalisation
with(df, plot(lgdp, religion_index, cex.lab = 1, pch = 20))
abline(lm(religion_index ~ lgdp, data = df), col = "blue")

# Checking correlation between rule of law and religious fractionalisation
with(df, plot(rule_of_law, religion_index, cex.lab = 1, pch = 20))
abline(lm(religion_index ~ rule_of_law, data = df), col = "blue")

# High religious fractionalisation
out <- att_gt(yname = "log_number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              data = subset(df, religion_index_tercile %in% c(1)), alp = 0.1)
summary(aggte(out, type = "group"))
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("Average Effect on Log No. of Conflicts Started (High Religious Fractionalisation)") +
  theme_classic(base_size = 12) +
  ylim(c(-9, 9))

# Medium tercile religious fractionalisation
out <- att_gt(yname = "log_number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              data = subset(df, religion_index_tercile %in% c(2)), alp = 0.1)
summary(aggte(out, type = "group"))
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("Average Effect on Log No. of Conflicts Started (Medium Tercile Religious Fractionalisation)") +
  theme_classic(base_size = 12) +
  ylim(c(-9, 9))

# Low religious fractionalisation
out <- att_gt(yname = "log_number_of_conflicts_started",
              gname = "first_discovery",
              idname = "country_ID",
              tname = "year",
              data = subset(df, religion_index_tercile %in% c(3)), alp = 0.1)
summary(aggte(out, type = "group"))
ggdid(aggte(out, type = "dynamic", na.rm = T)) +
  ggtitle("Average Effect on Log No. of Conflicts Started (Medium Tercile Religious Fractionalisation)") +
  theme_classic(base_size = 12) +
  ylim(c(-9, 9))


