# Load required packages
library(ggplot2)
library(dplyr)
library(lmerTest)
library(mgcv)
library(car)

# working directory 
setwd("C:/Users/Usuario/Desktop/AU_milking_tests/data_analysis/R/ultima_modificacion")
dir.create("models", showWarnings = FALSE)

df <- read.csv("C:/Users/Usuario/Desktop/AU_milking_tests/data_analysis/df_final2_AU_sin_cnd.csv")

# Define variables:
# - milk_yield: total daily milk yield
# - parity: parity group (0 = primiparous, 1 = multiparous)
# - stage_lact: lactation stage at time = 0, categorized as "Early lactation", "Middle lactation", or "Late lactation"
# - group_alarm: exposure group (0 = Unexposed, 1 = Exposed)
# - time_alarm: time when the alert begins, 0 = moment of the alert

# Ensure categorical variables are factors
df$parity <- as.factor(df$parity)
df$stage_lact <- as.factor(df$stage_lact)
df$group_alarm <- as.factor(df$group_alarm)

# Replace empty strings with NA
df$stage_lact[df$stage_lact == ""] <- NA

df_filtered <- df %>%
  filter(time_alarm >= -10, DIM <= 305)

# --------------------------------------------
# Check multicollinearity
linear_model <- lm(milk_yield ~ group_alarm * parity * stage_lact, data = df_filtered)
vif(linear_model)

# --------------------------------------------
# Function to fit and save models
fit_and_save_model <- function(formula, data, filename) {
  model <- gamm(formula, random = list(Cow = ~1), data = data)
  saveRDS(model, file = paste0("models/", filename, ".rds"))
  print(summary(model$gam))
  print(summary(model$lme))
  print(AIC(model$lme))
  return(model)
}

#########################################
# MODEL FITTING

# Models with time_alarm

# No interactions
model1 <- fit_and_save_model(
  milk_yield ~ s(time_alarm, k = 15) + group_alarm + parity + stage_lact,
  df_filtered,
  "model_1"
)

# Add smooth interaction
df_filtered$exp_parity_stage  <- interaction(df_filtered$group_alarm,
                                          df_filtered$parity,
                                          df_filtered$stage_lact)

model2 <- fit_and_save_model(
  milk_yield ~ s(time_alarm, by = exp_parity_stage , k = 15) +
    group_alarm + parity + stage_lact,
  df_filtered,
  "model_2"
)

# Add possibles interactions 
model3 <- fit_and_save_model(
  milk_yield ~ s(time_alarm, by = exp_parity_stage , k = 15) +
    group_alarm + parity + stage_lact + group_alarm:parity + group_alarm:stage_lact,
  df_filtered,
  "model_3"
)

# Remove non-significant group_alarm:parity interaction
model4 <- fit_and_save_model(
  milk_yield ~ s(time_alarm, by = exp_parity_stage , k = 15) +
    group_alarm + parity + stage_lact + group_alarm:stage_lact,
  df_filtered,
  "model_4"
)

# Same as above with k = 10
model5 <- fit_and_save_model(
  milk_yield ~ s(time_alarm, by = exp_parity_stage , k = 10) +
    group_alarm + parity + stage_lact + group_alarm:parity + group_alarm:stage_lact,
  df_filtered,
  "model_5"
)

# k = 10 and without group_alarm:parity interaction
model6 <- fit_and_save_model(
  milk_yield ~ s(time_alarm, by = exp_parity_stage , k = 10) +
    group_alarm + parity + stage_lact + group_alarm:stage_lact,
  df_filtered,
  "model_6"
)


#########################################
# AIC comparisons

model_1 <- readRDS("models/model_1.rds")
model_2 <- readRDS("models/model_2.rds")
model_3 <- readRDS("models/model_3.rds")
model_4 <- readRDS("models/model_4 .rds")
model_5 <- readRDS("models/model_5.rds")
model_6 <- readRDS("models/model_6.rds")


AIC(model_1$lme,
    model_2$lme,
    model_3$lme,
    model_4$lme,
    model_5$lme,
    model_6$lme)

# Final selected model:
# model_3 <- readRDS("models/model_3.rds")

#########################################
# DIM Models

# No interactions
model1 <- fit_and_save_model(
  milk_yield ~ s(DIM) + group_alarm + parity + stage_lact,
  df_filtered,
  "model_1_dim"
)

# Only parametric interactions
model2 <- fit_and_save_model(
  milk_yield ~ s(DIM) + group_alarm + parity + stage_lact +
    group_alarm:parity + group_alarm:stage_lact,
  df_filtered,
  "model_2_dim"
)

# Only smooth interaction with group_alarm
model3 <- fit_and_save_model(
  milk_yield ~ s(DIM, by = group_alarm) + group_alarm + parity + stage_lact +
    group_alarm:parity + group_alarm:stage_lact,
  df_filtered,
  "model_3_dim"
)

# Remove group_alarm:stage_lact
model4 <- fit_and_save_model(
  milk_yield ~ s(DIM, by = group_alarm) + group_alarm + parity + stage_lact +
    group_alarm:parity,
  df_filtered,
  "model_4_dim"
)

# Compare DIM models
model_1_dim <- readRDS("models/model_1_dim.rds")
model_2_dim <- readRDS("models/model_2_dim.rds")
model_3_dim <- readRDS("models/model_3_dim.rds")
model_4_dim <- readRDS("models/model_4_dim.rds")

AIC(model_1_dim$lme,
    model_2_dim$lme,
    model_3_dim$lme,
    model_4_dim$lme)

# Final selected model:
# model_4_dim <- readRDS("models/model_4_dim.rds")

