
# Load libraries
library(ggplot2)
library(dplyr)
library(lmerTest)
library(mgcv)


setwd("C:/Users/Usuario/Desktop/AU_milking_tests/data_analysis/R")
dir.create("models_next", showWarnings = FALSE)
dir.create("plots_next", showWarnings = FALSE)


df <- read.csv("C:/Users/Usuario/Desktop/AU_milking_tests/data_analysis/df_sig_MAYO25_prodCorr.csv")

valid_cows <- names(table(df$Cow)[table(df$Cow) >= 3])
df <- df[df$Cow %in% valid_cows, ]

# Define factors
df$group_alarm <- as.factor(df$group_alarm)
df$stage_lact <- as.factor(df$stage_lact_alarm)
df$stage_lact[df$stage_lact == ""] <- NA

# Create Lact_2 variable
df$Lact_2 <- as.factor(ifelse(df$Lact == 2, 0, 
                              ifelse(df$Lact > 2, 1, NA)))

# Checking collinearity
linear_model <- lm(milk_yield ~ group_alarm + Lact_2 + stage_lact, data = df)
vif(linear_model)

# Filter by DIM
df <- df[df$DIM <= 305, ]

# Fit and save models
fit_and_save_model <- function(formula, data, filename) {
  model <- gamm(formula, random = list(Cow = ~1), data = data)
  saveRDS(model, file = paste0("modelos_sig/", filename, ".rds"))  
  print(summary(model$gam))
  print(summary(model$lme))
  print(AIC(model$lme))
  return(model)
}

# Model 1: withno interactions
model1 <- fit_and_save_model(
  milk_yield ~ s(DIM) + group_alarm + stage_lact + Lact_2,
  df,
  "model_1"
)

# Model 2: interaction in smooth term
model2 <- fit_and_save_model(
  milk_yield ~ s(DIM, by = group_alarm) + group_alarm + stage_lact + Lact_2,
  df,
  "model_2"
)

# Model 3: full parametric interactions
model2 <- fit_and_save_model(
  milk_yield ~ s(DIM, by = group_alarm) + group_alarm + stage_lact + Lact_2 +
    group_alarm:stage_lact + group_alarm:Lact_2,
  df,
  "model_3"
)

# Model 4: exclude group_alarm:Lact_2 interaction
model4 <- fit_and_save_model(
  milk_yield ~ s(DIM, by = interaction(group_alarm, stage_lact, Lact_2)) +
    group_alarm + stage_lact + Lact_2 + group_alarm:stage_lact,
  df,
  "model_4"
)

# Final plot model: only group interaction
model5 <- fit_and_save_model(
  milk_yield ~ s(DIM, by = group_alarm) + group_alarm,
  df,
  "model_5"
)

# Load fitted models
mod_next_base <- readRDS("modelos_sig/model_1.rds")
mod_next_smooth <- readRDS("modelos_sig/model_2.rds")
mod_next_full <- readRDS("modelos_sig/model_3.rds")
mod_next_group_only <- readRDS("modelos_sig/model_5.rds")
mod_next_partial <- readRDS("modelos_sig/model_4.rds")

# Compare AICs
AIC(mod_next_base$lme,
    mod_next_smooth$lme,
    mod_next_full$lme,
    mod_next_partial$lme,
    mod_next_group_only$lme)

# Summary of final model
summary(mod_next_smooth$gam)

#########################################
# Plot 1: group-only interaction

new_data <- expand.grid(
  DIM = seq(0, 305, by = 5),
  group_alarm = factor(c(0, 1))
)

pred <- predict(mod_next_group_only$gam, newdata = new_data, type = "response", se.fit = TRUE)
new_data$prediccion <- pred$fit
new_data$se <- pred$se.fit
new_data$IC_inf <- new_data$prediccion - 1.96 * new_data$se
new_data$IC_sup <- new_data$prediccion + 1.96 * new_data$se

new_data$grupo <- factor(new_data$group_alarm, levels = c(0, 1), labels = c("Control", ">30%"))

new_data <- new_data %>%
  mutate(across(c(prediccion, IC_inf, IC_sup), ~ .x * 0.453592))

ggplot(new_data, aes(x = DIM, y = prediccion, color = grupo, fill = grupo)) +
  geom_ribbon(aes(ymin = IC_inf, ymax = IC_sup), alpha = 0.2, color = NA) +
  geom_line(size = 0.8) +
  scale_color_manual(values = c("Control" = "#FF8000", ">30%" = "#0072B2"),
                     labels = c("Control" = "Unexposed", ">30%" = "Exposed")) +
  scale_fill_manual(values = c("Control" = "#FF8000", ">30%" = "#0072B2"),
                    labels = c("Control" = "Unexposed", ">30%" = "Exposed")) +
  scale_x_continuous(limits = c(0, 305), breaks = seq(0, 300, 50), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 60, 10), expand = c(0, 0)) +
  labs(x = "Days in milk", y = "Milk yield (kg)", color = "Group", fill = "Group") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    panel.grid.minor = element_blank()
  )

ggsave("C:/Users/Usuario/Desktop/AU_milking_tests/paper/Figuras/Fig5_next_lactation.png",
       width = 12, height = 6, dpi = 300, bg = "white")

#########################################
# Plot 2: full interaction

new_data <- expand.grid(
  DIM = seq(6, 305, by = 5),
  group_alarm = factor(c(0, 1)),
  Lact_2 = factor(c(0, 1), levels = c(0, 1)),
  stage_lact = factor(c("Early lactation", "Middle lactation", "Late lactation"),
                           levels = c("Early lactation", "Middle lactation", "Late lactation"))
)

pred <- predict(mod_next_smooth$gam, newdata = new_data, type = "response", se.fit = TRUE)

new_data$prediccion <- pred$fit
new_data$se <- pred$se.fit
new_data$IC_inf <- new_data$prediccion - 1.96 * new_data$se
new_data$IC_sup <- new_data$prediccion + 1.96 * new_data$se

new_data$grupo <- factor(new_data$group_alarm, levels = c(0, 1), labels = c("Control", ">30%"))
new_data$Lact_cat <- factor(new_data$Lact_2, levels = c(0, 1), labels = c("Second lactation", "≥ Third lactation"))

new_data <- new_data %>%
  filter(
    (stage_lact == "Early lactation" & DIM <= 100) |
      (stage_lact == "Middle lactation" & DIM <= 200) |
      (stage_lact == "Late lactation" & DIM <= 305)
  ) %>%
  mutate(across(c(prediccion, IC_inf, IC_sup), ~ .x * 0.453592))

ggplot(new_data, aes(x = DIM, y = prediccion, color = grupo, fill = grupo)) +
  geom_line(size = 0.6) +
  geom_ribbon(aes(ymin = IC_inf, ymax = IC_sup), alpha = 0.2, color = NA) +
  facet_grid(Lact_cat ~ stage_lact, scales = "free_y") +
  labs(x = "Days in milk", y = "Milk yield (kg)", color = "Group") +
  scale_color_manual(values = c("#FF8000", "#0072B2"),
                     labels = c("Control" = "Unexposed", ">30%" = "Exposed")) +
  scale_fill_manual(values = c("#FF8000", "#0072B2"),
                    labels = c("Control" = "Unexposed", ">30%" = "Exposed")) +
  guides(fill = "none") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(20, 70)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 310), breaks = seq(0, 300, 50))

