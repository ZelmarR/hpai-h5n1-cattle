# Load required packages
library(ggplot2)
library(dplyr)
library(lmerTest)
library(mgcv)
library(tidyr)
library(pracma)
library(itsadug)

# Set working directory
setwd("C:/Users/Usuario/Desktop/AU_milking_tests/data_analysis/R/ultima_modificacion")

# Load data
df <- read.csv("C:/Users/Usuario/Desktop/AU_milking_tests/data_analysis/df_final2_AU_sin_cnd.csv")

# Ensure categorical variables are factors
df$grupo_2 <- as.factor(df$grupo_2)
df$prim_mult <- as.factor(df$prim_mult)
df$est_prod_ant <- as.factor(df$est_prod_ant)
df$est_prod_ant[df$est_prod_ant == ""] <- NA

# Filter data
df <- df[df$tiempo_0_grupo_sin_cnd >= -10, ]
df <- df[df$DIM <= 305, ]

# Load final models
modelo_interaccion_completa_mod <- readRDS("modelos/gamm_tiempo0_interaccion completa_modificada.rds")
modelo_dim_sGrupo_px1 <- readRDS("modelos/gamm_dim_int_sSoloGrupo_p solo grupo_2:prim_mult.rds")

# Model summaries
summary(modelo_interaccion_completa_mod$gam)
summary(modelo_dim_sGrupo_px1$gam)

#########################################
# FIGURE X6 - TIME (3-way interaction)


new_data <- expand.grid(
  tiempo_0_grupo_sin_cnd = seq(-10, 305, by = 5),
  grupo_2 = factor(c(0, 1)),
  est_prod_ant = factor(c("Early lactation", "Middle lactation", "Late lactation"),
                        levels = c("Early lactation", "Middle lactation", "Late lactation")),
  prim_mult = factor(c(0, 1))
)

new_data$grupo_prim_est <- interaction(new_data$grupo_2,
                                       new_data$prim_mult,
                                       new_data$est_prod_ant)

# Predict using the model
pred <- predict(modelo_interaccion_completa_mod$gam, newdata = new_data, type = "response", se.fit = TRUE)

# Add predictions and confidence intervals to dataframe
new_data$prediccion <- pred$fit
new_data$se <- pred$se.fit
new_data$IC_inf <- new_data$prediccion - 1.96 * new_data$se
new_data$IC_sup <- new_data$prediccion + 1.96 * new_data$se

# Readable labels for plotting
new_data$grupo <- factor(new_data$grupo_2,
                         levels = c(0, 1),
                         labels = c("Control", ">30%"))

new_data$paridad <- factor(new_data$prim_mult,
                           levels = c(0, 1),
                           labels = c("Primiparous", "Multiparous"))

# Convert predictions from pounds to kilograms (1 lb = 0.453592 kg)
new_data <- new_data %>%
  mutate(
    prediccion = prediccion * 0.453592,
    IC_inf = IC_inf * 0.453592,
    IC_sup = IC_sup * 0.453592
  )

# Filter data by expected duration of each lactation stage
new_data <- new_data %>%
  filter(
    (est_prod_ant == "Early lactation" & tiempo_0_grupo_sin_cnd <= 305) |
      (est_prod_ant == "Middle lactation" & tiempo_0_grupo_sin_cnd <= 195) |
      (est_prod_ant == "Late lactation" & tiempo_0_grupo_sin_cnd <= 95)
  )

# Plot
ggplot(new_data, aes(x = tiempo_0_grupo_sin_cnd, y = prediccion,
                     color = grupo, fill = grupo)) +
  geom_line(size = 0.6) +
  geom_ribbon(aes(ymin = IC_inf, ymax = IC_sup), alpha = 0.2, color = NA) +
  facet_grid(paridad ~ est_prod_ant, scales = "free_y") +
  labs(
    x = "Days post-event",
    y = "Milk yield (kg)",
    color = "Group"
  ) +
  scale_color_manual(values = c("#FF8000", "#0072B2"),
                     labels = c("Control" = "Unexposed", ">30%" = "Exposed")) +
  scale_fill_manual(values = c("#FF8000", "#0072B2"),
                    labels = c("Control" = "Unexposed", ">30%" = "Exposed")) +
  guides(fill = "none") +
  theme_minimal(base_size = 14, base_family = "DejaVu Sans") +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 13, face = "plain"),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 70)) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(-5, 310),
    breaks = seq(0, 300, by = 50)
  )

#########################################
# FIGURE X2 - DAYS IN MILK (DIM)

new_data <- expand.grid(
  DIM = seq(0, 305, by = 5),
  grupo_2 = (c(0, 1)),
  est_prod_ant = factor(c("Early lactation", "Middle lactation", "Late lactation"),
                        levels = c("Early lactation", "Middle lactation", "Late lactation")),
  prim_mult = c(0, 1)
)

new_data$grupo_prim_est <- interaction(new_data$grupo_2,
                                       new_data$prim_mult,
                                       new_data$est_prod_ant)

# Predict using the model
pred <- predict(modelo_dim_sGrupo_px1$gam, newdata = new_data, type = "response", se.fit = TRUE)

# Add predictions and confidence intervals to dataframe
new_data$prediccion <- pred$fit
new_data$se <- pred$se.fit
new_data$IC_inf <- new_data$prediccion - 1.96 * new_data$se
new_data$IC_sup <- new_data$prediccion + 1.96 * new_data$se

# Readable labels for plotting
new_data$grupo <- factor(new_data$grupo_2,
                         levels = c(0, 1),
                         labels = c("Control", ">30%"))

new_data$paridad <- factor(new_data$prim_mult,
                           levels = c(0, 1),
                           labels = c("Primiparous", "Multiparous"))

# Filter by expected duration of each lactation stage
new_data <- new_data %>%
  filter(
    (est_prod_ant == "Early lactation" & DIM >= 6 & DIM <= 305) |
      (est_prod_ant == "Middle lactation" & DIM >= 100 & DIM <= 305) |
      (est_prod_ant == "Late lactation" & DIM >= 180 & DIM <= 305)
  )

# Convert predictions to kilograms
new_data <- new_data %>%
  mutate(
    prediccion = prediccion * 0.453592,
    IC_inf = IC_inf * 0.453592,
    IC_sup = IC_sup * 0.453592
  )

# Plot only Early Lactation
new_data_filtrado <- new_data %>%
  filter(est_prod_ant == "Early lactation")

ggplot(new_data_filtrado, aes(x = DIM, y = prediccion,
                              color = grupo, fill = grupo)) +
  geom_line(size = 0.6) +
  geom_ribbon(aes(ymin = IC_inf, ymax = IC_sup), alpha = 0.2, color = NA) +
  facet_wrap(~ paridad, ncol = 2) +
  labs(
    x = "Days in milk",
    y = "Milk yield (kg)",
    color = "Group"
  ) +
  scale_color_manual(values = c("Control" = "#FF8000", ">30%" = "#0072B2"),
                     labels = c("Control" = "Unexposed", ">30%" = "Exposed")) +
  scale_fill_manual(values = c("Control" = "#FF8000", ">30%" = "#0072B2"),
                    labels = c("Control" = "Unexposed", ">30%" = "Exposed")) +
  guides(fill = "none") +
  theme_minimal(base_size = 14, base_family = "DejaVu Sans") +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 13, face = "plain"),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "lines")
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 75)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 305))

#########################################
# PRODUCTIVE LOSS ESTIMATION (DIM) (KG)


# Calculate AUCs: estimated point, lower and upper confidence intervals

auc_df <- new_data %>%
  group_by(grupo, paridad, est_prod_ant) %>%
  summarise(
    auc = trapz(DIM, prediccion),
    auc_inf = trapz(DIM, IC_inf),
    auc_sup = trapz(DIM, IC_sup),
    .groups = "drop"
  )

# Reshape data from long to wide for comparison
auc_wide <- auc_df %>%
  pivot_wider(
    names_from = grupo,
    values_from = c(auc, auc_inf, auc_sup)
  )

# Calculate estimated, minimum and maximum percentage losses
auc_wide <- auc_wide %>%
  mutate(
    loss = 100 * (`auc_Control` - `auc_>30%`) / `auc_Control`,
    loss_inf = 100 * (`auc_inf_Control` - `auc_sup_>30%`) / `auc_inf_Control`,
    loss_sup = 100 * (`auc_sup_Control` - `auc_inf_>30%`) / `auc_sup_Control`
  )

# Add duration of each productive stage
durations <- tibble(
  est_prod_ant = c("Early lactation", "Middle lactation", "Late lactation"),
  duracion_dias = c(305, 205, 105)
)

# Join durations with auc_wide
auc_final <- auc_wide %>%
  left_join(durations, by = "est_prod_ant") %>%
  mutate(
    perdida_kg_dia = (`auc_Control` - `auc_>30%`) / duracion_dias
  )

# Create a combined grouping variable
df <- df %>%
  mutate(grupo_paridad_est = paste(grupo_2, prim_mult, est_prod_ant, sep = "_"))

# Frequencies by group to weight
frequencies <- df %>%
  separate(grupo_paridad_est, into = c("grupo_2", "prim_mult", "est_prod_ant"), sep = "_") %>%
  mutate(
    grupo_2 = as.factor(grupo_2),
    prim_mult = as.factor(prim_mult),
    est_prod_ant = factor(est_prod_ant, levels = c("Early lactation", "Middle lactation", "Late lactation"))
  ) %>%
  filter(!is.na(prim_mult), !is.na(est_prod_ant)) %>%
  count(prim_mult, est_prod_ant) %>%
  mutate(freq_rel = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    paridad = factor(prim_mult, levels = c("0", "1"), labels = c("Primiparous", "Multiparous"))
  )

# Join general frequencies with auc_final
auc_final <- auc_final %>%
  left_join(frequencies, by = c("paridad", "est_prod_ant"))

# Weighted global loss (estimate and extremes)
weighted_total_loss <- sum(auc_final$loss * auc_final$freq_rel, na.rm = TRUE)
weighted_total_loss_inf <- sum(auc_final$loss_inf * auc_final$freq_rel, na.rm = TRUE)
weighted_total_loss_sup <- sum(auc_final$loss_sup * auc_final$freq_rel, na.rm = TRUE)

# Print global percentage loss
cat("Estimated weighted global daily percentage loss:", round(weighted_total_loss, 1), "%\n")
cat("Confidence interval of global percentage loss: [", round(weighted_total_loss_inf, 1), "% -", round(weighted_total_loss_sup, 1), "%]\n")

# Weighted global loss in kg
auc_final <- auc_final %>%
  mutate(
    perdida_kg = `auc_Control` - `auc_>30%`,
    perdida_kg_inf = `auc_inf_Control` - `auc_sup_>30%`,
    perdida_kg_sup = `auc_sup_Control` - `auc_inf_>30%`
  )

# Total weighted loss in kg
weighted_kg_loss_total <- sum(auc_final$perdida_kg * auc_final$freq_rel, na.rm = TRUE)
weighted_kg_loss_total_inf <- sum(auc_final$perdida_kg_inf * auc_final$freq_rel, na.rm = TRUE)
weighted_kg_loss_total_sup <- sum(auc_final$perdida_kg_sup * auc_final$freq_rel, na.rm = TRUE)

cat("Estimated weighted global daily loss in kg:", round(weighted_kg_loss_total, 1), "kg\n")
cat("Confidence interval of global kg loss: [", round(weighted_kg_loss_total_inf, 1), "kg -", round(weighted_kg_loss_total_sup, 1), "kg]\n")

# Frequencies by parity
frequencies_parity <- df %>%
  separate(grupo_paridad_est, into = c("grupo_2", "prim_mult", "est_prod_ant"), sep = "_") %>%
  mutate(
    grupo_2 = as.factor(grupo_2),
    prim_mult = as.factor(prim_mult),
    est_prod_ant = factor(est_prod_ant, levels = c("Early lactation", "Middle lactation", "Late lactation"))
  ) %>%
  filter(!is.na(prim_mult), !is.na(est_prod_ant)) %>%
  count(prim_mult, est_prod_ant) %>%
  group_by(prim_mult) %>%
  mutate(freq_rel_paridad = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    paridad = factor(prim_mult, levels = c("0", "1"), labels = c("Primiparous", "Multiparous"))
  )

# Join with auc_final
auc_final <- auc_final %>%
  left_join(frequencies_parity, by = c("paridad", "est_prod_ant"))

# Loss by parity (estimate and extremes)
loss_by_parity <- auc_final %>%
  group_by(paridad) %>%
  summarise(
    perdida_ponderada = sum(loss * freq_rel_paridad, na.rm = TRUE),
    perdida_ponderada_inf = sum(loss_inf * freq_rel_paridad, na.rm = TRUE),
    perdida_ponderada_sup = sum(loss_sup * freq_rel_paridad, na.rm = TRUE),
    .groups = "drop"
  )

print(loss_by_parity)
