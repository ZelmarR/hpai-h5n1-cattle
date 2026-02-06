#Analisis descriptivo para paper
library(ggplot2)
library(dplyr)
library(lmerTest)
library(mgcv)
library(tidyr)
library(pracma)
library(extrafont)
library(scales)  
library(lubridate)
library(readr)
setwd("C:/Users/Usuario/Desktop/AU_milking_tests/data_analysis/R")

#df:
#df <- read.csv("C:/Users/Usuario/Desktop/AU_milking_tests/data_analysis/df_sin_cnd_ajCat_tiempoRandom.csv")
df <- read_csv("df_final2_AU_sin_cnd.csv")


df$prim_mult <- as.factor(df$prim_mult)
df$estado_prd <- as.factor(df$estado_prd)
df$est_prod_ant <- as.factor(df$est_prod_ant)



df$est_prod_ant[df$est_prod_ant == ""] <- NA
df <- df[df$tiempo_0_grupo_sin_cnd >= -10, ]
df$grupo_2 <- as.factor(df$grupo_2)


# Unique cows
length(unique(df$Cow))

# Tabla de vacas únicas por grupo prim_mult
# Primero eliminar duplicados por vaca
df_unico <- df[!duplicated(df$Cow), ]

#  prim_mult
table(df_unico$prim_mult)
prop.table(table(df_unico$prim_mult))

#est_prod_ant
table(df_unico$est_prod_ant)
prop.table(table(df_unico$est_prod_ant))

#alarms
table(df_unico$grupo_2)
prop.table(table(df_unico$grupo_2))

#------------------------------------------------------
# Plot milk yield by time
df <- df %>%
  group_by(Cow) %>%
  mutate(
    grupo = case_when(
      max(as.numeric(as.character(grupo_2)), na.rm = TRUE) == 1 ~ "> 30%",
      TRUE ~ "Control"
    )
  ) %>%
  ungroup() %>%
  mutate(
    tiempo_0 = tiempo_0_grupo_sin_cnd,
    produccion_total_kg = produccion_total * 0.453592  # Conversión a kg
  )

# Median and SE in kg
df_summary <- df %>%
  group_by(grupo, tiempo_0) %>%
  summarise(
    media = mean(produccion_total_kg, na.rm = TRUE),
    se = sd(produccion_total_kg, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Plot in kg
ggplot(df_summary, aes(x = tiempo_0, y = media, color = grupo, group = grupo)) +
  geom_ribbon(
    aes(ymin = media - se, ymax = media + se, fill = grupo),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(size = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray", linewidth = 1) +
  scale_color_manual(
    values = c("Control" = "#FF8000", "> 30%" = "#0072B2"),
    labels = c("Control" = "Unexposed", "> 30%" = "Exposed")
  ) +
  scale_fill_manual(
    values = c("Control" = "#FF8000", "> 30%" = "#0072B2"),
    labels = c("Control" = "Unexposed", "> 30%" = "Exposed")
  ) +
  guides(fill = "none") +
  scale_x_continuous(
    limits = c(min(df_summary$tiempo_0, na.rm = TRUE), 305),
    breaks = seq(0, 305, by = 50)
  ) +
  scale_y_continuous(
    breaks = seq(
      floor(min(df_summary$media - df_summary$se, na.rm = TRUE) / 5) * 5,
      ceiling(max(df_summary$media + df_summary$se, na.rm = TRUE) / 5) * 5,
      by = 5
    )
  ) +
  labs(
    x = "Days post-event",
    y = "Milk yield (kg)",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal(base_size = 14, base_family = "DejaVu Sans") +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 13, face = "plain"),  # similares a fontweight='bold'
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    panel.grid.minor = element_blank()
  )

ggsave("C:/Users/Usuario/Desktop/AU_milking_tests/paper/Figuras/Fig2_descriptivo.png", width = 12, height = 6, dpi = 300, bg = "white")

df_summary <- df %>%
  group_by(grupo, tiempo_0) %>%
  summarise(
    n = sum(!is.na(produccion_total_kg)),
    media = mean(produccion_total_kg, na.rm = TRUE),
    se = sd(produccion_total_kg, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) %>%
  arrange(grupo, tiempo_0)


