library(epiR)
alerts <- read.csv("alerts.csv")
elisa <- read.csv("elisa.csv")


merged_data <- merge(alerts, elisa, by = "ID", all.y = TRUE)
merged_data  <- merge(alerts, elisa, by = "ID")
#View(merged_data)

#########################################

library(dplyr)

# 1) Collapse to ONE row per cow (ID)
df_cow <- merged_data %>%
  group_by(ID) %>%
  summarise(
    alarma = as.integer(any(alarma == 1, na.rm = TRUE)),  # clinical alert ever
    result_elisa = dplyr::first(result_elisa),
    .groups = "drop"
  )

# 2) Calculate percentages by ELISA category
tab <- df_cow %>%
  group_by(result_elisa) %>%
  summarise(
    n_clin  = sum(alarma == 1, na.rm = TRUE),
    n_total = n(),
    pct     = 100 * n_clin / n_total,
    .groups = "drop"
  )

tab

######################  
# Histogram ELISA, boxplot, Welch  

# Histogram of ELISA levels
hist(merged_data$level_elisa, breaks = 30, main = "Distribution of ELISA Levels", xlab = "S/N ratio")
hist(elisa$level_elisa, 
     breaks = 10,  # Fewer bins than default (adjust this number as desired)
     col = "grey", 
     main = "Distribution of ELISA Levels", 
     xlab = "S/N ratio")



boxplot(level_elisa ~ alarma, data = merged_data,
        names = c("No Alert", "Alert"),
        ylab = "ELISA S/N ratio", main = "ELISA Levels by Alert Status")


# Group means and standard errors
# Test difference
t.test(level_elisa ~ alarma, data = merged_data)
library(dplyr)
merged_data %>%
  group_by(alarma) %>%
  summarise(
    mean = mean(level_elisa, na.rm = TRUE),
    se = sd(level_elisa, na.rm = TRUE) / sqrt(n())
  )

# Logistic regression: higher S/N 
model <- glm(alarma ~ level_elisa, data = merged_data, family = binomial)
summary(model)



##### association between ELISA and ALERTS 
merged$lact_cat <- factor(merged$lact_cat, levels = c("primiparous", "multiparous"))
model2 <- glm(alarma ~ result_elisa +lact_cat+stage , data = merged, family = binomial)
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))

model_suppl2 <- glm(alarma ~ elisa_cat + lact_cat + stage_cat,
  data = your_data,
  family = binomial)
  
  
  

##### association between LACTATION and ELISA POSITIVE
merged <- merged_data

# Create lactation category: 1 vs 2 or greater
merged$lact_cat <- with(merged, ifelse(lact == 1, "1", "2+"))
merged$result_elisa_bin <- ifelse(merged$result_elisa == "Positive", 1, 0)

# Fit logistic regression model
model <- glm(result_elisa_bin ~ lact_cat, data = merged, family = binomial)


# View summary of the model
summary(model)

# Optionally, get odds ratios and confidence intervals
exp(cbind(OR = coef(model), confint(model)))



#########################################
# SPLINES ONLY WITH POSITIVES (<0.40)


library(splines)

## Subset to ELISA-positive cows (S/N < 0.4)
subset_data <- merged_data[
  merged_data$level_elisa < 0.4 &
    !is.na(merged_data$level_elisa),]

## Parity as numeric; quadratic term
subset_data$lact  <- as.numeric(subset_data$lact)
subset_data$lact2 <- subset_data$lact^2

## Logistic regression: linear S/N + lact
model_linear_sub <- glm(
  alarma ~ level_elisa + lact,
  data = subset_data,
  family = binomial
)
summary(model_linear_sub)

## Logistic regression: spline S/N + lact (TABLE)
model_spline_sub <- glm(
  alarma ~ ns(level_elisa, df = 3) + lact,
  data = subset_data,
  family = binomial
)
summary(model_spline_sub)

## Logistic regression: spline S/N + lact   (TABLE)
model_sub <- glm(
  alarma ~ ns(level_elisa, df = 3) + lact ,
  data = subset_data,
  family = binomial
)
summary(model_sub)


## Logistic regression: spline S/N + lact + lact^2  (NO USED IN TABLE)
model_quad_sub <- glm(
  alarma ~ ns(level_elisa, df = 3) + lact + lact2,
  data = subset_data,
  family = binomial
)
summary(model_quad_sub)

## Likelihood ratio test (spline vs spline + lact^2)
anova(model_spline_sub, model_quad_sub, test = "Chisq")

## ORs and 95% CI from the linear model
exp(cbind(
  OR = coef(model_linear_sub),
  confint(model_linear_sub)
))

## Predicted probability of alert vs S/N (holding lact at median)
elisa_seq_sub <- seq(
  min(subset_data$level_elisa, na.rm = TRUE),
  max(subset_data$level_elisa, na.rm = TRUE),
  length.out = 100
)

predicted_sub <- predict(
  model_linear_sub,
  newdata = data.frame(
    level_elisa = elisa_seq_sub,
    lact = median(subset_data$lact, na.rm = TRUE)
  ),
  type = "response"
)

plot(
  elisa_seq_sub,
  predicted_sub,
  type = "l", lwd = 2,
  xlab = "ELISA S/N ratio (<0.4 only)",
  ylab = "Predicted probability of clinical alert",
  main = ""
)

## OR per 0.05 increase in S/N
beta <- coef(model_linear_sub)["level_elisa"]
se   <- summary(model_linear_sub)$coefficients["level_elisa", "Std. Error"]

inc <- 0.05
OR_0.05     <- exp(beta * inc)
lower_CI    <- exp((beta - 1.96 * se) * inc)
upper_CI    <- exp((beta + 1.96 * se) * inc)

cat(
  "OR per 0.05-unit increase in S/N: ",
  round(OR_0.05, 2),
  " (95% CI ",
  round(lower_CI, 2), "–", round(upper_CI, 2), ")\n",
  sep = ""
)



