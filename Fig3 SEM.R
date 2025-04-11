# --- Load libraries ---
library(dplyr)
library(ggplot2)
library(randomForest)
library(lavaan)
library(lavaanPlot)

# --- Step 1: Read data and define variables ---
clm0 <- read.csv("clm_var_54.csv")
clmVar <- tolower(names(clm0)[7:60])
yVar <- "rng_size"
yMax <- "extent.ymax"

df <- read.csv("1-total_1970s_clm_1984pnt.csv")

# --- Step 2: Northern Hemisphere Analysis ---
df_Nor <- subset(df, Centrl >= 0)
data_north <- df_Nor[, c(clmVar, yVar, yMax)]

# Group by 1° bins using cold-edge limit
bins <- seq(2, 84, by = 1)
data_north$extent.ymax_group <- cut(data_north$extent.ymax, breaks = c(0, bins), labels = bins, right = FALSE)
data_north$extent.ymax_group <- as.numeric(as.character(data_north$extent.ymax_group))

result_north <- data_north %>%
  group_by(extent.ymax_group) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  distinct()
write.csv(result_north, "2-SEM_Nor.csv", row.names = FALSE)

# --- Step 3: Variable selection via stepwise regression ---
df_north <- read.csv("2-SEM_Nor.csv")
data <- df_north[, c(clmVar, yVar)]

x_scaled <- scale(data[, clmVar])
y_scaled <- scale(data[, yVar])
data_scaled <- as.data.frame(cbind(x_scaled, y_scaled))
names(data_scaled)[ncol(data_scaled)] <- yVar

full_formula <- as.formula(paste(yVar, "~", paste(clmVar, collapse = "+")))
null_model <- lm(as.formula(paste(yVar, "~ 1")), data = data_scaled)
full_model <- lm(full_formula, data = data_scaled)

step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "both")
selected_vars <- names(coef(step_model))[-1]

# --- Step 4: SEM for Northern Hemisphere ---
model_north <- '
  nffd_sm ~ tmin_sp
  cmd_wt ~ cmd_sp
  rng_size ~ nffd_sm + cmd_wt + tmin_sp + cmd_sp
'
fit_north <- lavaan::sem(model_north, data = data_scaled, std.lv = TRUE)
summary(fit_north, fit.measures = TRUE, standardized = TRUE)

# --- Step 5: Southern Hemisphere Analysis ---
df_Sou <- subset(df, Centrl < 0)
yMin <- "extent.ymin"
data_south <- df_Sou[, c(clmVar, yVar, yMin)]

bins_south <- seq(-57, -1, by = 0.5)
data_south$extent.ymin_group <- cut(data_south$extent.ymin, breaks = c(0, bins_south), labels = bins_south, right = FALSE)
data_south$extent.ymin_group <- as.numeric(as.character(data_south$extent.ymin_group))

result_south <- data_south %>%
  group_by(extent.ymin_group) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  distinct()
write.csv(result_south, "2-SEM_Sou.csv", row.names = FALSE)

# --- Step 6: Southern Hemisphere SEM ---
df_south <- read.csv("2-SEM_Sou.csv")
data <- df_south[, c(clmVar, yVar)]

x_scaled <- scale(data[, clmVar])
y_scaled <- scale(data[, yVar])
data_scaled <- as.data.frame(cbind(x_scaled, y_scaled))
names(data_scaled)[ncol(data_scaled)] <- yVar

# Simplified SEM with latent drought factor
model_south <- '
  Drought =~ cmd_wt + ppt_sp
  rng_size ~ Drought + tmin_at
  nffd_sm ~ tmin_at
'
fit_south <- lavaan::sem(model_south, data = data_scaled, std.lv = TRUE)
summary(fit_south, fit.measures = TRUE, standardized = TRUE)

# --- Step 7: Visualization ---
lavaanPlot(
  model = fit_south,
  coefs = TRUE,
  stand = TRUE,
  digits = 2,
  graph_options = list(
    rankdir = "LR",
    nodesep = 0.5,
    fontsize = 12,
    edge_width = 1.2,
    fixedsize = FALSE,
    layout = "dot"
  )
)
