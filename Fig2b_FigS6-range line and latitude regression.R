# Load required libraries
library(ggplot2)
library(dplyr)
library(viridis)
library(cowplot)

# ------------------------------------------
# 1. Load data and split into two hemispheres
# ------------------------------------------
data <- read.csv("0-total.csv")
north <- subset(data, centrl_y >= 0)
south <- subset(data, centrl_y < 0)

# ------------------------------------------
# 2. Regression model + expression builder
# ------------------------------------------
get_regression_eq_string <- function(model) {
  coefs <- coef(summary(model))
  intercept <- round(coefs[1, 1], 2)
  slope     <- round(coefs[2, 1], 2)
  r2        <- round(summary(model)$r.squared, 2)
  p_val     <- signif(coefs[2, 4], 2)
  
  # Format equation for parse = TRUE
  eq <- paste0(
    "italic(y) == ", intercept,
    ifelse(slope >= 0, " + ", " - "), abs(slope), " * italic(x)",
    " *','~ R^2 == ", r2,
    " *','~ italic(p) < 0.001 "
  )
  return(eq)
}

# ------------------------------------------
# 3. Northern Hemisphere
# ------------------------------------------
model_north <- lm(rng_size ~ extent.ymax, data = north)
eq_north <- get_regression_eq_string(model_north)

plot_north <- ggplot(north, aes(x = extent.ymax, y = rng_size)) +
  geom_point(aes(fill = log(raw.points)), shape = 21, color = "black", size = 5, alpha = 0.8) +
  stat_smooth(method = "lm", se = TRUE, size = 1.5, color = "blue", fill = "lightblue", alpha=0.3) +
  annotate("text", x = 60, y = 160, label = eq_north, parse = TRUE, size = 6, hjust = 1) +
  scale_fill_viridis_c(option = "plasma", name = "Log(raw.points)") +
  coord_cartesian(xlim = c(0, 90), ylim = c(0, 180)) +
  scale_x_continuous(breaks = seq(0, 90, 30), labels = function(x) paste0(x, "°")) +
  labs(
    title = "",
    x = "Latitude of cold range (°)",
    y = "Latitudinal range (°)"
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, face = "bold", hjust = 0),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14, face = "bold"),
    axis.line.y = element_line(size = 1.0),
    axis.line.x = element_line(size = 1.0),
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.8, "cm")
  )
plot_north
# Save North Hemisphere plot
tiff("Fig2b_lat_range_north.tiff", width = 9, height = 7, units = "in", res = 300, compression = "lzw")
print(plot_north)
dev.off()

# ------------------------------------------
# 4. Southern Hemisphere
# ------------------------------------------
model_south <- lm(rng_size ~ extent.ymin, data = south)
eq_south <- get_regression_eq_string(model_south)

plot_south <- ggplot(south, aes(x = extent.ymin, y = rng_size)) +
  geom_point(aes(fill = log(raw.points)), shape = 21, color = "black", size = 5, alpha = 0.8) +
  stat_smooth(method = "lm", se = TRUE, size = 1.5, color = "blue", fill = "lightblue") +
  annotate("text", x = -5, y = 120, label = eq_south, parse = TRUE, size = 6, hjust = 1) +
  scale_fill_viridis_c(option = "viridis", name = "Log(raw.points)") +
  coord_cartesian(xlim = c(-60, 0), ylim = c(0, 130)) +
  scale_x_continuous(breaks = seq(-60, 0, 20), labels = function(x) paste0(x, "°")) +
  labs(
    title = " ",
    x = "Latitude of cold range (°)",
    y = "Latitudinal range (°)"
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, face = "bold", hjust = 0),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14, face = "bold"),
    axis.line.y = element_line(size = 1.0),
    axis.line.x = element_line(size = 1.0),
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.8, "cm")
  )
plot_south
# Save South Hemisphere plot
tiff("FigS6_lat_range_south.tiff", width = 9, height = 7, units = "in", res = 300, compression = "lzw")
print(plot_south)
dev.off()
