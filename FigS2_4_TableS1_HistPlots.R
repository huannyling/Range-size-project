# Load necessary packages
library(dismo)
library(randomForest)
library(raster)
library(bit64)
library(sp)
library(rgdal)
library(doSNOW)
library(terra)
library(dplyr)
library(gplots)
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)

# -------------------------------
# Load and prepare data
# -------------------------------
data <- read.csv("0-total.csv")
data0 <- unique(data)

# -------------------------------
# 1. Range Size Frequency Plot
# -------------------------------
percentiles <- quantile(data0$rng_size, probs = c(0.5, 0.8, 0.9))
label_height <- max(table(cut(data0$rng_size, breaks=seq(min(data0$rng_size), max(data0$rng_size), by=5)))) + 5

sps_rng_freq <- ggplot(data0, aes(x = rng_size)) +
  geom_histogram(binwidth = 5, colour = "black", fill = "lightblue") +
  geom_density(alpha = 0.3, fill = "#FF6666") +
  geom_vline(xintercept = percentiles[1], color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = percentiles[3], color = "red", linetype = "solid", size = 1) +
  geom_text(aes(x = percentiles[1] - 15, y = label_height - 2000,
                label = sprintf("%.1f°", percentiles[1])), color = "black", vjust = -1, hjust = -0.1) +
  geom_text(aes(x = percentiles[3] + 3, y = label_height - 2000,
                label = sprintf("%.1f°", percentiles[3])), color = "black", vjust = -1, hjust = -0.1) +
  ylim(0, 12000) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_text(face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.margin = unit(c(1.5, 1, 1, 1), "cm")
  ) +
  labs(title = "Range Size Frequency",
       x = "Latitudinal Range (°)",
       y = "Number of Species")
sps_rng_freq
ggsave("FigS2_rng_size_frequency.tif", plot = sps_rng_freq, width = 8, height = 6, dpi = 300)

# -------------------------------
# 2. Cold Range Frequency (North)
# -------------------------------
data_north <- subset(data0, centrl_y > 0)
percentiles_north <- quantile(data_north$extent.ymax, probs = c(0.5, 0.9))

sps_cod_freq_north <- ggplot(data_north, aes(x = extent.ymax)) +
  geom_histogram(binwidth = 3, colour = "black", fill = "lightblue") +
  geom_density(alpha = 0.3, fill = "#FF6666") +
  geom_vline(xintercept = percentiles_north[1], color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = percentiles_north[2], color = "red", linetype = "solid", size = 1) +
  geom_text(aes(x = percentiles_north[1] + 3, y = 4000,
                label = sprintf("%.1f°", percentiles_north[1])), color = "black", vjust = -1, hjust = -0.1) +
  geom_text(aes(x = percentiles_north[2] + 3, y = 4000,
                label = sprintf("%.1f°", percentiles_north[2])), color = "black", vjust = -1, hjust = -0.1) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_text(face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  labs(title = "Cold Range Frequency (North)",
       x = "Latitude (°)",
       y = "Number of Species")

ggsave("FigS3_cold_north_frequency.tif", plot = sps_cod_freq_north, width = 8, height = 6, dpi = 300)

# -------------------------------
# 3. Cold Range Frequency (South)
# -------------------------------
data_south <- subset(data0, centrl_y <= 0)
percentiles_south <- quantile(data_south$extent.ymin, probs = c(0.5, 0.1))

sps_cod_freq_south <- ggplot(data_south, aes(x = extent.ymin)) +
  geom_histogram(binwidth = 2, colour = "black", fill = "lightblue") +
  geom_density(alpha = 0.3, fill = "#FF6666") +
  geom_vline(xintercept = percentiles_south[1], color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = percentiles_south[2], color = "red", linetype = "solid", size = 1) +
  geom_text(aes(x = percentiles_south[1] + 1, y = 5000,
                label = sprintf("%.1f°", percentiles_south[1])), color = "black", vjust = -1, hjust = -0.1) +
  geom_text(aes(x = percentiles_south[2] + 1, y = 5000,
                label = sprintf("%.1f°", percentiles_south[2])), color = "black", vjust = -1, hjust = -0.1) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_text(face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  labs(title = "Cold Range Frequency (South)",
       x = "Latitude (°)",
       y = "Number of Species")

ggsave("FigS4_cold_south_frequency.tif", plot = sps_cod_freq_south, width = 8, height = 6, dpi = 300)

# -------------------------------
# 4. Latitudinal Range Size Bin Summary
# -------------------------------
data0$Lat_range_bins <- cut(data0$Lat_range, 
                            breaks = seq(from = min(data0$Lat_range), 
                                         to = max(data0$Lat_range), by = 5), 
                            include.lowest = TRUE)

# Create and export frequency table
frequency_table <- table(data0$Lat_range_bins)
frequency_df <- as.data.frame(frequency_table)
names(frequency_df) <- c("Lat_range_Bins", "Frequency")

# Save to CSV
write.csv(frequency_df, "TableS1_frqn_lat_range.csv", row.names = FALSE)
