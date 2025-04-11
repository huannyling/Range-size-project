# 📦 Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)

# 📂 Load species distribution dataset
file_path <- "0-total.csv"
data <- read.csv(file_path)

# ------------------------------------------------------------
# 🔹 Northern Hemisphere: Group by extent.ymax latitude bands
# ------------------------------------------------------------
north_data <- subset(data, centrl_y >= 0)
north_data$latitude_group <- cut(
  north_data$extent.ymax,
  breaks = c(0, 20, 40, 60, Inf),
  labels = c("0-20N", "20-40N", "40-60N", "60-80N"),
  right = FALSE
)

# Count species per family within each latitude band
north_summary <- north_data %>%
  group_by(latitude_group, family) %>%
  summarise(species_count = n_distinct(scientificname), .groups = "drop")

# Total species per latitude band
north_group_total <- north_data %>%
  group_by(latitude_group) %>%
  summarise(total_species = n_distinct(scientificname), .groups = "drop")

# Merge and calculate species percentage per family
north_final <- merge(north_summary, north_group_total, by = "latitude_group") %>%
  mutate(species_percentage = (species_count / total_species) * 100)

# Count families per group
north_family_count <- north_final %>%
  group_by(latitude_group) %>%
  summarise(family_count = n_distinct(family), .groups = "drop")

# Finalize northern table
north_final <- merge(north_final, north_family_count, by = "latitude_group")
north_final$hemisphere <- "North"

# ------------------------------------------------------------
# 🔻 Southern Hemisphere: Group by extent.ymin latitude bands
# ------------------------------------------------------------
south_data <- subset(data, centrl_y < 0)
south_data$latitude_group <- cut(
  south_data$extent.ymin,
  breaks = c(-Inf, -60, -40, -20, 0),
  labels = c("60-80S", "40-60S", "20-40S", "0-20S"),
  right = FALSE
)

# Count species per family per band
south_summary <- south_data %>%
  group_by(latitude_group, family) %>%
  summarise(species_count = n_distinct(scientificname), .groups = "drop")

# Total species per latitude band
south_group_total <- south_data %>%
  group_by(latitude_group) %>%
  summarise(total_species = n_distinct(scientificname), .groups = "drop")

# Merge and calculate percentage
south_final <- merge(south_summary, south_group_total, by = "latitude_group") %>%
  mutate(species_percentage = (species_count / total_species) * 100)

# Count families per band
south_family_count <- south_final %>%
  group_by(latitude_group) %>%
  summarise(family_count = n_distinct(family), .groups = "drop")

# Finalize southern table
south_final <- merge(south_final, south_family_count, by = "latitude_group")
south_final$hemisphere <- "South"

# ------------------------------------------------------------
# 🌎 Combine and Export Summary Table
# ------------------------------------------------------------
final_summary <- rbind(north_final, south_final)
write.csv(final_summary, "latitude_family_species_summary.csv", row.names = FALSE)
cat("✅ Latitude–Family–Species summary saved to CSV.\n")

# ------------------------------------------------------------
# 🥧 Plotting Pie Charts by Latitude Zone
# ------------------------------------------------------------

# Pie for 0–20N with legend
nzero_20n_data <- subset(north_final, latitude_group == "0-20N")
pie1 <- ggplot(nzero_20n_data, aes(x = "", y = species_percentage, fill = family)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  ggtitle("0–20°N: Family Composition") +
  theme(legend.title = element_blank())
ggsave("Pie_legend_North.tif", plot = pie1, width = 16, height = 8, units = "in", dpi = 300)

# Pie for 0–20S with legend
szero_20s_data <- subset(south_final, latitude_group == "0-20S")
pie2 <- ggplot(szero_20s_data, aes(x = "", y = species_percentage, fill = family)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  ggtitle("0–20°S: Family Composition") +
  theme(legend.title = element_blank())
ggsave("Pie_legend_South.tif", plot = pie2, width = 16, height = 8, units = "in", dpi = 300)

# Generic pie chart function without legend
generate_pie_chart <- function(data, group_name, label) {
  ggplot(data, aes(x = "", y = species_percentage, fill = family)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    theme_void() +
    ggtitle(paste0(label, ". ", group_name)) +
    theme(plot.title = element_text(size = 16, hjust = 0.5), legend.position = "none")
}

# Northern Hemisphere multi-panel pie charts
north_plots <- mapply(function(group, label) {
  generate_pie_chart(subset(north_final, latitude_group == group), group, label)
}, unique(north_final$latitude_group), letters[1:4], SIMPLIFY = FALSE)

tiff("north_pie_charts.tif", width = 16, height = 4, units = "in", res = 300)
grid.arrange(grobs = north_plots, ncol = 4)
dev.off()

# Southern Hemisphere multi-panel pie charts
south_plots <- mapply(function(group, label) {
  generate_pie_chart(subset(south_final, latitude_group == group), group, label)
}, unique(south_final$latitude_group), letters[5:7], SIMPLIFY = FALSE)

tiff("south_pie_charts.tif", width = 12, height = 4, units = "in", res = 300)
grid.arrange(grobs = south_plots, ncol = 3)
dev.off()

cat("✅ Pie charts for family composition saved (north & south).\n")
