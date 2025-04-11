# --------------------------------------------------------
# 🧪 Analysis: Relationship Between Latitude and Range Size by Family
# --------------------------------------------------------

# 📦 Load necessary packages
library(dplyr)
library(broom)
library(ggplot2)
library(ggpmisc)

# 📂 Load the main dataset
data0 <- read.csv("0-total.csv", header = TRUE)

# 🌍 Split into Northern and Southern Hemisphere datasets
north_data <- data0 %>% filter(extent.ymax > 0)
south_data <- data0 %>% filter(extent.ymin < 0)

# 🔍 Filter families with more than 10 species
df_north_filtered <- north_data %>%
  group_by(family) %>%
  filter(n() > 10)

df_south_filtered <- south_data %>%
  group_by(family) %>%
  filter(n() > 10)

# --------------------------------------------------------
# 🔁 Define function to run per-family linear regression
# --------------------------------------------------------
analyze_family_full <- function(data, formula, term_name) {
  results <- data %>%
    group_by(family) %>%
    do({
      model <- lm(formula, data = .)
      tidy_model <- tidy(model)
      filter(tidy_model, term == term_name)
    }) %>%
    ungroup()
  return(results)
}

# Run full regression for all families
all_north <- analyze_family_full(df_north_filtered, rng_size ~ extent.ymax, "extent.ymax")
all_south <- analyze_family_full(df_south_filtered, rng_size ~ extent.ymin, "extent.ymin")

# Save full results (Tables S2 and S3)
TableS2 <- all_north %>% mutate(across(where(is.numeric), ~ round(., 3)))
TableS3 <- all_south %>% mutate(across(where(is.numeric), ~ round(., 3)))
write.csv(TableS2, "TableS2_family_lat_rngSize_north.csv", row.names = FALSE)
write.csv(TableS3, "TableS3_family_lat_rngSize_south.csv", row.names = FALSE)

# --------------------------------------------------------
# ⭐ Count significant families and compute percentage
# --------------------------------------------------------
sig_north <- TableS2 %>% filter(p.value < 0.05)
sig_south <- TableS3 %>% filter(p.value < 0.05)

cat("🔹 Northern Hemisphere:\n")
cat("  Total families analyzed:", length(unique(TableS2$family)), "\n")
cat("  Significant families (p < 0.05):", nrow(sig_north), "\n")
cat("  Percentage:", round(nrow(sig_north) / length(unique(TableS2$family)) * 100, 1), "%\n\n")

cat("🔹 Southern Hemisphere:\n")
cat("  Total families analyzed:", length(unique(TableS3$family)), "\n")
cat("  Significant families (p < 0.05):", nrow(sig_south), "\n")
cat("  Percentage:", round(nrow(sig_south) / length(unique(TableS3$family)) * 100, 1), "%\n")

# --------------------------------------------------------
# 🎯 Select Top 3 Most Significant Families for Plotting
# --------------------------------------------------------
top3_north <- sig_north %>% arrange(p.value) %>% head(3)
top3_south <- sig_south %>% arrange(p.value) %>% slice(2:4)

# Extract data for selected families
top3_north_data <- df_north_filtered %>%
  filter(family %in% top3_north$family) %>%
  rename(Family = family)

top3_south_data <- df_south_filtered %>%
  filter(family %in% top3_south$family) %>%
  rename(Family = family)

# --------------------------------------------------------
# 📈 Define plotting function for each hemisphere
# --------------------------------------------------------
plot_family <- function(data, x_var) {
  ggplot(data, aes_string(x = x_var, y = "rng_size", color = "Family")) +
    geom_point(size = 3, alpha = 0.8) +
    stat_smooth(method = "lm", se = TRUE, size = 1.5) +
    labs(x = "Latitude", y = "Range Size") +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.title.x = element_text(size = 25, face = "bold", margin = margin(t = 20)),
      axis.title.y = element_text(size = 25, face = "bold", margin = margin(r = 20)),
      axis.text = element_text(size = 18, face = "bold"),
      axis.line = element_line(size = 1.2),
      legend.position = "top",
      legend.title = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 15, face = "bold"),
      legend.key.size = unit(0.8, "cm"),
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    )
}

# Generate plots
plot_north <- plot_family(top3_north_data, "extent.ymax")
plot_south <- plot_family(top3_south_data, "extent.ymin")

# --------------------------------------------------------
# 💾 Save the plots
# --------------------------------------------------------
tiff("Top3_Families_North.tiff", width = 9, height = 7, units = "in", res = 300)
print(plot_north)
dev.off()

tiff("Top3_Families_South.tiff", width = 9, height = 7, units = "in", res = 300)
print(plot_south)
dev.off()

# --------------------------------------------------------
# 📤 Output top families
# --------------------------------------------------------
cat("\nTop 3 most significant families (North):\n")
print(top3_north$family)

cat("\nTop 3 most significant families (South):\n")
print(top3_south$family)
