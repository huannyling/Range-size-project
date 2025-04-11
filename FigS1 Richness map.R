# 📦 Load required packages
library(ncdf4)         # Read NetCDF files
library(raster)        # Handle raster data
library(dplyr)         # Data manipulation
library(ggplot2)       # Visualization
library(sf)            # Vector data (country borders)
library(rnaturalearth) # Load world map
library(viridis)       # Color palette

# Install rnaturalearthdata if needed
# install.packages("rnaturalearthdata")

# 📂 Load species range data from NetCDF file
nc_file <- "range_data_default.nc"
raster_data <- brick(nc_file, varname = "Native region")  # Load as raster brick

# ----------------------------------------
# 🔹 Function: Extract Highest Latitude Point
# ----------------------------------------
extract_highest_latitude <- function(species_layer) {
  points <- as.data.frame(rasterToPoints(species_layer))
  colnames(points) <- c("lon", "lat", "value")
  points <- points[points$value > 0, ]  # Keep only presence values
  if (nrow(points) == 0) return(data.frame(lon = NA, lat = NA, value = NA))
  max_lat_point <- points[which.max(points$lat), ]
  return(max_lat_point)
}

# Apply to all species layers
highest_latitudes <- do.call(rbind, lapply(1:nlayers(raster_data), function(i) {
  result <- extract_highest_latitude(raster_data[[i]])
  result$species <- paste0("Species_", i)
  return(result)
}))
write.csv(highest_latitudes, "highest_latitudes_native.csv", row.names = FALSE)

# ----------------------------------------
# 🔹 Function: Extract Lowest Latitude Point
# ----------------------------------------
extract_lowest_latitude <- function(species_layer) {
  points <- as.data.frame(rasterToPoints(species_layer))
  colnames(points) <- c("lon", "lat", "value")
  points <- points[points$value > 0, ]
  if (nrow(points) == 0) return(data.frame(lon = NA, lat = NA, value = NA))
  min_lat_point <- points[which.min(points$lat), ]
  return(min_lat_point)
}

# Apply to all species layers
lowest_latitudes <- do.call(rbind, lapply(1:nlayers(raster_data), function(i) {
  result <- extract_lowest_latitude(raster_data[[i]])
  result$species <- paste0("Species_", i)
  return(result)
}))
write.csv(lowest_latitudes, "lowest_latitudes_native.csv", row.names = FALSE)

# ----------------------------------------
# 🔹 Species Richness Calculation
# ----------------------------------------
species_richness <- calc(raster_data, fun = function(x) sum(x > 0, na.rm = TRUE))
richness_df <- as.data.frame(rasterToPoints(species_richness), xy = TRUE)
colnames(richness_df) <- c("lon", "lat", "richness")
write.csv(richness_df, "sps_richness_native.csv", row.names = FALSE)

# ----------------------------------------
# 🌍 Map Visualization: Species Richness
# ----------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")
richness_df$richness[is.na(richness_df$richness) | richness_df$richness == 0] <- NA

F1 <- ggplot() +
  geom_raster(data = richness_df, aes(x = lon, y = lat, fill = richness)) +
  scale_fill_gradientn(
    colors = c("lightyellow", "darkgreen", "blue"),
    name = "Species Richness",
    na.value = "#D6F5F5",
    guide = guide_colorbar(title.position = "top", barwidth = 20, barheight = 0.8)
  ) +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.5) +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "white", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    legend.key.width = unit(5, "npc"),
    panel.background = element_rect(fill = NA, color = NA)
  )

# Save the map
ggsave(
  filename = "species_richness_native.jpg",
  plot = F1,
  width = 6,
  height = 4,
  dpi = 500,
  units = "in"
)
