# Load necessary packages
library(raster)
library(Rmisc)
library("ggplot2")#package for better graphing
library(forcats)

# Load and prepare data
data0<- read.csv("0-total.csv", header = T)
data_nor<-subset(data0,centrl_y>0)
hd(data_nor)


# Northern Species
data_nor <- subset(data0, centrl_y >= 0)

F2 <- ggplot(data_nor, aes(x = reorder(speciesID, extent.ymax))) + 
  ggtitle("a. Range Line for Northern Species") +
  geom_point(aes(y = centrl_y), shape = 1, size = 0.0001) +  # Adjust point size for visibility
  geom_errorbar(aes(ymin = extent.ymin, ymax = extent.ymax, color = family), 
                width = 0.2, size = 0.8) +  # Color based on family
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) + # Add y=0 line
  coord_cartesian(ylim = c(-90, 90)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 30)) + 
  scale_color_manual(values = rainbow(length(unique(data_nor$family))), name = "Family") + 
  xlab("") + ylab("Latitude") + 
  theme_bw() + 
  theme(
    axis.line.y = element_line(colour = "black", size = 1), 
    axis.line.x = element_blank(),  # Remove bottom border
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),  # Remove borders
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.text.y = element_text(face = "bold", size = 15, color = "black"),
    plot.title = element_text(color = "black", size = 22, face = "bold", hjust = 0),
    axis.title.x = element_text(size = 22, face = "bold", color = "black"),
    axis.title.y = element_text(size = 20, face = "bold", color = "black", vjust = 2),
    plot.margin = unit(c(1, 0.5, 0.5, 1), "cm"),
    legend.position = "none"  # Remove legend
  )

# Save the Northern plot
tiff("Fig2a_Range_line_nor.tiff", units = "in", width = 10, height = 6, res = 500, compression = 'lzw')
print(F2)
dev.off()



# Southern Species
data_sou <- subset(data0, centrl_y < 0)

F3 <- ggplot(data_sou, aes(x = reorder(speciesID, extent.ymin))) + 
  ggtitle("Range Line for Southern Species") +
  geom_point(aes(y = centrl_y), shape = 1, size = 0.0001) +  # Adjust point size for visibility
  geom_errorbar(aes(ymin = extent.ymin, ymax = extent.ymax, color = family), 
                width = 0.2, size = 0.8) +  # Color based on family
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) + # Add y=0 line
  coord_cartesian(ylim = c(-90, 90)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 30)) + 
  scale_color_manual(values = rainbow(length(unique(data_sou$family))), name = "Family") + 
  xlab("") + ylab("Latitude") + 
  theme_bw() + 
  theme(
    axis.line.y = element_line(colour = "black", size = 1), 
    axis.line.x = element_blank(),  # Remove bottom border
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),  # Remove borders
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.text.y = element_text(face = "bold", size = 15, color = "black"),
    plot.title = element_text(color = "black", size = 22, face = "bold", hjust = 0),
    axis.title.x = element_text(size = 22, face = "bold", color = "black"),
    axis.title.y = element_text(size = 20, face = "bold", color = "black", vjust = 2),
    plot.margin = unit(c(1, 0.5, 0.5, 1), "cm"),
    legend.position = "none"  # Remove legend
  )

# Save the Southern plot
tiff("FigS2_Range_line_south.tiff", units = "in", width = 10, height = 6, res = 500, compression = 'lzw')
print(F3)
dev.off()

