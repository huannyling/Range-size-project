# Load necessary libraries
library(ggplot2)
library(ggpubr)
library(dplyr)
library(multcompView)

# Read the dataset
data <- read.csv("Trait_RngSize_2300.csv")

# Convert categorical variables to factors
data$support <- as.factor(data$support)
data$woody <- as.factor(data$woody)
data$Growth_form <- as.factor(data$Growth_form)
data$DecidEver <- as.factor(data$DecidEver)
data$Compound_Simple <- as.factor(data$Compound_Simple)

# Fit linear models (optional for coefficient table output)
model2 <- lm(rng_size ~ support + DecidEver + Compound_Simple, data = data)
write.csv(as.data.frame(summary(model2)$coefficients), "Traits_Coefficients.csv")

# Define consistent publication-ready theme
custom_theme <- theme_classic() +
  theme(
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 14, angle = 20, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16)
  )

# Function to calculate Tukey letters for group comparisons
get_group_letters <- function(data, group_col, y_col) {
  formula <- as.formula(paste(y_col, "~", group_col))
  aov_result <- aov(formula, data = data)
  tukey_result <- TukeyHSD(aov_result)
  letters <- multcompLetters4(aov_result, tukey_result)
  group_letters <- as.data.frame.list(letters[[group_col]])
  group_letters[[group_col]] <- rownames(group_letters)
  
  # Get median values for placing labels
  medians <- data %>%
    group_by(.data[[group_col]]) %>%
    summarise(median_y = median(.data[[y_col]], na.rm = TRUE))
  
  label_df <- merge(medians, group_letters, by = group_col)
  colnames(label_df)[3] <- "letter"
  return(label_df)
}

### PLOT 1: Support types (excluding Epiphytes)
support_data <- subset(data, support != "E")
support_labels <- c(
  "F" = "Free-standing", "C" = "Climber", "P" = "Parasite",
  "H" = "Hemi-epiphyte", "M" = "Mycoheterotroph", "A" = "Aquatic"
)
support_letters <- get_group_letters(support_data, "support", "rng_size")

p_support <- ggplot(support_data, aes(x = support, y = rng_size)) +
  geom_boxplot(fill = "skyblue") +
  scale_x_discrete(labels = support_labels) +
  ylab("Latitudinal Range Size") +
  custom_theme +
  geom_text(data = support_letters, aes(x = support, y = median_y + 5, label = letter),
            size = 6, color = "black")

ggsave("Boxplot_Support.tif", p_support, width = 6, height = 5, dpi = 300)

### PLOT 2: Deciduous vs Evergreen
decid_labels <- c("D" = "Deciduous", "E" = "Evergreen")
decid_letters <- get_group_letters(data, "DecidEver", "rng_size")

p_decid <- ggplot(data, aes(x = DecidEver, y = rng_size)) +
  geom_boxplot(fill = "skyblue") +
  scale_x_discrete(labels = decid_labels) +
  ylab("Latitudinal Range Size") +
  custom_theme +
  geom_text(data = decid_letters, aes(x = DecidEver, y = median_y + 5, label = letter),
            size = 6, color = "black")

ggsave("Boxplot_DecidEver.tif", p_decid, width = 4, height = 5, dpi = 300)

### PLOT 3: Compound vs Simple leaves
leaf_labels <- c("C" = "Compound", "S" = "Simple")
leaf_letters <- get_group_letters(data, "Compound_Simple", "rng_size")

p_leaf <- ggplot(data, aes(x = Compound_Simple, y = rng_size)) +
  geom_boxplot(fill = "skyblue") +
  scale_x_discrete(labels = leaf_labels) +
  ylab("Latitudinal Range Size") +
  custom_theme +
  geom_text(data = leaf_letters, aes(x = Compound_Simple, y = median_y + 5, label = letter),
            size = 6, color = "black")

ggsave("Boxplot_LeafType.tif", p_leaf, width = 4, height = 5, dpi = 300)
