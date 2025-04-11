# Part 1: Build the Phylogenetic Tree and Save It
# Install necessary packages (if not already installed)
#install.packages(c("rotl", "ape", "dplyr"))

# Load packages
library(rotl)
library(ape)
library(dplyr)

# Read the CSV file containing family names
family_data <- read.csv("2-family_names.csv")

# Extract unique family names
family_names <- unique(family_data$family)

# Match family names to Open Tree of Life (OTL) and retrieve OTT IDs
matched <- tnrs_match_names(family_names)

# Extract valid OTT IDs and associate with family names
ott_ids <- matched$ott_id[!is.na(matched$ott_id)]
names(ott_ids) <- matched$unique_name[!is.na(matched$ott_id)]

# Build the tree in batches to avoid API failure
valid_ids <- c()
batch_size <- 50
for (i in seq(1, length(ott_ids), by = batch_size)) {
  try_ids <- ott_ids[i:min(i + batch_size - 1, length(ott_ids))]
  try({
    subtree <- tol_induced_subtree(ott_ids = try_ids)
    valid_ids <- c(valid_ids, try_ids)
  }, silent = TRUE)
}

# Construct the full tree using valid OTT IDs
tree <- tol_induced_subtree(ott_ids = valid_ids)

# Save the tree to a Newick format file
write.tree(tree, file = "family_phylogeny_tree.nwk")

# Optional: basic visualization (optional)
plot(tree, cex = 0.5)
title("Phylogenetic Tree (Valid Families)")




#🌳 Part 2: Plot Circular Phylogenetic Tree with Family Names Only
library(ggtree)
library(ape)
library(dplyr)

# Read the Newick file
tree <- read.tree("family_phylogeny_tree.nwk")

# Clean the tip labels by removing OTT ID suffix (e.g., "_ott123456")
tip_labels <- tree$tip.label
cleaned_labels <- gsub("_ott[0-9]+", "", tip_labels)
cleaned_labels <- gsub("ott[0-9]+", "", cleaned_labels)
tree$tip.label <- cleaned_labels  # Replace tip labels

# Plot the tree in circular layout, showing family names only
p <- ggtree(tree, layout = "circular") +
  geom_tiplab(size = 2, align = TRUE, linesize = 0.2) +
  ggtitle("Phylogenetic Tree of Plant Families")

# Display the plot
print(p)

# Save a high-resolution figure (600 DPI)
ggsave("family_tree_labels_only_600dpi.jpg", plot = p, dpi = 600, width = 10, height = 10)





# 📊 Part 3: Test Phylogenetic Signal in Latitudinal Range Size
# Install and load required packages
packages <- c("ape", "phytools", "dplyr", "rotl")
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies = TRUE)
}
lapply(packages, install_if_missing)
lapply(packages, library, character.only = TRUE)

# Read dataset containing family and range size
data <- read.csv("2-family_names.csv")
data$family <- as.character(data$family)

# Match family names to Open Tree of Life
matched <- tnrs_match_names(data$family)

# Filter out unmatched families
valid_ott_ids <- matched$ott_id[!is.na(matched$ott_id)]
valid_names <- matched$unique_name[!is.na(matched$ott_id)]
names(valid_ott_ids) <- valid_names

# Build the tree using valid OTT IDs
tree <- tol_induced_subtree(ott_ids = valid_ott_ids)

# Add branch lengths if missing
if (is.null(tree$edge.length)) {
  tree <- compute.brlen(tree, method = "Grafen")
}

# Clean tip labels to extract family names
tree$tip.label_clean <- sub("_ott[0-9]+", "", tree$tip.label)

# Find common families between tree and data
common_families <- intersect(tree$tip.label_clean, data$family)

# Prune tree to retain only common families
tips_to_keep <- tree$tip.label[tree$tip.label_clean %in% common_families]
tree_pruned <- drop.tip(tree, setdiff(tree$tip.label, tips_to_keep))

# Match the range size data to the tree tip labels
tip_labels <- tree_pruned$tip.label
tip_labels_clean <- sub("_ott[0-9]+", "", tip_labels)

# Extract range size in the same order as tip labels
rng_data <- data[data$family %in% tip_labels_clean, ]
matched_rng <- rng_data$rng_size[match(tip_labels_clean, rng_data$family)]
names(matched_rng) <- tip_labels

# Test for phylogenetic signal
cat("\n📌 Blomberg's K:\n")
print(phylosig(tree_pruned, matched_rng, method = "K", test = TRUE))

cat("\n📌 Pagel's Lambda:\n")
print(phylosig(tree_pruned, matched_rng, method = "lambda", test = TRUE))

# Visualize range size trait mapped on the phylogeny
contMap(tree_pruned, matched_rng, plot = TRUE)
title("Latitudinal Range Size Mapped onto Phylogenetic Tree")
