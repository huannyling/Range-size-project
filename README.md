# Range-size-project
🌍 Global Plant Range Size Project
This repository contains the full code and analysis pipeline for the study "Hemispheric asymmetry in climatic drivers of plant latitudinal range size", which investigates global patterns in plant geographic distributions and the ecological, climatic, and evolutionary factors that shape them.

📊 Overview
We analyze the latitudinal range sizes of 47,675 vascular plant species using a combination of trait-based modeling, structural equation modeling (SEM), and phylogenetic signal analysis. Our objectives are to:

Test Rapoport’s rule in both hemispheres.

Identify climatic drivers of latitudinal range size using SEM.

Assess evolutionary lability of range size via phylogenetic signal tests.

Investigate plant trait associations with range size (growth form, leaf size, structure, phenology).

🧪 Major Components
1. Data Preparation
Climate and occurrence data were grouped by range edge (e.g., extent.ymax or extent.ymin) into 1°/0.5° bins.

Mean climatic variables and latitudinal range sizes were computed per bin for SEM input.

2. Predictor Selection
A Random Forest model was applied separately for Northern and Southern Hemisphere data to identify the top climatic variables influencing range size.

Stepwise regression was used to refine the variable list before SEM modeling.

3. Structural Equation Modeling (SEM)
SEMs were implemented using the lavaan package in R.

Model refinement was based on modification indices (MI), model fit statistics (CFI, RMSEA, SRMR), and ecological plausibility.

For the Southern Hemisphere, latent variables were constructed to represent drought and thermal variability.

4. Trait-Based Analysis
Trait data (growth form, leaf phenology, leaf structure, leaf size) were obtained from the TRY database and major global compilations.

Linear regression and Tukey HSD tests assessed the relationship between traits and range size.

Significance groups were visualized using ggplot2, ggpubr, and multcompView.

5. Phylogenetic Signal Testing
A family-level phylogenetic tree was built using the Open Tree of Life API.

Phylogenetic signal in range size was tested using Blomberg’s K and Pagel’s λ (phytools).

Continuous trait mapping visualized trait lability across lineages.

📁 Main Scripts and Files
Climate_Binning_and_RF.R – Climate grouping, RF modeling, and variable selection

SEM_Model_North.R – SEM fitting and refinement for the Northern Hemisphere

SEM_Model_South.R – SEM with latent variables for Southern Hemisphere

Trait_Analysis.R – Regression and visualization of trait–range size relationships

Phylogenetic_Signal.R – Phylogenetic tree construction and signal analysis

Supplementary_Figures.R – Code for all supplementary plots and statistics

📦 Dependencies
R (≥ 4.2.0)

lavaan, ggplot2, ggpubr, randomForest, dplyr, multcompView, phytools, rotl, broom, semPlot

📈 Outputs
SEM diagrams (including latent variable models)

Trait–range size boxplots with significance letters

Continuous trait maps on phylogenies

Supplementary tables and figures (for manuscript submission)

📜 Citation
If you use this code or dataset, please cite our forthcoming publication:

Author(s). Hemispheric asymmetry in climatic drivers of plant latitudinal range size. Nature (in review).
