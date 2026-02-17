# =============================================================================
# Master Script — Wetland Analysis Pipeline
# Runs all analysis steps in sequence.
# =============================================================================

# Load all packages used across the Wetland analysis pipeline

library(pacman)

p_load(
  tidyverse,
  brms,
  tidybayes,
  cansim,
  janitor,
  kableExtra,
  modelsummary,
  patchwork,
  ggrepel
)

source("code/Wetland/1CreatePostStratTable.R")
source("code/Wetland/2SampleComparison.R")
source("code/Wetland/3EstimateModels.R")
source("code/Wetland/4ImputeWTP.R")
source("code/Wetland/5GraphWTP.R")
source("code/Wetland/6PoeTest.R")
source("code/Wetland/7CreateTables.R")
