# =============================================================================
# Master Script — SRD Analysis Pipeline
# Loads packages, reads and prepares data, then runs all analysis steps.
# =============================================================================

rm(list = ls(all = TRUE))

library(pacman)

p_load(
  tidyverse,
  brms,
  tidybayes,
  janitor,
  kableExtra,
  modelsummary,
  patchwork,
  ggrepel
)

n_chains <- 4
n_iter <- 2000
n_draws <- 500
n_threads <- 4
n_cores <- 4
seed <- 13


cost_multiplier_linear <- 100
attribute_multiplier <- 10

# run background scripts

source("code/SRD/1CreatePostStratTable.R")
source("code/SRD/2SampleComparison.R")

source("code/SRD/3ModifyChoiceData.R")
source("code/SRD/4EstimateModels.R")
source("code/SRD/5ImputeWTP.R")
source("code/SRD/6GraphWTP.R")
source("code/SRD/7PoeTest.R")
source("code/SRD/8CreateTables.R")
