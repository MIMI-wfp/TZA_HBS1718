# ==============================================================================
# Author: Uchenna AGU
# Contributor: Mo Osman
# Use: Estimate food-item contributions to micronutrient intake among:
#      (1) Total population
#      (2) Households at risk of inadequacy
#      (3) Households not at risk of inadequacy
#
# Data: Tanzania Household Budget Survey (HBS) 2017/18
# Date: July 2026
# ==============================================================================
options(scipen = 99)
# ==============================================================================
# CREATE HOUSEHOLD-LEVEL MICRONUTRIENT INADEQUACY FLAGS
# ==============================================================================
# List of required packages
rq_packages <- c(
  "tidyverse",
  "readr",
  "gt"
)

# Install missing packages
installed_packages <- rq_packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

# Load packages into session
lapply(rq_packages, require, character.only = TRUE)

# Clean workspace
rm(list = c("rq_packages", "installed_packages"))
# Read household nutrient intake estimates
base_ai <- read_csv("processed_data/tza_hbs1718_base_ai.csv")

# Load EAR values and utility functions
source("src/00functions.R")

# Micronutrients to assess
micronutrients <- c(
  "vita_rae_mcg",
  "folate_mcg",
  "vitb12_mcg",
  "fe_mg",
  "zn_mg"
)

# Create binary inadequacy indicators
# 1 = At risk of inadequacy
# 0 = Not at risk of inadequacy
for (nutrient in micronutrients) {
  
  base_ai[[paste0(nutrient, "_inadequate")]] <-
    ifelse(
      base_ai[[nutrient]] <
        allen_ear$ear_value[
          allen_ear$nutrient == nutrient
        ],
      1,
      0
    )
  
}

# Retain only household ID and inadequacy flags
tza_flags <- base_ai %>%
  select(
    hhid,
    ends_with("_inadequate")
  )

# ==============================================================================
# FOOD CONTRIBUTION ANALYSIS
# ==============================================================================

# Calculate food-item contributions to nutrient intake
# Supports:
#   group = "total"      -> all households
#   group = "risk"       -> households at risk of inadequacy
#   group = "not_risk"   -> households not at risk of inadequacy

# ==============================================================================
# VITAMIN A (RAE)
# ==============================================================================

vitA_total <- food_contribution(
  vita_rae_mcg,
  group = "total"
)

vitA_risk <- food_contribution(
  vita_rae_mcg,
  group = "risk"
)

vitA_not_risk <- food_contribution(
  vita_rae_mcg,
  group = "not_risk"
)

# Build a unified table to compare food-item MN contributions between households not at risk of inadequacy and those at risk of inadequacy
vitA_contributions <- contribution_table(vitA_total, vitA_risk, vitA_not_risk, "Vitamin A")
vitA_contributions

# ==============================================================================
# FOLATE
# ==============================================================================

folate_total <- food_contribution(
  folate_mcg,
  group = "total"
)

folate_risk <- food_contribution(
  folate_mcg,
  group = "risk"
)

folate_not_risk <- food_contribution(
  folate_mcg,
  group = "not_risk"
)

folate_contributions <- contribution_table(folate_total, folate_risk, folate_not_risk, "Folate")
folate_contributions

# ==============================================================================
# VITAMIN B12
# ==============================================================================

b12_total <- food_contribution(
  vitb12_mcg,
  group = "total"
)

b12_risk <- food_contribution(
  vitb12_mcg,
  group = "risk"
)

b12_not_risk <- food_contribution(
  vitb12_mcg,
  group = "not_risk"
)

b12_contributions <- contribution_table(b12_total, b12_risk, b12_not_risk, "Vitamin B12")
b12_contributions

# ==============================================================================
# IRON
# ==============================================================================

iron_total <- food_contribution(
  fe_mg,
  group = "total"
)

iron_risk <- food_contribution(
  fe_mg,
  group = "risk"
)

iron_not_risk <- food_contribution(
  fe_mg,
  group = "not_risk"
)

iron_contributions <- contribution_table(iron_total, iron_risk, iron_not_risk, "Iron")
iron_contributions

# ==============================================================================
# ZINC
# ==============================================================================

zinc_total <- food_contribution(
  zn_mg,
  group = "total"
)

zinc_risk <- food_contribution(
  zn_mg,
  group = "risk"
)

zinc_not_risk <- food_contribution(
  zn_mg,
  group = "not_risk"
)

zinc_contributions <- contribution_table(zinc_total, zinc_risk, zinc_not_risk, "Zinc")
zinc_contributions

# ==============================================================================
# OUTPUTS
# ==============================================================================

# Save gt tables as images to file: 
gtsave(vitA_contributions, "figures/food_item_contributions/vitA.png")
gtsave(folate_contributions, "figures/food_item_contributions/folate.png")
gtsave(b12_contributions, "figures/food_item_contributions/b12.png")
gtsave(iron_contributions, "figures/food_item_contributions/iron.png")
gtsave(zinc_contributions, "figures/food_item_contributions/zinc.png")
