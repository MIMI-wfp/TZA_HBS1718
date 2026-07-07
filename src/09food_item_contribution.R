# ==============================================================================
# Author: Uchenna AGU
# Use: Estimate food-item contributions to micronutrient intake among:
#      (1) Total population
#      (2) Households at risk of inadequacy
#      (3) Households not at risk of inadequacy
#
# Data: Tanzania Household Budget Survey (HBS) 2017/18
# Date: July 2026
# ==============================================================================

# ==============================================================================
# CREATE HOUSEHOLD-LEVEL MICRONUTRIENT INADEQUACY FLAGS
# ==============================================================================
# List of required packages
rq_packages <- c(
  "tidyverse")

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

# ==============================================================================
# OUTPUTS
# ==============================================================================
#
# For each micronutrient, three datasets are generated:
#
# *_total
#     Top contributing foods among all households
#
# *_risk
#     Top contributing foods among households whose intake is
#     below the EAR (at risk of inadequacy)
#
# *_not_risk
#     Top contributing foods among households whose intake meets
#     or exceeds the EAR (not at risk of inadequacy)
#
# Each output contains:
#     item_code
#     item_name
#     contribution_pct
#
# contribution_pct =
#     Percentage contribution of each food item to total
#     micronutrient intake within the specified population group.
#
# ==============================================================================