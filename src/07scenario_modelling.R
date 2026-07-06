# Author: Uchenna Agu and Mo Osman
# Date created: May 2026
# Last edited: July 2026
#
# Purpose: Fortification scenario modelling

# ==============================================================================
# INSTALL AND LOAD REQUIRED PACKAGES
# ==============================================================================

# List of required packages
rq_packages <- c(
  "readr", "tidyverse", "ggplot2", "spdep",
  "sf", "wesanderson", "srvyr", "plotly"
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


# ==============================================================================
# SOURCE CUSTOM FUNCTIONS
# ==============================================================================

# Load user-defined functions used throughout the analysis
source("src/00functions.R")


# ==============================================================================
# DEFINE FOOD FORTIFICATION VEHICLES AND FORTIFICATION STANDARDS
# ==============================================================================

# ------------------------------------------------------------------------------
# MAIZE FLOUR PRODUCTS
# ------------------------------------------------------------------------------

# Food items assumed to be industrially fortified maize products
maize_products <- tibble(
  item_code = c(
    111504  # Maize Flour White - Loose
  ),
  fraction = c(
    1       # Assumed 100% maize flour content
  )
)

# Tanzanian maize flour fortification standard
# Units:
#   Vitamin B12 (mcg/kg)
#   Iron (mg/kg)
#   Zinc (mg/kg)
#   Folate (mcg/kg)
maize_standard <- list(
  vitb12_mcg = 0.5,
  fe_mg      = 1.5,
  zn_mg      = 2.25,
  folate_mcg = 150
)


# ------------------------------------------------------------------------------
# WHEAT FLOUR PRODUCTS
# ------------------------------------------------------------------------------

# Wheat flour and wheat-derived products
# Fractions represent assumed wheat flour contribution
wheat_products <- tibble(
  item_code = c(
    111506, # Wheat Flour
    111201, # Flat bread
    111202, # Loaf of white bread
    111203, # Round bread
    111204, # Sliced bread
    111205, # Sliced brown bread
    111206, # Sweet bread
    111207, # Traditional bread
    111299, # Other bread varieties
    111402, # Wheat buns / scones
    111401, # Cake / half cake
    111405, # Kalimati / Pie / Sambusa
    111208  # Biscuits
  ),
  fraction = c(
    1.000,
    0.750,
    0.750,
    0.750,
    0.750,
    0.750,
    0.750,
    0.750,
    0.750,
    0.330,
    0.635,
    0.277,
    0.630
  )
)

# Tanzanian wheat flour fortification standard
wheat_standard <- list(
  vitb12_mcg = 1.5,
  fe_mg      = 4,
  zn_mg      = 4,
  folate_mcg = 300
)


# ------------------------------------------------------------------------------
# EDIBLE OILS
# ------------------------------------------------------------------------------

# Oils assumed eligible for vitamin A fortification
oil_products <- tibble(
  item_code = c(
    115101, # Butter oil
    115301, # Olive oil
    115401, # Sunflower oil
    115402, # Cottonseed oil
    115403, # Groundnut oil
    115404, # Sesame oil
    115405  # Coconut cooking oil
  )
)

# Vitamin A fortification standard
# Units: mcg RAE/kg
oil_standard <- list(
  vita_rae_mcg = 3000
)


# ==============================================================================
# LOAD PROCESSED DATA
# ==============================================================================

# Create baseline fortification-adjusted nutrient intake dataset
fortification_ai <- fortification_scenario("tza_hbs1718")

# Household-level information including weights and geography
hh_information <- read_csv(
  "processed_data/tza_hbs1718_hh_information.csv"
)


# ==============================================================================
# LOAD AND PREPARE ADMINISTRATIVE BOUNDARIES
# ==============================================================================

# ADM1 shapefile used for regional mapping
tanzania_1 <- st_read(
  "shapefiles/tza_admbnda_adm1_20181019.shp"
) |>
  dplyr::select(
    adm1 = ADM1_EN,
    geometry
  ) |>
  # Exclude Zanzibar regions from mainland analysis
  filter(!str_detect(adm1, "Unguja|Pemba|Mjini")) |>
  # Harmonize naming with survey dataset
  mutate(
    adm1 = recode(
      adm1,
      "Dar-es-salaam" = "Dar Es Salaam"
    )
  )


# ==============================================================================
# CREATE BINARY INDICATORS OF NUTRIENT INADEQUACY
# ==============================================================================

# Nutrients evaluated against Allen EAR values
micronutrients <- c(
  "vita_rae_mcg",
  "folate_mcg",
  "vitb12_mcg",
  "fe_mg",
  "zn_mg"
)

# Create indicator:
# 1 = intake below EAR
# 0 = intake at or above EAR
for (i in micronutrients) {
  
  ear_value <- allen_ear$ear_value[
    allen_ear$nutrient == i
  ]
  
  new_col <- paste0(i, "_inadequate")
  
  fortification_ai[[new_col]] <-
    ifelse(
      fortification_ai[[i]] < ear_value,
      1,
      0
    )
}

rm(ear_value, i, new_col)


# ==============================================================================
# ESTIMATE PREVALENCE OF INADEQUATE INTAKE BY REGION
# ==============================================================================

# Merge household weights and regional identifiers
analysis_df <- fortification_ai |>
  left_join(
    hh_information |>
      dplyr::select(
        hhid,
        survey_wgt,
        adm1
      ),
    by = "hhid"
  ) |>
  as_survey_design(weights = survey_wgt)

# Weighted prevalence of inadequacy
mn_inadequacy <- analysis_df |>
  group_by(adm1) |>
  summarise(
    vita_inadequacy =
      survey_mean(vita_rae_mcg_inadequate,
                  na.rm = TRUE,
                  vartype = NULL),
    
    folate_inadequacy =
      survey_mean(folate_mcg_inadequate,
                  na.rm = TRUE,
                  vartype = NULL),
    
    vitb12_inadequacy =
      survey_mean(vitb12_mcg_inadequate,
                  na.rm = TRUE,
                  vartype = NULL),
    
    zn_inadequacy =
      survey_mean(zn_mg_inadequate,
                  na.rm = TRUE,
                  vartype = NULL)
  ) |>
  left_join(tanzania_1, by = "adm1")

# Convert proportions to percentages
mn_inadequacy <- mn_inadequacy |>
  mutate(across(-c(adm1, geometry), ~ .x * 100)) |>
  mutate(across(-c(adm1, geometry), ~ round(.x, 1)))


# ==============================================================================
# ESTIMATE IRON INADEQUACY USING FULL PROBABILITY APPROACH
# ==============================================================================

# Iron inadequacy cannot be estimated using simple EAR cut-point methods
# Therefore the full probability method is used assuming:
#   - 10% iron bioavailability

tza_fortification_ai_fe <- fortification_ai |>
  left_join(hh_information, by = "hhid") |>
  select(
    hhid,
    adm1,
    adm2,
    survey_wgt,
    fe_mg
  )

tza_fe_inadequacy_adm1 <- fe_full_prob(
  tza_fortification_ai_fe,
  group1 = adm1,
  bio_avail = 10,
  hh_weight = "hh_weight"
) %>%
  rename(fe_inadequacy = fe_mg_prop)

mn_inadequacy <- mn_inadequacy |>
  left_join(
    tza_fe_inadequacy_adm1,
    by = c("adm1" = "subpopulation")
  )


# ==============================================================================
# CALCULATE MEAN ADEQUACY RATIO (MAR)
# ==============================================================================

# MAR is calculated as the average of truncated nutrient adequacy ratios

mn_mar <- hh_information |>
  select(
    hhid,
    adm1,
    survey_wgt,
    res,
    sep_quintile
  ) |>
  left_join(
    fortification_ai,
    by = "hhid"
  ) |>
  
  # Nutrient Adequacy Ratios (NAR)
  mutate(
    va_nar =
      vita_rae_mcg /
      allen_ear[allen_ear$nutrient ==
                  "vita_rae_mcg", "ear_value"],
    
    vb12_nar =
      vitb12_mcg /
      allen_ear[allen_ear$nutrient ==
                  "vitb12_mcg", "ear_value"],
    
    fol_nar =
      folate_mcg /
      allen_ear[allen_ear$nutrient ==
                  "folate_mcg", "ear_value"],
    
    fe_nar =
      fe_mg /
      allen_ear[allen_ear$nutrient ==
                  "fe_mg", "ear_value"],
    
    zn_nar =
      zn_mg /
      allen_ear[allen_ear$nutrient ==
                  "zn_mg", "ear_value"]
  ) |>
  
  # Truncate NAR values at 1
  mutate(
    across(
      c(va_nar:zn_nar),
      ~ ifelse(. > 1, 1, .)
    )
  ) |>
  
  # Calculate MAR
  mutate(
    mar = rowMeans(
      across(c(va_nar:zn_nar)),
      na.rm = TRUE
    )
  )


# Define inadequate diet quality as MAR < 0.75
mn_mar <- mn_mar |>
  mutate(
    mar_inadequate =
      ifelse(mar < 0.75, 1, 0)
  ) |>
  select(
    hhid,
    res,
    sep_quintile,
    adm1,
    survey_wgt,
    mar_inadequate
  )


# Estimate prevalence of MAR inadequacy
mar_analysis_df <- mn_mar |>
  as_survey_design(weights = survey_wgt)

mar_inadequacy <- mar_analysis_df |>
  group_by(adm1) |>
  summarise(
    mar =
      survey_mean(
        mar_inadequate,
        na.rm = TRUE,
        vartype = NULL
      )
  ) |>
  left_join(tanzania_1, by = "adm1")

# Convert to percentages
mar_inadequacy <- mar_inadequacy |>
  mutate(
    mar = round(mar * 100, 1)
  )


# ==============================================================================
# PRODUCE CHOROPLETH MAPS
# ==============================================================================

# Indicators to map
micronutrients <- c(
  "vita_inadequacy",
  "folate_inadequacy",
  "vitb12_inadequacy",
  "fe_inadequacy",
  "zn_inadequacy"
)

mn_inadequacy <- st_as_sf(mn_inadequacy)

# Generate and save ADM1-level maps
for (i in micronutrients) {
  
  p <- plot_map(
    data = mn_inadequacy,
    col = i,
    title = "",
    metric = "Risk of inadequate intake (%)",
    outline_sf = tanzania_1
  )
  
  print(p)
  
  ggsave(
    filename = paste0(
      "figures/fortification_maps/",
      i,
      "_map.png"
    ),
    plot = p,
    width = 8,
    height = 6,
    dpi = 300
  )
}


################################################################################
################################# END OF SCRIPT ################################
=======
################################################################################
##################### COMPILE BASE MODEL - TZA HBS 2017–2018 ###################
################################################################################

# Author: Uchenna Agu and Mo Osman
# Date created: May 2026
# Last edited: July 2026
#
# Purpose: Fortification scenario modelling

# ==============================================================================
# INSTALL AND LOAD REQUIRED PACKAGES
# ==============================================================================

# List of required packages
rq_packages <- c(
  "readr", "tidyverse", "ggplot2", "spdep",
  "sf", "wesanderson", "srvyr", "plotly"
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


# ==============================================================================
# SOURCE CUSTOM FUNCTIONS
# ==============================================================================

# Load user-defined functions used throughout the analysis
source("src/00functions.R")


# ==============================================================================
# DEFINE FOOD FORTIFICATION VEHICLES AND FORTIFICATION STANDARDS
# ==============================================================================

# ------------------------------------------------------------------------------
# MAIZE FLOUR PRODUCTS
# ------------------------------------------------------------------------------

# Food items assumed to be industrially fortified maize products
maize_products <- tibble(
  item_code = c(
    111504  # Maize Flour White - Loose
  ),
  fraction = c(
    1       # Assumed 100% maize flour content
  )
)

# Tanzanian maize flour fortification standard
# Units:
#   Vitamin B12 (mcg/kg)
#   Iron (mg/kg)
#   Zinc (mg/kg)
#   Folate (mcg/kg)
maize_standard <- list(
  vitb12_mcg = 0.5,
  fe_mg      = 1.5,
  zn_mg      = 2.25,
  folate_mcg = 150
)


# ------------------------------------------------------------------------------
# WHEAT FLOUR PRODUCTS
# ------------------------------------------------------------------------------

# Wheat flour and wheat-derived products
# Fractions represent assumed wheat flour contribution
wheat_products <- tibble(
  item_code = c(
    111506, # Wheat Flour
    111201, # Flat bread
    111202, # Loaf of white bread
    111203, # Round bread
    111204, # Sliced bread
    111205, # Sliced brown bread
    111206, # Sweet bread
    111207, # Traditional bread
    111299, # Other bread varieties
    111402, # Wheat buns / scones
    111401, # Cake / half cake
    111405, # Kalimati / Pie / Sambusa
    111208  # Biscuits
  ),
  fraction = c(
    1.000,
    0.750,
    0.750,
    0.750,
    0.750,
    0.750,
    0.750,
    0.750,
    0.750,
    0.330,
    0.635,
    0.277,
    0.630
  )
)

# Tanzanian wheat flour fortification standard
wheat_standard <- list(
  vitb12_mcg = 1.5,
  fe_mg      = 4,
  zn_mg      = 4,
  folate_mcg = 300
)


# ------------------------------------------------------------------------------
# EDIBLE OILS
# ------------------------------------------------------------------------------

# Oils assumed eligible for vitamin A fortification
oil_products <- tibble(
  item_code = c(
    115101, # Butter oil
    115301, # Olive oil
    115401, # Sunflower oil
    115402, # Cottonseed oil
    115403, # Groundnut oil
    115404, # Sesame oil
    115405  # Coconut cooking oil
  )
)

# Vitamin A fortification standard
# Units: mcg RAE/kg
oil_standard <- list(
  vita_rae_mcg = 3000
)


# ==============================================================================
# LOAD PROCESSED DATA
# ==============================================================================

# Create baseline fortification-adjusted nutrient intake dataset
fortification_ai <- fortification_scenario("tza_hbs1718")

# Household-level information including weights and geography
hh_information <- read_csv(
  "processed_data/tza_hbs1718_hh_information.csv"
)


# ==============================================================================
# LOAD AND PREPARE ADMINISTRATIVE BOUNDARIES
# ==============================================================================

# ADM1 shapefile used for regional mapping
tanzania_1 <- st_read(
  "shapefiles/tza_admbnda_adm1_20181019.shp"
) |>
  dplyr::select(
    adm1 = ADM1_EN,
    geometry
  ) |>
  # Exclude Zanzibar regions from mainland analysis
  filter(!str_detect(adm1, "Unguja|Pemba|Mjini")) |>
  # Harmonize naming with survey dataset
  mutate(
    adm1 = recode(
      adm1,
      "Dar-es-salaam" = "Dar Es Salaam"
    )
  )


# ==============================================================================
# CREATE BINARY INDICATORS OF NUTRIENT INADEQUACY
# ==============================================================================

# Nutrients evaluated against Allen EAR values
micronutrients <- c(
  "vita_rae_mcg",
  "folate_mcg",
  "vitb12_mcg",
  "fe_mg",
  "zn_mg"
)

# Create indicator:
# 1 = intake below EAR
# 0 = intake at or above EAR
for (i in micronutrients) {
  
  ear_value <- allen_ear$ear_value[
    allen_ear$nutrient == i
  ]
  
  new_col <- paste0(i, "_inadequate")
  
  fortification_ai[[new_col]] <-
    ifelse(
      fortification_ai[[i]] < ear_value,
      1,
      0
    )
}

rm(ear_value, i, new_col)


# ==============================================================================
# ESTIMATE PREVALENCE OF INADEQUATE INTAKE BY REGION
# ==============================================================================

# Merge household weights and regional identifiers
analysis_df <- fortification_ai |>
  left_join(
    hh_information |>
      dplyr::select(
        hhid,
        survey_wgt,
        adm1
      ),
    by = "hhid"
  ) |>
  as_survey_design(weights = survey_wgt)

# Weighted prevalence of inadequacy
mn_inadequacy <- analysis_df |>
  group_by(adm1) |>
  summarise(
    vita_inadequacy =
      survey_mean(vita_rae_mcg_inadequate,
                  na.rm = TRUE,
                  vartype = NULL),
    
    folate_inadequacy =
      survey_mean(folate_mcg_inadequate,
                  na.rm = TRUE,
                  vartype = NULL),
    
    vitb12_inadequacy =
      survey_mean(vitb12_mcg_inadequate,
                  na.rm = TRUE,
                  vartype = NULL),
    
    zn_inadequacy =
      survey_mean(zn_mg_inadequate,
                  na.rm = TRUE,
                  vartype = NULL)
  ) |>
  left_join(tanzania_1, by = "adm1")

# Convert proportions to percentages
mn_inadequacy <- mn_inadequacy |>
  mutate(across(-c(adm1, geometry), ~ .x * 100)) |>
  mutate(across(-c(adm1, geometry), ~ round(.x, 1)))


# ==============================================================================
# ESTIMATE IRON INADEQUACY USING FULL PROBABILITY APPROACH
# ==============================================================================

# Iron inadequacy cannot be estimated using simple EAR cut-point methods
# Therefore the full probability method is used assuming:
#   - 10% iron bioavailability

tza_fortification_ai_fe <- fortification_ai |>
  left_join(hh_information, by = "hhid") |>
  select(
    hhid,
    adm1,
    adm2,
    survey_wgt,
    fe_mg
  )

tza_fe_inadequacy_adm1 <- fe_full_prob(
  tza_fortification_ai_fe,
  group1 = adm1,
  bio_avail = 10,
  hh_weight = "hh_weight"
) %>%
  rename(fe_inadequacy = fe_mg_prop)

mn_inadequacy <- mn_inadequacy |>
  left_join(
    tza_fe_inadequacy_adm1,
    by = c("adm1" = "subpopulation")
  )


# ==============================================================================
# CALCULATE MEAN ADEQUACY RATIO (MAR)
# ==============================================================================

# MAR is calculated as the average of truncated nutrient adequacy ratios

mn_mar <- hh_information |>
  select(
    hhid,
    adm1,
    survey_wgt,
    res,
    sep_quintile
  ) |>
  left_join(
    fortification_ai,
    by = "hhid"
  ) |>
  
  # Nutrient Adequacy Ratios (NAR)
  mutate(
    va_nar =
      vita_rae_mcg /
      allen_ear[allen_ear$nutrient ==
                  "vita_rae_mcg", "ear_value"],
    
    vb12_nar =
      vitb12_mcg /
      allen_ear[allen_ear$nutrient ==
                  "vitb12_mcg", "ear_value"],
    
    fol_nar =
      folate_mcg /
      allen_ear[allen_ear$nutrient ==
                  "folate_mcg", "ear_value"],
    
    fe_nar =
      fe_mg /
      allen_ear[allen_ear$nutrient ==
                  "fe_mg", "ear_value"],
    
    zn_nar =
      zn_mg /
      allen_ear[allen_ear$nutrient ==
                  "zn_mg", "ear_value"]
  ) |>
  
  # Truncate NAR values at 1
  mutate(
    across(
      c(va_nar:zn_nar),
      ~ ifelse(. > 1, 1, .)
    )
  ) |>
  
  # Calculate MAR
  mutate(
    mar = rowMeans(
      across(c(va_nar:zn_nar)),
      na.rm = TRUE
    )
  )


# Define inadequate diet quality as MAR < 0.75
mn_mar <- mn_mar |>
  mutate(
    mar_inadequate =
      ifelse(mar < 0.75, 1, 0)
  ) |>
  select(
    hhid,
    res,
    sep_quintile,
    adm1,
    survey_wgt,
    mar_inadequate
  )


# Estimate prevalence of MAR inadequacy
mar_analysis_df <- mn_mar |>
  as_survey_design(weights = survey_wgt)

mar_inadequacy <- mar_analysis_df |>
  group_by(adm1) |>
  summarise(
    mar =
      survey_mean(
        mar_inadequate,
        na.rm = TRUE,
        vartype = NULL
      )
  ) |>
  left_join(tanzania_1, by = "adm1")

# Convert to percentages
mar_inadequacy <- mar_inadequacy |>
  mutate(
    mar = round(mar * 100, 1)
  )


# ==============================================================================
# PRODUCE CHOROPLETH MAPS
# ==============================================================================

# Indicators to map
micronutrients <- c(
  "vita_inadequacy",
  "folate_inadequacy",
  "vitb12_inadequacy",
  "fe_inadequacy",
  "zn_inadequacy"
)

mn_inadequacy <- st_as_sf(mn_inadequacy)

# Generate and save ADM1-level maps
for (i in micronutrients) {
  
  p <- plot_map(
    data = mn_inadequacy,
    col = i,
    title = "",
    metric = "Risk of inadequate intake (%)",
    outline_sf = tanzania_1
  )
  
  print(p)
  
  ggsave(
    filename = paste0(
      "figures/fortification_maps/",
      i,
      "_map.png"
    ),
    plot = p,
    width = 8,
    height = 6,
    dpi = 300
  )
}


################################################################################
################################# END OF SCRIPT ################################
>>>>>>> a16e49bbf73a693f134c34aa8091960821b085d1
################################################################################