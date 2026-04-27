################################################################################
######################## INDICATORS OF INADEQUATE INTAKE #######################
################################################################################

# Author: Mo Osman
# Date created: 25-APR-2026

# SCRIPT FOR CONSTRUCTING INDICATORS OF INADEQUATE MICRONUTRIENT INTAKE: 

# Inadequacy of individual micronutrients: 
# - Using E-AR fixed cut point approach
# - Using full probability approach (iron)

# Composite indicators: 
# - Mean Adequacy Ratio (MAR)

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "ggplot2", "spdep", "sf", "wesanderson",
                 "srvyr", "plotly")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ DATA: 
base_ai <- read_csv("processed_data/tza_hbs1718_base_ai.csv")
hh_information <- read_csv("processed_data/tza_hbs1718_hh_information.csv")

#-------------------------------------------------------------------------------

# GET EAR VALUES AND FUNCTION FOR FULL PROBABILITY METHOD
source("src/00functions.R")

rm(list = setdiff(ls(), c("base_ai", "hh_information", "allen_ear", "plot_map", 
                  "fe_prob_inadequacy")))

#-------------------------------------------------------------------------------

# PART 1 - INADEQACY OF INDIVIDUAL MICRONUTRIENTS: 

# 1a: Compute the household risk of inadequacy for individual micronutrients
#     (Vitamin A, Thiamine, Riboflavin, vitamin B12 & zinc) using fixed cut-point

# Specify list of micronutrients: 
micronutrients <- c("vita_rae_mcg", "thia_mg", "ribo_mg", "vitb12_mcg",
                     "zn_mg")

# Write a function that binarises household risk of micronutrient inadquacy: 
# 1 = Inadequate, 0 = Adequate
for (i in micronutrients) {
  
  ear_value <- allen_ear$ear_value[allen_ear$nutrient == i]
  new_col <- paste0(i, "_inadequate")
  base_ai[[new_col]] <- ifelse(base_ai[[i]] < ear_value, 1, 0)
  
}

rm(ear_value, i, new_col)

# 1b: Compute household probability of inadequate iron intake 
#     (assuming moderate bioavailability 10%)

base_ai <- fe_prob_inadequacy(base_ai, bio_avail = 10)

#-------------------------------------------------------------------------------

# PART 2 - PREVALENCE OF RISK OF INADEQUATE INTAKE - AGGREGATED AT ADM1 LEVEL: 
# (for all micronutrients)

# Join survey weight information and ADM1 information: 
analysis_df <- base_ai |> 
  left_join(hh_information |> 
              dplyr::select(hhid, survey_wgt, adm1), 
            by = "hhid") |>
  as_survey_design(weights = survey_wgt)

# Read in ADM1 shapefile for mapping: 
tanzania_1 <- st_read("shapefiles/tanzania_1") |> 
  dplyr::select(
    adm1 = ADM1_EN, 
    geometry
  ) |> 
  mutate(adm1 = recode(
    adm1, 
    "Dar-es-salaam" = "Dar Es Salaam",
  ))


mn_inadequacy <- analysis_df |> 
  group_by(adm1) |> 
  summarise(vita_inadequacy = survey_mean(vita_rae_mcg_inadequate, na.rm = T, vartype = NULL),
            thia_inadequacy = survey_mean(thia_mg_inadequate, na.rm = T, vartype = NULL),
            ribo_inadequacy = survey_mean(ribo_mg_inadequate, na.rm = T, vartype = NULL),
            vitb12_inadequacy = survey_mean(vitb12_mcg_inadequate, na.rm = T, vartype = NULL),
            fe_inadequacy = survey_mean(fe_prob_inad, na.rm = T, vartype = NULL),
            zn_inadequacy = survey_mean(zn_mg_inadequate, na.rm = T, vartype = NULL)) |> 
  left_join(tanzania_1, by = "adm1")

# Multiply by 100 and round to 1 decimal place:
mn_inadequacy <- mn_inadequacy |> 
  mutate(across(-c(adm1, geometry), ~ .x * 100)) |> 
  mutate(across(-c(adm1, geometry), ~ round(.x, digits = 1)))

# MAP INADEQUACY:
 micronutrients <- c("vita_inadequacy", "thia_inadequacy", "ribo_inadequacy",
                    "vitb12_inadequacy", "fe_inadequacy", "zn_inadequacy")

mn_inadequacy <- st_as_sf(mn_inadequacy)

for (i in micronutrients) {

  p <- plot_map(
    data = mn_inadequacy,
    col = i,
    title = i,
    metric = "Risk of inadequate intake (%)",
    outline_sf = tanzania_1
  )

  print(p)

  
}

#-------------------------------------------------------------------------------

# PART 3: MEAN ADEQUACY RATIO (MAR):
# at the ADM1 level, 4 micronutrients (Vitamin A, Vitamin B12, Iron, Zinc)


mn_mar <- base_ai |>
  left_join(hh_information |> 
              dplyr::select(hhid, survey_wgt, adm1), 
            by = "hhid") |>
  # Nutrient adequacy ratio (NAR) for each micronutrient:
  mutate(va_nar = vita_rae_mcg /allen_ear[allen_ear$nutrient == "vita_rae_mcg", "ear_value"],
         vb12_nar = vitb12_mcg /allen_ear[allen_ear$nutrient == "vitb12_mcg", "ear_value"],
         fe_nar = fe_mg /allen_ear[allen_ear$nutrient == "fe_mg", "ear_value"],
         zn_nar = zn_mg /allen_ear[allen_ear$nutrient == "zn_mg", "ear_value"]) |> 
  # Truncate NAR values to 1:
  mutate(across(c(va_nar:zn_nar), ~ ifelse(. > 1, 1, .))) |>
  # Average values to obtain MAR:
  mutate(mar = rowMeans(across(c(va_nar:zn_nar)), na.rm = TRUE))

# Binarise MAR (inadequate if MAR < 0.75): 
mn_mar <- mn_mar |> 
  mutate(mar_inadequate = ifelse(mar < 0.75, 1, 0)) |> 
  dplyr::select(iso3, survey, hhid, adm1, survey_wgt, mar_inadequate)

# Compute survey weigthed average MAR for each ADM1 area:
mar_analysis_df <- mn_mar |> 
  as_survey_design(weights = survey_wgt)

mar_inadequacy <- mar_analysis_df |> 
  group_by(adm1) |> 
  summarise(mar = survey_mean(mar_inadequate, na.rm = T, vartype = NULL)) |> 
  left_join(tanzania_1, by = "adm1")

# Multiply by 100 and round to 1 decimal place:
mar_inadequacy <- mar_inadequacy |> 
  mutate(mar = mar * 100) |> 
  mutate(mar = round(mar, digits = 1))

# MAP MAR INADEQUACY:
mar_inadequacy <- st_as_sf(mar_inadequacy)

p_mar <- plot_map(
  data = mar_inadequacy,
  col = "mar",
  title = "MAR (Vitamin A, B12, Iron, Zinc)",
  metric = "Risk of inadequate intake (%)",
  outline_sf = tanzania_1
)

p_mar


#-------------------------------------------------------------------------------

# Clear environment:
rm(list = ls())

################################################################################
################################# END OF SCRIPT ################################
################################################################################