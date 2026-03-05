################################################################################
################### MIMI CAPACITY BUILDING - SURVEY ANALAYSIS ##################
################################################################################

# Author: Mo Osman
# Date created: 02-Mar-2026
# Last edited: 

# In this script, we will demonstrate survey analysis in R using the Tanzania
# Household Budget Survey (HBS) data. We will demonstrate how weighted estimates
# of inadequate micronutrient intake can be computed, and how estimates can be 
# stratified to identify vulnerable sub-populations. 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "srvyr", "ggplot2")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ IN DATA:
base_ai <- read_csv("processed_data/tza_hbs1718_base_ai.csv")
hh_information <- read_csv("processed_data/tza_hbs1718_hh_information.csv")



#-------------------------------------------------------------------------------

# GET ESTIMATED AVERAGE REQUIREMENT (EAR) VALUES:
source("src/00functions.R")

rm(list = setdiff(ls(), c("base_ai", "hh_information", "allen_ear", "fe_full_prob")))

#-------------------------------------------------------------------------------

# BINARISE RISK OF INADEQUATE MICRONUTRIENT INTAKE:

# Specify list of micronutrients: 
micronutrients <- c("vita_rae_mcg", "thia_mg", "ribo_mg", "vitb12_mcg",
                    "zn_mg", "ca_mg")

for (i in micronutrients) {
  
  ear_value <- allen_ear$ear_value[allen_ear$nutrient == i]
  new_col <- paste0(i, "_inadequate")
  base_ai[[new_col]] <- ifelse(base_ai[[i]] < ear_value, 1, 0)
  
}

rm(ear_value, i, new_col)

# NOTE THAT WE HAVE NOT BINARISED IRON INADEQUACY - THIS WILL BE COVERED IN A 
# FUTURE SESSION.

#-------------------------------------------------------------------------------

# JOIN WITH HOUSEHOLD INFORMATION DATA: 
analysis_df <- base_ai |> 
  dplyr::select(hhid, ends_with("_inadequate")) |>
  left_join(hh_information, by = "hhid")

#-------------------------------------------------------------------------------

# CREATE SURVEY DESIGN OBJECT:

# Create a variable for analytical strata (combination of region and urban/rural residence):
analysis_df <- analysis_df |>
  mutate(stratum = interaction(adm1, res, drop = TRUE))

analysis_df.svy <- analysis_df |> 
  as_survey_design(weights = survey_wgt, # Survey weights provided by NBS
                  #  ids = EA, # EA variable is missing from available data
                   strata = stratum)

# Note that cluster ID variable (EA) was not provided - can this be obtained from NBS?

#-------------------------------------------------------------------------------

# Calculate national estimates of inadequate micronutrient intake:
national_estimates <- analysis_df.svy |>
  summarise(vita_inadequacy = survey_mean(vita_rae_mcg_inadequate, na.rm = T, vartype = NULL),
            thia_inadequacy = survey_mean(thia_mg_inadequate, na.rm = T, vartype = NULL),
            ribo_inadequacy = survey_mean(ribo_mg_inadequate, na.rm = T, vartype = NULL),
            vitb12_inadequacy = survey_mean(vitb12_mcg_inadequate, na.rm = T, vartype = NULL),
            zn_inadequacy = survey_mean(zn_mg_inadequate, na.rm = T, vartype = NULL),
            ca_inadequacy = survey_mean(ca_mg_inadequate, na.rm = T, vartype = NULL)) |>
  pivot_longer(cols = everything(), names_to = "micronutrient", values_to = "estimate") |> 
  mutate(estimate = estimate * 100)

# Calculate estimates of inadequate micronutrient intake by urban/rural residence:
residence_estimates <- analysis_df.svy |>
  group_by(res) |>
  summarise(vita_inadequacy = survey_mean(vita_rae_mcg_inadequate, na.rm = T, vartype = NULL),
            thia_inadequacy = survey_mean(thia_mg_inadequate, na.rm = T, vartype = NULL),
            ribo_inadequacy = survey_mean(ribo_mg_inadequate, na.rm = T, vartype = NULL),
            vitb12_inadequacy = survey_mean(vitb12_mcg_inadequate, na.rm = T, vartype = NULL),
            zn_inadequacy = survey_mean(zn_mg_inadequate, na.rm = T, vartype = NULL),
            ca_inadequacy = survey_mean(ca_mg_inadequate, na.rm = T, vartype = NULL)) |>
  pivot_longer(cols = -res, names_to = "micronutrient", values_to = "estimate") |> 
  mutate(estimate = estimate * 100)

# Calculate estimates of inadequate micronutrient intake by adm1: 
adm1_estimates <- analysis_df.svy |>
  group_by(adm1) |>
  summarise(vita_inadequacy = survey_mean(vita_rae_mcg_inadequate, na.rm = T, vartype = NULL),
            thia_inadequacy = survey_mean(thia_mg_inadequate, na.rm = T, vartype = NULL),
            ribo_inadequacy = survey_mean(ribo_mg_inadequate, na.rm = T, vartype = NULL),
            vitb12_inadequacy = survey_mean(vitb12_mcg_inadequate, na.rm = T, vartype = NULL),
            zn_inadequacy = survey_mean(zn_mg_inadequate, na.rm = T, vartype = NULL),
            ca_inadequacy = survey_mean(ca_mg_inadequate, na.rm = T, vartype = NULL)) |>
  mutate(estimate = estimate * 100)

#--------------------------------------------------------------------------------

# Save ADM1 estimates for use in GIS session: 
write_csv(adm1_estimates, "processed_data/adm1_MN_estimates.csv")

#################################################################################
################################## END OF SCRIPT ################################
#################################################################################