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

rq_packages <- c("readr", "tidyverse", "srvyr", "ggplot2", "ggthemes")

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

# NATIONAL ESTIMATES OF INADEQUATE MICRONUTRIENT INTAKE:

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

#--------------------------------------------------------------------------------

# ESTIMATES OF INADEQUATE MICRONUTRIENT INTAKE BY URBAN/RURAL RESIDENCE:

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

# Also stratified by wealth quintile:
res_sep_estimates <- analysis_df.svy |>
  group_by(res, res_quintile) |>
  summarise(vita_inadequacy = survey_mean(vita_rae_mcg_inadequate, na.rm = T, vartype = NULL),
            thia_inadequacy = survey_mean(thia_mg_inadequate, na.rm = T, vartype = NULL),
            ribo_inadequacy = survey_mean(ribo_mg_inadequate, na.rm = T, vartype = NULL),
            vitb12_inadequacy = survey_mean(vitb12_mcg_inadequate, na.rm = T, vartype = NULL),
            zn_inadequacy = survey_mean(zn_mg_inadequate, na.rm = T, vartype = NULL),
            ca_inadequacy = survey_mean(ca_mg_inadequate, na.rm = T, vartype = NULL)) |>
  pivot_longer(cols = -c(res, res_quintile), names_to = "micronutrient", values_to = "estimate") |> 
  mutate(estimate = estimate * 100) |> 
  filter(!is.na(res_quintile))

#--------------------------------------------------------------------------------

# DATA VISUALISATION EXAMPLE - VITAMIN B12 INADEQUACY:
vitb12_stratified <- res_sep_estimates |>
  filter(micronutrient == "vitb12_inadequacy")

# Prepare wide-format data for vitamin B12 (Urban vs Rural) by wealth quintile
vitb12_df <- res_sep_estimates |>
  filter(micronutrient == "vitb12_inadequacy", res %in% c("Urban", "Rural")) |>
  mutate(res_quintile = case_when(
    res_quintile == 1 ~ "1 - Poorest",
    res_quintile == 5 ~ "5 - Wealthiest",
    TRUE ~ as.character(res_quintile)
  ),
  res_quintile = factor(res_quintile, levels = c("1 - Poorest", "2", "3", "4", "5 - Wealthiest"))
  ) |>
  select(res, res_quintile, estimate) |>
  pivot_wider(names_from = res, values_from = estimate) |>
  tidyr::drop_na(Rural, Urban) |> 
  mutate(diff = abs(Urban - Rural))

# Dumbbell plot for vitamin B12: Urban vs Rural by wealth quintile
p_vitB12_dumbbell <- ggplot(vitb12_df) +
  geom_segment(aes(x = Rural, xend = Urban, y = res_quintile, yend = res_quintile),
                        color = "gray80", size = 4.5, alpha = 0.7) +
  geom_text(aes(label = paste("Δ", round(diff, 1), "%"), x = (Rural + Urban) / 2, y = res_quintile),
            color = "#4a4e4d", fill = "white", family = "Segoe UI Semibold", size = 3) +
  geom_point(aes(x = Rural, y = res_quintile, color = "Rural"), size = 4) +
  geom_point(aes(x = Urban, y = res_quintile, color = "Urban"), size = 4) +
  scale_color_manual(name = "Residence", values = c("Rural" = "#762a83", "Urban" = "#009688")) +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(x = "Percentage of households at risk of inadequate intake (%)", y = "Wealth quintile",
                title = "Vitamin B12 - risk of inadequate intake",
                caption = "Source: Tanzania Household Budget Survey (HBS) 2017-18") +
  theme(panel.grid.minor = element_blank(),
                 legend.position = "right") 

p_vitB12_dumbbell
#--------------------------------------------------------------------------------

# ADM1 (REGION) LEVEL ESTIMATES OF INADEQUATE MICRONUTRIENT INTAKE:

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