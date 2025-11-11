library(readr)
library(tidyverse)
library(readxl)

food_groups <- read_csv("processed_data/tza_hbs1718_detailed_food_items_mddw.csv")

table(food_groups$food_group)

food_groups <- food_groups |> 
  mutate(food_group = dplyr::recode(
    food_group,
    "Dairy" = "dairy",
    "Eggs" = "eggs",
    "Meat, poultry, and fish" = "meat_poultry_fish",
    "Nuts and seeds" = "nuts_seeds",
    "Other Fruits" = "other_fruit",
    "Other Vitamin A-rich fruits and vegetables" = "vita_fruit_veg",
    "Dark leafy greens and vegetables" = "green_leafy_veg",
    "Grains, roots, and tubers" = "grains_roots_tubers",
    "Misc" = "misc",
    "Oils and fats" = "oils_fats",
    "Other vegetables" = "other_veg",
    "Pulses" = "pulses",
  ))

# Write data: 
# write_csv(food_groups, "processed_data/tza_hbs1718_food_groups.csv")

##########################################################################

# FCTs: 
fc_table <- read_excel(
  "processed_data/COUNTRY_NCT_FD_domain_Tanzania_2017.xlsx", 
  sheet = "survey NCT"
)

# Select and rename variables: 
fc_table <- fc_table |> 
  dplyr::select(
    item_code = item_cod,
    energy_kcal = fd_kcal,
    vita_rae_mcg = vita_RAE,
    thia_mg = vit_b1,
    ribo_mg = vit_b2,
    vitb6_mg = vit_b6,
    vitb12_mcg = vit_b12,
    fe_mg = iron,
    zn_mg = zinc,
    ca_mg = calcium,
    protein_g = fd_pro,
    fat_g = fd_fat,
    vitc_mg = vit_c
  )

# Get food item names:
item_names <- food_groups |> 
  dplyr::select(item_code, item_name)

# Join to FCT: 
fc_table <- fc_table |> 
  left_join(item_names, by = "item_code") |> 
  filter(!is.na(item_name))

# Add other variables and specify order: 
fc_table <- fc_table |> 
  mutate(
    iso3 = "TZA",
    survey = "hbs_1718"
  ) |> 
  dplyr::select(iso3, survey, item_code, item_name, everything())

# write_csv(fc_table, "processed_data/tza_hbs1718_fct.csv")

rm(list = ls())

#-------------------------------------------------------------------------------

# Correction of food_consumption data:
food_consumption <- read_csv("processed_data/tza_hbs1718_food_consumption.csv")

food_consumption <- food_consumption |> 
  mutate(
    iso3 = "TZA",
    survey = "hbs1718"
  ) |> 
  dplyr::select(iso3, survey, everything())

# write_csv(food_consumption, "processed_data/tza_hbs1718_food_consumption.csv")

rm(list = ls())
