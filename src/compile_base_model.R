################################################################################
##################### COMPILE BASE MODEL - TZA HBS 2017-2018 ###################
################################################################################

# Author: Mo Osman
# Date created: 11-Nov-2025
# Last edited: 

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "haven")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# Start with food consumption data:
food_consumption <- read_csv("processed_data/tza_hbs1718_food_consumption.csv")

# Get household IDs:
household_roster <- read_dta("raw_data/HBS 2017-18 _Final_Poverty+Individual_Data.dta")

household_ids <- household_roster |> 
  dplyr::select(
    hhid = HHID, 
    interview__id
  ) |> 
  distinct()

rm(household_roster)

# Join household IDs with food consumption data:
food_consumption <- food_consumption |> 
  left_join(household_ids, by = "interview__id")

# Get food consumption table into the correct format:
food_consumption <- food_consumption |> 
  mutate(
    iso3 = "TZA",
    survey = "hbs1718"
  ) |> 
  dplyr::select(iso3, survey, hhid, item_code, quantity_g, quantity_100g) |> 
  filter(!is.na(hhid))

# WRITE DATA: 
write_csv(food_consumption, "processed_data/tza_hbs1718_food_consumption.csv")

rm(list = ls())

# IMPORTANT NOTE: ##########
# AFTER BIS, ADAPT THE 01consumption_quantities.R SCRIPT TO DEAL WITH THE HOUSEHOLD
# ID ISSUE EARLIER IN THE ANALYSIS PIPELINE. THIS WILL ENSURE THAT THE 
# FOOD CONSUMPTION DATA IS IN THE CORRECT FORMAT TO START WITH. *******************

#--------------------------------------------------------------------------------

# SOURCE FUNCTIONS: 
source("src/00functions.R")

#--------------------------------------------------------------------------------

# APPARENT INTAKE ESTIMATES: 
base_ai <- apparent_intake("tza_hbs1718")

#---------------------------------------------------------------------------------

# Summary of energy intake:
summary(base_ai$energy_kcal)

# Still some very extreme outliers - NEED TO BE DEALT WITH - TO UPDATE WHEN UCHE HAS 
# DONE. IN THE INTERIM, FILTER OUT ANYTHING ABOVE 5000 KCAL - JUST TO CHECK THE SHAPE
# OF THE DISTRIBUTION:

filtered_kcal <- base_ai |> 
  filter(energy_kcal < 5000)

# Histogram of energy intake:
hist(filtered_kcal$energy_kcal, 
     main = "Histogram of Energy Intake (kcal)",
     breaks = 20,
     xlab = "Energy Intake (kcal)",
     ylab = "Frequency",
     col = "lightblue")

rm(filtered_kcal)

#---------------------------------------------------------------------------------

# TO COMPLETE WHEN WE HAVE THE DATA WITH OUTLIERS HANDLED.