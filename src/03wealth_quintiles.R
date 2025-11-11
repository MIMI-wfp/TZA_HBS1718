################################################################################
############### WEALTH QUINTILE CALCULATIONS - TZA HBS 2017-2018 ###############
################################################################################

# Author: Mo Osman
# Date created: 07-Nov-2025
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

# READ IN DATA: 
household_information <- read_csv("processed_data/tza_hbs1718_hh_information.csv")
household_roster <- read_dta("raw_data/HBS 2017-18 _Final_Poverty+Individual_Data.dta")
consumption_aggregates <- read_dta("raw_data/HBS_2017-18 Consumption Aggregate and Poverty Analysis Variables.dta")

#-------------------------------------------------------------------------------

# Get household IDs: 
household_ids <- household_roster |> 
  dplyr::select(
    hhid = HHID, 
    interview__id
  ) |> 
  distinct()

rm(household_roster)

#--------------------------------------------------------------------------------

# Get consumption/expendure aggregates:
consumption_aggregates <- consumption_aggregates |> 
  dplyr::select(
    interview__id, 
    exp_aeq_month = aex, # Expenditures per adult equivalents (monthly)
    cons_aeq_adj = aecd, # Consumption per adult equivalents (monthly), spatially and temporally adjusted
    cons_decile = decile # Consumption decile (per adult equivalent)
  ) 

# Scatter plot of expenditure and consumption - log scale:
ggplot(consumption_aggregates, aes(x = log(exp_aeq_month), y = log(cons_aeq_adj))) +
  geom_point() +
  labs(title = "Scatter plot of Expenditure vs Consumption",
       x = "Expenditure (AEQ Monthly)",
       y = "Consumption (AEQ Adjusted)") +
  xlim(0, 20) + 
  ylim(0, 20) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed")

# Essentially a linear relationship between variables. 
# Will opt to use the spatially and temporally adjusted consumption aggregate 
# (per adult equivalent) to generate wealth quintiles.

consumption_aggregates <- consumption_aggregates |> 
  dplyr::select(interview__id, cons_aeq_adj, exp_aeq_month) |> 
  left_join(household_ids, by = "interview__id") |> 
  dplyr::select(hhid, cons_aeq_adj, exp_aeq_month) |> 
  distinct(hhid, .keep_all = TRUE)

rm(household_ids)

#-------------------------------------------------------------------------------

# Compute wealth quintiles:
consumption_aggregates <- consumption_aggregates |> 
  mutate(sep_quintile = ntile(cons_aeq_adj, 5))

# Stratified by urban/rural sector:
consumption_aggregates <- consumption_aggregates |> 
  left_join(
    household_information |> 
      dplyr::select(hhid, res), 
    by = "hhid")

# Urban aggregates: 
urban_aggregates <- consumption_aggregates |> 
  filter(res == "Urban") |> 
  mutate(res_quintile = ntile(cons_aeq_adj, 5)) |> 
  dplyr::select(hhid, res_quintile)

# Rural aggregates:
rural_aggregates <- consumption_aggregates |> 
  filter(res == "Rural") |> 
  mutate(res_quintile = ntile(cons_aeq_adj, 5)) |> 
  dplyr::select(hhid, res_quintile)

# Select required columns:
consumption_aggregates <- consumption_aggregates |> 
  dplyr::select(
    hhid, 
    sep_quintile,
    pc_expenditure = exp_aeq_month
  )

ur_aggregates <- rbind(rural_aggregates, urban_aggregates)

# Clear environment:
rm(urban_aggregates, rural_aggregates)

#------------------------------------------------------------------------------

# Join to household information:
household_information <- household_information |> 
  left_join(consumption_aggregates, by = "hhid") |> 
  left_join(ur_aggregates, by = "hhid")

rm(consumption_aggregates, ur_aggregates)

# Finalise household information:
household_information <- household_information |> 
  mutate(
    survey = "hbs1718",
    iso3 = "TZA",
    zone = NA,
    adm2 = NA, 
    ea = NA
  ) |> 
  dplyr::select(survey, hhid, iso3, zone, adm1, adm2, ea, res, sep_quintile, 
                res_quintile, year, month, survey_wgt, afe, pc_expenditure)

# Write: 
write_csv(household_information, "processed_data/tza_hbs1718_hh_information.csv")

rm(list = ls())

################################################################################
################################# END OF SCRIPT ################################
################################################################################