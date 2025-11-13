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

# SOURCE FUNCTIONS: 
source("src/00functions.R")

#--------------------------------------------------------------------------------

# APPARENT INTAKE ESTIMATES: 
base_ai <- apparent_intake("tza_hbs1718")

#---------------------------------------------------------------------------------

# Summary of energy intake:
summary(base_ai$energy_kcal)

# Histogram of energy intake with median line:
ggplot(base_ai, aes(x = energy_kcal)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = median(energy_kcal)), color = "red", linetype = "dashed") +
  annotate("text", x = median(base_ai$energy_kcal) + 1000, y = 1500, 
           label = paste("Median:", round(median(base_ai$energy_kcal), 1)), 
           color = "red") +
  labs(title = "Energy Intake Distribution", x = "Energy Intake (kcal)", y = "Frequency") +
  xlim(0, 10000) +
  theme_minimal()

# ggsave("figures/energy_intake_distribution.png", width = 8, height = 6)

#---------------------------------------------------------------------------------

# STRUCTURE BASE AI SO THAT IT IS IN CORRECT FORMAT FOR THE MIMI DATABASE: 
base_ai <- base_ai |> 
  mutate(
    iso3 = "TZA",
    survey = "hbs1718"
  ) |> 
  dplyr::select(
    iso3, 
    survey, 
    hhid, 
    everything()
  )

# WRITE DATA: 
write_csv(base_ai, "processed_data/tza_hbs1718_base_ai.csv")

rm(list = ls())

################################################################################
################################## END OF SCRIPT ###############################
################################################################################
