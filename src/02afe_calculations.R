################################################################################
################ SCRIPT FOR AFE CALCULATIONS - TZA HBS 2017-2018 ###############
################################################################################

# Author: Mo Osman
# Date created: 06-Nov-2025
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

# READ DATA: 

household_roster <- read_dta("raw_data/HBS 2017-18 _Final_Poverty+Individual_Data.dta")
consumption_aggregates <- read_dta("raw_data/HBS_2017-18 Consumption Aggregate and Poverty Analysis Variables.dta")

#-------------------------------------------------------------------------------

# SELECT REQUIRED VARIABLES:
household_roster <- household_roster |> 
  dplyr::select(
    hhid = HHID,
    interview__id,
    roster_id = TUS__id,
    adm1 = REGION,
    sex = S1_2,
    month_birth = S1_3_1,
    year_birth = S1_3_2,
    age = calc_age,
    survey_wgt = weight,
    popweight,
    res = LOC,
    hh_size = hhsize,
    adult_equiv = aeq
  ) |> 
  mutate(
    month_birth = as.numeric(month_birth),
    year_birth = as.numeric(year_birth)
  )

# See all haven labels in the dataset:
for (var in names(household_roster)) {
  tryCatch({
    print_labels(household_roster[[var]], var)
  }, error = function(e) {
    message(paste("Error in variable:", var, " - ", e$message))
  })
}

# Mutate values to match labels:
household_roster <- household_roster |> 
  mutate(res = case_when(
    res == 1 ~ "Rural",
    res == 2 ~ "Urban",
    TRUE ~ NA_character_
  )) |> 
  mutate(sex = case_when(
    sex == 1 ~ "Male",
    sex == 2 ~ "Female",
    TRUE ~ NA_character_
  )) |> 
  mutate(adm1 = case_when(
    adm1 == 1 ~ "Dodoma",
    adm1 == 2 ~ "Arusha",
    adm1 == 3 ~ "Kilimanjaro",
    adm1 == 4 ~ "Tanga",
    adm1 == 5 ~ "Morogoro",
    adm1 == 6 ~ "Pwani",
    adm1 == 7 ~ "Dar Es Salaam",
    adm1 == 8 ~ "Lindi",
    adm1 == 9 ~ "Mtwara",
    adm1 == 10 ~ "Ruvuma",
    adm1 == 11 ~ "Iringa",
    adm1 == 12 ~ "Mbeya",
    adm1 == 13 ~ "Singida",
    adm1 == 14 ~ "Tabora",
    adm1 == 15 ~ "Rukwa",
    adm1 == 16 ~ "Kigoma",
    adm1 == 17 ~ "Shinyanga",
    adm1 == 18 ~ "Kagera",
    adm1 == 19 ~ "Mwanza",
    adm1 == 20 ~ "Mara",
    adm1 == 21 ~ "Manyara",
    adm1 == 22 ~ "Njombe",
    adm1 == 23 ~ "Katavi",
    adm1 == 24 ~ "Simiyu",
    adm1 == 25 ~ "Geita",
    adm1 == 26 ~ "Songwe"
  ))

# Grab survey date: 
date <- consumption_aggregates |> 
  dplyr::select(interview__id, date) |> 
  mutate(year = as.numeric(substr(date, 1, 4)),
         month = as.numeric(substr(date, 5, 6))) |> 
  dplyr::select(interview__id, year, month)

# Join to household_roster:
household_roster <- household_roster |> 
  left_join(date, by = "interview__id")

rm(date, var)

#--------------------------------------------------------------------------------

# LIST OF ASSUMPTIONS FOR AFE CALCULATIONS:

# There is no anthropometric data collected in the survey, therefore assumptions
# made for - weight:

# Average men's weight = 65kg
mens_weight <- 65

# Average women's weight = 55kg
womens_weight <- 55

# 1 AFE = 2291kca/day for a 55kg woman of reproductive age.
AFE <- 2291 # Source: FAO/WHO/UNU report on human energy requirements, 2004.

# Active/moderately active lifestyle assumed:
PAL <- 1.76 # reference: table 5.1 FAO/WHO/UNU (2004)

#-------------------------------------------------------------------------------

# CLEANING: 

# There are some non-sensical entries for year_birth and month_birth:
household_roster <- household_roster |> 
  mutate(year_birth = ifelse(year_birth > year, NA, year_birth))

household_roster <- household_roster |>
  mutate(month_birth = ifelse(year_birth == year & month_birth > month, NA, month_birth)) |> 
  mutate(month_birth = ifelse(month_birth > 12, NA, month_birth))

# Some individuals have been coded with age as -9998, if they are newborn - 
# recode age to 0:
household_roster <- household_roster |> 
  mutate(age = ifelse(age == -9998, 0, age))

#-------------------------------------------------------------------------------

# IDENTIFY HOUSEHOLD INDIVIDUALS BELONGING TO EACH DEMOGRAPHIC SUB-GROUP:

# CHILDREN UNDER 2 YEARS OLD:
u2 <- household_roster |> 
  filter(age <= 2)

# Calculate age in months for children under 2, and keep only those with
# age_months <= 24:
u2 <- u2 |> 
  mutate(age_months = (year - year_birth) * 12 + (month - month_birth)) |> 
  filter(age_months <= 24) |> 
  # Keep only relevant variables:
  dplyr::select(hhid, interview__id, roster_id, sex, age_months)

# CHILDREN 2-18 YEARS OLD: 
children_2_18 <- household_roster |> 
  filter(age >= 2 & age < 18) |> 
  # Keep only relevant variables:
  dplyr::select(hhid, interview__id, roster_id, sex, age) |> 
  # Filter out children who are in the u2 group:
  anti_join(u2, by = c("hhid", "interview__id", "roster_id"))

# Adults 18+:
adults <- household_roster |> 
  filter(age >= 18) |> 
  # Keep only relevant variables:
  dplyr::select(hhid, interview__id, roster_id, sex, age)

#-------------------------------------------------------------------------------

# CALCULATE AFE FOR EACH DEMOGRAPHIC SUB-GROUP:

# CHILDREN UNDER 2 YEARS OLD:

# Source:
# Book - Complementary feeding of young Children in Developing Countries, 
# Table 10, page 51.
# WHO 1998, edited by Kenneth Brown, Kathryn Dewey, Lindsay Allen

u2 <- u2 |> 
  mutate(kcalreq = case_when(
    age_months <= 2 ~ 0,   # only breast feeding - no food intake
    age_months >= 3 & age_months <= 5 ~ 76,  # energy from food is 76kcal per day for 3-5 months of age
    age_months >= 6 & age_months <= 8 ~ 269,  # 269kcal per day for 6-8 months of age
    age_months >= 9 & age_months <= 11 ~ 451,   # 451kcal per day for 9-11 months of age
    age_months >= 12 ~ 746)) # 746kcal per day for those aged 12-months - 2years

afe_u2 <- u2 |> 
  mutate(afe = kcalreq / AFE) |> 
  dplyr::select(hhid, interview__id, roster_id, afe)

# CHILDREN 2-18 YEARS OLD:
# Source - kcal requirements based on tables 4.5 and 4.6 in the FAO/WHO/UNU 
# report.
# Assumed moderate physical activity level for both boys and girls.
children_2_18 <- children_2_18 |> 
  mutate(kcalreq = case_when(
    sex == "Male" & age == 2 ~ 1125,
    sex == "Male" & age == 3 ~ 1250,
    sex == "Male" & age == 4 ~ 1350,
    sex == "Male" & age == 5 ~ 1475, 
    sex == "Male" & age == 6 ~ 1575,
    sex == "Male" & age == 7 ~ 1700,
    sex == "Male" & age == 8 ~ 1825, 
    sex == "Male" & age == 9 ~ 1975, 
    sex == "Male" & age == 10 ~ 2150,
    sex == "Male" & age == 11 ~ 2350, 
    sex == "Male" & age == 12 ~ 2550, 
    sex == "Male" & age == 13 ~ 2775, 
    sex == "Male" & age == 14 ~ 3000,
    sex == "Male" & age == 15 ~ 3175, 
    sex == "Male" & age == 16 ~ 3325, 
    sex == "Male" & age == 17 ~ 3400,
    sex == "Female" & age == 2 ~ 1050,
    sex == "Female" & age == 3 ~ 1150, 
    sex == "Female" & age == 4 ~ 1250,
    sex == "Female" & age == 5 ~ 1325,
    sex == "Female" & age == 6 ~ 1425, 
    sex == "Female" & age == 7 ~ 1550,
    sex == "Female" & age == 8 ~ 1700,
    sex == "Female" & age == 9 ~ 1850,
    sex == "Female" & age == 10 ~ 2000,
    sex == "Female" & age == 11 ~ 2150, 
    sex == "Female" & age == 12 ~ 2275, 
    sex == "Female" & age == 13 ~ 2375, 
    sex == "Female" & age == 14 ~ 2450, 
    sex == "Female" & age > 15 & age < 18 ~ 2500
  ))

afe_children_2_18 <- children_2_18 |>
  mutate(afe = kcalreq / AFE) |> 
  dplyr::select(hhid, interview__id, roster_id, afe)

# ADULTS 18+:
# Source: table 5.2 of the FAO/WHO/UNU report.
adults <- adults |> 
  # Firstly calculate BMR (Basal Metabolic Rate): 
  mutate(BMR = case_when(
    sex == "Male" & age >= 18 & age < 30 ~ 15.057 * mens_weight + 692.2,
    sex == "Male" & age >= 30 & age < 60 ~ 11.472 * mens_weight + 873.1, 
    sex == "Male" & age >= 60 ~ 11.711  * mens_weight + 587.7,
    sex == "Female" & age >= 18 & age < 30 ~ 14.818 * womens_weight + 486.6, 
    sex == "Female" & age >= 30 & age < 60 ~ 8.126 * womens_weight + 845.6,
    sex == "Female" & age >= 60 ~ 9.082 * womens_weight + 658.5
  )) |> 
  # Multiply by PAL to get total energy requirements:
  mutate(kcalreq = BMR * PAL)

afe_adults <- adults |> 
  mutate(afe = kcalreq / AFE) |> 
  dplyr::select(hhid, interview__id, roster_id, afe)

rm(u2, children_2_18, adults)

#-------------------------------------------------------------------------------

# CALCULATE TOTAL AFE FOR EACH HOUSEHOLD:
hh_afe <- bind_rows(afe_u2, afe_children_2_18, afe_adults) |>
  group_by(hhid) |>
  summarise(afe = sum(afe, na.rm = TRUE)) |>
  ungroup()

rm(afe_u2, afe_children_2_18, afe_adults)

#-------------------------------------------------------------------------------

# AFE CHECKS:
household_information <- household_roster |> 
  dplyr::select(hhid, adm1, res, survey_wgt, popweight, year, month, hh_size, adult_equiv) |>
  distinct() |>
  left_join(hh_afe, by = "hhid")

# Compare AFE to household size:
ggplot(household_information, aes(x = hh_size, y = afe)) +
  geom_jitter(width = 0.25, height = 0.25, alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "AFE vs Household Size",
       x = "Household Size",
       y = "AFE") +
  theme_minimal(base_family = "", base_size = 11) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

# For smaller households, almost a 1:1 relationship between AFE and household size, 
# which is to be expected. For larger households, the AFE tends to be lower than
# household size, which is expected as larger households tend to have more children.

# Compare AFE to adult equivalent:
ggplot(household_information, aes(x = adult_equiv, y = afe)) +
  geom_jitter(width = 0.25, height = 0.25, alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "AFE vs Adult Equivalent",
       x = "Adult Equivalent",
       y = "AFE") +
  theme_minimal(base_family = "", base_size = 11) +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

# Linear relationship between AFE and adult equivalent, as expected. However, 
# AFE tends to be higher than adult equivalent, this is expected as the energy
# requirements are lower for women compared with men. Therefore 1 AEQ unit, will
# equal more than 1 AFE unit.

# Exploratory analysis look reassuiring, therefore we can proceed to use the 
# calculated AFEs.

#-------------------------------------------------------------------------------

# Keep only required variables: 
household_information <- household_information |> 
  dplyr::select(hhid, adm1, res, survey_wgt, year, month, afe) |>
  distinct(hhid, .keep_all = TRUE)

# Write CSV: 
write_csv(household_information, "processed_data/tza_hbs1718_hh_information.csv")

# Clear environment:
rm(list = ls())

################################################################################
################################ END OF SCRIPT #################################
################################################################################