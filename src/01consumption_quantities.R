# Authors: Uche Agu & Mo Osman

# Load required packages
pacman::p_load(tidyverse,
               readxl,
               haven)

# Read in data and convert all quantities to grams based on the 'unit' code
daily_food_cons <- read_dta("HD_B1.dta") %>%
  select(interview__id, day, coicop4, coicop, unit, q) %>%
  rename(quantity=q) %>%
  mutate(quantity = case_when(
    unit == 1 ~ quantity,               
    unit == 2 ~ quantity * 1000,        
    unit == 3 ~ quantity,               
    unit == 4 ~ quantity * 1000,        
    unit == 5 ~ quantity,               
    unit == 6 ~ quantity,               
    unit == 7 ~ quantity * 55,               
    unit == 8 ~ quantity * 55,               
    TRUE ~ quantity                    
  ))

# Consumption quantities with detailed food item, remove bottled water, divide by 14
tzahbs1718_food_consumption <- daily_food_cons %>%
  select(interview__id, coicop, quantity) %>%
  filter(!(coicop %in% c(122101, 122102))) %>%  
  group_by(interview__id, coicop) %>%
  summarise(quantity_g = sum(quantity, na.rm = TRUE)/14, .groups = "drop") %>%
  rename(item_code = coicop)

# Load prepared hh info data
tza_hh_info <- read.csv("C:/Users/uchenna.agu/OneDrive - World Food Programme/Mohammed Aheed OSMAN's files - processed_data/tza_hbs1718_hh_information.csv") %>%
  select(hhid, afe) %>%
  mutate(hhid=as.character(hhid))

# Load HBS_2017_18_Final_Poverty_Individual_Data to extract interview__id and HHID
df <- read_dta("HBS 2017-18 _Final_Poverty+Individual_Data.dta") %>%
  select(interview__id, HHID) %>%
  mutate(HHID = as.character(HHID)) %>%
  distinct_all()

# Then join with the hh_info data
tza_hh <- tza_hh_info %>%
  left_join(df, by =c("hhid"="HHID"))

# Divide quantity_g by afe and remove values > 5000
tzahbs1718_food_consumption <- tzahbs1718_food_consumption %>%
  left_join(tza_hh, by = "interview__id") %>%
  mutate(quantity_g = quantity_g / afe) %>%
  filter(quantity_g != 0) %>%
  filter(quantity_g <= 5000) 

# Handle outliers : Log transform quantity_g
tzahbs1718_food_consumption <- tzahbs1718_food_consumption %>%
  mutate(log_quantity_g = log10(quantity_g))

# Handle outliers : Calculate cutpoint for each item_code
cutpoint <- tzahbs1718_food_consumption %>%
  group_by(item_code) %>%
  summarise(
    mean_log = mean(log_quantity_g, na.rm = TRUE),
    sd_log = sd(log_quantity_g, na.rm = TRUE)
  ) %>%
  mutate(cutpoint = mean_log + 3 * sd_log) %>%
  select(item_code, cutpoint)

# Handle outliers : Apply cutpoints to the data
tzahbs1718_food_consumption <- tzahbs1718_food_consumption %>%
  left_join(cutpoint, by = "item_code") %>%
  mutate(quantity_g = case_when(
    log_quantity_g > cutpoint ~ NA_real_,
    TRUE ~ quantity_g
  )) %>%
  select(-log_quantity_g, -cutpoint)

# Handle outliers : Replace NA values with median intake per item_code
tzahbs1718_food_consumption_gdafe <- tzahbs1718_food_consumption %>%
  group_by(item_code) %>%
  mutate(quantity_g = ifelse(is.na(quantity_g),
                             median(quantity_g, na.rm = TRUE),
                             quantity_g)) %>%
  ungroup()

rm(cutpoint)

# Update food consumption with cleaned value and add DB features
tzahbs1718_food_consumption  <- tzahbs1718_food_consumption_gdafe  %>%
  mutate(quantity_g = quantity_g * afe,
         quantity_100g=quantity_g/100,
         iso3 = "TZA",
         survey = "hbs1718") %>%
  select(iso3, survey, -interview__id, hhid, item_code, quantity_g, quantity_100g)

# Save in required folder
write.csv(tzahbs1718_food_consumption, file = "tzahbs1718_food_consumption.csv", row.names = FALSE)
