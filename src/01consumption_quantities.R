# Load required packages
pacman::p_load(tidyverse, haven)

# Select needed columns
daily_food_cons <- read_dta("HD_B1.dta") %>%
  select(hhid, day, coicop4, coicop, unit, q)

# Check % occurrence of units and convert to grams, using the table above
options(scipen = 99)
round(prop.table(table(round(daily_food_cons$unit))) * 100, 1)

# Convert all quantities to grams based on the 'unit' code
daily_food_cons <- daily_food_cons %>%
  rename(quantity=q) %>%
  mutate(quantity = case_when(
    unit == 1 ~ quantity,               
    unit == 2 ~ quantity * 1000,        
    unit == 3 ~ quantity,               
    unit == 4 ~ quantity * 1000,        
    unit == 5 ~ quantity,               
    unit == 6 ~ quantity,               
    unit == 7 ~ quantity,               
    unit == 8 ~ quantity,               
    TRUE ~ quantity                    
  ))

# Calculate quantities assuming a 14 day recall

# Consumption quantities with broad food item
daily_food_cons_broad <- daily_food_cons %>%
  select(hhid, coicop4, quantity) %>%
  group_by(hhid, coicop4) %>%
  summarise(quantity_g = sum(quantity, na.rm = TRUE)/14) %>%
  mutate(
    broad_item_name = as_factor(coicop4)
  )

# Consumption quantities with detailed food item
daily_food_cons_detailed <- daily_food_cons %>%
  select(hhid, coicop, quantity) %>%
  group_by(hhid, coicop) %>%
  summarise(quantity_g = sum(quantity, na.rm = TRUE)/14) %>%
  mutate(
    detailed_item_name = as_factor(coicop)
  )
