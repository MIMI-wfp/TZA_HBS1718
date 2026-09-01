# ============================================================
# TANZANIA HBS 2017-18: SCHOOL-ATTENDANCE DATA FOR CHILDREN AGED 7-18
# Authors: UA & MO
# Date: August 2026
# ============================================================


# ------------------------------------------------------------
# 1. Install and load the required packages
# ------------------------------------------------------------

# List the packages required for the analysis
rq_packages <- c(
  "readr",
  "readxl",
  "tidyverse",
  "haven"
)

# Check which required packages are already installed
installed_packages <- rq_packages %in%
  rownames(installed.packages())

# Install only packages that are not currently installed
if (any(installed_packages == FALSE)) {
  install.packages(
    rq_packages[!installed_packages]
  )
}

# Load all required packages
# invisible() prevents lapply() from printing its output
invisible(
  lapply(
    rq_packages,
    require,
    character.only = TRUE
  )
)

# Remove temporary package-management objects
rm(
  list = c(
    "rq_packages",
    "installed_packages"
  )
)

# ------------------------------------------------------------
# 2. Import the Tanzania HBS individual-level dataset
# ------------------------------------------------------------

household_roster <- read_dta("raw_data/HBS 2017-18 _Final_Poverty+Individual_Data.dta")

# ------------------------------------------------------------
# 3. Select and prepare the relevant variables
# ------------------------------------------------------------

household_roster <- household_roster |>
  
  # Retain household ID, age, current school attendance,
  # reason for not currently attending, and reason for
  # never attending school
  select(
    hhid = HHID,
    age = calc_age,
    in_school = S5_4,
    S5_6,
    S5_2
  ) |>
  
  # Remove observations with missing age or the special
  # invalid age code -9998
  filter(
    !is.na(age),
    age != -9998
  ) |>
  
  mutate(
    
    # Assign 2 to in_school for household members outside
    # the selected school-age range of 7-18 years.
    # For people aged 7-18, retain their original S5_4 value,
    # including missing values.
    in_school = if_else(
      age < 7 | age > 18,
      2,
      as.numeric(in_school)
    ),
    
    # Create an indicator for school-aged children:
    # 1 = aged 7-18
    # 0 = younger than 7 or older than 18
    school_age_7_18 = if_else(
      age >= 7 & age <= 18,
      1L,
      0L
    )
  )

# ------------------------------------------------------------
# 4. Check the prepared dataset
# ------------------------------------------------------------

# Display the variable names
colnames(household_roster)

# Count missing values in each variable
colSums(
  is.na(household_roster)
)

# ------------------------------------------------------------
# 5. Identify school-aged children with missing attendance
# ------------------------------------------------------------

missing_attendance_records <- household_roster |>
  filter(
    school_age_7_18 == 1,
    is.na(in_school)
  )

# Display the number of school-aged children whose
# current school-attendance status is missing
nrow(missing_attendance_records)

# ------------------------------------------------------------
# 6. Inspect the response labels for S5_2
# ------------------------------------------------------------

# S5_2 asks why the person never attended school
print_labels(
  household_roster$S5_2
)

# ------------------------------------------------------------
# 7. Summarise S5_2 among children with missing attendance
# ------------------------------------------------------------

S5_2_summary <- missing_attendance_records |>
  
  # Retain children with a recorded S5_2 response
  filter(
    !is.na(S5_2)
  ) |>
  
  # Convert the labelled Stata variable into readable labels
  mutate(
    S5_2_response = as_factor(S5_2)
  ) |>
  
  # Count children in each S5_2 response category
  count(
    S5_2_response,
    sort = TRUE,
    name = "count"
  ) |>
  
  # Calculate each category's percentage among children
  # with a non-missing S5_2 response
  mutate(
    percentage = round(
      100 * count / sum(count),
      2
    )
  )

# Display the S5_2 frequency table
S5_2_summary

# ------------------------------------------------------------
# 8. Recover non-attendance information using S5_2
# ------------------------------------------------------------

household_roster <- household_roster |>
  mutate(
    
    # Assign 2 to in_school when:
    # 1. The original school-attendance value is missing, and
    # 2. S5_2 contains a reason why the person never attended.
    #
    # A substantive S5_2 response indicates that the child
    # has never attended school and therefore is not currently
    # attending school.
    #
    # All other in_school values are left unchanged.
    in_school = if_else(
      is.na(in_school) & !is.na(S5_2),
      2,
      in_school
    )
  )

# ------------------------------------------------------------
# 9. Retain the final variables required for household analysis
# ------------------------------------------------------------

household_roster <- household_roster |>
  select(
    hhid,
    age,
    school_age_7_18,
    in_school
  )

# ------------------------------------------------------------
# 10. Check remaining missing values
# ------------------------------------------------------------

colSums(is.na(household_roster))

# ------------------------------------------------------------
# 11. Remove observations whose attendance remains unresolved
# ------------------------------------------------------------

# At this stage, remaining missing values should primarily be
# school-aged children for whom neither S5_4 nor S5_2 provides
# usable attendance information.
#
# These observations are removed from the analytical dataset.
household_roster <- household_roster |>
  filter(
    !is.na(in_school)
  )

# ------------------------------------------------------------
# 12. Final data-quality checks
# ------------------------------------------------------------

# Confirm that the analytical variables contain no missing values
colSums(is.na(household_roster))

# Display the dimensions of the final individual-level dataset
dim(household_roster)

# Check attendance status among children aged 7-18
household_roster |>
  filter(
    school_age_7_18 == 1
  ) |>
  count(
    in_school,
    name = "number_children"
  ) |>
  mutate(
    percentage = round(
      100 * number_children / sum(number_children),
      2
    )
  )

# Create household-level school-attendance dataset
hh_level <- household_roster |>
  group_by(hhid) |>
  summarise(
    # Number of household members aged 7-18
    number_school_age = sum(
      school_age_7_18 == 1,
      na.rm = TRUE
    ),
    
    # Number of school-aged children attending school
    number_attending = sum(
      school_age_7_18 == 1 & in_school == 1,
      na.rm = TRUE
    ),
    
    # Assign household status:
    # 1 = No school-age children
    # 2 = School-age children, at least one attending
    # 3 = School-age children, none attending
    hhid_status = case_when(
      number_school_age == 0 ~ 1L,
      number_attending >= 1 ~ 2L,
      number_school_age >= 1 & number_attending == 0 ~ 3L
    ),
    
    .groups = "drop"
  ) |>
  
  # Keep only the two requested variables
  select(
    hhid,
    hhid_status
  )

# Save
write_csv(hh_level, file = "csvs/hh_level_school_status.csv")

# Show count
hh_level |>
  count(hhid_status) |>
  mutate(
    percentage = round(
      100 * n / sum(n),
      2
    )
  )
