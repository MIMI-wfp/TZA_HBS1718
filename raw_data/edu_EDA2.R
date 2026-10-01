# ============================================================
# TANZANIA HBS 2017-18
# SCHOOL ATTENDANCE AND HOUSEHOLD-LEVEL CLASSIFICATION
# ============================================================

library(haven)
library(dplyr)


# ------------------------------------------------------------
# 1. Import the individual-level dataset
# ------------------------------------------------------------

household_roster <- read_dta(
  "HBS 2017-18 _Final_Poverty+Individual_Data.dta"
)


# ------------------------------------------------------------
# 2. Optional: inspect value labels before recoding
# ------------------------------------------------------------

print_labels(household_roster$S5_1)
print_labels(household_roster$S5_4)

table(household_roster$S5_1, useNA = "always")
table(household_roster$S5_4, useNA = "always")


# ------------------------------------------------------------
# 3. Specify the Yes and No codes
# Change these if print_labels() shows different codes
# ------------------------------------------------------------

yes_code <- 1
no_code  <- 2


# ------------------------------------------------------------
# 4. Select relevant variables and clean invalid values
# ------------------------------------------------------------

household_roster <- household_roster |>
  select(
    hhid = HHID,
    age = calc_age,
    ever_school = S5_1,
    reason_never_school = S5_2,
    in_school_original = S5_4,
    school_owner = S5_5,
    reason_not_attending = S5_6,
    current_grade = S5_7,
    previous_grade = S5_8,
    highest_grade = S5_9,
    travel_to_school = S5_10,
    travel_time = S5_11,
    missed_school = S5_12
  ) |>
  
  # Remove observations with missing or invalid age
  filter(
    !is.na(age),
    age != -9998
  ) |>
  
  # Convert special missing codes to standard NA
  mutate(
    across(
      c(
        ever_school,
        reason_never_school,
        in_school_original,
        school_owner,
        reason_not_attending,
        current_grade,
        previous_grade,
        highest_grade,
        travel_to_school,
        travel_time,
        missed_school
      ),
      ~ replace(
        .x,
        .x %in% c(-9998, -9999),
        NA
      )
    ),
    
    # Identify children aged 7 to 18
    school_age_7_18 = if_else(
      age >= 7 & age <= 18,
      1L,
      0L
    )
  )


# ------------------------------------------------------------
# 5. Identify supporting evidence about current attendance
# ------------------------------------------------------------

household_roster <- household_roster |>
  mutate(
    
    # Current grade is the strongest indirect evidence
    # of current school attendance
    current_grade_evidence =
      !is.na(current_grade),
    
    # Other questions normally asked about current schooling
    other_current_school_evidence =
      !is.na(school_owner) |
      !is.na(travel_to_school) |
      !is.na(travel_time) |
      !is.na(missed_school),
    
    # Any evidence related to current school participation
    current_school_evidence =
      current_grade_evidence |
      other_current_school_evidence,
    
    # Evidence that the person is not attending
    nonattendance_evidence =
      !is.na(reason_not_attending) |
      !is.na(reason_never_school) |
      ever_school == no_code
  )


# ------------------------------------------------------------
# 6. Check for potentially contradictory information
# ------------------------------------------------------------

contradictory_records <- household_roster |>
  filter(
    school_age_7_18 == 1,
    current_school_evidence,
    nonattendance_evidence
  ) |>
  select(
    hhid,
    age,
    ever_school,
    in_school_original,
    current_grade,
    school_owner,
    reason_not_attending,
    reason_never_school
  )

print(contradictory_records)


# ------------------------------------------------------------
# 7. Reconstruct school-attendance status
# ------------------------------------------------------------

household_roster <- household_roster |>
  mutate(
    attendance_status = case_when(
      
      # Household members outside the selected age range
      school_age_7_18 == 0 ~
        "Not school age",
      
      # Directly reported attendance status
      in_school_original == yes_code ~
        "Attending: reported",
      
      in_school_original == no_code ~
        "Not attending: reported",
      
      # Missing S5_4, but a current grade is recorded
      is.na(in_school_original) &
        current_grade_evidence ~
        "Attending: inferred from current grade",
      
      # Missing S5_4, but another current-school question
      # contains a valid response
      is.na(in_school_original) &
        other_current_school_evidence ~
        "Attending: inferred from school information",
      
      # Missing S5_4, but a reason for not currently
      # attending school is recorded
      is.na(in_school_original) &
        !is.na(reason_not_attending) ~
        "Not attending: inferred from reason",
      
      # Missing S5_4, but a reason for never attending
      # school is recorded
      is.na(in_school_original) &
        !is.na(reason_never_school) ~
        "Not attending: inferred from never-attended reason",
      
      # Person explicitly reported never going to school
      is.na(in_school_original) &
        ever_school == no_code ~
        "Not attending: inferred from never attended",
      
      # No reliable attendance information
      TRUE ~
        "Attendance unknown"
    ),
    
    # Create a numeric reconstructed attendance variable:
    # 1 = attending
    # 0 = not attending
    # NA = unknown or outside the school-age range
    in_school_reconstructed = case_when(
      attendance_status %in% c(
        "Attending: reported",
        "Attending: inferred from current grade",
        "Attending: inferred from school information"
      ) ~ 1L,
      
      attendance_status %in% c(
        "Not attending: reported",
        "Not attending: inferred from reason",
        "Not attending: inferred from never-attended reason",
        "Not attending: inferred from never attended"
      ) ~ 0L,
      
      TRUE ~ NA_integer_
    )
  )


# ------------------------------------------------------------
# 8. Examine individual-level reconstructed attendance
# ------------------------------------------------------------

individual_attendance_summary <- household_roster |>
  filter(school_age_7_18 == 1) |>
  count(
    attendance_status,
    name = "number_children"
  ) |>
  mutate(
    percentage = round(
      100 * number_children / sum(number_children),
      2
    )
  )

print(individual_attendance_summary)


# ------------------------------------------------------------
# 9. Compare missingness before and after reconstruction
# ------------------------------------------------------------

missingness_comparison <- household_roster |>
  filter(school_age_7_18 == 1) |>
  summarise(
    number_school_age_children = n(),
    
    original_attendance_missing =
      sum(is.na(in_school_original)),
    
    original_missing_percent =
      round(
        100 * mean(is.na(in_school_original)),
        2
      ),
    
    reconstructed_attendance_missing =
      sum(is.na(in_school_reconstructed)),
    
    reconstructed_missing_percent =
      round(
        100 * mean(is.na(in_school_reconstructed)),
        2
      )
  )

print(missingness_comparison)


# ------------------------------------------------------------
# 10. Create one observation per household
# ------------------------------------------------------------

household_level <- household_roster |>
  group_by(hhid) |>
  summarise(
    
    # Number of children aged 7 to 18
    number_school_age =
      sum(school_age_7_18 == 1),
    
    # Number confirmed or inferred as attending
    number_attending =
      sum(
        school_age_7_18 == 1 &
          in_school_reconstructed == 1,
        na.rm = TRUE
      ),
    
    # Number confirmed or inferred as not attending
    number_not_attending =
      sum(
        school_age_7_18 == 1 &
          in_school_reconstructed == 0,
        na.rm = TRUE
      ),
    
    # Number of school-aged children whose status remains unknown
    number_attendance_unknown =
      sum(
        school_age_7_18 == 1 &
          is.na(in_school_reconstructed)
      ),
    
    # Household categories
    household_school_group = case_when(
      
      number_school_age == 0 ~
        "No school-age children",
      
      number_attending >= 1 ~
        "School-age children, at least one attending",
      
      number_attending == 0 &
        number_not_attending >= 1 &
        number_attendance_unknown == 0 ~
        "School-age children, none attending",
      
      TRUE ~
        "School-age children, attendance unknown"
    ),
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# 11. Examine household-level results
# ------------------------------------------------------------

household_group_summary <- household_level |>
  count(
    household_school_group,
    name = "number_households"
  ) |>
  mutate(
    percentage = round(
      100 * number_households / sum(number_households),
      2
    )
  )

print(household_group_summary)


# ------------------------------------------------------------
# 12. Optional: create a three-category household variable
# ------------------------------------------------------------
# This keeps unresolved households as NA rather than incorrectly
# classifying them as households with no children attending.

household_level <- household_level |>
  mutate(
    household_school_group_3 = case_when(
      number_school_age == 0 ~
        "No school-age children",
      
      number_attending >= 1 ~
        "School-age children, at least one attending",
      
      number_school_age >= 1 &
        number_attending == 0 &
        number_attendance_unknown == 0 ~
        "School-age children, none attending",
      
      TRUE ~ NA_character_
    )
  )


# ------------------------------------------------------------
# 13. Summary of the three household categories
# ------------------------------------------------------------

household_group_3_summary <- household_level |>
  count(
    household_school_group_3,
    name = "number_households",
    .drop = FALSE
  ) |>
  mutate(
    percentage_all_households = round(
      100 * number_households / sum(number_households),
      2
    )
  )

print(household_group_3_summary)

