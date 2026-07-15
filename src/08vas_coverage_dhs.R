# Author: Uchenna Agu
# Use: DHS Vitamin A Supplementation coverage (TZA 22)
# Date: July 2026
# ==============================================================================
# INSTALL AND LOAD REQUIRED PACKAGES
# ==============================================================================
# List of required packages
rq_packages <- c(
  "haven", "tidyverse", "survey")

# Install missing packages
installed_packages <- rq_packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

# Load packages into session
lapply(rq_packages, require, character.only = TRUE)

# Clean workspace
rm(list = c("rq_packages", "installed_packages"))
#--------------------------------------------------
# 1. LOAD KR DATA
#--------------------------------------------------

KRdata <- read_dta("processed_data/TZKR82FL.DTA")

#--------------------------------------------------
# 2. EXTRACT ADM1 LABELS
#--------------------------------------------------

adm1_labels <- attr(KRdata$v024, "labels")

adm1_map <- data.frame(
  adm1 = as.numeric(adm1_labels),
  adm1_label = names(adm1_labels)
)

#--------------------------------------------------
# 3. KEEP REQUIRED VARIABLES
#--------------------------------------------------

KRdata <- KRdata |>
  select(
    v001,        # cluster
    v002,        # hh number
    v005,        # weight
    v024,        # region
    v008, v008a, # interview dates (CMS)
    b3, b5, b19, # DoB (CMC), Survival status, Age in Months
    h33d, h33m, h33y, h34  # Vitamin A
  )

#--------------------------------------------------
# 4. CHILD-LEVEL VARIABLES
#--------------------------------------------------

KRdata <- KRdata |>
  mutate(
    # Weight
    wt = v005 / 1000000,
    
    # Region code
    adm1 = v024,
    
    # Age (months)
    age = ifelse(!is.na(b19), b19, v008 - b3),
    
    # Eligible children (6–59m, alive)
    eligible_child = ifelse(age >= 6 & age <= 59 & b5 == 1, 1, 0),
    
    # Clean Vitamin A date
    h33m2 = na_if(h33m, 98),
    h33y2 = na_if(h33y, 9998),
    h33d2 = ifelse(h33d == 98 | is.na(h33d), 15, h33d),
    
    Date = as.Date(paste(h33y2, h33m2, h33d2, sep = "-"), "%Y-%m-%d"),
    
    # Months since Vitamin A
    days_since_origin = as.numeric(difftime(Date, "1960-01-01", units = "days")) + 21916,
    months_since_vita = (v008a - days_since_origin) / 30.4375,
    
    # Child-level VAS indicator
    vas_child = case_when(
      eligible_child == 0 ~ NA_real_,
      h34 == 1 ~ 1,
      !is.na(months_since_vita) & months_since_vita <= 6 ~ 1,
      TRUE ~ 0
    )
  )

#--------------------------------------------------
# ✅ 5. NATIONAL VAS COVERAGE
#--------------------------------------------------

child_vas <- KRdata |>
  filter(eligible_child == 1)

national_vas <- weighted.mean(
  child_vas$vas_child,
  w = child_vas$wt,
  na.rm = TRUE
) * 100

cat("National VAS coverage (%):", round(national_vas, 1), "/n")

#--------------------------------------------------
# ✅ 6. REGIONAL (ADM1) VAS COVERAGE
#--------------------------------------------------

adm1_vas <- child_vas |>
  group_by(adm1) |>
  summarise(
    vas_coverage = weighted.mean(vas_child, wt, na.rm = TRUE) * 100,
    .groups = "drop"
  )


# Add region names
adm1_vas <- adm1_vas |>
  left_join(adm1_map, by = "adm1") |>
  mutate(adm1 = adm1_label,
         adm1 = str_to_title(adm1)) |>
  select(adm1, vas_coverage) |>
  filter(!str_detect(adm1, "Unguja|Pemba|Mjini")) |>
  arrange(desc(vas_coverage))  

#--------------------------------------------------
# 7. Shapefiles
#--------------------------------------------------

tanzania_1 <- st_read("shapefiles/tza_admbnda_adm1_20181019.shp") |> 
  dplyr::select(
    adm1 = ADM1_EN, 
    geometry
  ) |> 
  filter(!str_detect(adm1, "Unguja|Pemba|Mjini")) |>
  mutate(adm1 = recode(
    adm1, 
    "Dar-es-salaam" = "Dar Es Salaam",
  ))

#--------------------------------------------------
# 8. Merge and plot (reverse palette) with predefined function
#--------------------------------------------------
tza_vas_coverage <- adm1_vas |>
  left_join(tanzania_1, by="adm1")

source("src/00functions.R")

vas_cov <- plot_map(
  data = tza_vas_coverage,
  col = "vas_coverage",
  title = "",
  metric = "Coverage (%)",
  outline_sf = tanzania_1,
  limits = c(0, 100)
) +
  scale_fill_gradientn(
    colours = rev(wes_palette("Zissou1", n = 100, type = "continuous")),
    limits = c(0, 100),
    name = "Coverage (%)"
  )

vas_cov

# Save
ggsave(
  filename = "figures/fortification_maps/VAS_coverage.png",
  plot = vas_cov,
  width = 8,
  height = 6,
  dpi = 300
)

#--------------------------------------------------
# 9. GAVA target 
#--------------------------------------------------
vas_gava_map <- plot_map(
  data = tza_vas_coverage |>
    mutate(
      GAVA = ifelse(vas_coverage >= 80, "80% or above", "Below 80%")
    ),
  col = "GAVA",
  title = "",
  metric = "",
  outline_sf = tanzania_1
) +
  scale_fill_manual(
    values = c(
      "Below 80%" = "#D7191C",
      "80% or above" = "#1A9641"
    )
  ) +
  theme(
    legend.position = "none"
  )

vas_gava_map 

# Save
ggsave(
  filename = "figures/fortification_maps/VAS_GAVA.png",
  plot = vas_gava_map,
  width = 8,
  height = 6,
  dpi = 300
)