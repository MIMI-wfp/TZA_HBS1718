## FUNCTIONS TO COMPILE BASE MODELS

#-------------------------------------------------------------------------------

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "here")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

path_to_file <- here::here("processed_data/")

allen_ear <- data.frame(
  nutrient = c(
    "energy_kcal",
    "vita_rae_mcg",
    "thia_mg",
    "ribo_mg",
    "niac_mg",
    "vitb6_mg",
    "folate_mcg",
    "vitb12_mcg",
    "fe_mg",
    "ca_mg",
    "zn_mg"
  ),
  ear_value = c(
    2100,#who
    490, 
    0.9,
    1.3, 
    11, 
    1.3, 
    250, 
    2, 
    22.4, #low absorption
    860, 
    10.2# unrefined
  )
)

#-------------------------------------------------------------------------------

read_in_survey <- function(name_of_survey, path_to_file = here::here("processed_data/")){
  # given the name of the survey of country
  # the function reads in each part of the base model into general 
  # object names
  
  hh_info <<-  read.csv(paste0(path_to_file, paste0(name_of_survey, "_hh_information.csv")))
  food_consumption<<- read.csv(paste0(path_to_file, paste0(name_of_survey, "_food_consumption.csv")))
  fc_table <<- read.csv(paste0(path_to_file, paste0(name_of_survey, "_fct.csv")))
}

#-------------------------------------------------------------------------------

apparent_intake <- function(name_of_survey, path_to_file = here::here("processed_data//")){
  # Estimates apparent intake of nutrients based on consumed food items
  # and adult female equivalent unit of the household
  read_in_survey(name_of_survey, path_to_file)
  
  hh_info <- hh_info |> dplyr::select(-c("iso3", "survey"))
  food_consumption <- food_consumption |> dplyr::select(-c("iso3", "survey"))
  fc_table <- fc_table |> dplyr::select(-c("iso3", "survey"))
  
  x <- food_consumption |>  
    left_join(fc_table, by = "item_code") |> 
    mutate(
      across(
        -c(item_code, hhid, item_name, quantity_100g, quantity_g),
        ~.x*quantity_100g
      )
    ) |> 
    group_by(hhid) |> 
    summarise(
      across(-c(item_code,item_name,quantity_100g,quantity_g),
             ~sum(.,na.rm = T))
    ) |> 
    left_join(hh_info |> select(hhid, afe), by = "hhid") %>% 
    mutate(
      across(
        -c(hhid,afe),
        ~.x/afe
      )
    ) |> 
    ungroup() |>  
    select(-afe)
  x  

}

#-------------------------------------------------------------------------------

# Define map function: 
plot_map <- function(data, col, title, metric, outline_sf,
                     palette = "Zissou1", n = 100, limits = c(0, 100),
                     add_labels = FALSE) {

  p <- ggplot() +
    # fill the states by your chosen variable
    geom_sf(data = data,
            aes_string(fill = col),
            color = "black",
            size = 0.2) +
    # add a single black outline
    geom_sf(data = outline_sf,
            fill = NA,
            color = "black",
            size = 1) +
    # continuous palette
    scale_fill_gradientn(
      colours = wes_palette(palette, n = n, type = "continuous"),
      limits = limits,
      name  = metric) +
    labs(title = title) +
    theme_minimal() +
    theme(
      plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.caption     = element_text(hjust = 0.5),
      panel.grid       = element_blank(),
      axis.title       = element_blank(),
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.title     = element_text(hjust = 0.5),
      legend.key.width = unit(1.35, "cm"),
      legend.key.height= unit(0.6, "cm")
    )

  if (isTRUE(add_labels)) {
    
    if (!"adm2" %in% names(data)) {
      stop("When add_labels = TRUE, 'data' must contain a column named 'adm2'.")
    }

    # Build labels: "Region Name\nXX%"; no rounding, use raw values + "%"
    label_data <- data
    nm   <- as.character(label_data[["adm2"]])
    vals <- label_data[[col]]

    # Keep rows with both a name and a value
    ok <- !is.na(nm) & !is.na(vals)
    label_data <- label_data[ok, , drop = FALSE]
    label_data$.__lab__ <- paste0(as.character(label_data[["adm2"]]),
                                  "\n",
                                  label_data[[col]], "%")

    # # Prepare labels (use raw values + %)
    # label_data <- data
    # label_data$.__lab__ <- paste0(label_data[["adm2"]], "\n", label_data[[col]], "%")
    # label_data <- label_data[!is.na(label_data$.__lab__), ]

    p <- p + geom_sf_text(
      data = label_data,
      aes(label = .__lab__),
      color = "white",
      size = 4,
      fontface = "bold",
      check_overlap = TRUE
    )
  }

  p
}


# plot_map <- function(data, col, title, metric, outline_sf, palette = "Zissou1", 
#                      n = 100, limits = c(0,100)) {
#   ggplot() +
#     # fill the states by your chosen variable
#     geom_sf(data = data,
#             aes_string(fill = col),
#             color = "black",
#             size = 0.2) +
#     # add a single black outline
#     geom_sf(data = outline_sf,
#             fill = NA,
#             color = "black",
#             size = 1) +
#     # continuous palette
#     scale_fill_gradientn(
#       colours = wes_palette(palette, n = n, type = "continuous"),
#       limits = limits,
#       name  = metric) +
#     labs(title   = title) +
#     theme_minimal() +
#     theme(
#       plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
#       plot.caption     = element_text(hjust = 0.5),
#       panel.grid       = element_blank(),
#       axis.title       = element_blank(),
#       axis.text        = element_blank(),
#       axis.ticks       = element_blank(),
#       legend.position  = "bottom",
#       legend.direction = "horizontal",
#       legend.title     = element_text(hjust = 0.5),
#       legend.key.width = unit(1.35, "cm"),
#       legend.key.height= unit(0.6, "cm")
#     )
# }

#-------------------------------------------------------------------------------

# PROBABILITY OF IRON INADEQUACY:

fe_full_prob <- function(data, group1 = NULL, group2 = NULL, bio_avail = 5, adjust_afe = FALSE){
  # Function that calculates the full probabilistic model for fe
  # Default, just calculates for full population
  # Add in parameters for group 1 and/or group 2 and it will calculate for
  # each sub population
  # The resultant data frame will have a column for each sub population with the 
  # first label corresponding to groups within group1, and the second label corresponding
  # to grousp within group2
  # Also can set the bioavailability to either 5%, 10% or 15%
  
  tryCatch(
  
  if(missing(group1)&missing(group2)){
    if(adjust_afe == TRUE){data <- data %>%mutate(ai_afe = fe_supply / afe)}
    data %>% 
      # mutate(ai_afe = fe_supply / afe) %>%                   # Generating apparent iron intake variable
      mutate(prob_inad = 
               case_when(
                 bio_avail == 5 ~
                   case_when(
                     # Dividing intake distribution into probability of inadequacy catagories 
                      ai_afe <= 15 ~ "1",
                      ai_afe <= 16.7 & ai_afe > 15 ~ "0.96",
                      ai_afe <= 18.7 & ai_afe > 16.7 ~ "0.93",
                      ai_afe <= 21.4 & ai_afe > 18.7 ~ "0.85",
                      ai_afe <= 23.6 & ai_afe > 21.4 ~ "0.75",
                      ai_afe <= 25.7 & ai_afe > 23.6 ~ "0.65",
                      ai_afe <= 27.8 & ai_afe > 25.7 ~ "0.55",
                      ai_afe <= 30.2 & ai_afe > 27.8 ~ "0.45",
                      ai_afe <= 33.2 & ai_afe > 30.2 ~ "0.35",
                      ai_afe <= 37.3 & ai_afe > 33.2 ~ "0.25",
                      ai_afe <= 45.0 & ai_afe > 37.3 ~ "0.15",
                      ai_afe <= 53.5 & ai_afe > 45.0 ~ "0.08",
                      ai_afe <= 63.0 & ai_afe > 53.5 ~ "0.04",
                      ai_afe > 63 ~ "0"),
                 bio_avail == 10 ~
                   case_when(                            # Dividing intake distribution into probability of inadequacy catagories 
                     ai_afe <= 7.5 ~ "1",
                     ai_afe <= 8.4 & ai_afe > 7.5 ~ "0.96",
                     ai_afe <= 9.4 & ai_afe > 8.4 ~ "0.93",
                     ai_afe <= 10.7 & ai_afe > 9.4 ~ "0.85",
                     ai_afe <= 11.8 & ai_afe > 10.7 ~ "0.75",
                     ai_afe <= 12.9 & ai_afe > 11.8 ~ "0.65",
                     ai_afe <= 13.9 & ai_afe > 12.9 ~ "0.55",
                     ai_afe <= 15.1 & ai_afe > 13.9 ~ "0.45",
                     ai_afe <= 16.6 & ai_afe > 15.1 ~ "0.35",
                     ai_afe <= 18.7 & ai_afe > 16.6 ~ "0.25",
                     ai_afe <= 22.5 & ai_afe > 18.7 ~ "0.15",
                     ai_afe <= 26.7 & ai_afe > 22.5 ~ "0.08",
                     ai_afe <= 31.5 & ai_afe > 26.7 ~ "0.04",
                     ai_afe > 31.5 ~ "0"),
                 bio_avail == 15 ~
                   case_when(                            # Dividing intake distribution into probability of inadequacy catagories 
                     ai_afe <= 5 ~ "1",
                     ai_afe <= 5.6 & ai_afe > 5 ~ "0.96",
                     ai_afe <= 6.2 & ai_afe > 5.6 ~ "0.93",
                     ai_afe <= 7.1 & ai_afe > 6.2 ~ "0.85",
                     ai_afe <= 7.9 & ai_afe > 7.1 ~ "0.75",
                     ai_afe <= 8.6 & ai_afe > 7.9 ~ "0.65",
                     ai_afe <= 9.3 & ai_afe > 8.6 ~ "0.55",
                     ai_afe <= 10.1 & ai_afe > 9.3 ~ "0.45",
                     ai_afe <= 11.1 & ai_afe > 10.1 ~ "0.35",
                     ai_afe <= 12.4 & ai_afe > 11.1 ~ "0.25",
                     ai_afe <= 15.0 & ai_afe > 12.4 ~ "0.15",
                     ai_afe <= 17.8 & ai_afe > 15.0 ~ "0.08",
                     ai_afe <= 21.0 & ai_afe > 17.8 ~ "0.04",
                     ai_afe > 21.0 ~ "0")
          )
        )%>% 
      group_by(prob_inad ) %>%     # Counting number of observations that fall into each probability category
      summarise(
        fe_prop = n()
      ) %>% 
      ungroup() %>% 
      summarise(   
        across(-prob_inad,
              ~ sum(.x*as.numeric(prob_inad))/sum(.x)*100
        )
      ) %>% 
      pivot_longer(cols = everything()) %>% 
      rename(subpopulation = name, prev_inad = value)
  }
  else{
    if(adjust_afe == TRUE){data <- data %>%mutate(ai_afe = fe_supply / afe)}
    
      data %>%
        # mutate(ai_afe = fe_supply / afe) %>%                   # Generating apparent iron intake variable
        mutate(prob_inad = case_when(
          bio_avail == 5 ~
            case_when(                            # Dividing intake distribution into probability of inadequacy catagories 
              ai_afe <= 15 ~ "1",
              ai_afe <= 16.7 & ai_afe > 15 ~ "0.96",
              ai_afe <= 18.7 & ai_afe > 16.7 ~ "0.93",
              ai_afe <= 21.4 & ai_afe > 18.7 ~ "0.85",
              ai_afe <= 23.6 & ai_afe > 21.4 ~ "0.75",
              ai_afe <= 25.7 & ai_afe > 23.6 ~ "0.65",
              ai_afe <= 27.8 & ai_afe > 25.7 ~ "0.55",
              ai_afe <= 30.2 & ai_afe > 27.8 ~ "0.45",
              ai_afe <= 33.2 & ai_afe > 30.2 ~ "0.35",
              ai_afe <= 37.3 & ai_afe > 33.2 ~ "0.25",
              ai_afe <= 45.0 & ai_afe > 37.3 ~ "0.15",
              ai_afe <= 53.5 & ai_afe > 45.0 ~ "0.08",
              ai_afe <= 63.0 & ai_afe > 53.5 ~ "0.04",
              ai_afe > 63 ~ "0"),
          bio_avail == 10 ~
            case_when(                            # Dividing intake distribution into probability of inadequacy catagories 
              ai_afe <= 7.5 ~ "1",
              ai_afe <= 8.4 & ai_afe > 7.5 ~ "0.96",
              ai_afe <= 9.4 & ai_afe > 8.4 ~ "0.93",
              ai_afe <= 10.7 & ai_afe > 9.4 ~ "0.85",
              ai_afe <= 11.8 & ai_afe > 10.7 ~ "0.75",
              ai_afe <= 12.9 & ai_afe > 11.8 ~ "0.65",
              ai_afe <= 13.9 & ai_afe > 12.9 ~ "0.55",
              ai_afe <= 15.1 & ai_afe > 13.9 ~ "0.45",
              ai_afe <= 16.6 & ai_afe > 15.1 ~ "0.35",
              ai_afe <= 18.7 & ai_afe > 16.6 ~ "0.25",
              ai_afe <= 22.5 & ai_afe > 18.7 ~ "0.15",
              ai_afe <= 26.7 & ai_afe > 22.5 ~ "0.08",
              ai_afe <= 31.5 & ai_afe > 26.7 ~ "0.04",
              ai_afe > 31.5 ~ "0"),
          bio_avail == 15 ~
            case_when(                            # Dividing intake distribution into probability of inadequacy catagories 
              ai_afe <= 5 ~ "1",
              ai_afe <= 5.6 & ai_afe > 5 ~ "0.96",
              ai_afe <= 6.2 & ai_afe > 5.6 ~ "0.93",
              ai_afe <= 7.1 & ai_afe > 6.2 ~ "0.85",
              ai_afe <= 7.9 & ai_afe > 7.1 ~ "0.75",
              ai_afe <= 8.6 & ai_afe > 7.9 ~ "0.65",
              ai_afe <= 9.3 & ai_afe > 8.6 ~ "0.55",
              ai_afe <= 10.1 & ai_afe > 9.3 ~ "0.45",
              ai_afe <= 11.1 & ai_afe > 10.1 ~ "0.35",
              ai_afe <= 12.4 & ai_afe > 11.1 ~ "0.25",
              ai_afe <= 15.0 & ai_afe > 12.4 ~ "0.15",
              ai_afe <= 17.8 & ai_afe > 15.0 ~ "0.08",
              ai_afe <= 21.0 & ai_afe > 17.8 ~ "0.04",
              ai_afe > 21.0 ~ "0")
        )
        ) %>%
        group_by(prob_inad, {{group1}},{{group2}}) %>%     # Counting number of observations that fall into each probability category
        summarise(
        fe_prop = n()
        )%>%
        ungroup() %>%
        pivot_wider(names_from = c({{group1}},{{group2}}), names_prefix = "", #this can be the name of the 'group by" group

                values_from = fe_prop)         %>%
        mutate(across(everything(),
              ~replace_na(.,0)))        %>%

        summarise(
          across(-prob_inad,
               ~sum(.x*as.numeric(prob_inad))/sum(.x)*100
          )
        ) %>%
        pivot_longer(cols = everything()) %>%
        rename(subpopulation = name, prev_inad = value)
    

  }
  )
    
}
