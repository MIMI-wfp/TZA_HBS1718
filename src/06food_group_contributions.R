# Title: Tanzania HBS 2017-18 Food Group Contributions
# Author: Uchenna Agu
## ---- Packages ----
pacman::p_load(
  tidyverse,
  cowplot,
  patchwork
)

# Load required data - hhinfo, consumption data, fct and food group categorization
hhinfo <- read.csv("processed_data/tza_hbs1718_hh_information.csv") %>%
  select(hhid, afe)

consdata <- read.csv("processed_data/tza_hbs1718_food_consumption.csv") %>%
  select(hhid, item_code, quantity_100g)

fctdata <- read.csv("processed_data/tza_hbs1718_fct.csv") %>%
  select(item_code, energy_kcal)

food_grouptbl <- read.csv("processed_data/tza_hbs1718_food_groups.csv") %>%
  select(item_code, food_group)

## ---- Early aggregation ----
consenergy <- consdata %>%
  inner_join(hhinfo, by = "hhid") %>%
  mutate(quantity_100g = quantity_100g / afe) %>%
  group_by(item_code) %>%
  summarise(
    qty100g = sum(quantity_100g, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(fctdata, by = "item_code") %>%
  left_join(food_grouptbl, by = "item_code") %>%
  mutate(
    energy_kcal = energy_kcal * qty100g
  ) %>%
  collect()

## ---- Food group regrouping ----
consenergy <- consenergy %>%
  mutate(
    food_group_new = case_when(
      food_group %in% c("grains_roots_tubers") ~ "Grains, roots and tubers",
      food_group %in% c("misc") ~ "Other foods",
      food_group %in% c("nuts_seeds", "pulses") ~ "Nuts, seeds and pulses",
      food_group %in% c("other_veg", "other_fruit") ~ "Other fruits and vegetables",
      food_group %in% c("meat_poultry_fish", "eggs", "dairy") ~ "Animal source foods",
      food_group %in% c("green_leafy_veg", "vita_fruit_veg") ~ "Vitamin A rich fruits and vegetables",
      TRUE ~ "Other foods"
    )
  )

## ---- Main food group contribution ----
food_groupcontribution <- consenergy %>%
  group_by(food_group_new) %>%
  summarise(
    totalenergy_kcal = sum(energy_kcal, na.rm = TRUE),
    .groups = "drop"
  )

## ---- Overall energy ----
overallenergy <- sum(food_groupcontribution$totalenergy_kcal)

## ---- Nutritious foods definition ----
nutritious_groups <- c(
  "Nuts, seeds and pulses",
  "Other fruits and vegetables",
  "Animal source foods",
  "Vitamin A rich fruits and vegetables"
)

## ---- Collapse to Nutritious vs others ----
food_groupcontribution <- food_groupcontribution %>%
  mutate(
    food_group_plot = if_else(
      food_group_new %in% nutritious_groups,
      "Nutritious foods",
      food_group_new
    )
  ) %>%
  group_by(food_group_plot) %>%
  summarise(
    totalenergy_kcal = sum(totalenergy_kcal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(totalenergy_kcal > 0) %>%
  mutate(
    energypct = 100 * totalenergy_kcal / overallenergy
  ) %>%
  arrange(desc(energypct))

food_groupcontribution$food_group_plot <-
  factor(food_groupcontribution$food_group_plot,
         levels = rev(food_groupcontribution$food_group_plot))

## ---- Main palette ----
mainpalette <- c(
  "Grains, roots and tubers" = "#E69F00",
  "Other foods"             = "#999999",
  "Nutritious foods"        = "#009E73"
)

## ---- Main plot ----
mainplot <- ggplot(
  food_groupcontribution,
  aes(x = 1, y = energypct, fill = food_group_plot)
) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(
    aes(label = paste0(round(energypct, 0), "%")),
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = mainpalette) +
  labs(x = NULL, y = NULL, fill = NULL) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(linewidth = 0.5),
    legend.position = "top"
  )

## ---- Nutritious foods breakdown ----
nutritiousbreakdown <- consenergy %>%
  filter(food_group_new %in% nutritious_groups) %>%
  group_by(food_group_new) %>%
  summarise(
    totalenergy_kcal = sum(energy_kcal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    energypct = 100 * totalenergy_kcal / overallenergy
  ) %>%
  arrange(desc(energypct))

nutritiousbreakdown$food_group_new <-
  factor(nutritiousbreakdown$food_group_new,
         levels = rev(nutritiousbreakdown$food_group_new))

nutritiouscolors <- colorRampPalette(
  c("#D9F0E6", "#009E73")
)(length(levels(nutritiousbreakdown$food_group_new)))
names(nutritiouscolors) <- levels(nutritiousbreakdown$food_group_new)

## ---- Breakdown plot ----
breakdownplot <- ggplot(
  nutritiousbreakdown,
  aes(x = 1, y = energypct, fill = food_group_new)
) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(
    aes(label = paste0(round(energypct, 0), "%")),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = nutritiouscolors) +
  labs(x = NULL, y = NULL, fill = NULL) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(linewidth = 0.5),
    legend.position = "bottom"
  )

## ---- Combine plots ----
combined <- plot_grid(
  mainplot,
  plot_grid(NULL, breakdownplot, NULL, ncol = 3, rel_widths = c(1, 4, 1)),
  ncol = 1,
  rel_heights = c(0.8, 0.5)
)

combined

# Add Caption if needed
final_plot <- ggdraw() +
  draw_plot(combined) +
  draw_label(
    "Other Foods: Edible Oils, Sugar, Condiments etc.",
    x = 0.5,
    y = 0.02,
    hjust = 0.5,
    vjust = 0.8,
    size = 8,
    fontface = "italic"
  )

final_plot

ggsave("tza hbs1718 energy contributions.png",
       plot = final_plot,
       width = 9, height = 6, units = "in", dpi = 300,
       bg = "white")



