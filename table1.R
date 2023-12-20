# plsc536 - final project: replicating Dell (2010)  - joshua bailey

# table 1

library(haven)
library(broom)
library(gt)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lmtest)
library(sandwich)

# Load data
gis_grid <- read_dta("data/gis_grid.dta")
consumption <- read_dta("data/consumption.dta")
spec_check1572 <- read_dta("data/spec_check1572.dta")
budget1572 <- read_dta("data/budget1572.dta")


# Compute summary statistics and robust standard errors
compute_summary_simple <- function(data, variable, distance, mita_var = "pothuan_mita", weight_var = NULL, cusco_var = "cusco") {
  # Filter data based on distance and cusco_var if present
  if (cusco_var %in% names(data)) {
    data <- filter(data, d_bnd < distance, !!sym(cusco_var) != 1)
  } else {
    data <- filter(data, d_bnd < distance)
  }
  
  # Compute means
  data_grouped <- data |> 
    group_by(!!sym(mita_var)) |> 
    summarise(mean_value = mean(!!sym(variable), na.rm = TRUE)) |>
    pivot_wider(names_from = !!sym(mita_var), values_from = mean_value)
  
  # Regression with robust standard errors
  formula <- as.formula(paste(variable, "~", mita_var))
  fit <- lm(formula, data = data, weights = if (!is.null(weight_var) && weight_var %in% names(data)) data[[weight_var]] else NULL)
  robust_se <- sqrt(diag(vcovHC(fit, type = "HC3")))[2]
  
  # Combine results
  c(data_grouped, "s.e." = robust_se)
}

# Bandwidths
distances <- c(100, 75, 50, 25)

# Repeat the process for other datasets and variables
elev_results <- map_df(distances, ~compute_summary_simple(gis_grid, "elev", .x), .id = "distance")
slope_results <- map_df(distances, ~compute_summary_simple(gis_grid, "slope", .x), .id = "distance")
quechua_results <- map_df(distances, ~compute_summary_simple(consumption, "QUE", .x), .id = "distance")
tribute_results <- map_df(distances, ~compute_summary_simple(spec_check1572, "ltrib_rate", .x, weight_var = "trib_pop"), .id = "distance")
priest_results <- map_df(distances, ~compute_summary_simple(budget1572, "sh_priest", .x), .id = "distance")
justice_results <- map_df(distances, ~compute_summary_simple(budget1572, "sh_justice", .x), .id = "distance")
cacique_results <- map_df(distances, ~compute_summary_simple(budget1572, "sh_cacique", .x), .id = "distance")
extract_results <- map_df(distances, ~compute_summary_simple(budget1572, "sh_extract", .x), .id = "distance")

# Combine results
results_list <- list(elev_results = elev_results,
                     slope_results = slope_results,
                     quechua_results = quechua_results,
                     tribute_results = tribute_results,
                     priest_results = priest_results,
                     justice_results = justice_results,
                     cacique_results = cacique_results,
                     extract_results = extract_results)

results_list


# Prepare each dataset
prepare_data <- function(df, variable_name) {
  df |>
    pivot_longer(cols = c(`0`, `1`, s.e..pothuan_mita), 
                 names_to = "category", 
                 values_to = "value") |>
    mutate(
      variable = variable_name,
      distance = case_when(
        distance == "1" ~ "<100 km",
        distance == "2" ~ "<75 km",
        distance == "3" ~ "<50 km",
        distance == "4" ~ "<25 km"
      ),
      category = case_when(
        category == "0" ~ "Outside",
        category == "1" ~ "Inside",
        category == "s.e..pothuan_mita" ~ "s.e."
      )
    ) |>
    pivot_wider(names_from = c(distance, category), values_from = value) |>
    rename_with(~ gsub(" ", "_", .x), starts_with("<"))
}

variable_names <- c(
  "Elevation", "Slope", "% Indigenous", "Log 1572 tribute rate",
  "Spanish Priests", "Spanish Justices", "Caciques", "Resource Extraction"
)

prepared_data <- map2(results_list, variable_names, ~prepare_data(.x, .y)) |> 
  bind_rows()

# Create the table
gt_table <- prepared_data |>
  gt() |>
  tab_header(title = "Summary Statistics") |>
  cols_label(variable = "Variable") |>
  fmt_number(
    columns = vars(matches("km_")),
    decimals = 2
  ) |>
  tab_spanner(
    label = "<100 km from Mita Boundary",
    columns = vars(`<100_km_Inside`, `<100_km_Outside`, `<100_km_s.e.`)
  ) |>
  tab_spanner(
    label = "<75 km from Mita Boundary",
    columns = vars(`<75_km_Inside`, `<75_km_Outside`, `<75_km_s.e.`)
  ) |>
  tab_spanner(
    label = "<50 km from Mita Boundary",
    columns = vars(`<50_km_Inside`, `<50_km_Outside`, `<50_km_s.e.`)
  ) |>
  tab_spanner(
    label = "<25 km from Mita Boundary",
    columns = vars(`<25_km_Inside`, `<25_km_Outside`, `<25_km_s.e.`)
  )

gt_table

latex_table <- as_latex(gt_table)

write(latex_table, "table1.tex")

