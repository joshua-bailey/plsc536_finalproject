# plsc536 - final project: replicating Dell (2010)  - joshua bailey

# table 2

library(tidyverse)
library(haven)
library(broom)
library(fixest)
library(dplyr)
library(gt)
library(rdd)
library(modelsummary)
library(dplyr)

# cols 1 - 3
consumption <- read_dta("data/consumption.dta")

# Define the models
formula_latlong <- lhhequiv ~ pothuan_mita + x + y + x2 + y2 + xy + x3 + y3 + x2y + xy2 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2 + bfe4_3
formula_potosi <- lhhequiv ~ pothuan_mita + dpot + dpot2 + dpot3 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2 + bfe4_3
formula_mita <- lhhequiv ~ pothuan_mita + dbnd_sh + dbnd_sh2 + dbnd_sh3 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2 + bfe4_3

# Filter the different bandwidths
data_100km <- filter(consumption, cusco != 1 & d_bnd < 100)
data_75km <- filter(consumption, cusco != 1 & d_bnd < 75)
data_50km <- filter(consumption, cusco != 1 & d_bnd < 50)

# Run the regressions
model_100km_latlong <- feols(formula_latlong, data_100km, vcov = ~ubigeo)
model_75km_latlong <- feols(formula_latlong, data_75km, vcov = ~ubigeo)
model_50km_latlong <- feols(formula_latlong, data_50km, vcov = ~ubigeo)

model_100km_potosi <- feols(formula_potosi, data_100km, vcov = ~ubigeo)
model_75km_potosi <- feols(formula_potosi, data_75km, vcov = ~ubigeo)
model_50km_potosi <- feols(formula_potosi, data_50km, vcov = ~ubigeo)

model_100km_mita <- feols(formula_mita, data_100km, vcov = ~ubigeo)
model_75km_mita <- feols(formula_mita, data_75km, vcov = ~ubigeo)
model_50km_mita <- feols(formula_mita, data_50km, vcov = ~ubigeo)


# cols 4 -7 
height <- read_dta("data/height.dta")

# Define the model
formula_latlong_height <- desnu ~ pothuan_mita + x + y + x2 + y2 + xy + x3 + y3 + x2y + xy2 + elv_sh + slope + bfe4_1 + bfe4_2 + bfe4_3
formula_potosi_height <- desnu ~ pothuan_mita + dpot + dpot2 + dpot3 + elv_sh + slope + bfe4_1 + bfe4_2 + bfe4_3
formula_mita_height <- desnu ~ pothuan_mita + dbnd_sh + dbnd_sh2 + dbnd_sh3 + elv_sh + slope + bfe4_1 + bfe4_2 + bfe4_3

# Filter the data
data_height_100km <- filter(height, cusco != 1 & d_bnd < 100)
data_height_75km <- filter(height, cusco != 1 & d_bnd < 75)
data_height_50km <- filter(height, cusco != 1 & d_bnd < 50)
data_height_border <- filter(height, cusco != 1 & border == 1)

# Run the regressions 
model_height_100km_latlong <- feols(formula_latlong_height, data_height_100km, vcov = ~ubigeo)
model_height_75km_latlong <- feols(formula_latlong_height, data_height_75km, vcov = ~ubigeo)
model_height_50km_latlong <- feols(formula_latlong_height, data_height_50km, vcov = ~ubigeo)
model_height_border_latlong <- feols(formula_latlong_height, data_height_border, vcov = ~ubigeo)

model_height_100km_potosi <- feols(formula_potosi_height, data_height_100km, vcov = ~ubigeo)
model_height_75km_potosi <- feols(formula_potosi_height, data_height_75km, vcov = ~ubigeo)
model_height_50km_potosi <- feols(formula_potosi_height, data_height_50km, vcov = ~ubigeo)
model_height_border_potosi <- feols(formula_potosi_height, data_height_border, vcov = ~ubigeo)

model_height_100km_mita <- feols(formula_mita_height, data_height_100km, vcov = ~ubigeo)
model_height_75km_mita <- feols(formula_mita_height, data_height_75km, vcov = ~ubigeo)
model_height_50km_mita <- feols(formula_mita_height, data_height_50km, vcov = ~ubigeo)
model_height_border_mita <- feols(formula_mita_height, data_height_border, vcov = ~ubigeo)


# Generate the regression tables

# latlong
model_table_latlong <- modelsummary(
  list(model_100km_latlong, model_75km_latlong, model_50km_latlong, 
       model_height_100km_latlong, model_height_75km_latlong, 
       model_height_50km_latlong, model_height_border_latlong),
  output = "gt", 
  coef_omit = "^(?!pothuan_mita).*$", 
  coef_rename = c(pothuan_mita = "Mita"), 
  gof_omit = "^(?!R2|N).*$", 
  statistic = "std.error", 
  stars = TRUE
)

model_table_gt_latlong <- model_table_latlong |>
  gt::tab_spanner(
    label = "Log. Equiv. Household Consumption (2001)",
    columns = 2:4
  ) |>
  gt::tab_spanner(
    label = "Stunted Growth, Children 6-9 (2005)",
    columns = 5:8
  ) |>
  gt::tab_header(
    title = "Regression Results: Lat-Long."
  ) |>
  gt::tab_footnote(
    footnote = "Clustered standard errors in parentheses",
    locations = gt::cells_column_labels(columns = 2:8) 
  )

print(model_table_gt_latlong)

gt::gtsave(model_table_gt_latlong, "regression_results_table.tex")


# potosi
model_table_potosi <- modelsummary(
  list(model_100km_potosi, model_75km_potosi, model_50km_potosi, 
       model_height_100km_potosi, model_height_75km_potosi, 
       model_height_50km_potosi, model_height_border_potosi),
  output = "gt", 
  coef_omit = "^(?!pothuan_mita).*$", 
  coef_rename = c(pothuan_mita = "Mita"), 
  gof_omit = "^(?!R2|N).*$", 
  statistic = "std.error", 
  stars = TRUE
)

model_table_gt_potosi <- model_table_potosi |>
  gt::tab_spanner(
    label = "Log. Equiv. Household Consumption (2001)",
    columns = 2:4
  ) |>
  gt::tab_spanner(
    label = "Stunted Growth, Children 6-9 (2005)",
    columns = 5:8
  ) |>
  gt::tab_header(
    title = "Regression Results: Potosí"
  ) |>
  gt::tab_footnote(
    footnote = "Clustered standard errors in parentheses",
    locations = gt::cells_column_labels(columns = 2:8) 
  )

print(model_table_gt_potosi)

gt::gtsave(model_table_gt_potosi, "regression_results_table_potosi.tex")

# mita
model_table_mita <- modelsummary(
  list(model_100km_mita, model_75km_mita, model_50km_mita, 
       model_height_100km_mita, model_height_75km_mita, 
       model_height_50km_mita, model_height_border_mita),
  output = "gt", 
  coef_omit = "^(?!pothuan_mita).*$", 
  coef_rename = c(pothuan_mita = "Mita"), 
  gof_omit = "^(?!R2|N).*$", 
  statistic = "std.error", 
  stars = TRUE
)

model_table_gt_mita <- model_table_mita |>
  gt::tab_spanner(
    label = "Log. Equiv. Household Consumption (2001)",
    columns = 2:4
  ) |>
  gt::tab_spanner(
    label = "Stunted Growth, Children 6-9 (2005)",
    columns = 5:8
  ) |>
  gt::tab_header(
    title = "Regression Results: Mita"
  ) |>
  gt::tab_footnote(
    footnote = "Clustered standard errors in parentheses",
    locations = gt::cells_column_labels(columns = 2:8) 
  )

print(model_table_gt_mita)

gt::gtsave(model_table_gt_mita, "regression_results_table_mita.tex")

