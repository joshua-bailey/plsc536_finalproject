# plsc536 - final project: replicating Dell (2010)  - joshua bailey

# extension 

# nb. Run after table2.r

library(spdep) 
library(tidyverse)
library(haven) 
library(fixest) 
library(sphet)
library(geostan)

consumption <- read_dta("data/consumption.dta")

data_50km <- filter(consumption, cusco != 1 & d_bnd < 50)

coords <- as.matrix(data_50km[, c("lon", "lat")])

# Create a neighbors list and spatial weights matrix
knn <- knn2nb(knearneigh(coords, k = 5))
weights <- nb2listw(knn, style = "W", zero.policy = TRUE)

# Function to calculate Moran's I
calculate_morans_i <- function(model, weights) {
  moran.test(residuals(model), listw = weights)
}

# Calculate Moran's I for each model
morans_i_latlong <- calculate_morans_i(model_50km_latlong, weights)
morans_i_potosi <- calculate_morans_i(model_50km_potosi, weights)
morans_i_mita <- calculate_morans_i(model_50km_mita, weights)


# Create a table
results_table <- tibble(
  Model = c("Latitude-Longitude Model", "Potosi Model", "Mita Model"),
  Moran_I = c(morans_i_latlong$estimate["Moran I statistic"], morans_i_potosi$estimate["Moran I statistic"], morans_i_mita$estimate["Moran I statistic"]),
  P_value = c(morans_i_latlong$p.value, morans_i_potosi$p.value, morans_i_mita$p.value),
  Expected = c(morans_i_latlong$estimate["Expectation"], morans_i_potosi$estimate["Expectation"], morans_i_mita$estimate["Expectation"])
)

results_table

# Format the table
gt_table <- results_table |>
  gt() |>
  tab_header(
    title = "Moran's I Test Results for Spatial Autocorrelation"
  ) |>
  cols_label(
    Model = "Model",
    Moran_I = "Moran's I",
    P_value = "P-value",
    Expected = "Expected Value"
  ) |>
  fmt_number(
    columns = c(Moran_I, P_value, Expected),
    decimals = 3
  )

latex_file_path <- "moran_i_results.tex"
gtsave(gt_table, latex_file_path)


# Generate the Moran plot

# Standardize the variable of interest
standardized_values <- scale(data_50km$lhhequiv)

# Create spatially lagged values
lagged_values <- lag.listw(weights, standardized_values)

plot_data <- data.frame(Standardized = standardized_values, Lagged = lagged_values)

# Generate the plot
ggplot(plot_data, aes(x = Standardized, y = Lagged)) +
  geom_point(color = "blue", alpha = 0.6) +  # Set point color and transparency
  geom_smooth(method = "lm", color = "red") +  # Add linear regression line
  theme_minimal() +
  labs(title = "",
       x = "Equalised Household Consumption (Standardised)",
       y = "Spatially Lagged Values") +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    text = element_text(size = 12)  # Adjust text size
  )
