# plsc536 - final project: replicating Dell (2010)  - joshua bailey

# figure

library(sf)
library(ggplot2)
library(tidyverse)
library(haven)
library(broom)
library(fixest)
library(dplyr)
library(gt)
library(rdd)

# Load the data
consumption <- read_dta("data/consumption.dta")
data_50km <- filter(consumption, cusco != 1 & d_bnd < 50)
data_100km <- filter(consumption, cusco != 1 & d_bnd < 100)

# Fit the models
model_linear <- lm(lhhequiv ~ bnd_dist_neg + pothuan_mita + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2 + bfe4_3, data = data_50km)
model_quad <- lm(lhhequiv ~ pothuan_mita + dbnd_sh + dbnd_sh2 + dbnd_sh3 + infants + children + adults + elv_sh + slope + bfe4_1 + bfe4_2 + bfe4_3, data = data_50km)

# Extract coefficients
linear_effect <- coef(model_linear)["pothuan_mita"]
quad_effect <- coef(model_quad)["pothuan_mita"]

# Create the RD plot 
rd_plot <- ggplot(data_50km, aes(x = bnd_dist_neg, y = lhhequiv)) +
  geom_point(aes(color = factor(bnd_dist_neg < 0)), alpha = 0.5) +  # Using bnd_dist_neg to determine pre- or post-treatment
  geom_smooth(data = subset(data_50km, bnd_dist_neg < 0), method = "lm", formula = y ~ x, aes(color = "Linear"), se = FALSE, linetype = "solid") +
  geom_smooth(data = subset(data_50km, bnd_dist_neg >= 0), method = "lm", formula = y ~ x, aes(color = "Linear"), se = FALSE, linetype = "solid") +
  geom_smooth(data = subset(data_50km, bnd_dist_neg < 0), method = "lm", formula = y ~ poly(x, 3), aes(color = "Cubic"), se = FALSE, linetype = "dashed") +
  geom_smooth(data = subset(data_50km, bnd_dist_neg >= 0), method = "lm", formula = y ~ poly(x, 3), aes(color = "Cubic"), se = FALSE, linetype = "dashed") +
  geom_smooth(data = subset(data_50km, bnd_dist_neg < 0), method = "lm", formula = y ~ poly(x, 2), aes(color = "Quadratic"), se = FALSE, linetype = "dotdash") +
  geom_smooth(data = subset(data_50km, bnd_dist_neg >= 0), method = "lm", formula = y ~ poly(x, 2), aes(color = "Quadratic"), se = FALSE, linetype = "dotdash") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red1") +
  annotate("text", x = Inf, y = Inf, label = paste("Linear Effect: ", round(linear_effect, 3), "\nCubic Effect: ", round(quad_effect, 3)), hjust = 1.1, vjust = 1.1) +
  scale_color_manual(values = c("Linear" = "navy", "Cubic" = "blueviolet", "Quadratic" = "dodgerblue", "TRUE" = "steelblue", "FALSE" = "tomato"),
                     breaks = c("Linear", "Cubic", "Quadratic")) +
  labs(x = "Distance to Mita Boundary", y = "Log. Equiv. Household Consumption (2001)", color = "Model") +
  ylim(3, 8) +  # Set y-axis limits
  theme_minimal() +
  theme(legend.position = "bottom")

print(rd_plot)

ggsave("rd_plot.png", rd_plot, width = 10, height = 6, dpi = 300)


