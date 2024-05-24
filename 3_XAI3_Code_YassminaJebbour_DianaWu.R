setwd(".")

list.of.packages <- c("reshape2", "dplyr", "caret", "ggplot2", "tidyr", "ROCR", "plotly", "randomForest", "party", "pdp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=TRUE)

kc <- read.csv("kc_house_data.csv")

# Set the random seed for reproducibility
set.seed(250)

# Define the features to be used in the model
features <- c("bedrooms", "bathrooms", "sqft_living", "sqft_lot", "floors", "yr_built")

# Sample 20% of the data for model training
kc_sample <- kc[sample(nrow(kc), size = 0.2 * nrow(kc)), ]

# Build the Random Forest model
set.seed(250)
rf_model <- randomForest(price ~ ., data = kc_sample, importance = TRUE)

# Define the function to generate the PDP plot
generate_pdp_plot <- function(model, pred_var, var_name, train_data, y_name = "Predicted price", grid_resolution = 20) {
  # Generate Partial Dependency Plot (PDP) data for the specified predictor variable
  pdp <- partial(model, pred.var = pred_var, train = train_data, grid.resolution = grid_resolution)
  # Convert PDP data to a data frame
  pdp_df <- as.data.frame(pdp)
  # Create the PDP plot
  pdp_plot <- ggplot(pdp_df) +
    geom_line(aes_string(x = pred_var, y = "yhat"), color = "black") +
    geom_rug(data = train_data, aes_string(x = pred_var), sides = "b") +
    theme_minimal() +
    labs(title = paste("Partial Dependence Plot:", var_name),
         x = pred_var, y = y_name)
  return(pdp_plot)
}

# Generate PDP plot for bedrooms
pdp_plot_bedrooms <- generate_pdp_plot(rf_model, "bedrooms", "Bedrooms", kc_sample)
print(pdp_plot_bedrooms)

# Generate PDP plot for bathrooms
pdp_plot_bathrooms <- generate_pdp_plot(rf_model, "bathrooms", "Bathrooms", kc_sample)
print(pdp_plot_bathrooms)

# Generate PDP plot for sqft_living
pdp_plot_sqft_living <- generate_pdp_plot(rf_model, "sqft_living", "Sqft Living Room", kc_sample)
print(pdp_plot_sqft_living)

# Generate PDP plot for sqft_lot
pdp_plot_sqft_lot <- generate_pdp_plot(rf_model, "sqft_lot", "Sqft Lot", kc_sample)
print(pdp_plot_sqft_lot)

# Generate PDP plot for floors
pdp_plot_floors <- generate_pdp_plot(rf_model, "floors", "Floors", kc_sample)
print(pdp_plot_floors)

# Generate PDP plot for yr_built
pdp_plot_yr_built <- generate_pdp_plot(rf_model, "yr_built", "Year", kc_sample)
print(pdp_plot_yr_built)
