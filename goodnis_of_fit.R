# create needed environment with loading packages
wants <- c("openxlsx",
           "drc", # needed for fit
           "dplyr",
           "Metrics", # needed for RMSE
           "tidyverse",
           "writexl"
)

has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
lapply(wants, require, character.only = TRUE)

# Function to fit the data
fit_data <- function(curr_data) {
  # Try fitting the curve and catch errors
  # (needed as, in the case that no fit can be calculated error message stops plotting)
  tryCatch({
    drm(signal ~ ligand_conc,
        data = curr_data,
        logDose = 10, # as provided ligand conc are in log10
        robust = 'mean',
        na.action = 'na.omit',
        fct = LL.4(names = c("Hill slope", "Min", "Max", "EC50")))
  }, error = function(e) {
    return(NULL)
  })
}

# Function to create a base plot
create_base_plot <- function(curr_data, experiment) {
  ggplot(curr_data, aes(x = ligand_conc, y = signal)) +
    geom_point(size = 2) + # Plot the actual data points
    stat_summary(fun = mean,
                 geom = "point",
                 aes(group = 1),
                 colour = "red",
                 size = 4) +  # Plot mean of replicates
    labs(x = "Ligand Concentration", y = "Signal", title = experiment) +
    theme_minimal()
}

# Function to add fit line to the plot
add_fit_line <- function(plot, fit_attempt, curr_data) {
  # Get x-values to predict y from
  lig_range <- unique(curr_data$ligand_conc)
  # Create smaller intervalls of x values fo rsmoother line
  lig_predict <- data.frame(ligand_conc = seq(min(lig_range),
                                              max(lig_range),
                                              0.1))
  # Prediction
  lig_predict$predicted <- predict(fit_attempt, newdata = lig_predict)
  plot + geom_line(data = lig_predict,
                   aes(x = ligand_conc,
                       y = predicted),
                   color = "red")
}

# Function to extract fit parameters and calculate RMSE and MAE
extract_fit_pars <- function(fit_attempt, curr_data, experiment) {
  # Get actual and prediced values
  actual_values <- curr_data$signal
  predicted_values <- predict(fit_attempt, newdata = data.frame(ligand_conc = curr_data$ligand_conc))

  # Calculate RMSE and MAE
  rmse_value <- rmse(actual_values, predicted_values)
  mae_value <- mae(actual_values, predicted_values)

  data.frame(
    experiment = experiment,
    Hill_slope = fit_attempt$fit$par[1],
    EC50 = fit_attempt$fit$par[4],
    RMSE = rmse_value,
    MAE = mae_value
  )
}

# Main function to process each grouped dataset
process_dataset <- function(data) {
  # Initialize an empty list to store fit parameters
  fit_pars_list <- list()

  # fit_pars <- data_frame()

  data %>%
    group_by(GPCR, bArr, cell_background, FlAsH) %>%
    group_walk(~{
      # Print out the current dataset factors
      # cat("Plotting dataset with factors:\n")
      # cat("GPCR:", .y$GPCR, "\n")
      # cat("bArr:", .y$bArr, "\n")
      # cat("cell_background:", .y$cell_background, "\n")
      # cat("FlAsH:", .y$FlAsH, "\n\n")

      # Define the current dataset
      curr_data <- .x
      # Experiment name for base plot
      experiment <- paste(.y$GPCR, .y$bArr, .y$cell_background, .y$FlAsH, sep = " - ")
      print(experiment)
      fit_attempt <- fit_data(curr_data)
      # Create the base plot
      if (!is.null(fit_attempt)) {
        plot <- create_base_plot(curr_data, experiment)
        plot <- add_fit_line(plot, fit_attempt, curr_data)
        temp_pars <- extract_fit_pars(fit_attempt, curr_data, experiment)
        fit_pars_list <<- append(fit_pars_list, list(temp_pars))
      } else {
        print("NO FIT")
        plot <- create_base_plot(curr_data, paste(experiment, "-- Fit could not be matched"))
      }

      # Save the plot to a PNG file
      ggsave(paste0(experiment, ".png"), plot = plot, width = 7, height = 5)
      # dev.off()
    })
  fit_pars <- bind_rows(fit_pars_list)
  return(fit_pars)
}


setwd(r"(C:\Users\monar\Google Drive\Arbeit\homeoffice\230918_EM_PROGRAM)")

# Load data
data <- readxl::read_xlsx("Master_reformat.xlsx")

# Call the main function
fit_pars <- process_dataset(data)

### plot parameters

# Function to extract outliers
extract_outliers <- function(data, column_name, use_log10 = FALSE) {
  # Helper function to identify outliers
  identify_outliers <- function(values) {
    # Calculate the IQR using the possibly transformed values
    Q1 <- quantile(values, 0.25)
    Q3 <- quantile(values, 0.75)
    IQR <- Q3 - Q1

    # Define the lower and upper bounds for outliers
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR

    print(paste("lower bound is", lower_bound))
    print(paste("upper bound is", upper_bound))

    # Identify outliers
    outliers <- data[(values < lower_bound) | (values > upper_bound),]

    # Select only the 'experiment' column
    outlier_experiments <- outliers[["experiment"]]

    return(outlier_experiments)
  }

  # Check the use_log10 flag and transform values if set to TRUE
  values <- if (use_log10) log10(data[[column_name]]) else data[[column_name]]

  # Identify outliers
  outliers <- identify_outliers(values)

  return(outliers)
}

# Function to remove outliers from a specified data frame
remove_outliers <- function(data, outliers) {
  # Find the set difference between the 'experiment' values in the fir_par df and the outlier df
  non_outlier_experiments <- setdiff(data$experiment, outliers)

  # Filter the data to keep only the non-outlier experiments
  non_outlier_data <- data[data$experiment %in% non_outlier_experiments,]

  return(non_outlier_data)
}

# Hill Slope outliers
outliers_HS <- extract_outliers(fit_pars, "Hill_slope")
# bounds are -3.255 and 5.432
# 33 outliers

# RMSE outliers
outliers_RMSE <- extract_outliers(fit_pars, "RMSE")
# bounds are -6.47 and 28.922
# 11 outliers

# MAE outliers
outliers_MAE <- extract_outliers(fit_pars, "MAE")
# bounds are -4.33 and 22.486
# 7 outliers

# EC50 outliers have to be detected separately for each core
b2_pars <- fit_pars[str_starts(fit_pars$experiment, "b2"),]
V2_pars <- fit_pars[str_starts(fit_pars$experiment, "V2"),]

outliers_b2EC50 <- extract_outliers(b2_pars, "EC50")
# bounds are -0.551 and 0.953
# 20 outliers

# no repeat with log10 EC50
outliers_logb2EC50 <- extract_outliers(b2_pars, "EC50", use_log10 = TRUE)
# bounds are -4.117 and 1.812
# 5 outliers

outliers_V2EC50 <- extract_outliers(V2_pars, "EC50")
# bounds are -0.636 and 1.117
# 22 outliers

# no repeat with log10 EC50
outliers_logV2EC50 <- extract_outliers(V2_pars, "EC50", use_log10 = TRUE)
# bounds are -3.675 and 1.66
# 17 outliers

# only 3 experiments which outliers in Hill_slop and EC50
common_outliers_HS_EC50 <- intersect(outliers_HS, c(outliers_b2EC50, outliers_V2EC50))

boxplot(fit_pars$Hill_slope)

b2_no_outliers <- remove_outliers(b2_pars, outliers_b2EC50)
V2_no_outliers <- remove_outliers(V2_pars, outliers_V2EC50)
boxplot(b2_no_outliers$EC50)
boxplot(V2_no_outliers$EC50)

# Function to add "_outlier" columns to fit_pars
add_outlier_columns <- function(fit_pars, outliers_list) {

  # Get parameter names from the names of the outliers_list
  parameter_names <- names(outliers_list)

  for (i in seq_along(outliers_list)) {
    # Create the column name
    column_name <- paste0(parameter_names[i], "_outlier")

    # Check if each experiment is an outlier for the current parameter
    fit_pars[[column_name]] <- fit_pars$experiment %in% outliers_list[[i]]
  }

  return(fit_pars)
}

# List of outlier experiments for each parameter
outliers_list <- list(outliers_HS,
                      c(outliers_V2EC50, outliers_b2EC50),
                      c(outliers_V2EC50, outliers_logb2EC50),
                      outliers_RMSE,
                      outliers_MAE)
names(outliers_list) <- c("Hill_slope", "EC50", "logEC50", "RMSE", "MAE")

# Add "_outlier" columns to fit_pars
fit_pars <- add_outlier_columns(fit_pars, outliers_list)
write_xlsx(fit_pars, "Fit_parameters.xlsx")


###
boxplot(fit_pars[, -1])
test <- fit_pars[-which.max(fit_pars$EC50),]
boxplot(test[, -1])
# very high EC50 values
hist(fit_pars$EC50)
less_EC50 <- fit_pars[which(fit_pars$EC50 < 10, fit_pars$EC50),]
hist(log(less_EC50$EC50))
# !!! EC50 has to be compared for each ligand separately!!!
boxplot(less_EC50$EC50)
boxplot(fit_pars$Hill_slope)
boxplot(fit_pars$RMSE)
boxplot(fit_pars$MAE)

# consider getting residuals from fit
residuals(model)