# create needed environment with loading packages
wants <- c("openxlsx",
           "drc", # needed for fit
           "dplyr",
           "Metrics", # needed for RMSE
           "tidyverse"
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

boxplot(fit_pars[, -1])
test <- fit_pars[-which.max(fit_pars$EC50),]
boxplot(test[, -1])
# very high EC50 values
hist(fit_pars$EC50)
less_EC50 <- fit_pars[which(fit_pars$EC50 < 10, fit_pars$EC50),]
hist(less_EC50$EC50)
# !!! EC50 has to be compared for each ligand separately!!!
boxplot(less_EC50$EC50)
boxplot(fit_pars$Hill_slope)
boxplot(fit_pars$RMSE)
boxplot(fit_pars$MAE)

# consider getting residuals from fit
residuals(model)