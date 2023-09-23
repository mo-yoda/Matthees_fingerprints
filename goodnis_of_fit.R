# create needed environment with loading packages
wants <- c("openxlsx",
           "drc", # needed for fit
           "dplyr",
           "Metrics" # needed for RMSE
)

has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
lapply(wants, require, character.only = TRUE)

# office path
# setwd(r"(B:\FuL\IMZ01\Hoffmann\Personal data folders\Mona\Paper\XXX_Matthees et al\goodnis of fit test)")

# homeoffice path
setwd(r"(C:\Users\monar\Google Drive\Arbeit\homeoffice\230809_EM_Goodnis of fit)")

# use crc data with OG ligand concentration
test_df <- read.xlsx("gdf_test_no_log.xlsx")
head(test_df)

# function needed for visualization purposes
# sigmoid = function(params, x) {
#   params[1] / (1 + exp(-params[2] * (x - params[3])))
# }

data_1 <- subset(test_df, condition == "Con_V2R_bArr1-F5")
plot(signal ~ log(AVP),
     data = data_1)
# Ns are plotted, for Con_V2R_bArr1-F5 n=3

#DR curve fitting
# drm() for fitting dose-response models
DR.m <- drm(signal ~ AVP,
            data = data_1,
            # condition, #fit separate curves for each condition
            robust = 'mean', #non-robust least squares estimation ("mean")
            fct = LL.4(names = c("Hill slope", "Min", "Max", "EC50")))
print(DR.m)

plot(signal ~ AVP,
     data = data_1, log = "x")
plot(DR.m, add = TRUE, col = "red")
# fitted EC50 visually fits, tested via:
# points( 0.07778, -15, type = "p" , col = "green")
# Hill slope DR.m$fit$par[1]
# EC50  DR.m$fit$par[4]


### all data

#DR curve fitting
DR.m_all <- drm(signal ~ AVP,
                data = test_df,
                # condition, #fit separate curves for each condition
                robust = 'mean', #non-robust least squares estimation ("mean")
                fct = LL.4(names = c("Hill slope", "Min", "Max", "EC50")))
print(DR.m_all)
# works that all fits are generated at once but independent of each other (no diff. to fitting conditions separately)
plot(DR.m_all, add = TRUE, col = "red")
# -> dont know how to select model of specific dataset
# --> thus, fit single datasets!

# subset single conditions
df_1 <- subset(test_df, condition == levels(as.factor(test_df$condition))[1])
df_2 <- subset(test_df, condition == levels(as.factor(test_df$condition))[2])
df_3 <- subset(test_df, condition == levels(as.factor(test_df$condition))[3])

# select df subset
subset_df <- df_2

plot(signal ~ AVP,
     subset_df,
     log = "x",
     col = "red",
     ylim = c(min(test_df$signal), max(test_df$signal)))
DR.m_sub <- drm(signal ~ AVP,
                data = subset_df,
                # condition, #fit separate curves for each condition
                robust = 'mean', #non-robust least squares estimation ("mean"), default
                fct = LL.4(names = c("Hill slope", "Min", "Max", "EC50")),
                separate = TRUE) #LL.4 means four-parameter log-logistic model!
plot(DR.m_sub, add = TRUE)

# "test" gets ligand concentrations, needed for RMSE
test <- as.data.frame(unique(subset_df[, 1]))
# predict(DR.m_sub, newdata = test)

DR.m_sub$coefficients[4]
# EC50 by drm()
# df_1 fit 0.07778
# df_2 fit 0.3007148
# df_3 fit 2.160918
# --> df_3 much bnigger EC50 than df_1 or 2
# two step selection? -> remove EC50 > X
# -> see whether there are still curves which should be removed

DR.m_sub$coefficients[1]
# Hill by drm()
# df_1 fit 0.9609228
# df_2 fit 2.649798
# df_3 fit 0.4734726

# RMSE
rmse(subset_df$signal, predict(DR.m_sub, newdata = test))
# df_1 fit 5.426582
# df_2 fit 22.20422
# df_3 fit 15.17092
# RMSE for bad data much different to nice data!
# BUT: df_2 is a bit better than df_3 in terms of concentration-dependency
# --> this is not reflected in RSME
# prob. due to higher spread in df_3
# --> discuss with Edda how she sees this!

# MAE - mean absolute error (this is less affected by outliers than RSME)
mae(subset_df$signal, predict(DR.m_sub, newdata = test))
# df_1 fit 4.602547
# df_2 fit 17.89563
# df_3 fit 11.73606
# --> gibt sich in diesem fall nicht viel (prozentualer Vergleich)

# get residuals
residuals(DR.m_sub)


#### plan for now: ####
# input -> folder, import all .xlsx (will be exports from prism
# reformat as test_data for this script
##
# fit via drm()
# calc. rmse() via prediction()
# export Hill coeff + EC50 + RMSE (.xlsx table)
# + plot fit together with real data, export as png (use factor name as filename)
# --> use same for all exports ylims!, adjusted to data range

# then, run with all data
# -> plot Hill coeff, EC50 and RSME as boxplots
# are there outliers? how is the distribution?
# define cutoffs - what is a good fit?

####
# use MASTER data

# Import necessary libraries
library(tidyverse)
library(drc)
library(Metrics)

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
      cat("Plotting dataset with factors:\n")
      cat("GPCR:", .y$GPCR, "\n")
      cat("bArr:", .y$bArr, "\n")
      cat("cell_background:", .y$cell_background, "\n")
      cat("FlAsH:", .y$FlAsH, "\n\n")

      # Define the current dataset
      curr_data <- .x
      # Experiment name for base plot
      experiment <- paste(.y$GPCR, .y$bArr, .y$cell_background, .y$FlAsH, sep = " - ")
      print("######before attempt")
      fit_attempt <- fit_data(curr_data)
      print("after attempt#######")
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
