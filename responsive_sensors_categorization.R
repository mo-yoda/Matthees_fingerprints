#### create needed environment with loading packages ####
wants <- c("openxlsx",
           "dplyr",
           "writexl"
)
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
lapply(wants, require, character.only = TRUE)

#### Set filter borders ####
# tower PC path
path <- r"(C:\path\to\folder)"
# laptop path
path <- r"(C:\path\to\folder)"
setwd(path)

# Load data
path_to_fit_paras <- paste0(path, r"(\start_normalised\)", "categories")
setwd(path_to_fit_paras)
fit_pars_og <- readxl::read_xlsx("Fit_parameters_classes.xlsx")

# constrain function
add_constraint_column <- function(df, hill_lower, ec50_upper, ec50_lower) {
  # Create the logical column
  col_name <- paste0("absHS_gt_", hill_lower, "_EC50_btwn_", ec50_lower, "_and_", ec50_upper)
  df[[col_name]] <- with(df, absolute_Hill_slope > 10^hill_lower &
    EC50 > 10^ec50_lower &
    EC50 < 10^ec50_upper)

  # Return the modified dataframe
  return(df)
}

# experiments with absolute_Hill_slope >10^-1 and 10^-3.0 > EC50 < 10^0.3
modified_data <- add_constraint_column(fit_pars_og, -1, 0.3, -3.0)
write_xlsx(modified_data, "Fit_parameters_classes_filtered.xlsx")

#### Apply filter ####
# tower PC path
path <- r"(C:\Users\monar\Google Drive\Arbeit\homeoffice\231119_EM_PROGRAM_newdata)"
# laptop path
path <- r"(C:\Users\marli\Desktop\231119_EM_PROGRAM_newdata)"
setwd(path)

# Load data
import_file <- "Master_SN_reformat.xlsx"
data <- readxl::read_xlsx(import_file)

include_data <- modified_data[
  modified_data$`absHS_gt_-1_EC50_btwn_-3_and_0.3`,]

set_unmatched_to_zero <- function(data, reference, manual_exclusion, signal_col) {
  # Create 'experiment' column for both data and reference dfs
  data$experiment <- paste(data$GPCR, data$bArr, data$cell_background, data$FlAsH, sep = "_")
  reference$experiment <- paste(reference$GPCR, reference$bArr, reference$cell_background, reference$FlAsH, sep = "_")

  # Identify rows in 'data' that are not found in 'reference'
  unmatched_data <- dplyr::anti_join(data, reference, by = "experiment")

  # If there are unmatched rows, set their mean_signal to 0
  if (nrow(unmatched_data) > 0) {
    data[data$experiment %in% unmatched_data$experiment, signal_col] <- 0
  }

  # Set mean_signal to 0 for sensors which were manually assigned as "non responder"
  if (length(manual_exclusion) > 0) {
    data[data$experiment %in% manual_exclusion, signal_col] <- 0
  }

  print("---- No. experiments set to 0 (non-responsive) ----")
  print(length(unmatched_data$experiment) + length(manual_exclusion))
  print(paste0(length(manual_exclusion), " were categorized as non-responder manually: "))
  print(manual_exclusion)

  return(data)
}

# prepare data for heatmap
prepare_matrix_data <- function(unprocessed_data, reference, manual_exclusion) {
  # Filter data for ligand_conc == 1
  subset_data <- unprocessed_data[unprocessed_data$ligand_conc == 1,]

  # Group by the combination of factors and filter groups with 3 or more values
  replicates_data <- subset_data %>%
    dplyr::group_by(GPCR, bArr, cell_background, FlAsH)

  # Calculate the mean of the signal for each group
  mean_data <- replicates_data %>%
    dplyr::summarise(mean_signal = mean(signal, na.rm = TRUE))

  print("###### No. of experiments measured #########")
  print(length(mean_data$GPCR))

  # Set signal to 0 for non-responsive conditions in mean_data and replicates_data
  filtered_data_list <- list()
  filtered_data_list[["mean_data_filtered"]] <-
    set_unmatched_to_zero(mean_data, reference, manual_exclusion, "mean_signal")
  print("-----filtered replicates-----")
  filtered_data_list[["replicates_data_filtered"]] <-
    set_unmatched_to_zero(replicates_data, reference, manual_exclusion, "signal")

  return(filtered_data_list)
}

nonResponder <- c("V2R_bArr1_dQ+GRK6_FlAsH3",
                  "b2AR_bArr2_dQ+EV_FlAsH4",
                  "b2AR_bArr2_dQ+EV_FlAsH9",
                  "b2AR_bArr2_dQ+GRK2_FlAsH9")
processed_data <- prepare_matrix_data(data, include_data, nonResponder)

write_xlsx(processed_data[[1]], "Filtered_SN_Master.xlsx")
write_xlsx(processed_data[[2]], "Replicates_Filtered_SN_Master.xlsx")
