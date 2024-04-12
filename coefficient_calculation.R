#### create needed environment with loading packages ####
wants <- c("openxlsx",
           "dplyr",
           "tidyr",
           "ggplot2",
           "writexl",
           "stringr"
)
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
lapply(wants, require, character.only = TRUE)

#### functions for fingerprint data normalisation ####
# Function to calculate mean signals
calculate_mean_signals <- function(data, signal_column) {
  data %>%
    group_by(cell_background, bArr, GPCR, FlAsH) %>%
    summarise(mean_signal = mean({ { signal_column } }, na.rm = TRUE), .groups = 'drop') %>%
    as.data.frame() # Ensure output is a data frame
}

# Function to identify maximal (= minimal value since they are negative) FlAsH level mean signals for each condition
find_max_flash_means <- function(mean_signals) {
  mean_signals <- as.data.frame(mean_signals)

  # Group by conditions
  max_flash_means <- mean_signals %>%
    group_by(cell_background, bArr, GPCR) %>%
    # Add a rank column based on mean_signal, resolve ties by taking the first occurrence
    mutate(rank = rank(mean_signal, ties.method = "first")) %>%
    # Filter to keep only the rows with the highest mean signal or the first if all are equal
    filter(rank == 1) %>%
    # Remove the rank column, it's no longer needed
    select(-rank) %>%
    ungroup() %>%
    as.data.frame()  # Ensure output remains a data frame
  return(max_flash_means)
}

# Function to normalize signals and add norm_to column
normalize_flash_signals <- function(data, max_flash_means) {
  normalized_data <- data %>%
    left_join(max_flash_means, by = c("cell_background", "bArr", "GPCR")) %>%
    select(-ID) %>% # remove this column, not needed
    rename(
      max_FlAsH = FlAsH.y,
      FlAsH = FlAsH.x,
      max_signal = mean_signal
    ) %>%
    mutate(normalized_signal =
             ifelse(max_signal == 0, 0, # if max_signal is 0, normalized_signal = 0
                    signal / max_signal)) # else
  return(normalized_data)
}

# Main function to run the normalization process
normalize_flash_data <- function(data) {
  # first calculate mean of each replicates
  mean_signals <- calculate_mean_signals(data, signal)
  # indentify F position with max signal for each GPCR/bArr/cell_background combination
  max_flash_means <- find_max_flash_means(mean_signals)
  # normalize replicates to the max mean
  normalized_data <- normalize_flash_signals(data, max_flash_means)
  return(normalized_data)
}

#### functions for assay data normalisation ####
find_max_assay_means <- function(data) {
  data <- as.data.frame(data)

  max_signals_per_experiment <- data %>%
    group_by(experiment) %>%
    # if later cell_background should be considered for normalisation, add cell_background to this group_by()
    summarize(max_signal = max(signal, na.rm = TRUE), .groups = 'drop')

  return(max_signals_per_experiment)
}

normalize_assay_signals <- function(data, max_signals_per_experiment) {
  normalized_data <- data %>%
    left_join(max_signals_per_experiment, by = c("experiment")) %>%
    mutate(mean_signal = ifelse(max_signal == 0, 0, signal / max_signal)) # %>%
    # select(-max_signal)  # Removing the max_signal column after normalization

  return(normalized_data)
}

normalize_assay_data <- function(data) {
  # Identify the max signal for each experiment
  max_assay_means <- find_max_assay_means(data)
  print(max_assay_means)

  # Normalize signals to the max mean within each experiment
  normalized_data <- normalize_assay_signals(data, max_assay_means)

  return(normalized_data)
}

#### import data ####
# tower PC path
path <- r"(C:\Users\monar\Google Drive\Arbeit\homeoffice\231119_EM_PROGRAM_newdata)"
# laptop path
path <- r"(C:\Users\marli\Desktop\231119_EM_PROGRAM_newdata)"
setwd(path)

# fingerint data
replicates_data_filtered <- as.data.frame(readxl::read_xlsx("Replicates_Filtered_SN_Master.xlsx"))
# remaining assays
assays_data <- as.data.frame(readxl::read_xlsx("240402_data-Fig6-notCC_for-MR_corr.xlsx"))

#### data normalisation ####
# calculate mean of normalised replicates of conformational change fingerprint data
CC_mean_norm_data <- calculate_mean_signals(CC_norm_data, normalized_signal)
# normalisation of assay data
norm_assay_data <- normalize_assay_data(assays_data)

# export normalised fingerprint data as replicates and mean
CC_norm_data <- normalize_flash_data(replicates_data_filtered)
write_xlsx(CC_norm_data, "CC_normalised_data_replicates.xlsx")
write_xlsx(CC_mean_norm_data, "CC_mean_normalised_data.xlsx")
# also export mean of non normalised data for later explanation of normalisation
CC_mean_NOTnorm_data <- calculate_mean_signals(CC_norm_data, signal)
write_xlsx(CC_mean_NOTnorm_data, "CC_mean_NOTnormalised_data.xlsx")
# export normalised assay data
write_xlsx(norm_assay_data, "Normalised_assay_data.xlsx")

#### functions to calculate differences between GPCRs ####
collect_differences <- function(data_subset) {
  GPCRs <- sort(unique(data_subset$GPCR)) # sort alphabetically to prevent differing order of levels
  # create GPCR combinations to name differences vector
  GPCR_combinations <- sapply(
    combn(GPCRs, 2, simplify = FALSE),
    paste, collapse = "_")

  differences_for_one_position <- c()
  for (comparison in GPCR_combinations) {
    differences_for_one_position <- c(differences_for_one_position,
                                      calculate_differences(data_subset, comparison))
  }
  names(differences_for_one_position) <- GPCR_combinations
  return(differences_for_one_position)
}

calculate_differences <- function(subset, comparison) {
  GPCRs <- strsplit(comparison, "_")
  # strsplit creates a list, thus [[1]][1] is needed to address single GPCRs
  diff <- subset[
    subset$GPCR == { { GPCRs[[1]][1] } },]$mean_signal -
    subset[subset$GPCR == { { GPCRs[[1]][2] } },]$mean_signal
  return(diff)
}

# function to calculate differences between GPCRs for each subset of data
apply_diff_calculation <- function(data) {
  # get factors of data
  all_factors <- names(data)
  # factor apart from GPCR or mean_signal
  factors <- all_factors[!all_factors %in% c("mean_signal", "GPCR")]

  # Split data into subsets based on the factors above
  data_subsets <- data %>%
    group_by(across(all_of(factors))) %>%
    group_split()

  # Initialize an empty list to store the results
  diff_results <- list()

  # Loop over each subset and apply the collect_differences function
  for (i in seq_along(data_subsets)) {
    subset <- data_subsets[[i]]
    # Dynamically construct result_name by iterating over factors
    result_name_parts <- sapply(factors, function(factor) subset[[factor]][1])
    result_name <- paste(result_name_parts, collapse = "_")
    print(result_name)
    # Perform difference calculation and save the result with the name
    diff_results[[result_name]] <- collect_differences(subset)
  }

  return(diff_results)
}

#### calculate absolute differences between GPCRs ####
# GPCR differences in conf change fingerprint data
CC_difference_results <- apply_diff_calculation(CC_mean_norm_data)

# GPCR differences in all other assays, use normalised data
ignore_cols <- c("Error", "comment", "max_signal", "signal")
assays_difference_results <- apply_diff_calculation(norm_assay_data[, !names(norm_assay_data) %in% ignore_cols])

#### functions to calculate tail and core coefficents ####
# coefficients are calculated from absolute difference
tailcore_coeff_from_diffs <- function(diff_result) {
  # Extract the necessary pairwise differences
  V2R_b2AR <- diff_result["b2AR_V2R"]
  V2R_V2b2 <- diff_result["V2b2_V2R"]
  b2V2_b2AR <- diff_result["b2AR_b2V2"]
  V2R_b2V2 <- diff_result["b2V2_V2R"]
  V2b2_b2AR <- diff_result["b2AR_V2b2"]

  # Calculate coefficients from absolute differences
  tail_transferability_diff <- (abs(V2R_V2b2) + abs(b2V2_b2AR))
  core_transferability_diff <- (abs(V2R_b2V2) + abs(V2b2_b2AR))
  tail_core_transferabiility_diff <- (tail_transferability_diff - core_transferability_diff)
  wildtype_diff <- abs(V2R_b2AR)

  # Return a named list of the calculated coefficients
  return(list(
    tail_transferability_diff = tail_transferability_diff,
    core_transferability_diff = core_transferability_diff,
    tail_core_transferabiility_diff = tail_core_transferabiility_diff,
    wildtype_diff = wildtype_diff
  ))
}

# converts coeff list to dataframe
list_to_df <- function(coefficients_list) {
  df <- data.frame(
    experiment = names(coefficients_list),
    tail_transferability_diff = sapply(coefficients_list, function(x) x$tail_transferability_diff),
    core_transferability_diff = sapply(coefficients_list, function(x) x$core_transferability_diff),
    tail_core_transferabiility_diff = sapply(coefficients_list, function(x) x$tail_core_transferabiility_diff),
    wildtype_diff = sapply(coefficients_list, function(x) x$wildtype_diff),
    stringsAsFactors = FALSE
  )
  if (any(str_detect(df$experiment, "FlAsH"))) {
    df <- df %>%
      tidyr::separate(experiment, into = c("cell_background", "bArr", "FlAsH"), sep = "_") %>%
      mutate(across(c(cell_background, bArr, FlAsH), as.factor))
  }else {
    df <- df %>%
      tidyr::separate(experiment, into = c("experiment", "cell_background"), sep = "_") %>%
      mutate(across(c(experiment, cell_background), as.factor))
  }
  return(df)
}

apply_coeff_calculation <- function(abs_difference_results) {
  # Initialize a list to store the coefficients
  coefficients_list <- list()

  # Iterate through each set of results in difference_results
  for (name in names(abs_difference_results)) {
    # do not consider bArr1 experiments for now (b2AR condition missing)
    if (!grepl("bArr1", name)) {
      # Extract the results for each data subset and process it
      diff_result <- abs_difference_results[[name]]
      coefficients_list[[name]] <- tailcore_coeff_from_diffs(diff_result)
    }
  }

  # Convert the coefficients list to a data frame
  coefficients_df <- list_to_df(coefficients_list)

  return(coefficients_df)
}

CC_coefficients_df <- apply_coeff_calculation(CC_difference_results)
assays_coefficients_df <- apply_coeff_calculation(assays_difference_results)

# export dataframes with coefficients
write_xlsx(CC_coefficients_df, "FlAsH_core,tail_coefficients.xlsx")
write_xlsx(assays_coefficients_df, "Assays_core,tail_coefficients.xlsx")