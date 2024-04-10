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

### functions for fingerprint data normalisation ###
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
normalize_signals <- function(data, max_flash_means) {
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
normalize_data <- function(data) {
  # first calculate mean of each replicates
  mean_signals <- calculate_mean_signals(data, signal)
  # indentify F position with max signal for each GPCR/bArr/cell_background combination
  max_flash_means <- find_max_flash_means(mean_signals)
  # normalize replicates to the max mean
  normalized_data <- normalize_signals(data, max_flash_means)
  return(normalized_data)
}

### import data ###
# tower PC path
path <- r"(C:\Users\monar\Google Drive\Arbeit\homeoffice\231119_EM_PROGRAM_newdata)"
# laptop path
path <- r"(C:\Users\marli\Desktop\231119_EM_PROGRAM_newdata)"
setwd(path)

#### fingerprint data normalisation ####
# Load data
replicates_data_filtered <- as.data.frame(readxl::read_xlsx("Replicates_Filtered_SN_Master.xlsx"))

# normalize to max flash for each GPCR
CC_norm_data <- normalize_data(replicates_data_filtered)
# calculate mean of normalised replicates
CC_mean_norm_data <- calculate_mean_signals(CC_norm_data, normalized_signal)

# export normalised fingerprint data as replicates and mean
write_xlsx(CC_norm_data, "CC_normalised_data_replicates.xlsx")
write_xlsx(CC_mean_norm_data, "CC_mean_normalised_data.xlsx")
# also export mean of non normalised data for later explanation of normalisation
CC_mean_NOTnorm_data <- calculate_mean_signals(CC_norm_data, signal)
write_xlsx(CC_mean_NOTnorm_data, "CC_mean_NOTnormalised_data.xlsx")

### functions to calculate differences between GPCRs ###
collect_differences <- function(data_subset) {
  GPCRs <- unique(data_subset$GPCR)
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
  diff <- subset[subset$GPCR == {{GPCRs[[1]][1]}},]$mean_signal - subset[subset$GPCR == {{GPCRs[[1]][2]}},]$mean_signal
  return(diff)
}

# function to calculate differences between GPCRs per FlAsH position for each subset of data
apply_diff_calculation <- function(data) {
  # Split data into subsets based on cell_background, bArr, and FlAsH
  data_subsets <- data %>%
    group_by(cell_background, bArr, FlAsH) %>%
    group_split()

  # Initialize an empty list to store the results
  diff_results <- list()

  # Loop over each subset and apply the collect_differences function
  for (i in seq_along(data_subsets)) {
    subset <- data_subsets[[i]]
    # Use the first row of each subset to generate a name for the result based on the grouping factors
    result_name <- paste(subset$cell_background[1], subset$bArr[1], subset$FlAsH[1], sep = "_")
    # Perform difference calculation and save the result with the name
    diff_results[[result_name]] <- collect_differences(subset)
  }

  return(diff_results)
}

#### calculate tail and core coefficents ####
## coefficients are calculated from absolute difference

# GPCR differences in conf change fingerprint data
CC_difference_results <- apply_diff_calculation(CC_mean_norm_data)

# Initialize a list to store the coefficients for each FlAsH position where bArr == "bArr2"
CC_coefficients_list <- list()

# Iterate through each set of results in difference_results
for (name in names(CC_difference_results)) {
  # Filter results for bArr2
  if (grepl("bArr2", name)) {
    # Extract the results for each data subset
    diff_result <- CC_difference_results[[name]]

    # Extract the necessary pairwise comparisons
    V2R_b2AR <- diff_result["V2R_b2AR"]
    V2b2_b2V2 <- diff_result["V2b2_b2V2"]
    V2R_V2b2 <- diff_result["V2R_V2b2"]
    b2V2_b2AR <- diff_result["b2AR_b2V2"]
    V2R_b2V2 <- diff_result["V2R_b2V2"]
    V2b2_b2AR <- diff_result["V2b2_b2AR"]

    # Calculate coefficients
    tail_transferability_diff <- (abs(V2R_V2b2) + abs(b2V2_b2AR))
    core_transferability_diff <- (abs(V2R_b2V2) + abs(V2b2_b2AR))
    tail_core_transferabiility_diff <- (tail_transferability_diff - core_transferability_diff)
    wildtype_diff <- abs(V2R_b2AR)

    # Store the results in the list
    CC_coefficients_list[[name]] <- list(
      tail_transferability_diff = tail_transferability_diff,
      core_transferability_diff = core_transferability_diff,
      tail_core_transferabiility_diff = tail_core_transferabiility_diff,
      wildtype_diff = wildtype_diff
    )
  }
}

# Convert the coefficients list to a data frame
CC_coefficients_df <- data.frame(
  combination = names(CC_coefficients_list),
  tail_transferability_diff = sapply(CC_coefficients_list, function(x) x$tail_transferability_diff),
  core_transferability_diff = sapply(CC_coefficients_list, function(x) x$core_transferability_diff),
  tail_core_transferabiility_diff = sapply(CC_coefficients_list, function(x) x$tail_core_transferabiility_diff),
  wildtype_diff = sapply(CC_coefficients_list, function(x) x$wildtype_diff),
  stringsAsFactors = FALSE
)

# Split the combination column into separate factors
CC_coefficients_df <- CC_coefficients_df %>%
  separate(combination, into = c("cell_background", "bArr", "FlAsH"), sep = "_") %>%
  mutate(across(c(cell_background, bArr, FlAsH), as.factor))

# export dataframe with fingerprint coefficients
write_xlsx(CC_coefficients_df, "FlAsH_core,tail_coefficients.xlsx")