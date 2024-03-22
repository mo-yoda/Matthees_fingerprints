###create needed environment with loading packages
wants <- c("openxlsx",
           "dplyr",
           "tidyr",
           "writexl",
           "multcompView" # for anova + tukey
)
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
lapply(wants, require, character.only = TRUE)

### Data processing
# tower PC path
path <- r"(C:\Users\monar\Google Drive\Arbeit\homeoffice\231119_EM_PROGRAM_newdata)"
# laptop path
# path <- r"(C:\Users\marli\Desktop\231119_EM_PROGRAM_newdata)"
setwd(path)

# Load data
replicates_data_filtered <- as.data.frame(readxl::read_xlsx("Replicates_Filtered_SN_Master.xlsx"))

# Function to calculate mean signals
calculate_mean_signals <- function(data) {
  data %>%
    group_by(cell_background, bArr, GPCR, FlAsH) %>%
    summarise(mean_signal = mean(signal, na.rm = TRUE), .groups = 'drop') %>%
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
normalize_signals <- function(data, mean_signals, max_flash_means) {
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
                    signal/max_signal)) # else
  return(normalized_data)
}

# Main function to run the normalization process
normalize_data <- function(data) {
  # first calculate mean of each replicates
  mean_signals <- calculate_mean_signals(data)
  # indentify F position with max signal for each GPCR/bArr/cell_background combination
  max_flash_means <- find_max_flash_means(mean_signals)
  # normalize replicates to the max mean
  normalized_data <- normalize_signals(data, mean_signals, max_flash_means)
  return(normalized_data)
}

norm_data <- normalize_data(replicates_data_filtered)

### ANOVA + Tukey of normalised data ###
perform_tukey <- function(data_subset){
  # anova <- aov(data_subset$signal ~ data_subset$GPCR)
  anova <- aov(data_subset$normalized_signal ~ data_subset$GPCR)
  tukey_result <- TukeyHSD(anova, 'data_subset$GPCR')
  # print(tukey_result)
  return(tukey_result)
}

# function to run the Tukey test to each subset of data
apply_tukey_tests <- function(data) {
  # Split data into subsets based on cell_background, bArr, and FlAsH
  data_subsets <- data %>%
    group_by(cell_background, bArr, FlAsH) %>%
    group_split()

  # Initialize an empty list to store the results
  tukey_results <- list()

  # Loop over each subset and apply the perform_tukey function
  for(i in seq_along(data_subsets)) {
    subset <- data_subsets[[i]]
    # Use the first row of each subset to generate a name for the result based on the grouping factors
    result_name <- paste(subset$cell_background[1], subset$bArr[1], subset$FlAsH[1], sep="_")
    # Perform Tukey test and save the result with the name
    tukey_results[[result_name]] <- perform_tukey(subset)
  }

  return(tukey_results)
}

# Apply the function to your normalized data
tukey_test_results <- apply_tukey_tests(norm_data)

### try different coefficients ###
# Initialize a list to store the coefficients for each FlAsH position where bArr == "bArr2"
coefficients_list <- list()

# Iterate through each set of results in tukey_test_results
for(name in names(tukey_test_results)) {
  # Filter results for bArr2
  if(grepl("bArr2", name)) {
    # Extract the Tukey HSD test result
    tukey_result <- tukey_test_results[[name]]$`data_subset$GPCR`

    # Extract the necessary pairwise comparisons
    V2R_b2AR <- tukey_result["V2R-b2AR", ]
    V2b2_b2V2 <- tukey_result["V2b2-b2V2", ]
    V2R_V2b2 <- tukey_result["V2R-V2b2", ]
    b2V2_b2AR <- tukey_result["b2V2-b2AR", ]
    V2R_b2V2 <- tukey_result["V2R-b2V2", ]
    V2b2_b2AR <- tukey_result["V2b2-b2AR", ]

    # Calculate coefficients
    tail_transferability_p <- (V2R_b2AR["p adj"] + V2b2_b2V2["p adj"]) + (V2R_V2b2["p adj"] + b2V2_b2AR["p adj"])
    core_transferability_p <- (V2R_b2AR["p adj"] + V2b2_b2V2["p adj"]) + (V2R_b2V2["p adj"] + V2b2_b2AR["p adj"])
    tail_transferability_diff <- (abs(V2R_V2b2["diff"]) + abs(b2V2_b2AR["diff"]))
    core_transferability_diff <- (abs(V2R_b2V2["diff"]) + abs(V2b2_b2AR["diff"]))
    # tail_transferability_diff <- (abs(V2R_b2AR["diff"]) + abs(V2b2_b2V2["diff"])) - (abs(V2R_V2b2["diff"]) + abs(b2V2_b2AR["diff"]))
    # core_transferability_diff <- (abs(V2R_b2AR["diff"]) + abs(V2b2_b2V2["diff"])) - (abs(V2R_b2V2["diff"]) + abs(V2b2_b2AR["diff"]))


    # Store the results in the list
    coefficients_list[[name]] <- list(
      tail_transferability_p = tail_transferability_p,
      core_transferability_p = core_transferability_p,
      tail_transferability_diff = tail_transferability_diff,
      core_transferability_diff = core_transferability_diff
    )
  }
}

# Convert the coefficients list to a data frame
coefficients_df <- data.frame(
  combination = names(coefficients_list),
  tail_transferability_p = sapply(coefficients_list, function(x) x$tail_transferability_p),
  core_transferability_p = sapply(coefficients_list, function(x) x$core_transferability_p),
  tail_transferability_diff = sapply(coefficients_list, function(x) x$tail_transferability_diff),
  core_transferability_diff = sapply(coefficients_list, function(x) x$core_transferability_diff),
  stringsAsFactors = FALSE
)

# Split the combination column into separate factors
coefficients_df <- coefficients_df %>%
  separate(combination, into = c("cell_background", "bArr", "FlAsH"), sep = "_") %>%
  mutate(across(c(cell_background, bArr, FlAsH), as.factor))

# View the resulting table
print(coefficients_df)

testing <- coefficients_df[coefficients_df$cell_background == "dQ+GRK2",]
plot(testing$core_transferability_diff ~ testing$FlAsH, ylim = c(0,1.4))
plot(testing$tail_transferability_diff ~ testing$FlAsH, ylim = c(0,1.4))

plot(testing$core_transferability_p ~ testing$FlAsH)
plot(testing$tail_transferability_p ~ testing$FlAsH)



# test_data <- norm_data[norm_data$FlAsH == "FlAsH10" &
#                          norm_data$cell_background == "dQ+GRK6" &
#                          norm_data$bArr == "bArr2",]
# boxplot(test_data$normalized_signal ~ test_data$GPCR)

# perform_tukey(test_data)
# test_tukey$`data_subset$GPCR`[,'p adj']

# eher tail transferable
# bArr2, GRK2, F9
# bArr2, GRK6, F2 (nichts significant though!

# testing poss. coefficients
