#### create needed environment with loading packages ####
wants <- c("openxlsx",
           "dplyr",
           "tidyr",
           "ggplot2",
           "writexl",
           "multcompView" # for anova + tukey
)
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
lapply(wants, require, character.only = TRUE)

#### data normalisation ####
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
  mean_signals <- calculate_mean_signals(data)
  # indentify F position with max signal for each GPCR/bArr/cell_background combination
  max_flash_means <- find_max_flash_means(mean_signals)
  # normalize replicates to the max mean
  normalized_data <- normalize_signals(data, max_flash_means)
  return(normalized_data)
}

norm_data <- normalize_data(replicates_data_filtered)

#### ANOVA + Tukey of normalised data ####
perform_tukey <- function(data_subset) {
  anova <- aov(data_subset$normalized_signal ~ data_subset$GPCR)
  tukey_result <- TukeyHSD(anova, 'data_subset$GPCR')
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
  for (i in seq_along(data_subsets)) {
    subset <- data_subsets[[i]]
    # Use the first row of each subset to generate a name for the result based on the grouping factors
    result_name <- paste(subset$cell_background[1], subset$bArr[1], subset$FlAsH[1], sep = "_")
    # Perform Tukey test and save the result with the name
    tukey_results[[result_name]] <- perform_tukey(subset)
  }

  return(tukey_results)
}

# Apply the function to your normalized data
tukey_test_results <- apply_tukey_tests(norm_data)

#### compare conf change for wildtype GPCRs with t test ####


#### calculate tail and core coefficents ####
## ultimately, coefficients were calculated from absolute difference
## thus, only the difference given in tukey results were used and not any p values

# Initialize a list to store the coefficients for each FlAsH position where bArr == "bArr2"
coefficients_list <- list()

# Iterate through each set of results in tukey_test_results
for (name in names(tukey_test_results)) {
  # Filter results for bArr2
  if (grepl("bArr2", name)) {
    # Extract the Tukey HSD test result
    tukey_result <- tukey_test_results[[name]]$`data_subset$GPCR`

    # Extract the necessary pairwise comparisons
    V2R_b2AR <- tukey_result["V2R-b2AR",]
    V2b2_b2V2 <- tukey_result["V2b2-b2V2",]
    V2R_V2b2 <- tukey_result["V2R-V2b2",]
    b2V2_b2AR <- tukey_result["b2V2-b2AR",]
    V2R_b2V2 <- tukey_result["V2R-b2V2",]
    V2b2_b2AR <- tukey_result["V2b2-b2AR",]

    # Calculate coefficients
    tail_transferability_p <- (V2R_b2AR["p adj"] + V2b2_b2V2["p adj"]) + (V2R_V2b2["p adj"] + b2V2_b2AR["p adj"])
    core_transferability_p <- (V2R_b2AR["p adj"] + V2b2_b2V2["p adj"]) + (V2R_b2V2["p adj"] + V2b2_b2AR["p adj"])
    tail_transferability_diff <- (abs(V2R_V2b2["diff"]) + abs(b2V2_b2AR["diff"]))
    core_transferability_diff <- (abs(V2R_b2V2["diff"]) + abs(V2b2_b2AR["diff"]))
    tail_core_transferabiility_diff <- (tail_transferability_diff - core_transferability_diff)
    # idea -> factor by abs wt difference to include that this must be given
    tail_core_transferabiility_diff_wt_factor <-
      (tail_transferability_diff - core_transferability_diff) * abs(V2R_b2AR["diff"])
    wildtype_diff <- abs(V2R_b2AR["diff"])

    # Store the results in the list
    coefficients_list[[name]] <- list(
      tail_transferability_p = tail_transferability_p,
      core_transferability_p = core_transferability_p,
      tail_transferability_diff = tail_transferability_diff,
      core_transferability_diff = core_transferability_diff,
      tail_core_transferabiility_diff = tail_core_transferabiility_diff,
      tail_core_transferabiility_diff_wt_factor = tail_core_transferabiility_diff_wt_factor,
      wildtype_diff = wildtype_diff
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
  tail_core_transferabiility_diff = sapply(coefficients_list, function(x) x$tail_core_transferabiility_diff),
  tail_core_transferabiility_diff_wt_factor = sapply(coefficients_list,
                                                     function(x) x$tail_core_transferabiility_diff_wt_factor),
  wildtype_diff = sapply(coefficients_list, function(x) x$wildtype_diff),
  stringsAsFactors = FALSE
)

# Split the combination column into separate factors
coefficients_df <- coefficients_df %>%
  separate(combination, into = c("cell_background", "bArr", "FlAsH"), sep = "_") %>%
  mutate(across(c(cell_background, bArr, FlAsH), as.factor))

#### create bargraphs from tail-core coeff ####
# Split data into subsets based on cell_background
coeff_subsets <- coefficients_df %>%
  group_by(cell_background) %>%
  group_split()

# Initialize an empty list to store the resulting plots
plot_list <- list()

# Loop over each subset and create barplot for each cell background
for (i in seq_along(coeff_subsets)) {
  temp_sub <- coeff_subsets[[i]]
  plot_name <- as.character(temp_sub$cell_background[1])
  barplot <- ggplot(temp_sub) +
    geom_col(aes(tail_core_transferabiility_diff, FlAsH)) +
    xlim(c(-2, 2)) +
    theme_classic() +
    geom_vline(xintercept = 0)
  plot_list[[plot_name]] <- barplot
}


creat_scatterplot <- function(dataframe, coefficient_col) {
  # create scatterplot with all cell backgrounds
  costumm_shapes <- c(16, 17, 15, 18)
  flash_order <- c("FlAsH1", "FlAsH10", "FlAsH9", "FlAsH7", "FlAsH5", "FlAsH4", "FlAsH3", "FlAsH2")

  scatterplot <- ggplot(dataframe,
                        aes(x = coefficient_col,
                            y = factor(FlAsH, levels = flash_order))) +
    geom_vline(xintercept = 0) +
    geom_point(aes(shape = cell_background,
                   color = cell_background,
                   size = 2)) +
    xlim(c(-2, 2)) +
    theme_classic() +
    theme(axis.text = element_text(size = 20), # bigger axis text
          axis.title = element_blank(),
          axis.ticks.length = unit(0.35, "cm"),
          legend.text = element_text(size = 14),
          legend.title = element_blank(),
          legend.key.size = unit(1.5, "lines"),
          panel.border = element_rect(color = "black", fill = NA, size = 1.3),
          panel.grid.major = element_line(color = "grey90")) +
    guides(size = "none", # removed part of the legend for the size of the points
           shape = guide_legend(override.aes = list(size = 5))) + # bigger symbols in legend
    scale_shape_manual(values = costumm_shapes)
  return(scatterplot)
}
tail_core_scatter <- creat_scatterplot(coefficients_df, coefficients_df$tail_core_transferabiility_diff)
plot_list[["tail_core_scatter"]] <- tail_core_scatter

tail_core_wt_factor_scatter <- creat_scatterplot(coefficients_df,
                                                 coefficients_df$tail_core_transferabiility_diff_wt_factor)
plot_list[["tail_core_wt_factor_scatter"]] <- tail_core_wt_factor_scatter


### export plots
export_plot_list <- function(plot_list, folder_name) {
  if (!dir.exists(folder_name)) {
    dir.create(folder_name)
  }
  setwd(paste0(getwd(), "/", folder_name, "/"))
  for (i in seq_along(plot_list)) {
    # file name based on the plot name
    file_names <- paste0(names(plot_list)[i], ".png")
    # Save the plot to a file
    for (file in file_names) {
      ggsave(file, plot = plot_list[[i]],
      )
    }
  }
}

export_plot_list(plot_list, "240322_tail-core_coeff")
write_xlsx(coefficients_df, "FlAsH_core,tail_coefficients.xlsx")