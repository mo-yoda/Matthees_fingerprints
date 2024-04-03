#### create needed environment with loading packages ####
wants <- c("openxlsx",
           "dplyr",
           "tidyr",
           "ggplot2",
           "writexl",
           "patchwork", # for joining plots
           "multcompView" # for anova + tukey
)
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
lapply(wants, require, character.only = TRUE)

#### data normalisation ####
# tower PC path
path <- r"(C:\Users\monar\Google Drive\Arbeit\homeoffice\231119_EM_PROGRAM_newdata)"
# laptop path
path <- r"(C:\Users\marli\Desktop\231119_EM_PROGRAM_newdata)"
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

#### compare conf change for wildtype GPCRs with t test ####
# function to run the t test to each subset of data
apply_t_test <- function(data) {
  # only wt GPCRs should be compared
  wt_data <- data[norm_data$bArr == "bArr2" &
                    !norm_data$GPCR == "b2V2" &
                    !norm_data$GPCR == "V2b2",]

  # Split data into subsets based on cell_background, bArr, and FlAsH
  data_subsets <- wt_data %>%
    group_by(cell_background, bArr, FlAsH) %>%
    group_split()
  # Initialize an empty list to store the results
  ttest_results <- list()

  # Loop over each subset and apply the ttest
  for (i in seq_along(data_subsets)) {
    subset <- data_subsets[[i]]
    # Use the first row of each subset to generate a name for the result based on the grouping factors
    result_name <- paste(subset$cell_background[1], subset$bArr[1], subset$FlAsH[1], sep = "_")
    # Perform t test and save the result with the name
    ttest_results[[result_name]] <- t.test(subset$normalized_signal ~ subset$GPCR)
  }

  return(ttest_results)
}

t_test_results <- apply_t_test(norm_data)

# save the results of the t test in coefficients_df
# Initialize a new column in coefficients_df for p-values
coefficients_df$sign_WT_diff <- NA

for (name in names(t_test_results)) {
  # Extract factors from the name
  factors <- strsplit(name, "_")[[1]]
  cell_background <- factors[1]
  bArr <- factors[2]
  FlAsH <- factors[3]

  # Extract the p-value from the t-test result
  p_value <- t_test_results[[name]]$p.value

  # Find the row in coefficients_df that matches the factors
  row_index <- which(coefficients_df$cell_background == cell_background &
                       coefficients_df$bArr == bArr &
                       coefficients_df$FlAsH == FlAsH)

  # Assign the p-value to the matching row
  coefficients_df$sign_WT_diff[row_index] <- p_value
}


#### create plots from tail-core coeff ####
# Split data into subsets based on cell_background
coeff_subsets <- coefficients_df %>%
  group_by(cell_background) %>%
  group_split()

# Initialize an empty list to store the resulting plots
plot_list <- list()

create_scatterplot <- function(dataframe, coefficient_col,
                               cell_backgrounds_to_show = c("Con", "dQ+EV", "dQ+GRK2", "dQ+GRK6"),
                               set_xlim = TRUE, show_legend = TRUE,
                               scale_points = TRUE) {
  # use colors depending on the levels found in dataframe$cell_background
  costum_colors <- c(Con = "#000080", `dQ+EV` = "#808080", `dQ+GRK2` = "#F94040", `dQ+GRK6` = "#077E97")
  needed_colors <- costum_colors[as.character(unique(dataframe$cell_background))]

  # use this sequence of FlAsH positions
  flash_order <- c("FlAsH1", "FlAsH10", "FlAsH9", "FlAsH7", "FlAsH5", "FlAsH4", "FlAsH3", "FlAsH2")

  # Create a column for controlling visibility based on cell_background
  dataframe$visible <- ifelse(dataframe$cell_background %in% cell_backgrounds_to_show, 1, 0)

  scatterplot <- ggplot(dataframe,
                        aes(x = !!sym(coefficient_col),
                            y = factor(FlAsH, levels = flash_order),
                            size = wildtype_diff, # size of points correspond to WT diff
                            color = cell_background)) +
    geom_vline(xintercept = 0)

  # make sure that if not all cell_backgrounds are supposed to be displayed,
  # others are made invisble (alpha = 0)
  if (length(cell_backgrounds_to_show) < 4) {
    scatterplot <- scatterplot +
      geom_point(aes(alpha = visible)) +
      scale_alpha_continuous(range = c(0, 1))
  } else {
    scatterplot <- scatterplot +
      geom_point()
  }
  scatterplot <- scatterplot +
    scale_color_manual(values = needed_colors) +
    theme_classic() +
    theme(axis.text = element_text(size = 20), # bigger axis text
          axis.title = element_blank(),
          axis.ticks.length = unit(0.35, "cm"),
          legend.text = element_text(size = 14),
          legend.title = element_blank(),
          legend.key.size = unit(1.5, "lines"),
          panel.border = element_rect(color = "black", fill = NA, size = 1.3),
          panel.grid.major = element_line(color = "grey90")) +
    guides(alpha = FALSE)

  # logicial variables
  if (scale_points) {
    scatterplot <- scatterplot + scale_size(range = c(2.8, 10), name = "absolute difference V2R and b2AR")
  } else {
    scatterplot <- scatterplot +
      scale_size(range = c(3.5, 3.5)) +
      guides(size = "none")
  }
  if (set_xlim) {
    scatterplot <- scatterplot + xlim(c(-2, 2))
  }
  if (!show_legend) {
    scatterplot <- scatterplot + theme(legend.position = "none")
  }
  return(scatterplot)
}

# create different options of scatterplots
plot_list[["all_data_scatter_scaled"]] <-
  create_scatterplot(coefficients_df, "tail_core_transferabiility_diff")

plot_list[["all_data_wt_factor_scatter_scaled"]] <-
  create_scatterplot(coefficients_df, "tail_core_transferabiility_diff_wt_factor")

plot_list[["all_data_wt_factor_scatter_NOTscaled"]] <-
  create_scatterplot(coefficients_df, "tail_core_transferabiility_diff_wt_factor", scale_points = FALSE)

# scaled plots for all cell backgrounds separately
for (condition in levels(as.factor(coefficients_df$cell_background))) {
  plot_name_temp <- paste(condition, "scatter_scaled", sep = "_")
  plot_list[[plot_name_temp]] <- create_scatterplot(coefficients_df, "tail_core_transferabiility_diff",
                                                    cell_backgrounds_to_show = condition)
  plot_name_temp <- paste(condition, "scatter_NOTscaled", sep = "_")
  plot_list[[plot_name_temp]] <- create_scatterplot(coefficients_df, "tail_core_transferabiility_diff",
                                                    cell_backgrounds_to_show = condition, scale_points = FALSE)
}

plot_list[["Con_dQ+EV_scatter_scaled"]] <-
  create_scatterplot(coefficients_df, "tail_core_transferabiility_diff",
                     cell_backgrounds_to_show = c("Con", "dQ+EV"))
plot_list[["dQ+GRK2_dQ+GRK6_scatter_NOTscaled"]] <-
  create_scatterplot(coefficients_df, "tail_core_transferabiility_diff",
                     cell_backgrounds_to_show = c("dQ+GRK2", "dQ+GRK6"))

# all data plots for combining with wt diff
all_data_scatter_NOTscaled <- create_scatterplot(coefficients_df, "tail_core_transferabiility_diff", scale_points = FALSE)
plot_list[["all_data_scatter_NOTscaled"]] <- all_data_scatter_NOTscaled

#### add classification tiles dependent on difference of WT GPCRs ####
# Custom colors for cell_backgrounds and order of FlAsH levels
costum_colors <- c(Con = "#000080", `dQ+EV` = "#808080", `dQ+GRK2` = "#F94040", `dQ+GRK6` = "#077E97", "Outline" = "white")
flash_order <- c("FlAsH1", "FlAsH10", "FlAsH9", "FlAsH7", "FlAsH5", "FlAsH4", "FlAsH3", "FlAsH2")

# if p is significant, tile should be solid
pvalue_visualization <- coefficients_df %>%
  mutate(fill_status = ifelse(
    is.na(sign_WT_diff) | sign_WT_diff >= 0.1,
    "Outline", "Filled"))
pvalue_visualization <- pvalue_visualization %>%
  mutate(tile_color = ifelse(fill_status == "Filled", as.character(cell_background), NA), # Filled or NA for outline
         outline_color = ifelse(fill_status == "Filled", as.character(cell_background), as.character(cell_background))) # Use cell_background color for outline


# Creating the heatmap-like classification panel
pvalue_tiles <- ggplot(pvalue_visualization, aes(x = cell_background,
                                                 y = factor(FlAsH, levels = flash_order))) +
  geom_tile(aes(fill = ifelse(fill_status == "Filled", as.character(cell_background), "Outline"),
                color = ifelse(fill_status == "Filled", as.character(cell_background), as.character(cell_background))),
            size = 1, width = 0.9, height = 0.9) +
  scale_fill_manual(values = costum_colors) +
  scale_color_manual(values = costum_colors) +
  scale_x_discrete(position = "top", limits = c("Con", "dQ+EV", "dQ+GRK2", "dQ+GRK6")) +
  scale_color_manual(values = costum_colors, name = "Cell Background") +
  coord_fixed(ratio = 1) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

plot_list[["WT_sign_tiles"]] <- pvalue_tiles
combined_plotA <- pvalue_tiles +
  all_data_scatter_NOTscaled +
  plot_layout(widths = c(4, 10))
plot_list[["WT_sign_tiles_and_scatter"]] <- combined_plotA


pvalue_scatter <- create_scatterplot(coefficients_df,
                                     "wildtype_diff",
                                     scale_points = FALSE,
                                     set_xlim = FALSE,
                                     show_legend = FALSE)
plot_list[["WT_sign_scatter"]] <- pvalue_scatter
combined_plotB <- pvalue_scatter +
  all_data_scatter_NOTscaled +
  plot_layout(widths = c(4, 10))
plot_list[["WT_sign_scatter_and_scatter"]] <- combined_plotB

### Supplementary Figure for coeff explainination ###
# Con b2AR F10, 4 and 1 as examples

# first, calculate mean of normalised replicates
mean_norm_data <- calculate_mean_signals(norm_data)

subset_example_data <- function(data, cell_background, FlAsH) {
  subset <- data %>%
    filter(cell_background == {{cell_background}} &
             bArr == "bArr2" &
             FlAsH == {{FlAsH}})
  # {{}} needed as function parameters have the same name as column name
  return(subset)
}

plot_example_bars <- function(data) {
  # Extract levels for dynamic title construction
  cell_background_level <- unique(data$cell_background)
  FlAsH_level <- unique(data$FlAsH)

  # Construct plot title dynamically
  plot_title <- paste(cell_background_level, FlAsH_level, sep = "_")

  ggplot(data, aes(x = GPCR, y = mean_signal)) +
    geom_bar(stat = "identity", position = position_dodge()) + # Use identity stat for pre-summarized data
    theme_classic() +
    theme(plot.title = element_text(size = 20, face = "bold"),  # Increase plot title font size and make it bold
          axis.title = element_text(size = 18),  # Increase axis titles font size
          axis.text.x = element_text(size = 16),  # Increase x axis text font size
          axis.text.y = element_text(size = 16),  # Increase y axis text font size
          legend.title = element_text(size = 16),  # Increase legend title font size
          legend.text = element_text(size = 14)  # Increase legend text font size
    ) +
    labs(title = plot_title, y = "Mean Signal") +
    geom_hline(yintercept = 0) +
    coord_cartesian(ylim = c(-51, 0))
}

# define which FlAsH positions should be used as examples
example_flash <- c("FlAsH1", "FlAsH4", "FlAsH10")
for(flash in example_flash){
  temp_subset <- subset_example_data(mean_norm_data, "Con", flash)
  temp_plot <- plot_example_bars(temp_subset)

  plot_title <- paste("Example_bar", flash, sep = "_")
  plot_list[[plot_title]] <- temp_plot
}

#### export plots ####
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

# get today's date for export folder
today_date <- Sys.Date()
formatted_date <- format(today_date, "%Y-%m-%d")

export_plot_list(plot_list, paste(formatted_date, "tail_core_coeff"))
write_xlsx(norm_data, "Normalised_data_replicates.xlsx")
write_xlsx(mean_norm_data, "Mean_normalised_data.xlsx")
write_xlsx(coefficients_df, "FlAsH_core,tail_coefficients.xlsx")