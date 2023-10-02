###create needed environment with loading packages
wants <- c("openxlsx",
           "tidyverse",
           "stringr",
           "dplyr",
           "ggplot2",
           "writexl"
)
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
lapply(wants, require, character.only = TRUE)

# Function to replace "-" with "_" in column names
replace_hyphen_with_underscore <- function(df) {
  # Get the current column names
  col_names <- colnames(df)

  # Replace "-" with "_" in the column names
  new_col_names <- gsub("-", "_", col_names)

  # Assign the new column names back to the dataframe
  colnames(df) <- new_col_names

  # Return the updated dataframe
  return(df)
}

# Function to translate "y" and "n" to TRUE and FALSE
translate_to_logical <- function(column) {
  case_when(
    str_detect(column, fixed("y")) ~ TRUE,
    str_detect(column, fixed("n")) ~ FALSE,
    #TRUE ~ NA_logical_  # for any other values or NA
  )
}


path <- r"(C:\Users\monar\Google Drive\Arbeit\homeoffice\230918_EM_PROGRAM)"
setwd(path)

# Load data
fit_pars_og <- readxl::read_xlsx("Fit_parameters.xlsx")
classes <- readxl::read_xlsx(paste0(path, r"(\categories\230922_categorized-master-plot_List.xlsx)"))

# Split the 'experiment' column into separate columns for each factor
fit_pars <- fit_pars_og %>%
  separate(
    col = experiment,  # The column to split
    into = c("GPCR", "bArr", "cell_background", "FlAsH"),  # Names of the new columns
    sep = " - "  # The separator between the factors
  )

# Adjust the 'FlAsH' column in the new data to match the format in fit_pars_separated
classes <- classes %>%
  mutate(FlAsH = paste0("FlAsH", FlAsH))

# make sure that there are no "-" in column names
classes <- replace_hyphen_with_underscore(classes)

# Create the "unclear" column based on the presence of "p" in the "conc_dep" column
classes <- classes %>%
  mutate(
    unclear = str_detect(`conc_dep`, "p")
  )

# Merge the fit_pars and classes dfs based on the matching factors
merged_data <- fit_pars %>%
  left_join(subset(classes, select = -fit), by = c("GPCR", "bArr", "cell_background", "FlAsH"))

# Apply the function to specified columns
merged_data <- merged_data %>%
  mutate(
    outliers_large_spread = translate_to_logical(outliers_large_spread),
    conc_dep = translate_to_logical(conc_dep),
    critical = translate_to_logical(critical)
  )

### plot parameters based on different factors

create_boxplot <- function(data, plot_col,
                           factor1, factor2 = NULL,
                           ylim_range = c(0, 4), log_transform = FALSE) {
  # Convert column names to symbols
  plot_col <- sym(plot_col)
  factor1 <- sym(factor1)

  # Convert factor2 to symbol if it is not NULL
  if (!is.null(factor2)) {
    factor2 <- sym(factor2)
  }

   # Log transform the plot column if log_transform is TRUE
  if (log_transform) {
    data <- data %>% mutate(!!plot_col := log10(!!plot_col))
  }

  # To separately label outliers
    # Get the stats for the boxplot to identify outliers
  box_stats <- boxplot.stats(data[[as.character(plot_col)]])

  # Filter the data to only include the outliers
  outlier_data <- data %>% filter(!!plot_col %in% box_stats$out)

  # Create boxplot
  p <- ggplot(data, aes(x = interaction(!!factor1, !!factor2, lex.order = TRUE), y = !!plot_col)) +
    geom_boxplot() +
    geom_text(
      aes(label = paste(GPCR, bArr, cell_background, FlAsH, sep = ", ")),
      data = outlier_data,
      vjust = 1.5,
      size = 3,
      check_overlap = TRUE
    ) +
    labs(
      title = paste("Boxplots of", ifelse(log_transform, paste0("log10(", plot_col, ")"), plot_col),
                    "for GPCR with", paste(unique(data$GPCR), collapse = " ")),
      x = paste("Groups (", factor1, if (!is.null(factor2)) paste0(" and ", factor2), ")"),
    )  +
    ylim(ylim_range)  # Set y-axis limit based on the ylim_range input
    # adjust font size
    theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14)
    ) # +
    # geom_text(aes(label = paste0(plot_col, "_outlier"), hjust = -.5))


  # Return the plot
  return(p)
}

# Separation for EC50 based on the starting string of GPCR
data_V2 <- merged_data %>%
  filter(str_starts(GPCR, "V2"))

data_b2 <- merged_data %>%
  filter(str_starts(GPCR, "b2"))

# Usage
# Warning: Removed XX rows containing non-finite values (`stat_boxplot()`)
# -> due to ylim setting!, there are no NAs or Inf values in this dataset
plot_V2 <- create_boxplot(data_V2, "EC50", "conc_dep", "unclear")
plot_b2 <- create_boxplot(data_b2, "EC50", "conc_dep", "unclear")
hillSlope <- create_boxplot(merged_data, "Hill_slope", "conc_dep", "unclear")
rmse <- create_boxplot(merged_data, "RMSE", "conc_dep", "unclear")
mae <- create_boxplot(merged_data, "MAE", "conc_dep", "unclear")




# For single factor
plot_V2_single_factor <- create_boxplot(data_V2, "EC50", "conc_dep")
plot_b2_single_factor <- create_boxplot(data_b2, "EC50", "conc_dep")

