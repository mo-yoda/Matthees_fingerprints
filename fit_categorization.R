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

### data processing
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
write_xlsx(merged_data, paste0(path, r"(\categories\Fit_parameters_classes.xlsx)"))

### plot parameters based on different factors
# function for plotting
create_boxplot <- function(data, plot_col,
                           factor1, factor2 = NULL,
                           ylim_range = c(0, 4), log_transform = FALSE) {
  # Convert column names to symbols
  plot_col_sym <- sym(plot_col)
  factor1 <- sym(factor1)

  # Convert factor2 to symbol if it is not NULL
  if (!is.null(factor2)) {
    factor2 <- sym(factor2)
  }

  # Log transform the plot column if log_transform is TRUE
  if (log_transform) {
    data <- data %>% mutate(!!plot_col_sym := log10(!!plot_col_sym))
  }

  # Create a grouping variable
  data <- if (is.null(factor2)) {
    data %>% mutate(group_var = as.character(!!factor1))
  } else {
    data %>% mutate(group_var = interaction(!!factor1, !!factor2, lex.order = TRUE))
  }

  # Create boxplot
  p <- ggplot(data, aes(x = group_var, y = !!plot_col_sym)) +
    geom_boxplot() +
    geom_jitter(aes(color = "#ff460b"), width = 0.2, size = 2, alpha = 0.6, show.legend = FALSE) +  # Add jittered points
    labs(
      title = paste("Boxplots of", ifelse(log_transform, paste0("log10(", plot_col, ")"), plot_col),
                    "for GPCR with", paste(unique(data$GPCR), collapse = " ")),
      x = paste("Groups (", factor1, if (!is.null(factor2)) paste0(" and ", factor2), ")"),
      y = ifelse(log_transform, paste0("log10(", plot_col, ")"), plot_col)  # Adjust y-axis label based on log_transform
    ) +
    ylim(ylim_range) +  # Set y-axis limit based on the ylim_range input
    theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14)
    )

  # Return the plot
  return(p)
}

# function to save plots in list with name
add_plot <- function(plot_list, new_plot, plot_name) {
  # Combine the existing list with the new plot
  plot_list <<- c(plot_list, setNames(list(new_plot), plot_name))
  return(plot_list)
}

# Separation for EC50 based on the starting string of GPCR
data_V2 <- merged_data %>%
  filter(str_starts(GPCR, "V2"))

data_b2 <- merged_data %>%
  filter(str_starts(GPCR, "b2"))

# test Separation based on Cterm
data_V2Cterm <- merged_data %>%
  filter(str_detect(GPCR, "b2V2") | str_detect(GPCR, "V2R"))

data_b2Cterm <- merged_data %>%
  filter(str_detect(GPCR, "b2AR") | str_detect(GPCR, "V2b2"))

# initialize plot_list
plot_list <- list()

### Boxplot plotting
# Warning: Removed XX rows containing non-finite values (`stat_boxplot()`)
# -> due to ylim setting!, there are no NAs or Inf values in this dataset

### conc_dep + unclear factors
# EC50
add_plot(plot_list,
         create_boxplot(data_V2, "EC50",
                        "conc_dep", "unclear",
                        ylim_range = c(-0.5, 3)),
         "concDep_uncl_V2")
# 16x values not displayed as EC50 > 3; all in conc_dep = FALSE
add_plot(plot_list,
         create_boxplot(data_b2, "EC50",
                        "conc_dep", "unclear",
                        ylim_range = c(-0.5, 10)),
         "concDep_uncl_b2_A")
# 8x values not displayed as EC50 > 10; all in conc_dep = FALSE
add_plot(plot_list,
         create_boxplot(data_b2, "EC50",
                        "conc_dep", "unclear",
                        ylim_range = c(-0.5, 3)),
         "concDep_uncl_b2_B")
# 14x values not displayed as EC50 > 3; in all different groups!
add_plot(plot_list,
         create_boxplot(data_V2Cterm, "EC50",
                        "conc_dep", "unclear",
                        ylim_range = c(-0.5, 10)),
         "concDep_uncl_V2Cterm")
add_plot(plot_list,
         create_boxplot(data_b2Cterm, "EC50",
                        "conc_dep", "unclear",
                        ylim_range = c(-0.5, 10)),
         "concDep_uncl_b2Cterm")


# EC50 log transformed
add_plot(plot_list,
         create_boxplot(data_V2, "EC50",
                        "conc_dep", "unclear",
                        ylim_range = c(-7, 10), log_transform = TRUE),
         "concDep_uncl_V2_log")
# 3x values not displayed as log10(EC50) > 10; all in conc_dep = FALSE
add_plot(plot_list,
         create_boxplot(data_b2, "EC50",
                        "conc_dep", "unclear",
                        ylim_range = c(-5, 5), log_transform = TRUE),
         "concDep_uncl_b2_log")
# all values displayed
add_plot(plot_list,
         create_boxplot(data_V2Cterm, "EC50",
                        "conc_dep", "unclear",
                        ylim_range = c(-7, 10), log_transform = TRUE),
         "concDep_uncl_V2Cterm_log")
add_plot(plot_list,
         create_boxplot(data_b2Cterm, "EC50",
                        "conc_dep", "unclear",
                        ylim_range = c(-5, 5), log_transform = TRUE),
         "concDep_uncl_b2Cterm_log")

# remaining parameters
add_plot(plot_list,
         create_boxplot(merged_data, "Hill_slope",
                        "conc_dep", "unclear",
                        ylim_range = c(-11, 13)),
         "concDep_uncl_hillSlope")
add_plot(plot_list,
         create_boxplot(merged_data, "RMSE",
                        "conc_dep", "unclear",
                        ylim_range = c(0, 65)),
         "concDep_uncl_rmse")
add_plot(plot_list,
         create_boxplot(merged_data, "MAE",
                        "conc_dep", "unclear",
                        ylim_range = c(0, 55)),
         "concDep_uncl_mae")

### conc_dep + critical
# EC50
add_plot(plot_list,
         create_boxplot(data_V2, "EC50",
                        "conc_dep", "critical",
                        ylim_range = c(-0.5, 3)),
         "concDep_crit_V2")
# 16x values not displayed as EC50 > 3; all in conc_dep = FALSE
add_plot(plot_list,
         create_boxplot(data_b2, "EC50",
                        "conc_dep", "critical",
                        ylim_range = c(-0.5, 10)),
         "concDep_crit_b2")
# 8x values not displayed as EC50 > 10; all in conc_dep = FALSE
add_plot(plot_list,
         create_boxplot(data_V2Cterm, "EC50",
                        "conc_dep", "critical",
                        ylim_range = c(-0.5, 10)),
         "concDep_crit_V2Cterm")
add_plot(plot_list,
         create_boxplot(data_b2Cterm, "EC50",
                        "conc_dep", "critical",
                        ylim_range = c(-0.5, 10)),
         "concDep_crit_b2Cterm")


# EC50 log transformed
add_plot(plot_list,
         create_boxplot(data_V2, "EC50",
                        "conc_dep", "critical",
                        ylim_range = c(-7, 10), log_transform = TRUE),
         "concDep_crit_V2_log")
# 3x values not displayed as log10(EC50) > 10; all in conc_dep = FALSE
add_plot(plot_list,
         create_boxplot(data_b2, "EC50",
                        "conc_dep", "critical",
                        ylim_range = c(-5, 5), log_transform = TRUE),
         "concDep_crit_b2_log")
# all values displayed
add_plot(plot_list,
         create_boxplot(data_V2Cterm, "EC50",
                        "conc_dep", "critical",
                        ylim_range = c(-7, 10), log_transform = TRUE),
         "concDep_crit_V2Cterm_log")
add_plot(plot_list,
         create_boxplot(data_b2Cterm, "EC50",
                        "conc_dep", "critical",
                        ylim_range = c(-5, 5), log_transform = TRUE),
         "concDep_crit_b2Cterm_log")


# remaining parameters
add_plot(plot_list,
         create_boxplot(merged_data, "Hill_slope",
                        "conc_dep", "critical",
                        ylim_range = c(-11, 13)),
         "concDep_crit_hillSlope")
add_plot(plot_list,
         create_boxplot(merged_data, "RMSE",
                        "conc_dep", "critical",
                        ylim_range = c(0, 65)),
         "concDep_crit_rmse")
add_plot(plot_list,
         create_boxplot(merged_data, "MAE",
                        "conc_dep", "critical",
                        ylim_range = c(0, 55)),
         "concDep_crit_mae")

### conc_dep single factor
# EC50
add_plot(plot_list,
         create_boxplot(data_V2, "EC50",
                        "conc_dep",
                        ylim_range = c(-0.5, 3)),
         "concDep_V2")
add_plot(plot_list,
         create_boxplot(data_b2, "EC50",
                        "conc_dep",
                        ylim_range = c(-0.5, 10)),
         "concDep_b2")
add_plot(plot_list,
         create_boxplot(data_V2Cterm, "EC50",
                        "conc_dep",
                        ylim_range = c(-0.5, 10)),
         "concDep_V2Cterm")
add_plot(plot_list,
         create_boxplot(data_b2Cterm, "EC50",
                        "conc_dep",
                        ylim_range = c(-0.5, 10)),
         "concDep_b2Cterm")

# EC50 log transformed
add_plot(plot_list,
         create_boxplot(data_V2, "EC50",
                        "conc_dep",
                        ylim_range = c(-7, 10), log_transform = TRUE),
         "concDep_V2_log")
add_plot(plot_list,
         create_boxplot(data_b2, "EC50",
                        "conc_dep",
                        ylim_range = c(-5, 5), log_transform = TRUE),
         "concDep_b2_log")
add_plot(plot_list,
         create_boxplot(data_V2Cterm, "EC50",
                        "conc_dep",
                        ylim_range = c(-7, 10), log_transform = TRUE),
         "concDep_V2Cterm_log")
add_plot(plot_list,
         create_boxplot(data_b2Cterm, "EC50",
                        "conc_dep",
                        ylim_range = c(-5, 5), log_transform = TRUE),
         "concDep_b2Cterm_log")

# remaining parameters
add_plot(plot_list,
         create_boxplot(merged_data, "Hill_slope",
                        "conc_dep",
                        ylim_range = c(-11, 13)),
         "concDep_hillSlope")
add_plot(plot_list,
         create_boxplot(merged_data, "RMSE",
                        "conc_dep",
                        ylim_range = c(0, 65)),
         "concDep_rmse")
add_plot(plot_list,
         create_boxplot(merged_data, "MAE",
                        "conc_dep",
                        ylim_range = c(0, 55)),
         "concDep_mae")

### 2D plotting
create_xy_plot <- function(data, x_col, y_col, factor1 = NULL, factor2 = NULL,
                           x_range = NULL, y_range = NULL, log_x = FALSE, log_y = FALSE) {
  # Convert column names to symbols
  x_col <- sym(x_col)
  y_col <- sym(y_col)
  factor1 <- sym(factor1)
  factor2 <- sym(factor2)

  # Create ggplot
  p <- ggplot(data, aes(x = !!x_col, y = !!y_col))

  # Add points with color aesthetics based on factors
  if (!is.null(factor1) && !is.null(factor2)) {
    p <- p + geom_point(aes(color = interaction(!!factor1, !!factor2)))
  } else if (!is.null(factor1)) {
    p <- p + geom_point(aes(color = !!factor1))
  } else {
    p <- p + geom_point()
  }

  # Apply log10 transformation if specified, and set axes ranges if provided
  if (log_x) {
    p <- p + scale_x_log10(limits = x_range)
  } else if (!is.null(x_range)) {
    p <- p + lims(x = x_range)
  }

  if (log_y) {
    p <- p + scale_y_log10(limits = y_range)
  } else if (!is.null(y_range)) {
    p <- p + lims(y = y_range)
  }

  # Enhance plot
  p <- p +
    theme_minimal(base_size = 15) +  # Increase base font size
    labs(
      title = paste("Scatter plot of", quo_name(x_col), "versus", quo_name(y_col)),
      x = quo_name(x_col),
      y = quo_name(y_col)
    )

  # Return the plot
  return(p)
}

# Usage:
xy_plot <- create_xy_plot(merged_data,
                          "EC50", "Hill_slope",
                          "conc_dep", "critical",
                          x_range = c(0, 3))
xy_plot <- create_xy_plot(merged_data,
                          "EC50", "Hill_slope",
                          "conc_dep", "critical",
                          x_range = c(-5, 300),
                          log_x = TRUE)
xy_plot <- create_xy_plot(merged_data,
                          "EC50", "Hill_slope",
                          "conc_dep", "critical",
                          x_range = c(-5, 300),
                          log_x = TRUE,
                          log_y = TRUE)
xy_plot


# export created plots
setwd(paste0(path, r"(\categories\)"))
for (i in seq_along(plot_list)) {
  # file name based on the plot name
  file_name <- paste0(names(plot_list)[i], ".png")
  # Save the plot to a file
  ggsave(file_name, plot = plot_list[[i]], width = 7, height = 5)
}