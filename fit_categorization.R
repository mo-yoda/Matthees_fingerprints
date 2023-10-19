###create needed environment with loading packages
wants <- c("openxlsx",
           "tidyverse",
           "stringr",
           "dplyr",
           "ggplot2",
           "scales",
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
path <- r"(C:\Users\monar\Google Drive\Arbeit\homeoffice\231119_EM_PROGRAM_newdata)"
setwd(path)

# Load data
classes <- readxl::read_xlsx("231012_categorized-master-plot_List.xlsx")

# CHOOSE: normalised or non-normalised data
path_to_Normfits <- paste0(path, r"(\start_normalised\)")
path_to_NonNormfits <- paste0(path, r"(\non_normalised\)")

setwd(path_to_Normfits)
fit_pars_og <- readxl::read_xlsx("Fit_parameters.xlsx")

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
folder_name <- c("categories")
if (!dir.exists(folder_name)) {
    dir.create(folder_name)
  }
write_xlsx(merged_data, paste0(getwd(), r"(\categories\Fit_parameters_classes.xlsx)"))

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
add_plot(plot_list,
         create_boxplot(merged_data, "EC50",
                        "GPCR", "conc_dep",
                        ylim_range = c(-5, 5), log_transform = TRUE),
         "concDep_GPCR_log")

# remaining parameters
add_plot(plot_list,
         create_boxplot(merged_data, "Hill_slope",
                        "conc_dep",
                        ylim_range = c(-11, 13)),
         "concDep_hillSlope")
add_plot(plot_list,
         create_boxplot(merged_data, "Hill_slope",
                        "GPCR", "conc_dep",
                        ylim_range = c(-11, 13)),
         "concDep_hillSlope_GPCR")
add_plot(plot_list,
         create_boxplot(merged_data, "Hill_slope",
                        "conc_dep",
                        ylim_range = c(-5, 3), log_transform = TRUE),
         "concDep_hillSlopeLog")
add_plot(plot_list,
         create_boxplot(merged_data, "Hill_slope",
                        "GPCR", "conc_dep",
                        ylim_range = c(-5, 3), log_transform = TRUE),
         "concDep_hillSlopeLog_GPCR")
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
                           x_range = NULL, y_range = NULL, log_x = FALSE, log_y = FALSE,
                           label_above_y = NULL, label_below_y = NULL,
                           label_above_x = NULL, label_below_x = NULL) {
  # Convert column names to symbols
  x_col <- sym(x_col)
  y_col <- sym(y_col)
  # Convert factors to symbols if they're not NULL
  if (!is.null(factor1)) {
    factor1 <- sym(factor1)
  }
  if (!is.null(factor2)) {
    factor2 <- sym(factor2)
  }

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
    p <- p + scale_x_log10(limits = x_range,
                           labels = trans_format("log10", math_format(10^.x)))
  } else if (!is.null(x_range)) {
    p <- p + lims(x = x_range)
  }

  if (log_y) {
    p <- p + scale_y_log10(limits = y_range,
                           labels = trans_format("log10", math_format(10^.x)))
  } else if (!is.null(y_range)) {
    p <- p + lims(y = y_range)
  }

  # Add labels based on the conditions specified
  if (!is.null(label_above_y) ||
    !is.null(label_below_y) ||
    !is.null(label_above_x) ||
    !is.null(label_below_x)) {

    conditions <- c()
    if (!is.null(label_above_y)) conditions <- c(conditions, paste0(y_col, " > ", label_above_y))
    if (!is.null(label_below_y)) conditions <- c(conditions, paste0(y_col, " < ", label_below_y))
    if (!is.null(label_above_x)) conditions <- c(conditions, paste0(x_col, " > ", label_above_x))
    if (!is.null(label_below_x)) conditions <- c(conditions, paste0(x_col, " < ", label_below_x))

    condition_str <- paste(conditions, collapse = " | ")

    data_to_label <- filter(data, !!rlang::parse_expr(condition_str))

    p <- p + geom_label_repel(mapping = aes(label = paste(GPCR, bArr, cell_background, FlAsH, sep = ", ")),
                              data = data_to_label,
                              vjust = -1,
                              size = 3,
                              box.padding = 0, point.padding = 0.25,
                              label.padding = 0.1,
                              fill = NA,
                              segment.color = "gray50",
                              label.size = NA,
                              seed = 1234,
                              alpha = 0.7)
  }

  # Set the axis labels based on transformation
  x_label <- if (log_x) paste0("log10(", quo_name(x_col), ")") else quo_name(x_col)
  y_label <- if (log_y) paste0("log10(", quo_name(y_col), ")") else quo_name(y_col)

  # Enhance plot
  unique_gpcr <- paste(unique(data$GPCR), collapse = ", ")
  p <- p +
    theme_minimal(base_size = 15) +  # Increase base font size
    labs(
      title = paste("Scatter plot of", quo_name(x_col), "versus", quo_name(y_col),
                    "for GPCRs:", unique_gpcr),
      x = x_label,
      y = y_label
    )

  # Return the plot
  return(p)
}

# Usage:
# 2x warnings if axis limits exclude some data points:
# trans$transform(limits) : NaNs produced
# Warning: Removed XX rows containing missing values (`geom_point()`).
create_xy_plot(merged_data,
               "EC50", "Hill_slope",
               "conc_dep", "critical",
               x_range = c(-5, 300),
               log_x = TRUE)
create_xy_plot(merged_data,
               "EC50", "Hill_slope",
               "conc_dep", "critical",
               x_range = c(-5, 300),
               log_x = TRUE,
               log_y = TRUE)
# log10(Hill_slope) cannot plot negative hillslopes (49x values)
rows_HS_neg <- which(merged_data$Hill_slope < 0)
rows_HS_neg_but_cd <- rows_HS_neg[merged_data$conc_dep[rows_HS_neg] == TRUE]
# 10x conditions which are concentration-dependent but have neg. HS
HS_neg_but_cd_df <- merged_data[rows_HS_neg_but_cd,]
# these should be checked separately! -> they would be excluded, if we use this 2D plot for classification
write_xlsx(HS_neg_but_cd_df, paste0(getwd(), r"(\categories\Neg_Hillslope_but_CD.xlsx)"))
merged_data[rows_HS_neg ,]
write_xlsx(merged_data[rows_HS_neg,], paste0(getwd(), r"(\categories\Neg_Hillslope.xlsx)"))

# critical HS, > 0.01 & < 0.1; some are cd and some are not
rows_HS_critical <- which(merged_data$Hill_slope > 0.01 & merged_data$Hill_slope < 0.1)
write_xlsx(merged_data[rows_HS_critical,], paste0(getwd(), r"(\categories\Critical_HillSlope.xlsx)"))


add_plot(plot_list,
         create_xy_plot(merged_data,
                        "EC50", "Hill_slope",
                        "conc_dep", "critical",
                        x_range = c(-5, 300),
                        log_x = TRUE,
                        label_below_y = -1),
         "logEC50_HS_all_data_crit")
add_plot(plot_list,
         create_xy_plot(merged_data,
                        "EC50", "Hill_slope",
                        "conc_dep", "critical",
                        x_range = c(-5, 300),
                        log_x = TRUE,
                        log_y = TRUE),
         "logEC50_logHS_all_data_crit")
add_plot(plot_list,
         create_xy_plot(merged_data,
                        "EC50", "Hill_slope",
                        "conc_dep",
                        x_range = c(-5, 300),
                        log_x = TRUE),
         "logEC50_HS_all_data")
add_plot(plot_list,
         create_xy_plot(merged_data,
                        "EC50", "Hill_slope",
                        "conc_dep",
                        x_range = c(-5, 300),
                        log_x = TRUE,
                        label_below_y = 0.03,
                        label_below_x = 0.0003),
         "logEC50_HS_all_data_label")
add_plot(plot_list,
         create_xy_plot(merged_data,
                        "EC50", "Hill_slope",
                        "conc_dep",
                        x_range = c(-5, 300),
                        log_x = TRUE,
                        log_y = TRUE),
         "logEC50_logHS_all_data_cd")
add_plot(plot_list,
         create_xy_plot(merged_data,
                        "EC50", "Hill_slope",
                        "conc_dep",
                        x_range = c(-5, 300),
                        log_x = TRUE,
                        log_y = TRUE,
                        label_below_x = 0.001,
                        label_above_x = 3,
                        label_below_y = 0.01),
         "logEC50_logHS_all_data_cd_label")
add_plot(plot_list,
         create_xy_plot(data_b2,
                        "EC50", "Hill_slope",
                        "conc_dep",
                        x_range = c(-5, 300),
                        log_x = TRUE,
                        log_y = TRUE),
         "logEC50_logHS_b2_cd")
add_plot(plot_list,
         create_xy_plot(data_b2,
                        "EC50", "Hill_slope",
                        "conc_dep",
                        x_range = c(-5, 300),
                        log_x = TRUE,
                        log_y = TRUE,
                        label_below_x = 0.001,
                        label_below_y = 0.1),
         "logEC50_logHS_b2_cd_label")
add_plot(plot_list,
         create_xy_plot(data_V2,
                        "EC50", "Hill_slope",
                        "conc_dep",
                        x_range = c(-5, 300),
                        log_x = TRUE,
                        log_y = TRUE),
         "logEC50_logHS_V2_cd")
add_plot(plot_list,
         create_xy_plot(data_V2,
                        "EC50", "Hill_slope",
                        "conc_dep",
                        x_range = c(-5, 300),
                        log_x = TRUE,
                        log_y = TRUE,
                        label_below_x = 0.0001,
                        label_above_x = 3),
         "logEC50_logHS_V2_cd_label")
add_plot(plot_list,
         create_xy_plot(data_b2Cterm,
                        "EC50", "Hill_slope",
                        "conc_dep",
                        x_range = c(-5, 300),
                        log_x = TRUE,
                        log_y = TRUE),
         "logEC50_logHS_b2Cterm_cd")
add_plot(plot_list,
         create_xy_plot(data_b2Cterm,
                        "EC50", "Hill_slope",
                        "conc_dep",
                        x_range = c(-5, 300),
                        log_x = TRUE,
                        log_y = TRUE,
                        label_below_x = 0.0001,
                        label_above_x = 3,
                        label_below_y = 0.04),
         "logEC50_logHS_b2Cterm_cd_label")
add_plot(plot_list,
         create_xy_plot(data_V2Cterm,
                        "EC50", "Hill_slope",
                        "conc_dep",
                        x_range = c(-5, 300),
                        log_x = TRUE,
                        log_y = TRUE),
         "logEC50_logHS_V2Cterm_cd")
add_plot(plot_list,
         create_xy_plot(data_V2Cterm,
                        "EC50", "Hill_slope",
                        "conc_dep",
                        x_range = c(-5, 300),
                        log_x = TRUE,
                        log_y = TRUE,
                        label_above_x = 3,
                        label_below_x = 0.001,
                        label_below_y = 0.03),
         "logEC50_logHS_V2Cterm_cd_label")


# export created plots
setwd(paste0(getwd(), r"(\categories\)"))
for (i in seq_along(plot_list)) {
  # file name based on the plot name
  file_name <- paste0(names(plot_list)[i], ".png")
  # Save the plot to a file
  ggsave(file_name, plot = plot_list[[i]], width = 10, height = 7)
}