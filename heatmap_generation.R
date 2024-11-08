#### create needed environment with loading packages ####
wants <- c("openxlsx",
           "dplyr",
           "tidyr",
           "pheatmap",
           "ggplot2",
           "writexl"
)
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
lapply(wants, require, character.only = TRUE)

#### load data ####
# tower PC path
path <- r"(C:\path\to\folder)"
# laptop path
path <- r"(C:\path\to\folder)"
setwd(path)

# Load data
import_file <- "Filtered_SN_Master.xlsx"
filtered_data <- readxl::read_xlsx(import_file)

normalize_by_factor <- function(data, factor_name) {
  # Calculate the minimum/maximum value of mean_signal for each level of the factor
  # if normalized data is normalized again, norm to max value, else to min
  if (!max(data$mean_signal) == 1) {
    max_values <- data %>%
      dplyr::group_by(!!dplyr::sym(factor_name)) %>%
      dplyr::summarise(max_signal = min(mean_signal, na.rm = TRUE))
  } else {
    max_values <- data %>%
      dplyr::group_by(!!dplyr::sym(factor_name)) %>%
      dplyr::summarise(max_signal = max(mean_signal, na.rm = TRUE))
  }

  # Join the min/max_values with the main data
  data <- data %>%
    dplyr::left_join(max_values, by = factor_name)

  # Normalize the mean_signal by dividing it by min_signal
  data <- data %>%
    dplyr::mutate(normalized_signal = mean_signal / max_signal)

  # Drop the min_signal column as it's no longer needed
  # data$max_signal <- NULL

  return(data)
}

# Create matrix for heatmap (optional subsetting of several levels)
create_matrix_from_factors <- function(data, col_factor,
                                       subset_factor = NULL,
                                       subset_levels = NULL,
                                       factor_order = NULL) {
  # Check if a subset is requested
  if (!is.null(subset_factor) && !is.null(subset_levels)) {
    data <- dplyr::filter(data, !!sym(subset_factor) %in% subset_levels)
  }
  # if factor order is provided, reorder combined factor accordingly
  if (!is.null(factor_order)) {
    data[[col_factor]] <- factor(data[[col_factor]], levels = factor_order)
  }

  # Create a combined factor from the factors that aren't the col_factor
  factors <- c("GPCR", "bArr", "cell_background", "FlAsH")
  row_factors <- setdiff(factors, col_factor)
  data$combined_factor <- apply(data[row_factors], 1, paste, collapse = "_")

  # Spread the data to form the matrix
  matrix_data <- data %>%
    dplyr::select(combined_factor, !!sym(col_factor), mean_signal) %>%
    tidyr::spread(!!sym(col_factor), mean_signal)

  # Convert tibble to a standard data.frame
  matrix_data <- as.data.frame(matrix_data)

  # Set row names
  rownames(matrix_data) <- matrix_data$combined_factor
  matrix_data$combined_factor <- NULL  # Remove the 'combined_factor' column

  return(matrix_data)
}

create_colors_and_breaks <- function(matrix_data) {
  # Determine the range of your data
  min_value <- min(matrix_data, na.rm = TRUE)
  max_value <- max(matrix_data, na.rm = TRUE)

  # Adjust these numbers based on your specific needs
  num_colors_below_zero <- 100
  num_colors_above_zero_to_two <- 100

  # workaround for specific coloring of non-responders:
  # define breaks with very small intervals around 0
  breaks <- c(min_value,
              seq(min_value, -1e-10, length.out = num_colors_below_zero + 1)[-1],
              -1e-10, 1e-10,  # Small interval around 0
              seq(1e-10, 2, length.out = num_colors_above_zero_to_two + 1)[-1],
              max_value)
  breaks <- unique(breaks)  # Ensure breaks are unique

  # Define colors
  # Gradient for < 0
  colors_below_zero <- colorRampPalette(c("#FF0000", "#0000FF"))(num_colors_below_zero)
  # Gradient for 0 < x <= 2
  colors_above_zero_to_two <- colorRampPalette(c("#FFFFFF", "#CAFFCA"))(num_colors_above_zero_to_two)
  # Color for non-responder
  nonResponderColor <- "#A6A4A4"
  # Color for x > 2
  colors_above_two <- "#CAFFCA"

  # Assemble the color vector
  colors <- c(colors_below_zero, nonResponderColor, colors_above_zero_to_two, colors_above_two)

  # Return the colors and breaks
  return(list(colors = colors, breaks = breaks))
}

draw_heatmap <- function(matrix_data,
                         clustering_distance_rows = "manhattan",
                         clustering_distance_cols = "manhattan",
                         cutree_rows = NA,
                         cutree_cols = NA,
                         display_numbers = FALSE,
                         height = 8,
                         width = 10,
                         fontsize = 10) {

  # If clustering distance is defined, then clustering is enabled
  clustering_rows <- !is.null(clustering_distance_rows)
  clustering_cols <- !is.null(clustering_distance_cols)

  # If cutree is not specified, set to one
  if (is.na(cutree_rows)) cutree_rows <- 1
  if (is.na(cutree_cols)) cutree_cols <- 1

  # Construct title with clustering distances
  if (!is.null(clustering_distance_rows) && !is.null(clustering_distance_cols)) {
    title <- paste("Row Dist:", clustering_distance_rows,
                   "& Col Dist:", clustering_distance_cols)
  }
  if (!is.null(clustering_distance_rows)) title <- paste("Row Dist:", clustering_distance_rows)
  if (!is.null(clustering_distance_cols)) title <- paste("Row Dist:", clustering_distance_cols)

  colors_and_breaks <- create_colors_and_breaks(matrix_data)

  pheatmap(matrix_data,
           clustering_distance_rows = clustering_distance_rows,
           clustering_distance_cols = clustering_distance_cols,
           cutree_rows = cutree_rows,
           cutree_cols = cutree_cols,
           col = colors_and_breaks$colors,
           breaks = colors_and_breaks$breaks,
           cellwidth = width,
           cellheight = height,
           fontsize = fontsize,
           display_numbers = display_numbers,
           cluster_rows = clustering_rows,
           cluster_cols = clustering_cols,
           main = title,
           border_color = NA)
}

add_plot <- function(plot_list, new_plot, plot_name) {
  # Combine the existing list with the new plot
  updated_list <- append(plot_list, setNames(list(new_plot), plot_name))
  return(updated_list)
}

collect_heatmaps <- function(plot_list, data, col_factor,
                             normalize = FALSE, normalize_factor = NULL,
                             subset_factor = NULL, subset_levels = NULL,
                             factor_order = NULL,
                             clustering_distance_rows = "manhattan",
                             clustering_distance_cols = "manhattan",
                             cutree_rows = NA,
                             cutree_cols = NA,
                             display_numbers = FALSE,
                             height = 20,
                             width = 20,
                             fontsize = 10,
                             notes = NULL) {
  # Optionally subset the data
  if (!is.null(subset_factor) && !is.null(subset_levels)) {
    data <- dplyr::filter(data, !!sym(subset_factor) %in% subset_levels)
  }

  # Optionally normalize the data
  if (normalize && !is.null(normalize_factor)) {
    data <- normalize_by_factor(data, normalize_factor)
    # Replace mean_signal with normalized_signal for further processing
    data$mean_signal <- data$normalized_signal
  }

  # Create the matrix
  matrix_data <- create_matrix_from_factors(data, col_factor, subset_factor, subset_levels, factor_order)

  # Draw the heatmap
  heatmap_plot <- draw_heatmap(matrix_data,
                               clustering_distance_rows, clustering_distance_cols,
                               cutree_rows, cutree_cols,
                               display_numbers,
                               height, width,
                               fontsize)

  # Create the plot name based on normalization and col_factor
  plot_name <- paste(col_factor,
                     # if only a subset of GPCRs is used as input, display this is plot name
                     if (!is.null(subset_factor) &&
                       !subset_factor == "GPCR" &&
                       !length(levels(as.factor(data$GPCR))) == 4)
                       paste("only ", paste(levels(as.factor(data$GPCR)), collapse = ", ")),
                     # include normalization in plot name
                     ifelse(normalize, paste("norm by", normalize_factor), "not norm"),
                     # include subset in plot name
                     ifelse(!is.null(subset_factor), paste("subset by",
                                                           paste(subset_levels, collapse = ", ")
                     ), "all data"),
                     if (!is.null(notes)) notes,
                     sep = "_")

  # Add the heatmap to the existing plot list
  plot_list <- add_plot(plot_list, heatmap_plot, plot_name)

  return(plot_list)
}

# export non-normalised matrix
sensor_order <- c("FlAsH2", "FlAsH3", "FlAsH4", "FlAsH5", "FlAsH7", "FlAsH9", "FlAsH10", "FlAsH1")
flash_matrix <- create_matrix_from_factors(filtered_data,
                                           col_factor = "FlAsH",
                                           factor_order = sensor_order)
flash_matrix <- cbind("Factors" = rownames(flash_matrix), flash_matrix)
write_xlsx(flash_matrix, "Matrix_SN.xlsx")

#### create heatmaps ####
# initialize plot_list
figure_plot_list <- list()

# main figure 4
# separate for GRK2 and GRK6, all GPCRs, all bArr, no norm
GRK_conditions <- levels(as.factor(filtered_data$cell_background))
for (level in GRK_conditions[3:4]) {
  figure_plot_list <- collect_heatmaps(figure_plot_list,
                                       filtered_data,
                                       "FlAsH",
                                       clustering_distance_cols = NULL,
                                       subset_factor = "cell_background", subset_levels = level,
                                       # normalize = TRUE, normalize_factor = "bArr",
                                       factor_order = sensor_order,
                                       # cutree_rows = 5,
                                       height = 20, width = 20)
}

# export plots
export_plot_list <- function(plot_list, folder_name) {
  if (!dir.exists(folder_name)) {
    dir.create(folder_name)
  }
  setwd(paste0(getwd(), "/", folder_name, "/"))
  for (i in seq_along(plot_list)) {
    # file name based on the plot name
    file_names <- c(paste0(i, "_", names(plot_list)[i], ".png"),
                    paste0(i, "_", names(plot_list)[i], ".emf"))
    # Save the plot to a file
    for (file in file_names) {
      ggsave(file, plot = plot_list[[i]],
             # width = 10,
             # height = 7
      )
    }
  }
}

setwd(path)
export_plot_list(figure_plot_list, folder_name = "240312_figure_heatmaps")
