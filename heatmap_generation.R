###create needed environment with loading packages
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

### Data processing
# tower PC path
path <- r"(C:\Users\monar\Google Drive\Arbeit\homeoffice\231119_EM_PROGRAM_newdata)"
# laptop path
path <- r"(C:\Users\marli\Desktop\231119_EM_PROGRAM_newdata)"
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
                                       subset_levels = NULL) {
  # Check if a subset is requested
  if (!is.null(subset_factor) && !is.null(subset_levels)) {
    data <- dplyr::filter(data, !!sym(subset_factor) %in% subset_levels)
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

  pheatmap(matrix_data,
           clustering_distance_rows = clustering_distance_rows,
           clustering_distance_cols = clustering_distance_cols,
           cutree_rows = cutree_rows,
           cutree_cols = cutree_cols,
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
  matrix_data <- create_matrix_from_factors(data, col_factor, subset_factor, subset_levels)

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
                     if (!subset_factor == "GPCR" && !length(levels(as.factor(data$GPCR))) == 4)
                       paste("only", levels(as.factor(data$GPCR)), collapse = ", "),
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
  print(names(plot_list))

  return(plot_list)
}

# initialize plot_list
plot_list <- list()

### planned heatmaps ###
# options
# plot_list <- collect_heatmaps(plot_list,
#                               filtered_data,
#                               col_factor,
#                               clustering_distance_rows = "manhattan",
#                               clustering_distance_cols = NULL,
#                               subset_factor = "cell_background", subset_levels = c("dQ+EV", "Con"),
#                               normalize = TRUE, normalize_factor = "GPCR",
#                               cutree_rows = 5,
#                               height = 15, width = 15)

norm_GPCR_data <- normalize_by_factor(filtered_data, "GPCR")
norm_GPCR_data$mean_signal <- norm_GPCR_data$normalized_signal
norm_GPCR_data$max_signal <- NULL
norm_GPCR_data$normalized_signal <- NULL

plot_list <- collect_heatmaps(plot_list,
                              norm_GPCR_data,
                              "FlAsH",
                              clustering_distance_cols = NULL,
                              normalize = TRUE, normalize_factor = "bArr",
                              # cutree_rows = 10,
                              height = 11, width = 11,
                              notes = "norm_GPCR_data")

plot_list <- collect_heatmaps(plot_list,
                              filtered_data,
                              "FlAsH",
                              clustering_distance_cols = NULL,
                              subset_factor = "cell_background", subset_levels = "dQ+EV",
                              # normalize = TRUE, normalize_factor = "GPCR",
                              cutree_rows = 3,
                              height = 15, width = 15)
plot_list <- collect_heatmaps(plot_list,
                              filtered_data,
                              "FlAsH",
                              clustering_distance_cols = NULL,
                              subset_factor = "cell_background", subset_levels = "dQ+GRK2",
                              # normalize = TRUE, normalize_factor = "GPCR",
                              cutree_rows = 3,
                              height = 15, width = 15)
plot_list <- collect_heatmaps(plot_list,
                              filtered_data,
                              "FlAsH",
                              clustering_distance_cols = NULL,
                              subset_factor = "cell_background", subset_levels = "dQ+GRK6",
                              # normalize = TRUE, normalize_factor = "GPCR",
                              cutree_rows = 3,
                              height = 15, width = 15)
plot_list <- collect_heatmaps(plot_list,
                              filtered_data,
                              "FlAsH",
                              clustering_distance_cols = NULL,
                              subset_factor = "cell_background", subset_levels = "Con",
                              # normalize = TRUE, normalize_factor = "GPCR",
                              cutree_rows = 3,
                              height = 15, width = 15)
plot_list <- collect_heatmaps(plot_list,
                              filtered_data,
                              "FlAsH",
                              clustering_distance_cols = NULL,
                              subset_factor = "cell_background", subset_levels = c("dQ+EV", "Con"),
                              # normalize = TRUE, normalize_factor = "GPCR",
                              cutree_rows = 3,
                              height = 15, width = 15)
plot_list <- collect_heatmaps(plot_list,
                              filtered_data,
                              "FlAsH",
                              clustering_distance_cols = NULL,
                              normalize = TRUE, normalize_factor = "bArr",
                              subset_factor = "GPCR", subset_levels = "b2AR",
                              cutree_rows = 3,
                              height = 15, width = 15)
plot_list <- collect_heatmaps(plot_list,
                              filtered_data,
                              "FlAsH",
                              clustering_distance_cols = NULL,
                              normalize = TRUE, normalize_factor = "bArr",
                              subset_factor = "GPCR", subset_levels = "b2V2",
                              cutree_rows = 3,
                              height = 15, width = 15)
plot_list <- collect_heatmaps(plot_list,
                              filtered_data,
                              "FlAsH",
                              clustering_distance_cols = NULL,
                              subset_factor = "GPCR", subset_levels = "V2b2",
                              normalize = TRUE, normalize_factor = "bArr",
                              cutree_rows = 3,
                              height = 15, width = 15)
plot_list <- collect_heatmaps(plot_list,
                              filtered_data,
                              "FlAsH",
                              clustering_distance_cols = NULL,
                              subset_factor = "GPCR", subset_levels = "V2R",
                              normalize = TRUE, normalize_factor = "bArr",
                              cutree_rows = 3,
                              height = 15, width = 15)
plot_list <- collect_heatmaps(plot_list,
                              filtered_data,
                              "FlAsH",
                              clustering_distance_cols = NULL,
                              subset_factor = "bArr", subset_levels = "bArr1",
                              normalize = TRUE, normalize_factor = "GPCR",
                              cutree_rows = 3,
                              height = 15, width = 15)
plot_list <- collect_heatmaps(plot_list,
                              filtered_data,
                              "FlAsH",
                              clustering_distance_cols = NULL,
                              subset_factor = "bArr", subset_levels = "bArr2",
                              normalize = TRUE, normalize_factor = "GPCR",
                              cutree_rows = 3,
                              height = 15, width = 15)
plot_list <- collect_heatmaps(plot_list,
                              filtered_data[filtered_data$GPCR == "b2AR",],
                              "FlAsH",
                              clustering_distance_cols = NULL,
                              subset_factor = "cell_background", subset_levels = "dQ+GRK2",
                              # normalize = TRUE, normalize_factor = "bArr",
                              # cutree_rows = 3,
                              height = 15, width = 15)
plot_list <- collect_heatmaps(plot_list,
                              filtered_data[filtered_data$GPCR == "b2V2",],
                              "FlAsH",
                              clustering_distance_cols = NULL,
                              subset_factor = "cell_background", subset_levels = "dQ+GRK6",
                              # normalize = TRUE, normalize_factor = "bArr",
                              # cutree_rows = 3,
                              height = 15, width = 15)
plot_list <- collect_heatmaps(plot_list,
                              filtered_data[filtered_data$GPCR == "V2b2",],
                              "FlAsH",
                              clustering_distance_cols = NULL,
                              subset_factor = "cell_background", subset_levels = "dQ+GRK2",
                              # normalize = TRUE, normalize_factor = "bArr",
                              # cutree_rows = 3,
                              height = 15, width = 15)
plot_list <- collect_heatmaps(plot_list,
                              filtered_data[filtered_data$GPCR == "V2R",],
                              "FlAsH",
                              clustering_distance_cols = NULL,
                              # subset_factor = "cell_background", subset_levels = c("dQ+GRK2", "dQ+GRK6"),
                              # normalize = TRUE, normalize_factor = "bArr",
                              cutree_rows = 3,
                              height = 15, width = 15)
plot_list <- collect_heatmaps(plot_list,
                              filtered_data,
                              "FlAsH",
                              clustering_distance_cols = NULL,
                              subset_factor = "cell_background", subset_levels = "dQ+GRK2",
                              normalize = TRUE, normalize_factor = "GPCR",
                              cutree_rows = 3,
                              height = 15, width = 15)
plot_list <- collect_heatmaps(plot_list,
                              filtered_data,
                              "FlAsH",
                              clustering_distance_cols = NULL,
                              subset_factor = "cell_background", subset_levels = "dQ+GRK6",
                              # normalize = TRUE, normalize_factor = "GPCR",
                              cutree_rows = 3,
                              height = 15, width = 15)
### export plots
folder_name <- c("231023_heatmaps")
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}
# setwd(paste0(getwd(), "/", folder_name, "/"))
for (i in seq_along(plot_list)) {
  # file name based on the plot name
  file_name <- paste0(i, "_", names(plot_list)[i], ".png")
  # Save the plot to a file
  ggsave(file_name, plot = plot_list[[i]],
         # width = 10,
         # height = 7
  )
}