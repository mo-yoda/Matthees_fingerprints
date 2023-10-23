###create needed environment with loading packages
wants <- c("openxlsx",
           "dplyr",
           "tidyr",
           "pheatmap",
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

  # If cutree is not specified, set to zero
  if (is.na(cutree_rows)) cutree_rows <- 1
  if (is.na(cutree_cols)) cutree_cols <- 1

  # Create a custom annotation bar for the cell numbers
  annotation = NULL
  if (display_numbers) {
    annotation <- matrix_data
    annotation_colors <- colorRampPalette(c("white", "black"))(length(unique(c(matrix_data))))
    names(annotation_colors) <- sort(unique(c(matrix_data)))
  }

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
           border_color = NA)
}


m1 <- create_matrix_from_factors(filtered_data, "FlAsH")

m2 <- create_matrix_from_factors(filtered_data,
                                 "cell_background",
                                 "GPCR",
                                 c("V2R", "V2b2"))

draw_heatmap(m2)
