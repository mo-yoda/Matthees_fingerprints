###create needed environment with loading packages
wants <- c("openxlsx",
           "dplyr",
           "tidyr",
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

# Create matrix for heatmap

# Helper function to create the matrix
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

m1 <- create_matrix_from_factors(filtered_data, "FlAsH")

m2 <- create_matrix_from_factors(filtered_data,
                                 "cell_background",
                                 "GPCR",
                                 c("V2R", "V2b2"))


