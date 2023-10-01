###create needed environment with loading packages
wants <- c("openxlsx",
           "tidyverse",
           "stringr",
           "dplyr",
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
  left_join(classes, by = c("GPCR", "bArr", "cell_background", "FlAsH"))

# # Create the "unclear" column based on the presence of "p" in the original "conc-dep" column
# merged_data <- merged_data %>%
#   mutate(
#     unclear = str_detect(`conc_dep`, "p")
#   )

# Apply the function to specified columns
merged_data <- merged_data %>%
  mutate(
    fit = translate_to_logical(fit),
    outliers_large_spread = translate_to_logical(outliers_large_spread),
    conc_dep = translate_to_logical(conc_dep),
    critical = translate_to_logical(critical)
  )

###
# Load necessary library
library(dplyr)

# Find rows present in fit_pars but not in classes
missing_in_classes <- anti_join(fit_pars, classes,
                                by = c("GPCR", "bArr", "cell_background", "FlAsH"))

# Find rows present in classes but not in fit_pars
missing_in_fit_pars <- anti_join(classes, fit_pars,
                                 by = c("GPCR", "bArr", "cell_background", "FlAsH"))

# Print the missing rows
if (nrow(missing_in_classes) > 0) {
  print("Rows present in fit_pars but not in classes:")
  print(missing_in_classes)
  x <- missing_in_classes
}

if (nrow(missing_in_fit_pars) > 0) {
  print("Rows present in classes but not in fit_pars:")
  print(missing_in_fit_pars)
}

# If no missing rows, print a success message
if (nrow(missing_in_classes) == 0 && nrow(missing_in_fit_pars) == 0) {
  print("All rows match between the two data frames.")
}
