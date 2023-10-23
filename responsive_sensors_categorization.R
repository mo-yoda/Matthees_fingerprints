###create needed environment with loading packages
wants <- c("openxlsx",
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

# CHOOSE: normalised or non-normalised data
# r"(\start_normalised\)"
# r"(\non_normalised\)"
path_to_fit_paras <- paste0(path, r"(\start_normalised\)", "categories")

setwd(path_to_fit_paras)
fit_pars_og <- readxl::read_xlsx("Fit_parameters_classes.xlsx")

# constrain function
add_constraint_column <- function(df, hill_lower, ec50_upper, ec50_lower) {
  # Create the logical column
  col_name <- paste0("HS_gt_", hill_lower, "_EC50_btwn_", ec50_lower, "_and_", ec50_upper)
  df[[col_name]] <- with(df, Hill_slope > 10^hill_lower & EC50 > 10^ec50_lower & EC50 < 10^ec50_upper)

  # Return the modified dataframe
  return(df)
}

# experiments with Hill_slope >10^-1 and 10^-3.5> EC50 < 10^1
modified_data <- add_constraint_column(fit_pars_og, -1, 1, -3.5)
modified_data <- add_constraint_column(modified_data, -1, 0.3, -3.5)
modified_data <- add_constraint_column(modified_data, -1, 0.3, -2.5)
write_xlsx(modified_data, "Fit_parameters_classes_filtered.xlsx")

filter_comparison <- modified_data[
  !modified_data$`HS_gt_-1_EC50_btwn_-3.5_and_0.3`
    == modified_data$`HS_gt_-1_EC50_btwn_-2.5_and_0.3`,]

filtered_but_concdep <- modified_data[
  modified_data$conc_dep &
    !modified_data$`HS_gt_-1_EC50_btwn_-3.5_and_0.3`, ]
write_xlsx(filtered_but_concdep, "Filtered_but_concDep.xlsx")

notfiltered_but_notconcdep <- modified_data[
  !modified_data$conc_dep &
    modified_data$`HS_gt_-1_EC50_btwn_-3.5_and_0.3`, ]
write_xlsx(notfiltered_but_notconcdep, "NotFiltered_but_NotconcDep.xlsx")

big_HS <- modified_data[modified_data$Hill_slope > 1,]
summary(as.factor(big_HS$FlAsH))
# no specific F position with higher HS
