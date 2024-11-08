#### create needed environment with loading packages ####
wants <- c("openxlsx",
           "dplyr",
           "tidyr",
           "ggplot2",
           "writexl",
           "stringr"
)
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
lapply(wants, require, character.only = TRUE)

### import data for plot creation ###
# tower PC path
path <- r"(C:\path\to\folder)"
# laptop path
path <- r"(C:\path\to\folder)"
setwd(path)

# import excel sheets
# for coefficient plots
CC_coefficients_df <- as.data.frame(readxl::read_xlsx("FlAsH_core,tail_coefficients.xlsx"))
assays_coefficients_df <- as.data.frame(readxl::read_xlsx("Assays_core,tail_coefficients.xlsx"))
# for normalisation explanation
CC_mean_norm_data <- as.data.frame(readxl::read_xlsx("CC_mean_normalised_data.xlsx"))
CC_mean_NOTnorm_data <- as.data.frame(readxl::read_xlsx("CC_mean_NOTnormalised_data.xlsx"))

#### create plots from tail-core coeff ####
create_scatterplot <- function(dataframe, coefficient_col = "tail_core_transferabiility_diff",
                               cell_backgrounds_to_show = c("Con", "dQ+EV", "dQ+GRK2", "dQ+GRK6"),
                               set_xlim = TRUE, show_legend = TRUE,
                               scale_points = TRUE) {
  # use colors depending on the levels found in dataframe$cell_background
  costum_colors <- c(Con = "#000080", `dQ+EV` = "#808080", `dQ+GRK2` = "#F94040", `dQ+GRK6` = "#077E97")
  needed_colors <- costum_colors[as.character(unique(dataframe$cell_background))]

  # Determine the factor name and its level order dynamically
  factor_name <- "FlAsH"  # Default to FlAsH if no conditions match
  level_order <- c("FlAsH1", "FlAsH10", "FlAsH9", "FlAsH7", "FlAsH5", "FlAsH4", "FlAsH3", "FlAsH2")

  if (any(str_detect(names(dataframe), "experiment"))) {
    factor_name <- "experiment"
    full_level_order <- c("internalization-bArr2-vs-Rab",
                          "internalization-R-vs-Rab(+bArr2)",
                          "bArr2-recr-bars",
                          "pERK",
                          "phosphorylation-dist",
                          "phosphorylation-prox")

    if (all(cell_backgrounds_to_show %in% c("Con", "dQ+EV"))) {
      level_order <- full_level_order[!full_level_order %in% c("phosphorylation-prox", "phosphorylation-dist")]
    } else if (all(cell_backgrounds_to_show %in% c("dQ+GRK2", "dQ+GRK6"))) {
      level_order <- full_level_order[-which(full_level_order == "pERK")]
    } else {
      level_order <- full_level_order
    }
  }

  # Ensure all levels are represented and create the plot
  dataframe[[factor_name]] <- factor(dataframe[[factor_name]], levels = level_order)
  dataframe <- dataframe[!is.na(dataframe[[factor_name]]),]

  # Create a column for controlling visibility based on cell_background
  dataframe$visible <- ifelse(dataframe$cell_background %in% cell_backgrounds_to_show, 1, 0)

  # Add dummy data to control point scaling
  dummy_data <- dataframe[1:2, , drop = FALSE]  # Copy the first two rows
  dummy_data$cell_background <- "dummy_cell"
  dummy_data$wildtype_diff <- c(0, 1)  # Create two rows with wildtype_diff 0 and 1
  dummy_data$visible <- c(0, 0)
  dataframe <- rbind(dataframe, dummy_data)  # Bind the dummy data to the original dataframe

  scatterplot <- ggplot(dataframe,
                        aes(x = !!sym(coefficient_col),
                            y = factor(.data[[factor_name]], levels = level_order),
                            size = wildtype_diff, # size of points correspond to WT diff
                            color = cell_background)) +
    geom_vline(xintercept = 0)

  # make sure that if not all cell_backgrounds are supposed to be displayed,
  # others are made invisble (alpha = 0)
  if (length(cell_backgrounds_to_show) < 5) {
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

# Initialize an empty list to store the resulting plots
CC_plot_list <- list()
Assay_plot_list <- list()

# create different options of scatterplots
# assay data
Assay_plot_list[["assay_all_data_scatter"]] <- create_scatterplot(assays_coefficients_df)
# conformational change data (CC)
CC_plot_list[["CC_all_data_scatter"]] <- create_scatterplot(CC_coefficients_df)

# scaled plots for cell backgrounds separately
split_cell_background <- list(c("Con", "dQ+EV"), c("dQ+GRK2", "dQ+GRK6"))
cell_backgrounds <- as.list(unique(CC_coefficients_df$cell_background))
conditions <- c(cell_backgrounds, split_cell_background)

for (condition in conditions) {
  plot_name_temp <- paste(
    paste(unlist(condition), collapse = "_"),
    "scatter", sep = "_")
  CC_plot_list[[paste("CC", plot_name_temp, sep = "_")]] <- create_scatterplot(CC_coefficients_df, cell_backgrounds_to_show = condition)
  Assay_plot_list[[paste("assay", plot_name_temp, sep = "_")]] <- create_scatterplot(assays_coefficients_df, cell_backgrounds_to_show = condition)
}

#### Supplementary Figure for coeff explanation ####
# Con b2AR F10, 4 and 1 as examples

# Initialize an empty list to store the resulting plots
Suppl_plot_list <- list()

# function to create subset data for example barplots
subset_example_data <- function(data, cell_background, FlAsH) {
  subset <- data %>%
    filter(cell_background == { { cell_background } } &
             bArr == "bArr2" &
             FlAsH == { { FlAsH } })
  # {{}} needed as function parameters have the same name as column name
  return(subset)
}

plot_example_bars <- function(data, normalised = TRUE) {
  # Extract levels for dynamic title construction
  cell_background_level <- unique(data$cell_background)
  FlAsH_level <- levels(as.factor(data$FlAsH))

  if (length(FlAsH_level) > 1) {
    # plot fingerprint for one GPCR
    fingerprint = TRUE
    x_factor <- "FlAsH"
    level_order <- c("FlAsH2", "FlAsH3", "FlAsH4", "FlAsH5", "FlAsH7", "FlAsH9", "FlAsH10", "FlAsH1")
    plot_title <- paste(cell_background_level, unique(data$GPCR), sep="_")
  } else {
    # plot one F position for all GPCRs
    fingerprint = FALSE
    x_factor <- "GPCR"
    level_order <- levels(as.factor(data$GPCR))
    plot_title <- paste(cell_background_level, FlAsH_level, sep = "_")
  }

  # Adjust data frame based on the x-factor level ordering
  data[[x_factor]] <- factor(data[[x_factor]], levels = level_order)

  p <- ggplot(data, aes(x = .data[[x_factor]], y = mean_signal)) +
    geom_bar(stat = "identity", position = position_dodge()) + # Use identity stat for pre-summarized data
    theme_classic() +
    theme(plot.title = element_text(size = 20, face = "bold"),  # Increase plot title font size and make it bold
          axis.title = element_text(size = 18),  # Increase axis titles font size
          axis.text.x = element_text(size = 16, angle = if (fingerprint) 45, hjust =1),  # Increase x axis text font size
          axis.text.y = element_text(size = 16),  # Increase y axis text font size
          legend.title = element_text(size = 16),  # Increase legend title font size
          legend.text = element_text(size = 14)  # Increase legend text font size
    ) +
    labs(title = plot_title, y = "Mean Signal") +
    geom_hline(yintercept = 0)
  if (normalised) {
    p <- p + coord_cartesian(ylim = c(0, 1))
  } else {
    p <- p +
      coord_cartesian(ylim = c(-54, 0))
  }
  return(p)
}

# define which FlAsH positions should be used as examples
example_flash <- c("FlAsH1", "FlAsH4", "FlAsH10")
for (flash in example_flash) {
  temp_subset <- subset_example_data(CC_mean_norm_data, "Con", flash)
  temp_plot <- plot_example_bars(temp_subset)

  plot_title <- paste("Example_bar", flash, sep = "_")
  Suppl_plot_list[[plot_title]] <- temp_plot
}

#### barplots for normalisation explanation ####
# also, create barplots from non-normalised data to explain normalisation

fingerprint_barplot <- function(data, normalised, plot_list) {
  for (GPCR in levels(as.factor(data$GPCR))) {
    temp_subset <- data %>%
      filter(cell_background == "Con" &
               bArr == "bArr2" &
               GPCR == { { GPCR } })

    temp_plot <- plot_example_bars(temp_subset, normalised)

    if (normalised) {
      status <- "normalized"
    } else {
      status <- "NOTnormalized"
    }
    GPCR <- unique(temp_subset$GPCR)

    plot_title <- paste(
      status, GPCR, sep = "_")
    plot_list[[plot_title]] <- temp_plot
  }
  return(plot_list)
}

Suppl_plot_list <- fingerprint_barplot(CC_mean_norm_data, normalised = TRUE, Suppl_plot_list)
Suppl_plot_list <- fingerprint_barplot(CC_mean_NOTnorm_data, normalised = FALSE, Suppl_plot_list)

#### export plots ####
export_plot_list <- function(plot_list, folder_name) {
  # create folder, if it does not exist yet
  if (!dir.exists(folder_name)) {
    dir.create(folder_name)
  }
  setwd(paste0(getwd(), "/", folder_name, "/"))
  for (i in seq_along(plot_list)) {
    # file name based on the plot name
    file_names <- paste0(names(plot_list)[i], ".png")
    # Save the plot to a file
    for (file in file_names) {
      if (str_detect(file, "CC")) {
        # sizing for conformational change sensors
        width <- 7
        height <- 7
      } else if (str_detect(file, "assay")) {
        # sizing for assay scatter plots
        width <- 10
        height <- 7
      } else if (str_detect(file, "Example")) {
        width <- 4
        height <- 5
      } else {
        # sizing of barcharts
        width <- 5
        height <- 5
      }
      ggsave(file, plot = plot_list[[i]],
             width = width,
             height = height
      )
    }
  }
}

# get today's date for export folder
today_date <- Sys.Date()
formatted_date <- format(today_date, "%Y-%m-%d")

# export CC-based plots
setwd(path)
export_plot_list(CC_plot_list, paste(formatted_date, "CC_tail_core_coeff", sep = "_"))
# export other assay data plots
setwd(path)
export_plot_list(Assay_plot_list, paste(formatted_date, "Assay_tail_core_coeff", sep = "_"))
# export plots for suppl figure
setwd(path)
export_plot_list(Suppl_plot_list, paste(formatted_date, "Coefficient_supplementary", sep = "_"))
