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

#### create plots from tail-core coeff ####
# Split data into subsets based on cell_background
coeff_subsets <- coefficients_df %>%
  group_by(cell_background) %>%
  group_split()

# Initialize an empty list to store the resulting plots
plot_list <- list()

create_scatterplot <- function(dataframe, coefficient_col,
                               cell_backgrounds_to_show = c("Con", "dQ+EV", "dQ+GRK2", "dQ+GRK6"),
                               set_xlim = TRUE, show_legend = TRUE,
                               scale_points = TRUE) {
  # use colors depending on the levels found in dataframe$cell_background
  costum_colors <- c(Con = "#000080", `dQ+EV` = "#808080", `dQ+GRK2` = "#F94040", `dQ+GRK6` = "#077E97")
  needed_colors <- costum_colors[as.character(unique(dataframe$cell_background))]

  # use this sequence of FlAsH positions
  flash_order <- c("FlAsH1", "FlAsH10", "FlAsH9", "FlAsH7", "FlAsH5", "FlAsH4", "FlAsH3", "FlAsH2")

  # Create a column for controlling visibility based on cell_background
  dataframe$visible <- ifelse(dataframe$cell_background %in% cell_backgrounds_to_show, 1, 0)

  scatterplot <- ggplot(dataframe,
                        aes(x = !!sym(coefficient_col),
                            y = factor(FlAsH, levels = flash_order),
                            size = wildtype_diff, # size of points correspond to WT diff
                            color = cell_background)) +
    geom_vline(xintercept = 0)

  # make sure that if not all cell_backgrounds are supposed to be displayed,
  # others are made invisble (alpha = 0)
  if (length(cell_backgrounds_to_show) < 4) {
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

# create different options of scatterplots
plot_list[["all_data_scatter_scaled"]] <-
  create_scatterplot(coefficients_df, "tail_core_transferabiility_diff")
plot_list[["all_data_scatter_NOTscaled"]] <-
  create_scatterplot(coefficients_df, "tail_core_transferabiility_diff", scale_points = FALSE)

# scaled and non-scaled plots for all cell backgrounds separately
for (condition in levels(as.factor(coefficients_df$cell_background))) {
  plot_name_temp <- paste(condition, "scatter_scaled", sep = "_")
  plot_list[[plot_name_temp]] <- create_scatterplot(coefficients_df, "tail_core_transferabiility_diff",
                                                    cell_backgrounds_to_show = condition)
  plot_name_temp <- paste(condition, "scatter_NOTscaled", sep = "_")
  plot_list[[plot_name_temp]] <- create_scatterplot(coefficients_df, "tail_core_transferabiility_diff",
                                                    cell_backgrounds_to_show = condition, scale_points = FALSE)
}

# scaled plots of phos vs no phos and GRK2 vs GRK6
plot_list[["Con_dQ+EV_scatter_scaled"]] <-
  create_scatterplot(coefficients_df, "tail_core_transferabiility_diff",
                     cell_backgrounds_to_show = c("Con", "dQ+EV"))
plot_list[["dQ+GRK2_dQ+GRK6_scatter_scaled"]] <-
  create_scatterplot(coefficients_df, "tail_core_transferabiility_diff",
                     cell_backgrounds_to_show = c("dQ+GRK2", "dQ+GRK6"))

### Supplementary Figure for coeff explanation ###
# Con b2AR F10, 4 and 1 as examples

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
    x_factor <- "FlAsH"
  } else {
    x_factor <- "GPCR"
  }

  # Construct plot title dynamically
  plot_title <- paste(cell_background_level, FlAsH_level, sep = "_")

  p <- ggplot(data, aes(x = .data[[x_factor]], y = mean_signal)) +
    geom_bar(stat = "identity", position = position_dodge()) + # Use identity stat for pre-summarized data
    theme_classic() +
    theme(plot.title = element_text(size = 20, face = "bold"),  # Increase plot title font size and make it bold
          axis.title = element_text(size = 18),  # Increase axis titles font size
          axis.text.x = element_text(size = 16),  # Increase x axis text font size
          axis.text.y = element_text(size = 16),  # Increase y axis text font size
          legend.title = element_text(size = 16),  # Increase legend title font size
          legend.text = element_text(size = 14)  # Increase legend text font size
    ) +
    labs(title = plot_title, y = "Mean Signal")
  if (normalised) {
    p <- p + coord_cartesian(ylim = c(0, 1))
  } else {
    p <- p +
      coord_cartesian(ylim = c(-54, 0)) +
      geom_hline(yintercept = 0)
  }
  return(p)
}

# define which FlAsH positions should be used as examples
example_flash <- c("FlAsH1", "FlAsH4", "FlAsH10")
for (flash in example_flash) {
  temp_subset <- subset_example_data(mean_norm_data, "Con", flash)
  temp_plot <- plot_example_bars(temp_subset)

  plot_title <- paste("Example_bar", flash, sep = "_")
  plot_list[[plot_title]] <- temp_plot
}

### barplots for normalisation explanation ###
# also, create barplots from non-normalised data to explain normalisation
mean_NOTnorm_data <- calculate_mean_signals(norm_data, signal)

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

plot_list <- fingerprint_barplot(mean_norm_data, normalised = TRUE, plot_list)
plot_list <- fingerprint_barplot(mean_NOTnorm_data, normalised = FALSE, plot_list)

#### export plots ####
export_plot_list <- function(plot_list, folder_name) {
  if (!dir.exists(folder_name)) {
    dir.create(folder_name)
  }
  setwd(paste0(getwd(), "/", folder_name, "/"))
  for (i in seq_along(plot_list)) {
    # file name based on the plot name
    file_names <- paste0(names(plot_list)[i], ".png")
    # Save the plot to a file
    for (file in file_names) {
      if (str_detect(file, "scatter")) {
        width <- 7
        height <- 7
      } else {
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

export_plot_list(plot_list, paste(formatted_date, "tail_core_coeff"))