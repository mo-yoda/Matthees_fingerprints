#### create needed environment with loading packages ####
wants <- c("ggplot2",
           "webr",
           "dplyr")

has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
lapply(wants, require, character.only = TRUE)

filter_stats <- data.frame("classification" = c(rep("non responder", 3),
                                                "responder"),
                           "reason" = c("out of EC50 or HS range",
                                        "manual revision",
                                        "no fit", "responder"),
                           "freq" = c(92, 4, 19, 109))

pie <- PieDonut(filter_stats, aes(classification, reason, count = freq),
                start = 8,
                pieAlpha = 1,
                donutAlpha = 1,
                showPieName = FALSE,
                showRatioThreshold = 0.001,
                labelpositionThreshold = 5,
                donutLabelSize = 5,
                pieLabelSize = 5,
                ratioByGroup = TRUE,
                explode = 2,
                r0 = 0.3, r1 = 0.9)

# PieDonut(filter_stats, aes(reason, classification, count = freq),
#          ratioByGroup = TRUE, explode = 4, r0 = 0.45, r1 = 0.9)

# tower PC path
path <- r"(C:\path\to\folder)"
# laptop path
path <- r"(C:\path\to\folder)"
setwd(path)

# prepare to save plot
bmp(file = "donut_plot_filter_stats.bmp",
    height = 64, width = 64, units = "cm", res = 100)
# generate plot
pie <- PieDonut(filter_stats, aes(classification, reason, count = freq),
                start = 8,
                pieAlpha = 1,
                donutAlpha = 1,
                showPieName = FALSE,
                showRatioThreshold = 0.001,
                labelpositionThreshold = 5,
                # donutLabelSize = 20,
                pieLabelSize = 20,
                ratioByGroup = TRUE,
                explode = 2,
                r0 = 0.3, r1 = 0.9); dev.off()

# second donut layer containing cell backgrounds
filter_stats_cells <- data.frame("classification" = c(rep("responder", 4),
                                                      rep("non responder - filter", 4),
                                                      rep("non responder - no fit", 4),
                                                      rep("non responder - manual", 4)),
                                 "cell" = c(rep(c("dQ+EV", "dQ+GRK2", "dQ+GRK6", "Con"), 4)),
                                 "freq" = c(14, 38, 26, 31, 33, 16, 23, 20, 7, 1, 6, 5, 2, 1, 1, 0))




# prepare to save plot
bmp(file = "donut_plot_filter_stats_Cells.bmp",
    height = 64, width = 64, units = "cm", res = 100)
# generate plot
pie <- PieDonut(filter_stats_cells, aes(classification, cell, count = freq),
                start = 8,
                pieAlpha = 1,
                donutAlpha = 1,
                showPieName = FALSE,
                showRatioThreshold = 0.001,
                labelpositionThreshold = 5,
                # donutLabelSize = 20,
                # pieLabelSize = 20,
                ratioByGroup = TRUE,
                explode = 4,
                r0 = 0.3, r1 = 0.9); dev.off()
