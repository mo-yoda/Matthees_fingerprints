# create needed environment with loading packages
wants <- c("openxlsx",
           "drc", # needed for fit
           "dplyr",
           "Metrics" # needed for RMSE
)

has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
lapply(wants, require, character.only = TRUE)

# office path
# setwd(r"(B:\FuL\IMZ01\Hoffmann\Personal data folders\Mona\Paper\XXX_Matthees et al\goodnis of fit test)")

# homeoffice path
setwd(r"(C:\Users\monar\Google Drive\Arbeit\homeoffice\230809_EM_Goodnis of fit)")

# use crc data with OG ligand concentration
test_df <- read.xlsx("gdf_test_no_log.xlsx")
head(test_df)

# function needed for visualization purposes
# sigmoid = function(params, x) {
#   params[1] / (1 + exp(-params[2] * (x - params[3])))
# }

data_1 <- subset(test_df, condition == "Con_V2R_bArr1-F5")
plot(signal ~ log(AVP),
     data = data_1)
# Ns are plotted, for Con_V2R_bArr1-F5 n=3

#DR curve fitting
# drm() for fitting dose-response models
DR.m <- drm(signal ~ AVP,
            data = data_1,
            # condition, #fit separate curves for each condition
            robust = 'mean', #non-robust least squares estimation ("mean")
            fct = LL.4(names = c("Hill slope", "Min", "Max", "EC50")))
print(DR.m)

plot(signal ~ AVP,
     data = data_1, log = "x")
plot(DR.m, add = TRUE, col = "red")
# fitted EC50 visually fits, tested via:
# points( 0.07778, -15, type = "p" , col = "green")


### all data

#DR curve fitting
DR.m_all <- drm(signal ~ AVP,
                data = test_df,
                # condition, #fit separate curves for each condition
                robust = 'mean', #non-robust least squares estimation ("mean")
                fct = LL.4(names = c("Hill slope", "Min", "Max", "EC50")))
print(DR.m_all)
# works that all fits are generated at once but independent of each other (no diff. to fitting conditions separately)
plot(DR.m_all, add = TRUE, col = "red")
# -> dont know how to select model of specific dataset
# --> thus, fit single datasets!

# subset single conditions
df_1 <- subset(test_df, condition == levels(as.factor(test_df$condition))[1])
df_2 <- subset(test_df, condition == levels(as.factor(test_df$condition))[2])
df_3 <- subset(test_df, condition == levels(as.factor(test_df$condition))[3])

# select df subset
subset_df <- df_3

plot(signal ~ AVP,
     subset_df,
     log = "x",
     col = "red",
     ylim = c(min(test_df$signal), max(test_df$signal)))
DR.m_sub <- drm(signal ~ AVP,
                data = subset_df,
                # condition, #fit separate curves for each condition
                robust = 'mean', #non-robust least squares estimation ("mean"), default
                fct = LL.4(names = c("Hill slope", "Min", "Max", "EC50")),
                separate = TRUE) #LL.4 means four-parameter log-logistic model!
plot(DR.m_sub, add = TRUE)

# "test" gets ligand concentrations, needed for RMSE
test <- as.data.frame(unique(subset_df[, 1]))
# predict(DR.m_sub, newdata = test)

DR.m_sub$coefficients[4]
# EC50 by drm()
# df_1 fit 0.07778
# df_2 fit 0.3007148
# df_3 fit 2.160918
# --> df_3 much bnigger EC50 than df_1 or 2
# two step selection? -> remove EC50 > X
# -> see whether there are still curves which should be removed

# RMSE
rmse(subset_df$signal, predict(DR.m_sub, newdata = test))
# df_1 fit 5.426582
# df_2 fit 22.20422
# df_3 fit 15.17092
# RMSE for bad data much different to nice data!
# BUT: df_2 is a bit better than df_3 in terms of concentration-dependency
# --> this is not reflected in RSME
# prob. due to higher spread in df_3
# --> discuss with Edda how she sees this!

# MAE - mean absolute error (this is less affected by outliers than RSME)
mae(subset_df$signal, predict(DR.m_sub, newdata = test))
# df_1 fit 4.602547
# df_2 fit 17.89563
# df_3 fit 11.73606
# --> gibt sich in diesem fall nicht viel (prozentualer Vergleich)

# get residuals
residuals(DR.m_sub)




#### plan for now: ####
# input -> folder, import all .xlsx (will be exports from prism
# reformat as test_data for this script
##
# fit via drm()
# calc. rmse() via prediction()
# export Hill coeff + EC50 + RMSE (.xlsx table)
# + plot fit together with real data, export as png (use factor name as filename)
# --> use same for all exports ylims!, adjusted to data range

# then, run with all data
# -> plot Hill coeff, EC50 and RSME as boxplots
# are there outliers? how is the distribution?
# define cutoffs - what is a good fit?



