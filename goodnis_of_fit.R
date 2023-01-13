library(openxlsx)
library(drc) # needed for fit
library(dplyr)
library(Metrics) # needed for RMSE


setwd(r"(B:\FuL\IMZ01\Hoffmann\Personal data folders\Mona\Paper\XXX_Matthees et al\goodnis of fit test)")

test_df <- read.xlsx("gdf_test_no_log.xlsx")

head(test_df)


# function needed for visualization purposes
# sigmoid = function(params, x) {
#   params[1] / (1 + exp(-params[2] * (x - params[3])))
# }

data_1 <- subset(test_df, condition == "Con_V2R_bArr1-F5")
plot(signal ~ log(AVP),
            data= data_1)

#DR curve fitting
DR.m <- drm(signal ~ AVP,
            data= data_1,
            condition, #fit separate curves for each condition
            robust = 'mean', #non-robust least squares estimation ("mean")
            fct = LL.4(names = c("Hill slope", "Min", "Max", "EC50")))
print(DR.m)

plot(signal ~ AVP,
            data= data_1, log = "x")
plot(DR.m, add=TRUE)


### all data

#DR curve fitting
DR.m_all <- drm(signal ~ AVP,
            data= test_df,
            condition, #fit separate curves for each condition
            robust = 'mean', #non-robust least squares estimation ("mean")
            fct = LL.4(names = c("Hill slope", "Min", "Max", "EC50")))
print(DR.m_all)

# does not plot fits and all data together

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
      ylim = c(min(test_df$signal),max(test_df$signal)))
DR.m_sub <- drm(signal ~ AVP,
            data= subset_df,
            condition, #fit separate curves for each condition
            robust = 'mean', #non-robust least squares estimation ("mean")
            fct = LL.4(names = c("Hill slope", "Min", "Max", "EC50")),
separate = TRUE) #LL.4 means four-parameter log-logistic model!
plot(DR.m_sub, add=TRUE)




test <- as.data.frame(unique(subset_df[,1]))
# predict(DR.m_sub, newdata = test)


# RMSE
rmse(df_3$signal, predict(DR.m_sub, newdata = test))
# df_1 fit compare df_1 5.426582
# df_1 fit compare df_2 23.51098
# df_1 fit compare df_3 20.29443

# df_2 fit compare df_1 9.096881
# df_2 fit compare df_2 22.20422
# df_2 fit compare df_3 17.3022

# df_3 fit compare df_1 14.11679
# df_3 fit compare df_2 23.63923
# df_3 fit compare df_3 15.17092

# RMSE for bad data much different to nice data!



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



