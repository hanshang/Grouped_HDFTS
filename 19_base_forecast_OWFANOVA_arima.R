# ----------------------------------------------------------------------------
# 19_base_forecast_OWFANOVA_arima.R
#
# One-way FANOVA + functional factor model base forecasts, ARIMA scores.
#
# Replication code for Shang, H. L., 'High-dimensional functional time
# series forecasting with a group structure'.
#
# Run with the working directory set to the code_submission/ folder.
# Scripts are numbered 01-51 in the order in which they must be run.
# See README.md for a one-line description of each.
# ----------------------------------------------------------------------------

###########################
# subnational (prefecture)
###########################

# fmethod = "arima"

OWFANOVA_FFM_arima_mae_male = OWFANOVA_FFM_arima_rmse_male =
OWFANOVA_FFM_arima_mae_female = OWFANOVA_FFM_arima_rmse_female =
OWFANOVA_FFM_arima_mae_total = OWFANOVA_FFM_arima_rmse_total = matrix(NA, n_state, 10)
for(iwk in 1:10)
{
    # female

    dum = OWFANOVA_fun(smooth_data_set = state_smooth_F_array, testdata_set = state_F_array,
                       horizon = iwk, fore_method = "arima")
    OWFANOVA_FFM_arima_mae_female[,iwk] = dum$mae_val
    OWFANOVA_FFM_arima_rmse_female[,iwk] = dum$rmse_val
    rm(dum)

    # male

    dum = OWFANOVA_fun(smooth_data_set = state_smooth_M_array, testdata_set = state_M_array,
                       horizon = iwk, fore_method = "arima")
    OWFANOVA_FFM_arima_mae_male[,iwk] = dum$mae_val
    OWFANOVA_FFM_arima_rmse_male[,iwk] = dum$rmse_val
    rm(dum)

    # total

    dum = OWFANOVA_fun(smooth_data_set = state_smooth_T_array, testdata_set = state_T_array,
                       horizon = iwk, fore_method = "arima")
    OWFANOVA_FFM_arima_mae_total[,iwk] = dum$mae_val
    OWFANOVA_FFM_arima_rmse_total[,iwk] = dum$rmse_val
    print(iwk); rm(iwk); rm(dum)
}
colnames(OWFANOVA_FFM_arima_mae_male) = colnames(OWFANOVA_FFM_arima_mae_female) = colnames(OWFANOVA_FFM_arima_mae_total) =
colnames(OWFANOVA_FFM_arima_rmse_male) = colnames(OWFANOVA_FFM_arima_rmse_female) = colnames(OWFANOVA_FFM_arima_rmse_total) = 1:10

rownames(OWFANOVA_FFM_arima_mae_male) = rownames(OWFANOVA_FFM_arima_mae_female) = rownames(OWFANOVA_FFM_arima_mae_total) =
rownames(OWFANOVA_FFM_arima_rmse_male) = rownames(OWFANOVA_FFM_arima_rmse_female) = rownames(OWFANOVA_FFM_arima_rmse_total) = state

#######################
# subnational (region)
#######################

# fmethod = "arima"

OWFANOVA_FFM_region_arima_mae_male = OWFANOVA_FFM_region_arima_rmse_male =
OWFANOVA_FFM_region_arima_mae_female = OWFANOVA_FFM_region_arima_rmse_female =
OWFANOVA_FFM_region_arima_mae_total = OWFANOVA_FFM_region_arima_rmse_total = matrix(NA, n_region, 10)
for(iwk in 1:10)
{
    # female

    dum = OWFANOVA_fun(smooth_data_set = region_smooth_F_array, testdata_set = region_F_array,
                       horizon = iwk, fore_method = "arima")
    OWFANOVA_FFM_region_arima_mae_female[,iwk] = dum$mae_val
    OWFANOVA_FFM_region_arima_rmse_female[,iwk] = dum$rmse_val
    rm(dum)

    # male

    dum = OWFANOVA_fun(smooth_data_set = region_smooth_M_array, testdata_set = region_M_array,
                       horizon = iwk, fore_method = "arima")
    OWFANOVA_FFM_region_arima_mae_male[,iwk] = dum$mae_val
    OWFANOVA_FFM_region_arima_rmse_male[,iwk] = dum$rmse_val
    rm(dum)

    # total

    dum = OWFANOVA_fun(smooth_data_set = region_smooth_T_array, testdata_set = region_T_array,
                       horizon = iwk, fore_method = "arima")
    OWFANOVA_FFM_region_arima_mae_total[,iwk] = dum$mae_val
    OWFANOVA_FFM_region_arima_rmse_total[,iwk] = dum$rmse_val
    print(iwk); rm(iwk); rm(dum)
}
colnames(OWFANOVA_FFM_region_arima_mae_male) = colnames(OWFANOVA_FFM_region_arima_mae_female) = colnames(OWFANOVA_FFM_region_arima_mae_total) =
colnames(OWFANOVA_FFM_region_arima_rmse_male) = colnames(OWFANOVA_FFM_region_arima_rmse_female) = colnames(OWFANOVA_FFM_region_arima_rmse_total) = 1:10

rownames(OWFANOVA_FFM_region_arima_mae_male) = rownames(OWFANOVA_FFM_region_arima_mae_female) = rownames(OWFANOVA_FFM_region_arima_mae_total) =
rownames(OWFANOVA_FFM_region_arima_rmse_male) = rownames(OWFANOVA_FFM_region_arima_rmse_female) = rownames(OWFANOVA_FFM_region_arima_rmse_total) = region

######################
# Japan national data
######################

Japan_F_fore_arima_K6 = Japan_M_fore_arima_K6 = Japan_T_fore_arima_K6 = array(NA, dim = c(10, 101, 10))
for(iw in 1:10)
{
    # females

    ind_dat = extract.years(Japan_demo_smooth_F, years = 1975:(2013 + iw))
    fun_forc = forecast(fdm(ind_dat, series = "Female", order = 6), h = 10, method = "ets")
    Japan_F_fore_arima_K6[,,iw] = t(fun_forc$rate$female)
    rm(ncomp)

    # males

    ind_dat = extract.years(Japan_demo_smooth_M, years = 1975:(2013 + iw))
    fun_forc = forecast(fdm(ind_dat, series = "Male", order = 6), h = 10, method = "ets")
    Japan_M_fore_arima_K6[,,iw] = t(fun_forc$rate$male)
    rm(ncomp)

    # total

    ind_dat = extract.years(Japan_demo_smooth_T, years = 1975:(2013 + iw))
    fun_forc = forecast(fdm(ind_dat, series = "Total", order = 6), h = 10, method = "ets")
    Japan_T_fore_arima_K6[,,iw] = t(fun_forc$rate$total)
    rm(iw); rm(ncomp)
}

# MAE and RMSE

Japan_F_fore_mae_arima_K6 = Japan_F_fore_rmse_arima_K6 =
Japan_M_fore_mae_arima_K6 = Japan_M_fore_rmse_arima_K6 =
Japan_T_fore_mae_arima_K6 = Japan_T_fore_rmse_arima_K6 = vector("numeric", 10)
for(k in 1:10)
{
    # females

    Japan_F_fore_mae_arima_K6[k]  = mae(Japan_F_fore_arima_K6[k,,1:(11 - k)],  extract.years(Japan_demo_F, years = (2014 + k):2024)$rate$Female)
    Japan_F_fore_rmse_arima_K6[k] = rmse(Japan_F_fore_arima_K6[k,,1:(11 - k)],  extract.years(Japan_demo_F, years = (2014 + k):2024)$rate$Female)

    # males

    Japan_M_fore_mae_arima_K6[k]  = mae(Japan_M_fore_arima_K6[k,,1:(11 - k)],  extract.years(Japan_demo_M, years = (2014 + k):2024)$rate$Male)
    Japan_M_fore_rmse_arima_K6[k] = rmse(Japan_M_fore_arima_K6[k,,1:(11 - k)],  extract.years(Japan_demo_M, years = (2014 + k):2024)$rate$Male)

    # totals

    Japan_T_fore_mae_arima_K6[k]  = mae(Japan_T_fore_arima_K6[k,,1:(11 - k)],  extract.years(Japan_demo_T, years = (2014 + k):2024)$rate$Total)
    Japan_T_fore_rmse_arima_K6[k] = rmse(Japan_T_fore_arima_K6[k,,1:(11 - k)],  extract.years(Japan_demo_T, years = (2014 + k):2024)$rate$Total)
    rm(k)
}
