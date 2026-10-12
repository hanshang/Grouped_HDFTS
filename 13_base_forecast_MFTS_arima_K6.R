# ----------------------------------------------------------------------------
# 13_base_forecast_MFTS_arima_K6.R
#
# MFTS base forecasts: ARIMA scores, number of components fixed at K = 6.
#
# Replication code for Shang, H. L., 'High-dimensional functional time
# series forecasting with a group structure'.
#
# Run with the working directory set to the code_submission/ folder.
# Scripts are numbered 01-51 in the order in which they must be run.
# See README.md for a one-line description of each.
# ----------------------------------------------------------------------------

########
# K = 6
########

# subnational (prefecture)

MFTS_model_fun_K6_arima_mae_F = MFTS_model_fun_K6_arima_rmse_F =
MFTS_model_fun_K6_arima_mae_M = MFTS_model_fun_K6_arima_rmse_M =
MFTS_model_fun_K6_arima_mae_T = MFTS_model_fun_K6_arima_rmse_T = matrix(NA, n_state, 10)
for(iwk in 1:10)
{
    # female

    dum = MFTS_model_fun_err(data_var = state_smooth_F_array, test_data = state_F_array, horizon = iwk,
                             ncomp_selection_method = "provide", fmethod = "arima")
    MFTS_model_fun_K6_arima_mae_F[,iwk] = dum$mae_err
    MFTS_model_fun_K6_arima_rmse_F[,iwk] = dum$rmse_err
    rm(dum)

    # male

    dum = MFTS_model_fun_err(data_var = state_smooth_M_array, test_data = state_M_array, horizon = iwk,
                             ncomp_selection_method = "provide", fmethod = "arima")
    MFTS_model_fun_K6_arima_mae_M[,iwk] = dum$mae_err
    MFTS_model_fun_K6_arima_rmse_M[,iwk] = dum$rmse_err
    rm(dum)

    # total

    dum = MFTS_model_fun_err(data_var = state_smooth_T_array, test_data = state_T_array, horizon = iwk,
                             ncomp_selection_method = "provide", fmethod = "arima")
    MFTS_model_fun_K6_arima_mae_T[,iwk] = dum$mae_err
    MFTS_model_fun_K6_arima_rmse_T[,iwk] = dum$rmse_err
    print(iwk); rm(iwk); rm(dum)
}
rownames(MFTS_model_fun_K6_arima_mae_F) = rownames(MFTS_model_fun_K6_arima_rmse_F) =
rownames(MFTS_model_fun_K6_arima_mae_M) = rownames(MFTS_model_fun_K6_arima_rmse_M) =
rownames(MFTS_model_fun_K6_arima_mae_T) = rownames(MFTS_model_fun_K6_arima_rmse_T) = state

colnames(MFTS_model_fun_K6_arima_mae_F) = colnames(MFTS_model_fun_K6_arima_rmse_F) =
colnames(MFTS_model_fun_K6_arima_mae_M) = colnames(MFTS_model_fun_K6_arima_rmse_M) =
colnames(MFTS_model_fun_K6_arima_mae_T) = colnames(MFTS_model_fun_K6_arima_rmse_T) = 1:10

MFTS_model_fun_K6_arima_overall = cbind(colMeans(MFTS_model_fun_K6_arima_mae_F),
                                        colMeans(MFTS_model_fun_K6_arima_rmse_F),

                                        colMeans(MFTS_model_fun_K6_arima_mae_M),
                                        colMeans(MFTS_model_fun_K6_arima_rmse_M),

                                        colMeans(MFTS_model_fun_K6_arima_mae_T),
                                        colMeans(MFTS_model_fun_K6_arima_rmse_T))
colnames(MFTS_model_fun_K6_arima_overall) = c("K6 (arima, F, mae)", "K6 (arima, F, rmse)",
                                              "K6 (arima, M, mae)", "K6 (arima, M, rmse)",
                                              "K6 (arima, T, mae)", "K6 (arima, T, rmse)")


# subnational (region)

MFTS_model_region_fun_K6_arima_mae_F = MFTS_model_region_fun_K6_arima_rmse_F =
MFTS_model_region_fun_K6_arima_mae_M = MFTS_model_region_fun_K6_arima_rmse_M =
MFTS_model_region_fun_K6_arima_mae_T = MFTS_model_region_fun_K6_arima_rmse_T = matrix(NA, n_region, 10)
for(iwk in 1:10)
{
    # female

    dum = MFTS_model_fun_err(data_var = region_smooth_F_array, test_data = region_F_array, horizon = iwk,
                             ncomp_selection_method = "provide", fmethod = "arima")
    MFTS_model_region_fun_K6_arima_mae_F[,iwk] = dum$mae_err
    MFTS_model_region_fun_K6_arima_rmse_F[,iwk] = dum$rmse_err
    rm(dum)

    # male

    dum = MFTS_model_fun_err(data_var = region_smooth_M_array, test_data = region_M_array, horizon = iwk,
                             ncomp_selection_method = "provide", fmethod = "arima")
    MFTS_model_region_fun_K6_arima_mae_M[,iwk] = dum$mae_err
    MFTS_model_region_fun_K6_arima_rmse_M[,iwk] = dum$rmse_err
    rm(dum)

    # total

    dum = MFTS_model_fun_err(data_var = region_smooth_T_array, test_data = region_T_array, horizon = iwk,
                             ncomp_selection_method = "provide", fmethod = "arima")
    MFTS_model_region_fun_K6_arima_mae_T[,iwk] = dum$mae_err
    MFTS_model_region_fun_K6_arima_rmse_T[,iwk] = dum$rmse_err
    print(iwk); rm(iwk); rm(dum)
}
rownames(MFTS_model_region_fun_K6_arima_mae_F) = rownames(MFTS_model_region_fun_K6_arima_rmse_F) =
rownames(MFTS_model_region_fun_K6_arima_mae_M) = rownames(MFTS_model_region_fun_K6_arima_rmse_M) =
rownames(MFTS_model_region_fun_K6_arima_mae_T) = rownames(MFTS_model_region_fun_K6_arima_rmse_T) = region

colnames(MFTS_model_region_fun_K6_arima_mae_F) = colnames(MFTS_model_region_fun_K6_arima_rmse_F) =
colnames(MFTS_model_region_fun_K6_arima_mae_M) = colnames(MFTS_model_region_fun_K6_arima_rmse_M) =
colnames(MFTS_model_region_fun_K6_arima_mae_T) = colnames(MFTS_model_region_fun_K6_arima_rmse_T) = 1:10

######################
# Japan national data
######################

Japan_F_fore_arima_K6 = Japan_M_fore_arima_K6 = Japan_T_fore_arima_K6 = array(NA, dim = c(10, 101, 10))
for(iw in 1:10)
{
    # females

    ind_dat = extract.years(Japan_demo_smooth_F, years = 1975:(2013 + iw))
    fun_forc = forecast(fdm(ind_dat, series = "Female", order = 6), h = 10, method = "arima")
    Japan_F_fore_arima_K6[,,iw] = t(fun_forc$rate$female)

    # males

    ind_dat = extract.years(Japan_demo_smooth_M, years = 1975:(2013 + iw))
    fun_forc = forecast(fdm(ind_dat, series = "Male", order = 6), h = 10, method = "arima")
    Japan_M_fore_arima_K6[,,iw] = t(fun_forc$rate$male)

    # total

    ind_dat = extract.years(Japan_demo_smooth_T, years = 1975:(2013 + iw))
    fun_forc = forecast(fdm(ind_dat, series = "Total", order = 6), h = 10, method = "arima")
    Japan_T_fore_arima_K6[,,iw] = t(fun_forc$rate$total)
    rm(iw)
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
