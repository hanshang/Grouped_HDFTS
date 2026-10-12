# ----------------------------------------------------------------------------
# 10_base_forecast_MFTS_ets_EVR.R
#
# MFTS base forecasts: ETS scores, components chosen by the EVR criterion.
#
# Sources: 03_auxiliary_point.R
#
# Replication code for Shang, H. L., 'High-dimensional functional time
# series forecasting with a group structure'.
#
# Run with the working directory set to the code_submission/ folder.
# Scripts are numbered 01-51 in the order in which they must be run.
# See README.md for a one-line description of each.
# ----------------------------------------------------------------------------

source("03_auxiliary_point.R")

##########################
# subnational prefectures
##########################

## ETS

n_state = length(state)

# EVR

MFTS_model_fun_EVR_ets_mae_F = MFTS_model_fun_EVR_ets_rmse_F =
MFTS_model_fun_EVR_ets_mae_M = MFTS_model_fun_EVR_ets_rmse_M =
MFTS_model_fun_EVR_ets_mae_T = MFTS_model_fun_EVR_ets_rmse_T = matrix(NA, n_state, 10)
for(iwk in 1:10)
{
    # female

    dum = MFTS_model_fun_err(data_var = state_smooth_F_array, test_data = state_F_array, horizon = iwk,
                             ncomp_selection_method = "EVR", fmethod = "ets")
    MFTS_model_fun_EVR_ets_mae_F[,iwk] = dum$mae_err
    MFTS_model_fun_EVR_ets_rmse_F[,iwk] = dum$rmse_err
    rm(dum)

    # male

    dum = MFTS_model_fun_err(data_var = state_smooth_M_array, test_data = state_M_array, horizon = iwk,
                             ncomp_selection_method = "EVR", fmethod = "ets")
    MFTS_model_fun_EVR_ets_mae_M[,iwk] = dum$mae_err
    MFTS_model_fun_EVR_ets_rmse_M[,iwk] = dum$rmse_err
    rm(dum)

    # total

    dum = MFTS_model_fun_err(data_var = state_smooth_T_array, test_data = state_T_array, horizon = iwk,
                             ncomp_selection_method = "EVR", fmethod = "ets")
    MFTS_model_fun_EVR_ets_mae_T[,iwk] = dum$mae_err
    MFTS_model_fun_EVR_ets_rmse_T[,iwk] = dum$rmse_err
    print(iwk); rm(iwk); rm(dum)
}
rownames(MFTS_model_fun_EVR_ets_mae_F) = rownames(MFTS_model_fun_EVR_ets_rmse_F) =
rownames(MFTS_model_fun_EVR_ets_mae_M) = rownames(MFTS_model_fun_EVR_ets_rmse_M) =
rownames(MFTS_model_fun_EVR_ets_mae_T) = rownames(MFTS_model_fun_EVR_ets_rmse_T) = state

colnames(MFTS_model_fun_EVR_ets_mae_F) = colnames(MFTS_model_fun_EVR_ets_rmse_F) =
colnames(MFTS_model_fun_EVR_ets_mae_M) = colnames(MFTS_model_fun_EVR_ets_rmse_M) =
colnames(MFTS_model_fun_EVR_ets_mae_T) = colnames(MFTS_model_fun_EVR_ets_rmse_T) = 1:10

MFTS_model_fun_EVR_ets_overall = cbind(colMeans(MFTS_model_fun_EVR_ets_mae_F),
                                       colMeans(MFTS_model_fun_EVR_ets_rmse_F),

                                       colMeans(MFTS_model_fun_EVR_ets_mae_M),
                                       colMeans(MFTS_model_fun_EVR_ets_rmse_M),

                                       colMeans(MFTS_model_fun_EVR_ets_mae_T),
                                       colMeans(MFTS_model_fun_EVR_ets_rmse_T))
colnames(MFTS_model_fun_EVR_ets_overall) = c("EVR (ets, F, mae)", "EVR (ets, F, rmse)",
                                             "EVR (ets, M, mae)", "EVR (ets, M, rmse)",
                                             "EVR (ets, T, mae)", "EVR (ets, T, rmse)")

#######
## ETS
#######

# EVR

MFTS_model_region_fun_EVR_ets_mae_F = MFTS_model_region_fun_EVR_ets_rmse_F =
MFTS_model_region_fun_EVR_ets_mae_M = MFTS_model_region_fun_EVR_ets_rmse_M =
MFTS_model_region_fun_EVR_ets_mae_T = MFTS_model_region_fun_EVR_ets_rmse_T = matrix(NA, n_region, 10)
for(iwk in 1:10)
{
    # female

    dum = MFTS_model_fun_err(data_var = region_smooth_F_array, test_data = region_F_array, horizon = iwk,
                             ncomp_selection_method = "EVR", fmethod = "ets")
    MFTS_model_region_fun_EVR_ets_mae_F[,iwk] = dum$mae_err
    MFTS_model_region_fun_EVR_ets_rmse_F[,iwk] = dum$rmse_err
    rm(dum)

    # male

    dum = MFTS_model_fun_err(data_var = region_smooth_M_array, test_data = region_M_array, horizon = iwk,
                             ncomp_selection_method = "EVR", fmethod = "ets")
    MFTS_model_region_fun_EVR_ets_mae_M[,iwk] = dum$mae_err
    MFTS_model_region_fun_EVR_ets_rmse_M[,iwk] = dum$rmse_err
    rm(dum)

    # total

    dum = MFTS_model_fun_err(data_var = region_smooth_T_array, test_data = region_T_array, horizon = iwk,
                             ncomp_selection_method = "EVR", fmethod = "ets")
    MFTS_model_region_fun_EVR_ets_mae_T[,iwk] = dum$mae_err
    MFTS_model_region_fun_EVR_ets_rmse_T[,iwk] = dum$rmse_err
    print(iwk); rm(iwk); rm(dum)
}
rownames(MFTS_model_region_fun_EVR_ets_mae_F) = rownames(MFTS_model_region_fun_EVR_ets_rmse_F) =
rownames(MFTS_model_region_fun_EVR_ets_mae_M) = rownames(MFTS_model_region_fun_EVR_ets_rmse_M) =
rownames(MFTS_model_region_fun_EVR_ets_mae_T) = rownames(MFTS_model_region_fun_EVR_ets_rmse_T) = region

colnames(MFTS_model_region_fun_EVR_ets_mae_F) = colnames(MFTS_model_region_fun_EVR_ets_rmse_F) =
colnames(MFTS_model_region_fun_EVR_ets_mae_M) = colnames(MFTS_model_region_fun_EVR_ets_rmse_M) =
colnames(MFTS_model_region_fun_EVR_ets_mae_T) = colnames(MFTS_model_region_fun_EVR_ets_rmse_T) = 1:10

######################
# Japan national data
######################

Japan_F_fore_ets_EVR = Japan_M_fore_ets_EVR = Japan_T_fore_ets_EVR = array(NA, dim = c(10, 101, 10))
for(iw in 1:10)
{
    # females

    ind_dat = extract.years(Japan_demo_smooth_F, years = 1975:(2013 + iw))
    ncomp = select_K(tau = 10^-3, eigen(cov(t(ind_dat$rate$Female)))$values)
    fun_forc = forecast(fdm(ind_dat, series = "Female", order = ncomp), h = 10, method = "ets")
    Japan_F_fore_ets_EVR[,,iw] = t(fun_forc$rate$female)
    rm(ncomp)

    # males

    ind_dat = extract.years(Japan_demo_smooth_M, years = 1975:(2013 + iw))
    ncomp = select_K(tau = 10^-3, eigen(cov(t(ind_dat$rate$Male)))$values)
    fun_forc = forecast(fdm(ind_dat, series = "Male", order = ncomp), h = 10, method = "ets")
    Japan_M_fore_ets_EVR[,,iw] = t(fun_forc$rate$male)
    rm(ncomp)

    # total

    ind_dat = extract.years(Japan_demo_smooth_T, years = 1975:(2013 + iw))
    ncomp = select_K(tau = 10^-3, eigen(cov(t(ind_dat$rate$Total)))$values)
    fun_forc = forecast(fdm(ind_dat, series = "Total", order = ncomp), h = 10, method = "ets")
    Japan_T_fore_ets_EVR[,,iw] = t(fun_forc$rate$total)
    rm(iw); rm(ncomp)
}

# MAE and RMSE

Japan_F_fore_mae_ets_EVR = Japan_F_fore_rmse_ets_EVR =
Japan_M_fore_mae_ets_EVR = Japan_M_fore_rmse_ets_EVR =
Japan_T_fore_mae_ets_EVR = Japan_T_fore_rmse_ets_EVR = vector("numeric", 10)
for(k in 1:10)
{
    # females

    Japan_F_fore_mae_ets_EVR[k]  = mae(Japan_F_fore_ets_EVR[k,,1:(11 - k)],  extract.years(Japan_demo_F, years = (2014 + k):2024)$rate$Female)
    Japan_F_fore_rmse_ets_EVR[k] = rmse(Japan_F_fore_ets_EVR[k,,1:(11 - k)],  extract.years(Japan_demo_F, years = (2014 + k):2024)$rate$Female)

    # males

    Japan_M_fore_mae_ets_EVR[k]  = mae(Japan_M_fore_ets_EVR[k,,1:(11 - k)],  extract.years(Japan_demo_M, years = (2014 + k):2024)$rate$Male)
    Japan_M_fore_rmse_ets_EVR[k] = rmse(Japan_M_fore_ets_EVR[k,,1:(11 - k)],  extract.years(Japan_demo_M, years = (2014 + k):2024)$rate$Male)

    # totals

    Japan_T_fore_mae_ets_EVR[k]  = mae(Japan_T_fore_ets_EVR[k,,1:(11 - k)],  extract.years(Japan_demo_T, years = (2014 + k):2024)$rate$Total)
    Japan_T_fore_rmse_ets_EVR[k] = rmse(Japan_T_fore_ets_EVR[k,,1:(11 - k)],  extract.years(Japan_demo_T, years = (2014 + k):2024)$rate$Total)
    rm(k)
}
