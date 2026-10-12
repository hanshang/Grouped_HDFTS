# ----------------------------------------------------------------------------
# 18_base_forecast_OWFANOVA_ets.R
#
# One-way FANOVA + functional factor model base forecasts, ETS scores.
#
# Sources: 03_auxiliary_point.R, 01_load_packages.R
#
# Replication code for Shang, H. L., 'High-dimensional functional time
# series forecasting with a group structure'.
#
# Run with the working directory set to the code_submission/ folder.
# Scripts are numbered 01-51 in the order in which they must be run.
# See README.md for a one-line description of each.
# ----------------------------------------------------------------------------

source("03_auxiliary_point.R")
source("01_load_packages.R")

years = 1975:2024

#######################
# one-way FANOVA + FFM
#######################

# data_set: n_age x n_year x n_state
# fh: forecast horizon
# fmethod: forecasting method

fore_rate_one_way_FANOVA <- function(data_set, fh, fmethod)
{
    n_age = dim(data_set)[1]
    n_year = dim(data_set)[2]
    n_state = dim(data_set)[3]

    data_set_mat = matrix(NA, (n_year * n_state), n_age)
    for(ik in 1:n_state)
    {
        data_set_mat[((ik - 1) * n_year + 1):(ik * n_year), ] = t(data_set[,,ik])
        rm(ik)
    }

    FANOVA_means <- One_way_mean(data_pop1 = t(data_set_mat), year = years[1:n_year], age = ages, n_prefecture = n_state)

    # grand effect + row effect + residuals

    FANOVA_means_decomp_combo = matrix(NA, n_age, n_state)
    for(iw in 1:n_state)
    {
        FANOVA_means_decomp_combo[, iw] = FANOVA_means$GE_mean + FANOVA_means$FRE_mean[iw,]
        rm(iw)
    }

    # FANOVA residuals

    FANOVA_residuals <- One_way_mean_residuals(t(data_set_mat), n_prefecture = n_state,
                                               n_year = n_year, n_age = n_age)

    FANOVA_residuals_combo = array(NA, dim = c(n_year, n_state, n_age))
    for(ik in 1:n_state)
    {
        FANOVA_residuals_combo[,ik,] = t(FANOVA_residuals$Residuals[,((ik - 1) * n_year + 1):(ik * n_year)])
        rm(ik)
    }

    # functional factor model: input is sample_size x no_pop x n_age

    HDFTS_factor_decomp_combo <- HDFTS_factor_decomp(FANOVA_residuals_combo)
    HDFTS_factor_decomp_combo_factor_loading <- HDFTS_factor_decomp_combo$factor_loading
    HDFTS_factor_decomp_combo_factors <- HDFTS_factor_decomp_combo$factors
    HDFTS_factor_decomp_combo_residuals <- HDFTS_factor_decomp_combo$error

    # forecasting

    n_factor <- ncol(HDFTS_factor_decomp_combo_factors)
    HDFTS_factor_decomp_combo_factors_forecast = matrix(NA, fh, n_factor)
    for(ik in 1:n_factor)
    {
        if(fmethod == "ets")
        {
            HDFTS_factor_decomp_combo_factors_forecast[,ik] = forecast(ets(HDFTS_factor_decomp_combo_factors[,ik]), h = fh)$mean
        }
        else if(fmethod == "arima")
        {
            HDFTS_factor_decomp_combo_factors_forecast[,ik] = forecast(auto.arima(HDFTS_factor_decomp_combo_factors[,ik]), h = fh)$mean
        }
        rm(ik)
    }

    HDFTS_factor_decomp_combo_forecast <- array(NA, dim = c(fh, n_age, n_state))
    for(ik in 1:n_state)
    {
        HDFTS_factor_decomp_combo_forecast[,,ik] = HDFTS_factor_decomp_combo_factors_forecast %*% HDFTS_factor_decomp_combo_factor_loading[,,ik]
        rm(ik)
    }

    FANOVA_FFM_forecast = array(NA, dim = c(fh, n_age, n_state))
    for(iw in 1:fh)
    {
        FANOVA_FFM_forecast[iw,,] = HDFTS_factor_decomp_combo_forecast[iw,,] + FANOVA_means_decomp_combo
        rm(iw)
    }
    rm(HDFTS_factor_decomp_combo_forecast); rm(FANOVA_means_decomp_combo); rm(n_factor)
    return(FANOVA_FFM_forecast)
}

# smooth_data_set: smooth data set
# testdata_set:
# horizon: forecast horizon
# fore_method: univariate time-series forecasting method "ets" or "arima"

OWFANOVA_fun <- function(smooth_data_set, testdata_set, horizon, fore_method)
{
    n_age = dim(smooth_data_set)[1]
    n_year = dim(smooth_data_set)[2]
    n_state = dim(smooth_data_set)[3]

    forecast_val = array(NA, dim = c(n_age, (11 - horizon), n_state))
    for(ij in 1:(11 - horizon))
    {
        forecast_val[,ij,] = fore_rate_one_way_FANOVA(data_set = smooth_data_set[,1:(n_year - 11 + ij),],
                                                      fh = horizon, fmethod = fore_method)[horizon,,]
        rm(ij)
    }

    # holdout data

    holdout_val   <- array(testdata_set[, (n_year - 10 + horizon):n_year,], dim = c(n_age, (11 - horizon), n_state))

    mae_val = rmse_val = vector("numeric", n_state)
    for(ik in 1:n_state)
    {
        mae_val[ik]  = mae(forecast = forecast_val[,,ik], true = holdout_val[,,ik])
        rmse_val[ik] = rmse(forecast = forecast_val[,,ik], true = holdout_val[,,ik])
        rm(ik)
    }
    rm(holdout_val); rm(forecast_val); rm(n_state); rm(n_year); rm(n_age)
    return(list(mae_val = mae_val, rmse_val = rmse_val))
}

###########################
# subnational (prefecture)
###########################

# ets

n_state = length(state)

OWFANOVA_FFM_ets_mae_male = OWFANOVA_FFM_ets_rmse_male =
OWFANOVA_FFM_ets_mae_female = OWFANOVA_FFM_ets_rmse_female =
OWFANOVA_FFM_ets_mae_total = OWFANOVA_FFM_ets_rmse_total = matrix(NA, n_state, 10)
for(iwk in 1:10)
{
    # female

    dum = OWFANOVA_fun(smooth_data_set = state_smooth_F_array, testdata_set = state_F_array,
                       horizon = iwk, fore_method = "ets")
    OWFANOVA_FFM_ets_mae_female[,iwk] = dum$mae_val
    OWFANOVA_FFM_ets_rmse_female[,iwk] = dum$rmse_val
    rm(dum)

    # male

    dum = OWFANOVA_fun(smooth_data_set = state_smooth_M_array, testdata_set = state_M_array,
                       horizon = iwk, fore_method = "ets")
    OWFANOVA_FFM_ets_mae_male[,iwk] = dum$mae_val
    OWFANOVA_FFM_ets_rmse_male[,iwk] = dum$rmse_val
    rm(dum)

    # total

    dum = OWFANOVA_fun(smooth_data_set = state_smooth_T_array, testdata_set = state_T_array,
                       horizon = iwk, fore_method = "ets")
    OWFANOVA_FFM_ets_mae_total[,iwk] = dum$mae_val
    OWFANOVA_FFM_ets_rmse_total[,iwk] = dum$rmse_val
    print(iwk); rm(iwk); rm(dum)
}
colnames(OWFANOVA_FFM_ets_mae_male) = colnames(OWFANOVA_FFM_ets_mae_female) = colnames(OWFANOVA_FFM_ets_mae_total) =
colnames(OWFANOVA_FFM_ets_rmse_male) = colnames(OWFANOVA_FFM_ets_rmse_female) = colnames(OWFANOVA_FFM_ets_rmse_total) = 1:10

rownames(OWFANOVA_FFM_ets_mae_male) = rownames(OWFANOVA_FFM_ets_mae_female) = rownames(OWFANOVA_FFM_ets_mae_total) =
rownames(OWFANOVA_FFM_ets_rmse_male) = rownames(OWFANOVA_FFM_ets_rmse_female) = rownames(OWFANOVA_FFM_ets_rmse_total) = state

#######################
# subnational (region)
#######################

# fmethod = "ets"

n_region = length(region)

OWFANOVA_FFM_region_ets_mae_male = OWFANOVA_FFM_region_ets_rmse_male =
OWFANOVA_FFM_region_ets_mae_female = OWFANOVA_FFM_region_ets_rmse_female =
OWFANOVA_FFM_region_ets_mae_total = OWFANOVA_FFM_region_ets_rmse_total = matrix(NA, n_region, 10)
for(iwk in 1:10)
{
    # female

    dum = OWFANOVA_fun(smooth_data_set = region_smooth_F_array, testdata_set = region_F_array,
                       horizon = iwk, fore_method = "ets")
    OWFANOVA_FFM_region_ets_mae_female[,iwk] = dum$mae_val
    OWFANOVA_FFM_region_ets_rmse_female[,iwk] = dum$rmse_val
    rm(dum)

    # male

    dum = OWFANOVA_fun(smooth_data_set = region_smooth_M_array, testdata_set = region_M_array,
                       horizon = iwk, fore_method = "ets")
    OWFANOVA_FFM_region_ets_mae_male[,iwk] = dum$mae_val
    OWFANOVA_FFM_region_ets_rmse_male[,iwk] = dum$rmse_val
    rm(dum)

    # total

    dum = OWFANOVA_fun(smooth_data_set = region_smooth_T_array, testdata_set = region_T_array,
                       horizon = iwk, fore_method = "ets")
    OWFANOVA_FFM_region_ets_mae_total[,iwk] = dum$mae_val
    OWFANOVA_FFM_region_ets_rmse_total[,iwk] = dum$rmse_val
    print(iwk); rm(iwk); rm(dum)
}
colnames(OWFANOVA_FFM_region_ets_mae_male) = colnames(OWFANOVA_FFM_region_ets_mae_female) = colnames(OWFANOVA_FFM_region_ets_mae_total) =
colnames(OWFANOVA_FFM_region_ets_rmse_male) = colnames(OWFANOVA_FFM_region_ets_rmse_female) = colnames(OWFANOVA_FFM_region_ets_rmse_total) = 1:10

rownames(OWFANOVA_FFM_region_ets_mae_male) = rownames(OWFANOVA_FFM_region_ets_mae_female) = rownames(OWFANOVA_FFM_region_ets_mae_total) =
rownames(OWFANOVA_FFM_region_ets_rmse_male) = rownames(OWFANOVA_FFM_region_ets_rmse_female) = rownames(OWFANOVA_FFM_region_ets_rmse_total) = region

######################
# Japan national data
######################

Japan_F_fore_ets_K6 = Japan_M_fore_ets_K6 = Japan_T_fore_ets_K6 = array(NA, dim = c(10, 101, 10))
for(iw in 1:10)
{
    # females

    ind_dat = extract.years(Japan_demo_smooth_F, years = 1975:(2013 + iw))
    fun_forc = forecast(fdm(ind_dat, series = "Female", order = 6), h = 10, method = "ets")
    Japan_F_fore_ets_K6[,,iw] = t(fun_forc$rate$female)
    rm(ncomp)

    # males

    ind_dat = extract.years(Japan_demo_smooth_M, years = 1975:(2013 + iw))
    fun_forc = forecast(fdm(ind_dat, series = "Male", order = 6), h = 10, method = "ets")
    Japan_M_fore_ets_K6[,,iw] = t(fun_forc$rate$male)
    rm(ncomp)

    # total

    ind_dat = extract.years(Japan_demo_smooth_T, years = 1975:(2013 + iw))
    fun_forc = forecast(fdm(ind_dat, series = "Total", order = 6), h = 10, method = "ets")
    Japan_T_fore_ets_K6[,,iw] = t(fun_forc$rate$total)
    rm(iw); rm(ncomp)
}

# MAE and RMSE

Japan_F_fore_mae_ets_K6 = Japan_F_fore_rmse_ets_K6 =
Japan_M_fore_mae_ets_K6 = Japan_M_fore_rmse_ets_K6 =
Japan_T_fore_mae_ets_K6 = Japan_T_fore_rmse_ets_K6 = vector("numeric", 10)
for(k in 1:10)
{
    # females

    Japan_F_fore_mae_ets_K6[k]  = mae(Japan_F_fore_ets_K6[k,,1:(11 - k)],  extract.years(Japan_demo_F, years = (2014 + k):2024)$rate$Female)
    Japan_F_fore_rmse_ets_K6[k] = rmse(Japan_F_fore_ets_K6[k,,1:(11 - k)],  extract.years(Japan_demo_F, years = (2014 + k):2024)$rate$Female)

    # males

    Japan_M_fore_mae_ets_K6[k]  = mae(Japan_M_fore_ets_K6[k,,1:(11 - k)],  extract.years(Japan_demo_M, years = (2014 + k):2024)$rate$Male)
    Japan_M_fore_rmse_ets_K6[k] = rmse(Japan_M_fore_ets_K6[k,,1:(11 - k)],  extract.years(Japan_demo_M, years = (2014 + k):2024)$rate$Male)

    # totals

    Japan_T_fore_mae_ets_K6[k]  = mae(Japan_T_fore_ets_K6[k,,1:(11 - k)],  extract.years(Japan_demo_T, years = (2014 + k):2024)$rate$Total)
    Japan_T_fore_rmse_ets_K6[k] = rmse(Japan_T_fore_ets_K6[k,,1:(11 - k)],  extract.years(Japan_demo_T, years = (2014 + k):2024)$rate$Total)
    rm(k)
}
