# ----------------------------------------------------------------------------
# 20_base_forecast_TWFANOVA.R
#
# Two-way FANOVA base forecasts (exploratory; not reported in the paper).
#
# Sources: 01_load_packages.R, 03_auxiliary_point.R
#
# Replication code for Shang, H. L., 'High-dimensional functional time
# series forecasting with a group structure'.
#
# Run with the working directory set to the code_submission/ folder.
# Scripts are numbered 01-51 in the order in which they must be run.
# See README.md for a one-line description of each.
# ----------------------------------------------------------------------------

source("01_load_packages.R")
source("03_auxiliary_point.R")

mae = ftsa:::mae
rmse = ftsa:::rmse

years = 1975:2024
ages = 0:100


# data_set_M: (n_year x n_age x n_state) male data
# data_set_F: (n_year x n_age x n_state) female data
# fh: forecast horizon
# fmethod: forecasting method, such as ets or arima

fore_rate_FANOVA <- function(data_set_M, data_set_F, fh, fmethod)
{
      n_year = nrow(data_set_M)
      n_age  = ncol(data_set_M)
      n_state = dim(data_set_M)[3]

      data_set_M_mat = data_set_F_mat = matrix(NA, (n_year * n_state), n_age)
      for(ik in 1:n_state)
      {
            data_set_M_mat[((ik - 1) * n_year + 1):(ik * n_year), ] = data_set_M[,,ik]
            data_set_F_mat[((ik - 1) * n_year + 1):(ik * n_year), ] = data_set_F[,,ik]
            rm(ik)
      }

      FANOVA_means <- Two_way_mean(data_pop1 = t(data_set_M_mat), data_pop2 = t(data_set_F_mat),
                                 year = years[1:n_year], age = ages, n_prefecture = n_state, n_populations = 2)

      # grand effect + row effect + column effect

      FANOVA_means_decomp_combo = array(NA, dim = c(n_age, n_state, 2), dimnames = list(ages, state, c("M", "F")))
    for(iw in 1:n_state)
      {
            for(ij in 1:2)
            {
                  FANOVA_means_decomp_combo[, iw, ij] = FANOVA_means$FGE_mean + FANOVA_means$FRE_mean[iw,] + FANOVA_means$FCE_mean[ij,]
                  rm(ij)
            }
            rm(iw)
      }

      # FANOVA residuals

      FANOVA_residuals <- Two_way_mean_residuals(data_pop1 = t(data_set_M_mat), data_pop2 = t(data_set_F_mat),
                                                 year = years[1:n_year], age = ages, n_prefecture = n_state, n_populations = 2)

      FANOVA_residuals_male <- array(FANOVA_residuals$residuals1_mean, dim = c(n_year, n_state, n_age), dimnames = list(years[1:n_year], state, ages))
      FANOVA_residuals_female <- array(FANOVA_residuals$residuals2_mean, dim = c(n_year, n_state, n_age), dimnames = list(years[1:n_year], state, ages))

      # stacking male and female residuals

      FANOVA_residuals_combo = array(NA, dim = c(n_year, (n_state * 2), n_age), dimnames = list(years[1:n_year], c(state, state), ages))
      FANOVA_residuals_combo[,1:n_state,] = FANOVA_residuals_male
      FANOVA_residuals_combo[,(n_state + 1):(n_state * 2),] = FANOVA_residuals_female

      # functional factor model

      HDFTS_factor_decomp_combo <- HDFTS_factor_decomp(FANOVA_residuals_combo)
      HDFTS_factor_decomp_combo_factor_loading <- HDFTS_factor_decomp_combo$factor_loading
      HDFTS_factor_decomp_combo_factors <- HDFTS_factor_decomp_combo$factors
      HDFTS_factor_decomp_combo_residuals <- HDFTS_factor_decomp_combo$error

      # forecasting

      n_factor <- ncol(HDFTS_factor_decomp_combo_factors)
      HDFTS_factor_decomp_combo_factors_forecast = matrix(NA, 1, n_factor)
      for(ik in 1:n_factor)
      {
          if(fmethod == "ets")
          {
              HDFTS_factor_decomp_combo_factors_forecast[,ik] = forecast(ets(HDFTS_factor_decomp_combo_factors[,ik]), h = fh)$mean[fh]
          }
          else if(fmethod == "arima")
          {
              HDFTS_factor_decomp_combo_factors_forecast[,ik] = forecast(auto.arima(HDFTS_factor_decomp_combo_factors[,ik]), h = fh)$mean[fh]
          }
          rm(ik)
      }

      HDFTS_factor_decomp_combo_forecast <- matrix(NA, n_age, (n_state * 2))
      for(ik in 1:(n_state * 2))
      {
          HDFTS_factor_decomp_combo_forecast[,ik] = as.numeric(HDFTS_factor_decomp_combo_factors_forecast %*% HDFTS_factor_decomp_combo_factor_loading[,,ik])
          rm(ik)
    }
      HDFTS_factor_decomp_combo_forecast_male = HDFTS_factor_decomp_combo_forecast[,1:n_state]
      HDFTS_factor_decomp_combo_forecast_female = HDFTS_factor_decomp_combo_forecast[,(n_state + 1):(n_state * 2)]

      HDFTS_factor_decomp_combo_forecast_array = array(NA, dim = c(n_age, n_state, 2), dimnames = list(ages, state, c("M", "F")))
      HDFTS_factor_decomp_combo_forecast_array[,,1] = HDFTS_factor_decomp_combo_forecast_male
      HDFTS_factor_decomp_combo_forecast_array[,,2] = HDFTS_factor_decomp_combo_forecast_female

      # adding FANOVA means back

      FANOVA_FFM_forecast = HDFTS_factor_decomp_combo_forecast_array + FANOVA_means_decomp_combo
      FANOVA_FFM_forecast_male = FANOVA_FFM_forecast[,,1]
      FANOVA_FFM_forecast_female = FANOVA_FFM_forecast[,,2]
      return(list(male_fore = FANOVA_FFM_forecast_male, female_fore = FANOVA_FFM_forecast_female))
}

TWFANOVA_fun <- function(smooth_data_set_M, smooth_data_set_F, testdata_set_M, testdata_set_F, horizon, fore_method)
{
    n_age = dim(smooth_data_set_M)[1]
    n_year = dim(smooth_data_set_M)[2]
    n_state = dim(smooth_data_set_M)[3]
    forecast_val_male = forecast_val_female = array(NA, dim = c(n_age, (11 - horizon), n_state))
    for(ij in 1:(11 - horizon))
    {
        dum = fore_rate_FANOVA(data_set_F = aperm(smooth_data_set_F, c(2, 1, 3))[1:(n_year - 11 + ij),,],
                               data_set_M = aperm(smooth_data_set_M, c(2, 1, 3))[1:(n_year - 11 + ij),,],
                               fh = horizon, fmethod = fore_method)
        forecast_val_male[,ij,] = dum$male_fore
        forecast_val_female[,ij,] = dum$female_fore
        rm(ij)
    }

    # holdout data

    holdout_val_male   <- array(testdata_set_M[, (n_year - 10 + horizon):n_year,], dim = c(n_age, (11 - horizon), n_state))
    holdout_val_female <- array(testdata_set_F[, (n_year - 10 + horizon):n_year,], dim = c(n_age, (11 - horizon), n_state))

    mae_male = rmse_male = mae_female = rmse_female = vector("numeric", n_state)
    for(ik in 1:n_state)
    {
        # male

        mae_male[ik]  = mae(forecast = forecast_val_male[,,ik], true = holdout_val_male[,,ik])
        rmse_male[ik] = rmse(forecast = forecast_val_male[,,ik], true = holdout_val_male[,,ik])

        # female

        mae_female[ik]  = mae(forecast = forecast_val_female[,,ik], true = holdout_val_female[,,ik])
        rmse_female[ik] = rmse(forecast = forecast_val_female[,,ik], true = holdout_val_female[,,ik])
        rm(ik)
    }
    rm(holdout_val_male); rm(holdout_val_female); rm(forecast_val_male); rm(forecast_val_female)
    rm(n_state); rm(n_year); rm(n_age)
    return(list(mae_male = mae_male, rmse_male = rmse_male, mae_female = mae_female, rmse_female = rmse_female))
}

######
# ets
######

TWFANOVA_FFM_ets_mae_male = TWFANOVA_FFM_ets_rmse_male =
TWFANOVA_FFM_ets_mae_female = TWFANOVA_FFM_ets_rmse_female =
TWFANOVA_FFM_ets_mae_total = TWFANOVA_FFM_ets_rmse_total = matrix(NA, n_state, 10)
for(iwk in 1:10)
{
    dum = TWFANOVA_fun(smooth_data_set_M = state_smooth_M_array, smooth_data_set_F = state_smooth_F_array,
                       testdata_set_M = state_M_array, testdata_set_F = state_F_array,
                       horizon = iwk, fore_method = "ets")
    TWFANOVA_FFM_ets_mae_male[,iwk] = dum$mae_male
    TWFANOVA_FFM_ets_rmse_male[,iwk] = dum$rmse_male
    TWFANOVA_FFM_ets_mae_female[,iwk] = dum$mae_female
    TWFANOVA_FFM_ets_rmse_female[,iwk] = dum$rmse_female
    print(iwk); rm(iwk); rm(dum)
}
colnames(TWFANOVA_FFM_ets_mae_male) = colnames(TWFANOVA_FFM_ets_rmse_male) =
colnames(TWFANOVA_FFM_ets_mae_female) = colnames(TWFANOVA_FFM_ets_rmse_female) = 1:10

rownames(TWFANOVA_FFM_ets_mae_male) = rownames(TWFANOVA_FFM_ets_rmse_male) =
rownames(TWFANOVA_FFM_ets_mae_female) = rownames(TWFANOVA_FFM_ets_rmse_female) = state

TWFANOVA_FFM_ets_overall = cbind(colMeans(TWFANOVA_FFM_ets_mae_female),
                                 colMeans(TWFANOVA_FFM_ets_rmse_female),

                                 colMeans(TWFANOVA_FFM_ets_mae_male),
                                 colMeans(TWFANOVA_FFM_ets_rmse_male),

                                 colMeans(TWFANOVA_FFM_ets_mae_total),
                                 colMeans(TWFANOVA_FFM_ets_rmse_total))
colnames(TWFANOVA_FFM_ets_overall) = c("ets, mae, F", "ets, rmse, F",
                                       "ets, mae, M", "ets, rmse, M",
                                       "ets, mae, T", "ets, rmse, T")
rownames(TWFANOVA_FFM_ets_overall) = 1:10

########
# arima
########

TWFANOVA_FFM_arima_mae_male = TWFANOVA_FFM_arima_rmse_male = TWFANOVA_FFM_arima_mae_female = TWFANOVA_FFM_arima_rmse_female = matrix(NA, n_state, 10)
for(iwk in 1:10)
{
    dum = TWFANOVA_fun(smooth_data_set_M = state_smooth_M_array, smooth_data_set_F = state_smooth_F_array,
                       testdata_set_M = state_M_array, testdata_set_F = state_F_array,
                       horizon = iwk, fore_method = "arima")
    TWFANOVA_FFM_arima_mae_male[,iwk] = dum$mae_male
    TWFANOVA_FFM_arima_rmse_male[,iwk] = dum$rmse_male
    TWFANOVA_FFM_arima_mae_female[,iwk] = dum$mae_female
    TWFANOVA_FFM_arima_rmse_female[,iwk] = dum$rmse_female
    rm(dum)
}
colnames(TWFANOVA_FFM_arima_mae_male) = colnames(TWFANOVA_FFM_arima_rmse_male) =
colnames(TWFANOVA_FFM_arima_mae_female) = colnames(TWFANOVA_FFM_arima_rmse_female) = 1:10

rownames(TWFANOVA_FFM_arima_mae_male) = rownames(TWFANOVA_FFM_arima_rmse_male) =
rownames(TWFANOVA_FFM_arima_mae_female) = rownames(TWFANOVA_FFM_arima_rmse_female) = state


TWFANOVA_FFM_arima_overall = cbind(colMeans(TWFANOVA_FFM_arima_mae_female),
                                   colMeans(TWFANOVA_FFM_arima_rmse_female),

                                   colMeans(TWFANOVA_FFM_arima_mae_male),
                                   colMeans(TWFANOVA_FFM_arima_rmse_male),

                                   colMeans(TWFANOVA_FFM_arima_mae_total),
                                   colMeans(TWFANOVA_FFM_arima_rmse_total))
colnames(TWFANOVA_FFM_arima_overall) = c("arima, mae, F", "arima, rmse, F",
                                         "arima, mae, M", "arima, rmse, M",
                                         "arima, mae, T", "arima, rmse, T")
rownames(TWFANOVA_FFM_arima_overall) = 1:10
