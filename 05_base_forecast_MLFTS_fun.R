# ----------------------------------------------------------------------------
# 05_base_forecast_MLFTS_fun.R
#
# Helper that turns MLFTS forecasts into MAE/RMSE forecast errors.
#
# Replication code for Shang, H. L., 'High-dimensional functional time
# series forecasting with a group structure'.
#
# Run with the working directory set to the code_submission/ folder.
# Scripts are numbered 01-51 in the order in which they must be run.
# See README.md for a one-line description of each.
# ----------------------------------------------------------------------------

#############################################
# compute the forecast errors from the MLFTS
#############################################

# data_var: data array
# test_data: test data
# horizon: forecast horizon
# ncomp_selection_method: EVR or K = 6
# fmethod: univariate time-series forecasting method

MLFTS_model_fun_err <- function(data_var, test_data, horizon, ncomp_selection_method, fmethod)
{
    n_age = dim(data_var)[1]
    n_year = dim(data_var)[2]
    n_state = dim(data_var)[3]
    dum_fore_array = array(NA, dim = c(n_age, n_state, (11 - horizon)))
    for(iw in 1:(11 - horizon))
    {
        # needs to be modified

        dum = MLFTS_model(data_input = data_var[,1:(n_year - 11 + iw),], aux_var = NULL,
                          ncomp_method = ncomp_selection_method, fh = horizon, fore_method = fmethod)

        fore_val = array(NA, dim = c(n_age, n_state, 1))
        for(ik in 1:n_state)
        {
            fore_val[,ik,] = dum[[ik]]
            rm(ik)
        }
        dum_fore_array[,,iw] = fore_val
        rm(iw); rm(dum); rm(fore_val)
    }

    holdout_data = array(test_data[,(40 + horizon):n_year,], dim = c(n_age, (11 - horizon), n_state))

    mae_err = rmse_err = vector("numeric", n_state)
    for(ik in 1:n_state)
    {
        mae_err[ik]  = mae(forecast = dum_fore_array[,ik,], true = holdout_data[,,ik])
        rmse_err[ik] = rmse(forecast = dum_fore_array[,ik,], true = holdout_data[,,ik])
        rm(ik)
    }
    rm(holdout_data); rm(dum_fore_array)
    return(list(mae_err = mae_err, rmse_err = rmse_err))
}
