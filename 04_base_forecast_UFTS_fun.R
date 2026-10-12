# ----------------------------------------------------------------------------
# 04_base_forecast_UFTS_fun.R
#
# Helper that turns UFTS forecasts into MAE/RMSE forecast errors.
#
# Replication code for Shang, H. L., 'High-dimensional functional time
# series forecasting with a group structure'.
#
# Run with the working directory set to the code_submission/ folder.
# Scripts are numbered 01-51 in the order in which they must be run.
# See README.md for a one-line description of each.
# ----------------------------------------------------------------------------

###########################################
# univariate functional time-series method
###########################################

# data_var: smooth data (F or M)
# index: index of state
# ncomp_method: K = 6 or EVR
# uni_fore_method: univariate time-series forecasting method

ind_forecast_fun <- function(data_var, index, ncomp_method, uni_fore_method)
{
    if(substr(data_var[1], nchar(data_var[1]), nchar(data_var[1])) == "F")
    {
      sex_var = "Female"
    }
    else if(substr(data_var[1], nchar(data_var[1]), nchar(data_var[1])) == "M")
    {
      sex_var = "Male"
    }
    else if(substr(data_var[1], nchar(data_var[1]), nchar(data_var[1])) == "T")
    {
      sex_var = "Total"
    }
    else
    {
      warning("sex variable should either be females or males or total.")
    }
    res_fore = array(NA, dim = c(10, 101, 10))
    for(iw in 1:10)
    {
      ind_dat = extract.years(get(data_var[index]), years = 1975:(2013 + iw))
      if(sex_var == "Female")
      {
        eigen_value = eigen(cov(t(ind_dat$rate$Female)))$values
        if(ncomp_method == "EVR")
        {
          ncomp = select_K(tau = 10^-2, eigenvalue = eigen_value)
        }
        else if(ncomp_method == "provide")
        {
          ncomp = 6
        }
        else
        {
          warning("The number of components is required.")
        }
        fun_forc = forecast(fdm(ind_dat, series = "Female", order = ncomp), h = 10, method = uni_fore_method)
        res_fore[,,iw] = t(fun_forc$rate$female)
      }
      else if(sex_var == "Male")
      {
        eigen_value = eigen(cov(t(ind_dat$rate$Male)))$values
        if(ncomp_method == "EVR")
        {
          ncomp = select_K(tau = 10^-2, eigenvalue = eigen_value)
        }
        else if(ncomp_method == "provide")
        {
          ncomp = 6
        }
        else
        {
          warning("The number of components is required.")
        }
        fun_forc = forecast(fdm(ind_dat, series = "Male", order = 6), h = 10, method = uni_fore_method)
        res_fore[,,iw] = t(fun_forc$rate$male)
      }
      else if(sex_var == "Total")
      {
        eigen_value = eigen(cov(t(ind_dat$rate$Total)))$values
        if(ncomp_method == "EVR")
        {
          ncomp = select_K(tau = 10^-2, eigenvalue = eigen_value)
        }
        else if(ncomp_method == "provide")
        {
          ncomp = 6
        }
        else
        {
          warning("The number of components is required.")
        }
        fun_forc = forecast(fdm(ind_dat, series = "Total", order = 6), h = 10, method = uni_fore_method)
        res_fore[,,iw] = t(fun_forc$rate$total)
      }
      rm(iw)
    }

    # MAE and RMSE

    last_year = tail(years,1)

    fore_mae = fore_rmse = vector("numeric", 10)
    for(k in 1:10)
    {
      if(sex_var == "Female")
      {
        fore_mae[k]  = mae(res_fore[k,,1:(11 - k)],  extract.years(get(state_F[index]), years = (last_year - 10 + k):last_year)$rate$Female)
        fore_rmse[k] = rmse(res_fore[k,,1:(11 - k)],  extract.years(get(state_F[index]), years = (last_year - 10 + k):last_year)$rate$Female)
      }
      else if(sex_var == "Male")
      {
        fore_mae[k]  = mae(res_fore[k,,1:(11 - k)],  extract.years(get(state_M[index]), years = (last_year - 10 + k):last_year)$rate$Male)
        fore_rmse[k] = rmse(res_fore[k,,1:(11 - k)],  extract.years(get(state_M[index]), years = (last_year - 10 + k):last_year)$rate$Male)
      }
      else if(sex_var == "Total")
      {
        fore_mae[k]  = mae(res_fore[k,,1:(11 - k)],  extract.years(get(state_T[index]), years = (last_year - 10 + k):last_year)$rate$Total)
        fore_rmse[k] = rmse(res_fore[k,,1:(11 - k)],  extract.years(get(state_T[index]), years = (last_year - 10 + k):last_year)$rate$Total)
      }
      rm(k)
    }
    return(list(fore_val = res_fore, fore_mae = fore_mae, fore_rmse = fore_rmse))
}
