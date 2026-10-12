# ----------------------------------------------------------------------------
# 26_base_forecast_MLFTS_forecasts_fun.R
#
# Assemble the MLFTS base forecast values (ETS scores, components as
# configured) into the arrays consumed by the reconciliation script in this
# folder.
#
# Replication code for Shang, H. L., 'High-dimensional functional time
# series forecasting with a group structure'.
#
# Run with the working directory set to the code_submission/ folder.
# Scripts are numbered 01-51 in the order in which they must be run.
# See README.md for a one-line description of each.
# ----------------------------------------------------------------------------

################################################
# compute the forecasts obtained from the MLFTS
################################################

# data_input: data array
# aux_var: auxiliary variable
# ncomp_method: way for selecting the number of components
# fh: forecast horizon
# fore_method: forecasting method

MLFTS_model_forecasts <- function(data_input, aux_var, ncomp_method, fh, fore_method)
{
    n_age  = dim(data_input)[1]
    n_year = dim(data_input)[2]
    n_pop  = dim(data_input)[3]

    # clean the data

    data_set_array = array(NA, dim = c(n_age, n_year, n_pop))
    if(any(!is.finite(data_input)))
    {
        for(iw in 1:n_pop)
        {
            for(ij in 1:n_age)
            {
                data_set_array[ij,,iw] = na.interp(data_input[ij,,iw])
                rm(ij)
            }
        }
    }
    else
    {
        data_set_array = data_input
    }

    # compute the mean function

    mean_function_list = list()
    for(ik in 1:n_pop)
    {
        mean_function_list[[ik]] = rowMeans(data_set_array[,,ik], na.rm = TRUE)
        rm(ik)
    }

    data_set = array(NA, dim = c(n_age, n_year, n_pop))
    for(ik in 1:n_pop)
    {
        data_set[,,ik] = t(scale(t(data_set_array[,,ik]), center = TRUE, scale = FALSE))
        rm(ik)
    }

    if(missing(aux_var)|is.null(aux_var))
    {
        aggregate_data = apply(data_set, c(1, 2), mean)
    }
    else
    {
        aggregate_data = t(aux_var)
    }
    colnames(aggregate_data) = 1:n_year
    rownames(aggregate_data) = 1:n_age

    # 1st FPCA

    eigen_value_aggregate = eigen(cov(t(aggregate_data)))$values
    if(ncomp_method == "EVR")
    {
        ncomp_aggregate = select_K(tau = 10^-3, eigenvalue = eigen_value_aggregate)
    }
    else if(ncomp_method == "provide")
    {
        ncomp_aggregate = 6
    }
    ftsm_aggregate = ftsm(fts(1:n_age, aggregate_data), order = ncomp_aggregate)

    # calculate sum of lambda_k
    sum_lambda_k = sum(eigen_value_aggregate[1:ncomp_aggregate])

    # compute the residual trend
    data_residual = array(NA, dim = c(n_age, n_year, n_pop))
    for(iw in 1:n_pop)
    {
        data_residual[,,iw] = data_set[,,iw] - ftsm_aggregate$fitted$y
        colnames(data_residual[,,iw]) = 1:n_year
        rownames(data_residual[,,iw]) = 1:n_age
        rm(iw)
    }

    # 2nd FPCA

    if(ncomp_method == "EVR")
    {
        ncomp_resi = vector("numeric", n_pop)
        for(iw in 1:n_pop)
        {
            eigen_value_resi = eigen(cov(t(data_residual[,,iw])))$values
            ncomp_resi[iw] = select_K(tau = 10^-3, eigenvalue = eigen_value_resi)
            rm(iw)
        }
    }
    else if(ncomp_method == "provide")
    {
        ncomp_resi = rep(6, n_pop)
    }

    sum_lambda_l = vector("numeric", n_pop)
    for(iw in 1:n_pop)
    {
        eigen_value_resi = eigen(cov(t(data_residual[,,iw])))$values

        # calculate sum of lambda_l
        sum_lambda_l[iw] = sum(eigen_value_resi[1:(ncomp_resi[iw])])
        rm(iw)
    }

    ftsm_resi = list()
    for(iw in 1:n_pop)
    {
        ftsm_resi[[iw]] = ftsm(fts(1:n_age, data_residual[,,iw]), order = ncomp_resi[iw])
        rm(iw)
    }

    # within-cluster variability

    within_cluster_variability = vector("numeric", n_pop)
    for(iw in 1:n_pop)
    {
        within_cluster_variability[iw] = sum_lambda_k/(sum_lambda_k + sum_lambda_l[iw])
        rm(iw)
    }

    # reconstruction

    coef_fore = matrix(NA, ncomp_aggregate, fh)
    if(fore_method == "arima")
    {
        for(ik in 1:ncomp_aggregate)
        {
            coef_fore[ik,] = forecast(auto.arima(ftsm_aggregate$coeff[,ik+1]), h = fh)$mean
            rm(ik)
        }
    }
    else if(fore_method == "ets")
    {
        for(ik in 1:ncomp_aggregate)
        {
            coef_fore[ik,] = forecast(ets(ftsm_aggregate$coeff[,ik+1]), h = fh)$mean
            rm(ik)
        }
    }
    else
    {
        warning("Forecasting method can either be ARIMA or ETS.")
    }
    rownames(coef_fore) = 1:ncomp_aggregate
    colnames(coef_fore) = 1:fh

    if(ncomp_aggregate == 1)
    {
        aggregate_fore = as.matrix(ftsm_aggregate$basis[,2]) %*% matrix(coef_fore, nrow = 1)
    }
    else
    {
        aggregate_fore = ftsm_aggregate$basis[,2:(ncomp_aggregate+1)] %*% coef_fore
    }

    # residual forecasts

    coef_fore_resi_list = list()
    for(iw in 1:n_pop)
    {
        coef_fore_resi = matrix(NA, ncomp_resi[iw], fh)
        if(fore_method == "arima")
        {
            for(ik in 1:ncomp_resi[iw])
            {
                coef_fore_resi[ik,] = forecast(auto.arima(ftsm_resi[[iw]]$coeff[,ik+1]), h = fh)$mean
                rm(ik)
            }
        }
        else if(fore_method == "ets")
        {
            for(ik in 1:ncomp_resi[iw])
            {
                coef_fore_resi[ik,] = forecast(ets(ftsm_resi[[iw]]$coeff[,ik+1]), h = fh)$mean
                rm(ik)
            }
        }
        else
        {
            warning("Forecasting method can either be ARIMA or ETS.")
        }
        coef_fore_resi_list[[iw]] = coef_fore_resi
        rm(iw)
    }

    resi_fore = list()
    for(iw in 1:n_pop)
    {
        resi_fore[[iw]] = ftsm_resi[[iw]]$basis[,2:(ncomp_resi[iw] + 1)] %*% coef_fore_resi_list[[iw]]
        rm(iw)
    }

    final_fore = list()
    for(iw in 1:n_pop)
    {
        final_fore[[iw]] = mean_function_list[[iw]] + (aggregate_fore + resi_fore[[iw]])
        rm(iw)
    }
    return(final_fore)
}


# data_var: n_age x n_year x n_state
# ncomp_selection_method: way for selecting the number of components
# fmethod: forecasting method

MLFTS_model_fun_forecasts <- function(data_var, ncomp_selection_method, fmethod)
{
    n_age = dim(data_var)[1]
    n_year = dim(data_var)[2]
    n_state = dim(data_var)[3]
    dum_fore_array = array(NA, dim = c(10, n_age, n_state, 10))
    for(iw in 1:10)
    {
        dum = MLFTS_model_forecasts(data_input = data_var[,1:(n_year - 11 + iw),], aux_var = NULL,
                                    ncomp_method = ncomp_selection_method, fh = 10, fore_method = fmethod)

        fore_val = array(NA, dim = c(n_age, n_state, 10))
        for(ik in 1:n_state)
        {
            fore_val[,ik,] = dum[[ik]]
            rm(ik)
        }
        dum_fore_array[,,,iw] = aperm(fore_val, c(3, 1, 2))
        rm(iw); rm(dum); rm(fore_val)
    }
    return(dum_fore_array)
}
