# ----------------------------------------------------------------------------
# 03_auxiliary_point.R
#
# Core model fitting routines: the MFTS model and the point forecast helpers
# shared by the base-forecast scripts.
#
# Replication code for Shang, H. L., 'High-dimensional functional time
# series forecasting with a group structure'.
#
# Run with the working directory set to the code_submission/ folder.
# Scripts are numbered 01-51 in the order in which they must be run.
# See README.md for a one-line description of each.
# ----------------------------------------------------------------------------

######################################
# multivariate functional time series
######################################

# data_input: multivariate functional time series
# ncomp_method: way of selecting the number of components
# fh: forecast horizon
# fore_method: forecasting method
# object_interest: point or interval forecast accuracy
# boot_number: number of bootstrap samples
# PI_level: nominal coverage probability

MFTS_model <- function(data_input, ncomp_method, fh, fore_method)
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
            rm(iw)
        }
    }
    else
    {
        data_set_array = data_input
    }

    rowmeans_object = sd_object = decenter_object = list()
    for(ik in 1:n_pop)
    {
        # compute mean and sd function
        rowmeans_object[[ik]] = rowMeans(data_set_array[,,ik], na.rm = TRUE)
        sd_object[[ik]] = apply(data_set_array[,,ik], 1, sd, na.rm = TRUE)

        # de-center functional data
        decenter_object[[ik]] = t(scale(t(data_set_array[,,ik]), center = TRUE, scale = TRUE))
        rm(ik)
    }

    comb_object = do.call(rbind, decenter_object)
    # comb_object = do.call(rbind, comb_object_raw)
    colnames(comb_object) = 1:ncol(comb_object)

    eigen_value = eigen(cov(t(comb_object)))$values
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
    fore_ftsm = forecast(ftsm(fts(1:nrow(comb_object), comb_object), order = ncomp), h = fh,
                         method = fore_method)
    res_fore = as.matrix(fore_ftsm$mean$y * do.call(c, sd_object) + do.call(c, rowmeans_object))
    return(res_fore)
}

##########################################
# Multilevel functional time series model
##########################################

# data_set: a list of p by n data matrix
# aux_var: an aggregated p by n data matrix
# ncomp_method: method for selecting the number of components
# fh: forecast horizon
# fore_method: univariate time-series forecasting method, such as "ETS"

MLFTS_model <- function(data_input, aux_var, ncomp_method, fh, fore_method)
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
        final_fore[[iw]] = mean_function_list[[iw]] + (aggregate_fore + resi_fore[[iw]])[,fh]
        rm(iw)
    }
    return(final_fore)
}

###########################################
# High-dimensional functional factor model
###########################################

# data: (sample size x no_pop x no_grid)

HDFTS_factor_decomp <- function(data)
{
    sample_size = dim(data)[1]
    no_pop = dim(data)[2]
    D_val = dim(data)[3]

    Delta = matrix(0, sample_size, sample_size)
    for(ij in 1:no_pop)
    {
        temp = matrix(NA, sample_size, sample_size)
        for(t in 1:sample_size)
        {
            for(s in 1:sample_size)
            {
                temp[t,s] = matrix(data[t,ij,], nrow = 1) %*% matrix(data[s,ij,], ncol = 1)
            }
        }
        Delta = Delta + temp/D_val
        rm(temp)
    }
    rm(t); rm(s); rm(ij)

    Delta_mat = Delta/no_pop
    Delta_mat_eigen = eigen(Delta_mat)

    # Two ways of selecting the number of components

    # K_val_k_criterion = k_criterion(eigenvalue = Delta_mat_eigen$values, plot = FALSE)
    # K_val_select_K = select_K(tau = 10^-3, eigenvalue = Delta_mat_eigen$values)

    # new way of selecting the number of components

    K_val = vector("numeric", sample_size)
    for(ik in 1:sample_size)
    {
        K_val[ik] = select_K_new(eigenvalue = Delta_mat_eigen$values, index = ik, sample_size = sample_size,
                                 no_pop = no_pop)
        rm(ik)
    }

    q_val_est = max(which.min(K_val) - 1, 1)
    if(q_val_est == 0)
    {
        warning("The number of components is zero.")
    }
    Delta_eigen_vector = as.matrix(Delta_mat_eigen$vectors[,1:q_val_est]) * sqrt(sample_size)

    ###############################
    # estimate the eigen-dimension
    ###############################

    factor_loading = array(NA, dim = c(q_val_est, D_val, no_pop))
    for(ij in 1:no_pop)
    {
        factor_loading[,,ij] = (crossprod(Delta_eigen_vector, data[,ij,]))/sample_size
        rm(ij)
    }

    # variance

    C_X = array(NA, dim = c(no_pop, no_pop, D_val, D_val))
    for(ij in 1:no_pop)
    {
        for(iw in 1:no_pop)
        {
            C_X[ij,iw,,] = crossprod(factor_loading[,,ij], factor_loading[,,iw])
        }
    }
    rm(ij); rm(iw)

    error = array(NA, dim = c(sample_size, no_pop, D_val))
    for(ij in 1:no_pop)
    {
        error[,ij,] = data[,ij,] - (Delta_eigen_vector %*% factor_loading[,,ij])
        rm(ij)
    }
    return(list(raw_data = data, factor_loading = factor_loading,
                factors = Delta_eigen_vector, error = error))
}

###########################################################
# Three-ways of selecting the number of factors
# Numbers of factors selection in Leng et al. (2026, JBES)
###########################################################

select_K_new <- function(eigenvalue, index, sample_size, no_pop)
{
    return((eigenvalue[index])/sample_size + index * (max(sample_size, no_pop)^(-0.5)))
}

#######################################
# The method of den Reijer et al. 2021
#######################################

k_criterion <- function(eigenvalue, plot = TRUE)
{
    n = length(eigenvalue)
    Hn = sum(1/(1:n))

    eigen_diff = eigenvalue[-n] - eigenvalue[-1]
    lambda_bar = 1/((2:n)*Hn)

    n_cross = which(eigen_diff <= lambda_bar)[1]

    if(n_cross > 1)
    {
        n_return = n_cross - 1
    }
    else
    {
        n_return = 1
    }

    if(plot)
    {
        plot(eigen_diff[1:(n_cross+6)], type = "b", xlab = expression("Eigenvalue number" ~ italic(k)), ylab = expression("Eigenvalue" ~~~ lambda))
        lines(lambda_bar[1:(n_cross+6)], type ="b", col = 2)
        abline(v = n_return, lty = 2, col = 4)
        legend("topright", lty = c(1,1), lwd = c(2,2), col = c(1,2), c(expression(lambda ~~ "(eigenvalue)  "), expression(bar(lambda) ~~ "(threshold)  ")))
    }
    return(n_return)
}

###################################################################
# selecting the number of components by eigenvalue ratio criterion
###################################################################

# tau: a tuning parameter
# eigenvalue: estimated eigenvalues

select_K <- function(tau, eigenvalue)
{
    k_max = length(eigenvalue)
    k_all = rep(0, k_max-1)
    for(k in 1:(k_max-1))
    {
        k_all[k] = (eigenvalue[k+1]/eigenvalue[k])*ifelse(eigenvalue[k]/eigenvalue[1] > tau, 1, 0) + ifelse(eigenvalue[k]/eigenvalue[1] < tau, 1, 0)
        rm(k)
    }
    K_hat = which.min(k_all)
    return(K_hat)
}

#######################################
# compute the forecast errors for MFTS
#######################################

# data_var: full data array (n_age x n_year x n_state)
# test_data: full data array (n_age x n_year x n_state)
# horizon: forecast horizon
# ncomp_selection_method: EVR or K = 6
# fmethod: forecasting method ets or arima

MFTS_model_fun_err <- function(data_var, test_data, horizon, ncomp_selection_method, fmethod)
{
    n_age = dim(data_var)[1]
    n_year = dim(data_var)[2]
    n_state = dim(data_var)[3]

    dum_fore_array = array(NA, dim = c(n_age, n_state, (11 - horizon)))
    for(iw in 1:(11 - horizon))
    {
        dum = MFTS_model(data_input = data_var[,1:(n_year - 11 + iw),], ncomp_method = ncomp_selection_method, fh = horizon, fore_method = fmethod)
        dum_fore_array[,,iw] = array(dum[,horizon], dim = c(n_age, n_state, 1))
        rm(iw); rm(dum)
    }

    holdout_data = array(test_data[,(n_year - 10 + horizon):length(1975:2024),], dim = c(n_age, (11 - horizon), n_state))

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

# forecasting high-dimensional functional time series

forecast.hdfpca <- function(object, h = 3, fmethod, ...)
{
    order = object$order
    r = object$r
    m = object$m
    p = object$p
    n = dim(object$y[[1]])[2]
    resid <- list()
    for(im in 1:m)
    {
        resid[[im]] <- object$y[[im]] - object$fitted[[im]]
    }
    mod.fore = load.fore <- list()
    for(io in 1:order)
    {
        load.fore[[io]] <- array(NA, dim = c(h, r))
        mod.fore[[io]] <- list()
        for(ir in 1:r)
        {
            if(fmethod == "arima")
            {
                mod <- auto.arima(object$model$model2[[io]]$coef[, ir])
            }
            else if(fmethod == "ets")
            {
                mod <- ets(object$model$model2[[io]]$coef[, ir])
            }
            else
            {
                warning("fmethod should either be arima or ets.")
            }
            mod.fore[[io]][[ir]] <- forecast(mod, h)
            load.fore[[io]][, ir] <- mod.fore[[io]][[ir]]$mean
        }
    }
    score.fore <- list()
    for(io in 1:order)
    {
        score.fore[[io]] <- object$model$model2[[io]]$basis[, 1] + object$model$model2[[io]]$basis[, 2:(1 + r)] %*%
        t(load.fore[[io]])
    }
    score <- list()
    for(im in 1:m)
    {
        score[[im]] <- sapply(score.fore, "[", ((im - 1) * h + 1):(im * h))
    }
    fun.fore <- list()
    if(h == 1)
    {
        for(im in 1:m)
        {
            fun.fore[[im]] <- object$model$model1[[im]]$basis[, 1] + object$model$model1[[im]]$basis[, 2:(1 + order)] %*% score[[im]]
        }
    }
    else if (h >= (n/2))
    {
        stop("forecast horizon is too big considering the sample size")
    }
    else
    {
        for(im in 1:m)
        {
            fun.fore[[im]] <- object$model$model1[[im]]$basis[, 1] + object$model$model1[[im]]$basis[, 2:(1 + order)] %*% t(score[[im]])
        }
    }
    return(structure(list(forecast = fun.fore), class = "forecast.hdfpca"))
}

mae = ftsa:::mae
rmse = ftsa:::rmse
