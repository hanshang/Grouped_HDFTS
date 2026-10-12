# ----------------------------------------------------------------------------
# 25_UFTS_BU_optim_fun.R
#
# Reconciliation engine: bottom-up, OLS and WLS (structural) optimal
# combination, plus the MAE/RMSE errors at every level of the structure.
#
# Replication code for Shang, H. L., 'High-dimensional functional time
# series forecasting with a group structure'.
#
# Run with the working directory set to the code_submission/ folder.
# Scripts are numbered 01-51 in the order in which they must be run.
# See README.md for a one-line description of each.
# ----------------------------------------------------------------------------

############################################################
# female_data_raw: Female age-specific exposure-to-risk
# male_data_raw: Male age-specific exposure-to-risk
# female_data_raw_rate: Female age-specific mortality rates
# male_data_raw_rate: Male age-specific mortality rates
# age: Age variable
# kj: forecast horizon
# hier_method: bottom-up or optimal combination method, (bottom-up, OLS or GLS)
############################################################

BU_optim_hier_for <- function(total_data_raw_rate_region, female_data_raw_rate_region,
                              male_data_raw_rate_region, female_data_raw_rate, male_data_raw_rate,
                              total_data_raw_rate, age, kj, hier_method = c("BU", "comb_OLS", "comb_GLS"))
{
    hier_method = match.arg(hier_method)

    hier_rate = matrix(NA, 168, (11-kj))

    # Level_0 (Total)
    hier_rate[1,] = get(total_data_raw_rate[1])[kj,age,1:(11-kj)]

    # Level_1 (Sex, female, male)
    hier_rate[2,] = get(female_data_raw_rate[1])[kj,age,1:(11-kj)]
    hier_rate[3,] = get(male_data_raw_rate[1])[kj,age,1:(11-kj)]

    # Level_2 (Region + Total, Female, Male)
    for(iw in 1:8)
    {
        # Region total
        hier_rate[4:11,]  = get(total_data_raw_rate_region[iw])[kj,age,1:(11-kj)]  # first arg: horizon; # third arg: number of forecasting years

        # Region females
        hier_rate[12:19,] = get(female_data_raw_rate_region[iw])[kj,age,1:(11-kj)]

        # Region males
        hier_rate[20:27,] = get(male_data_raw_rate_region[iw])[kj,age,1:(11-kj)]
        rm(iw)
    }

    # Level_3 (Prefecture + Total, Female, Male)

    for(iw in 1:n_state)
    {
        hier_rate[iw+27,] = get(total_data_raw_rate[iw])[kj,age,1:(11-kj)]
        rm(iw)
    }
    for(iw in 1:n_state)
    {
        hier_rate[(2*iw+73),] = get(female_data_raw_rate[iw])[kj,age,1:(11-kj)]
        hier_rate[(2*iw+74),] = get(male_data_raw_rate[iw])[kj,age,1:(11-kj)]
        rm(iw)
    }

    # forecast via bottom-up or optimal combination
    hier_fore = matrix(NA, 168, (11-kj))
    summing_mat = Smat_fun(kj = kj, age = age)
    for(ik in 1:(11-kj))
    {
        hier = summing_mat[,,ik]
        if(hier_method == "BU")
        {
            hier_fore[,ik] = (hier %*% hier_rate[75:168,ik])
        }
        else if(hier_method == "comb_OLS")
        {
            hier_fore[,ik] = hier %*% ginv(t(hier) %*% hier) %*% t(hier) %*% hier_rate[,ik]
        }
        else if(hier_method == "comb_GLS")
        {
            hier_fore[,ik] = hier %*% ginv(t(hier) %*% ginv(diag(rowSums(hier^2))) %*% hier) %*% t(hier) %*% ginv(diag(rowSums(hier^2))) %*% hier_rate[,ik]
        }
        rm(ik)
    }
    return(hier_fore)
}

############################################################
# ik: forecast horizon
# fmethod: bottom-up or optimal combination
# female_data_raw: Female age-specific exposure-to-risk
# male_data_raw: Male age-specific exposure-to-risk
# female_data_raw_rate: Female age-specific mortality rates
# male_data_raw_rate: Male age-specific mortality rates
# total_data_raw_rate: Total age-specific mortality rates
############################################################

BU_optim_err <- function(ik, hier_method, total_data_raw_rate_region, female_data_raw_rate_region,
                         male_data_raw_rate_region, female_data_raw_rate,
                         male_data_raw_rate, total_data_raw_rate)
{
    BU_optim_hier_comb = array(NA, dim = c(101, 168, (11-ik)))
    for(ik_age in 1:101)
    {
        BU_optim_hier_comb[ik_age,,] = BU_optim_hier_for(total_data_raw_rate_region = total_data_raw_rate_region,
                                                         female_data_raw_rate_region = female_data_raw_rate_region,
                                                         male_data_raw_rate_region = male_data_raw_rate_region,
                                                         female_data_raw_rate = female_data_raw_rate,
                                                         male_data_raw_rate = male_data_raw_rate,
                                                         total_data_raw_rate = total_data_raw_rate,
                                                         age = ik_age, kj = ik, hier_method = hier_method)
    }
    ######
    # MAE
    ######

    last_year = tail(get(state[1])$year, 1)

    # Level 0 (Total)
    mae_total_err   = mae(BU_optim_hier_comb[,1,],   extract.years(get(state[1]), years = (last_year - 10 + ik):last_year)$rate$total)
    rmse_total_err  = rmse(BU_optim_hier_comb[,1,],  extract.years(get(state[1]), years = (last_year - 10 + ik):last_year)$rate$total)

    # Level 1 (Sex)
    mae_female_err = mae(BU_optim_hier_comb[,2,],  extract.years(get(state[1]), years = (last_year - 10 + ik):last_year)$rate$female)
    mae_male_err   = mae(BU_optim_hier_comb[,3,],  extract.years(get(state[1]), years = (last_year - 10 + ik):last_year)$rate$male)

    rmse_female_err = rmse(BU_optim_hier_comb[,2,],  extract.years(get(state[1]), years = (last_year - 10 + ik):last_year)$rate$female)
    rmse_male_err   = rmse(BU_optim_hier_comb[,3,],  extract.years(get(state[1]), years = (last_year - 10 + ik):last_year)$rate$male)

    # Level 2 (Region + Total, Female and Male)
    mae_total_R_err = mae_female_R_err = mae_male_R_err = rmse_total_R_err = rmse_female_R_err = rmse_male_R_err = vector("numeric",8)
    for(iw in 1:8)
    {
        mae_total_R_err[iw]  = mae(BU_optim_hier_comb[,(iw+3),],  extract.years(get(region[iw]), years = (last_year - 10 + ik):last_year)$rate$total)
        mae_female_R_err[iw] = mae(BU_optim_hier_comb[,(iw+11),], extract.years(get(region[iw]), years = (last_year - 10 + ik):last_year)$rate$female)
        mae_male_R_err[iw]   = mae(BU_optim_hier_comb[,(iw+19),], extract.years(get(region[iw]), years = (last_year - 10 + ik):last_year)$rate$male)

        rmse_total_R_err[iw]  = rmse(BU_optim_hier_comb[,(iw+3),],  extract.years(get(region[iw]), years = (last_year - 10 + ik):last_year)$rate$total)
        rmse_female_R_err[iw] = rmse(BU_optim_hier_comb[,(iw+11),], extract.years(get(region[iw]), years = (last_year - 10 + ik):last_year)$rate$female)
        rmse_male_R_err[iw]   = rmse(BU_optim_hier_comb[,(iw+19),], extract.years(get(region[iw]), years = (last_year - 10 + ik):last_year)$rate$male)
        rm(iw)
    }

    # Level 3 (Prefecture + Total)

    rmse_state_err = mae_state_err = bottom_female_rmse = bottom_female_mae = bottom_male_mae = bottom_male_rmse = vector("numeric",47)
    for(iwk in 1:n_state)
    {
        mae_state_err[iwk]   = mae(BU_optim_hier_comb[,(iwk+27),],   extract.years(get(state[iwk]), years = (last_year - 10 + ik):last_year)$rate$total)
        rmse_state_err[iwk]  = rmse(BU_optim_hier_comb[,(iwk+27),],  extract.years(get(state[iwk]), years = (last_year - 10 + ik):last_year)$rate$total)
        rm(iwk)
    }

    for(iwk in 1:n_state)
    {
        bottom_female_mae[iwk]  = mae(BU_optim_hier_comb[,(73+2*iwk),],   extract.years(get(state[iwk]), years = (last_year - 10 + ik):last_year)$rate$female)
        bottom_female_rmse[iwk] = rmse(BU_optim_hier_comb[,(73+2*iwk),],  extract.years(get(state[iwk]), years = (last_year - 10 + ik):last_year)$rate$female)

        bottom_male_mae[iwk]    = mae(BU_optim_hier_comb[,(74+2*iwk),],   extract.years(get(state[iwk]), years = (last_year - 10 + ik):last_year)$rate$male)
        bottom_male_rmse[iwk]   = rmse(BU_optim_hier_comb[,(74+2*iwk),],  extract.years(get(state[iwk]), years = (last_year - 10 + ik):last_year)$rate$male)
        rm(iwk)
    }
    return(list(mae_total_err = mae_total_err, mae_female_err = mae_female_err,
                mae_male_err  = mae_male_err,  mae_total_R_err = mae_total_R_err,
                mae_female_R_err = mae_female_R_err, mae_male_R_err = mae_male_R_err,
                mae_state_err  = mae_state_err, mae_bottom_female_err = bottom_female_mae,
                mae_bottom_male_err = bottom_male_mae,

                rmse_total_err = rmse_total_err, rmse_female_err = rmse_female_err,
                rmse_male_err  = rmse_male_err,
                rmse_total_R_err = rmse_total_R_err,
                rmse_female_R_err = rmse_female_R_err,
                rmse_male_R_err = rmse_male_R_err,
                rmse_state_err  = rmse_state_err,
                rmse_bottom_female_err = bottom_female_rmse, rmse_bottom_male_err = bottom_male_rmse))
}
