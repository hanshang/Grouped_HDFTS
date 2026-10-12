# ----------------------------------------------------------------------------
# 38_MFTS_BU_optim_arima_K6.R
#
# Reconcile the MFTS base forecasts (ARIMA scores, K = 6 components) bottom-
# up and by optimal combination (OLS and WLS), then tabulate forecast errors
# by level of the group structure.
#
# Sources: 25_UFTS_BU_optim_fun.R, 24_Japan_S_mat.R, 03_auxiliary_point.R
# Writes:  figures/MFTS_K6_arima_forecast_accuracy_by_level.pdf
#
# Replication code for Shang, H. L., 'High-dimensional functional time
# series forecasting with a group structure'.
#
# Run with the working directory set to the code_submission/ folder.
# Scripts are numbered 01-51 in the order in which they must be run.
# See README.md for a one-line description of each.
# ----------------------------------------------------------------------------

source("25_UFTS_BU_optim_fun.R")

source("24_Japan_S_mat.R")

source("03_auxiliary_point.R")

###################
# fmethod = "Base"
###################

# MAE

MFTS_ind_all_level_err_mae_arima_K6 = cbind(Japan_T_fore_mae_arima_K6,
                                            rowMeans(cbind(Japan_F_fore_mae_arima_K6, Japan_M_fore_mae_arima_K6)),
                                            colMeans(MFTS_model_region_fun_K6_arima_mae_T),
                                            colMeans(rbind(MFTS_model_region_fun_K6_arima_mae_F, MFTS_model_region_fun_K6_arima_mae_M)),
                                            colMeans(MFTS_model_fun_K6_arima_mae_T),
                                            colMeans(rbind(MFTS_model_fun_K6_arima_mae_F, MFTS_model_fun_K6_arima_mae_M)))
colnames(MFTS_ind_all_level_err_mae_arima_K6) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

# RMSE

MFTS_ind_all_level_err_rmse_arima_K6 = cbind(Japan_T_fore_rmse_arima_K6,
                                             rowMeans(cbind(Japan_F_fore_rmse_arima_K6, Japan_M_fore_rmse_arima_K6)),
                                             colMeans(MFTS_model_region_fun_K6_arima_rmse_T),
                                             colMeans(rbind(MFTS_model_region_fun_K6_arima_rmse_F, MFTS_model_region_fun_K6_arima_rmse_M)),
                                             colMeans(MFTS_model_fun_K6_arima_rmse_T),
                                             colMeans(rbind(MFTS_model_fun_K6_arima_rmse_F, MFTS_model_fun_K6_arima_rmse_M)))
colnames(MFTS_ind_all_level_err_rmse_arima_K6) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")


#################
# fmethod = "BU"
#################

MFTS_BU_Level_T_err_arima_K6 = MFTS_BU_Level_F_err_arima_K6 = MFTS_BU_Level_M_err_arima_K6 = matrix(NA, 10, 2)
MFTS_BU_Level_T_R_err_arima_K6 = MFTS_BU_Level_F_R_err_arima_K6 = MFTS_BU_Level_M_R_err_arima_K6 = array(NA, dim = c(10, 2, 8),
                                                                                                   dimnames = list(1:10, c("MAE", "RMSE"), region))
MFTS_BU_Level_T_state_err_arima_K6 = MFTS_BU_Level_F_state_err_arima_K6 = MFTS_BU_Level_M_state_err_arima_K6 = array(NA, dim = c(47, 10, 2),
                                                                                                               dimnames = list(state, 1:10, c("MAE", "RMSE")))
for(ikw in 1:10)
{
    dum = BU_optim_err(ik = ikw, hier_method = "BU",
                       total_data_raw_rate_region = MFTS_region_forc_total_arima_K6,
                       female_data_raw_rate_region = MFTS_region_forc_female_arima_K6,
                       male_data_raw_rate_region = MFTS_region_forc_male_arima_K6,

                       female_data_raw_rate = MFTS_state_forc_female_arima_K6,
                       male_data_raw_rate = MFTS_state_forc_male_arima_K6,
                       total_data_raw_rate = MFTS_state_forc_total_arima_K6)

    # Total + Sex

    MFTS_BU_Level_T_err_arima_K6[ikw,1] = dum$mae_total_err
    MFTS_BU_Level_F_err_arima_K6[ikw,1] = dum$mae_female_err
    MFTS_BU_Level_M_err_arima_K6[ikw,1] = dum$mae_male_err

    MFTS_BU_Level_T_err_arima_K6[ikw,2] = dum$rmse_total_err
    MFTS_BU_Level_F_err_arima_K6[ikw,2] = dum$rmse_female_err
    MFTS_BU_Level_M_err_arima_K6[ikw,2] = dum$rmse_male_err

    # Region + Sex

    MFTS_BU_Level_T_R_err_arima_K6[ikw,1,] = dum$mae_total_R_err
    MFTS_BU_Level_F_R_err_arima_K6[ikw,1,] = dum$mae_female_R_err
    MFTS_BU_Level_M_R_err_arima_K6[ikw,1,] = dum$mae_male_R_err

    MFTS_BU_Level_T_R_err_arima_K6[ikw,2,] = dum$rmse_total_R_err
    MFTS_BU_Level_F_R_err_arima_K6[ikw,2,] = dum$rmse_female_R_err
    MFTS_BU_Level_M_R_err_arima_K6[ikw,2,] = dum$rmse_male_R_err

    # Prefecture + Sex

    MFTS_BU_Level_T_state_err_arima_K6[,ikw,1] = dum$mae_state_err
    MFTS_BU_Level_F_state_err_arima_K6[,ikw,1] = dum$mae_bottom_female_err
    MFTS_BU_Level_M_state_err_arima_K6[,ikw,1] = dum$mae_bottom_male_err

    MFTS_BU_Level_T_state_err_arima_K6[,ikw,2] = dum$rmse_state_err
    MFTS_BU_Level_F_state_err_arima_K6[,ikw,2] = dum$rmse_bottom_female_err
    MFTS_BU_Level_M_state_err_arima_K6[,ikw,2] = dum$rmse_bottom_male_err

    print(ikw); rm(ikw); rm(dum)
}

MFTS_BU_all_level_err_mae_arima_K6 = cbind(MFTS_BU_Level_T_err_arima_K6[,1],
                                           rowMeans(cbind(MFTS_BU_Level_F_err_arima_K6[,1], MFTS_BU_Level_M_err_arima_K6[,1])),
                                           rowMeans(MFTS_BU_Level_T_R_err_arima_K6[,1,]),
                                           rowMeans(cbind(rowMeans(MFTS_BU_Level_F_R_err_arima_K6[,1,]), rowMeans(MFTS_BU_Level_M_R_err_arima_K6[,1,]))),
                                           colMeans(MFTS_BU_Level_T_state_err_arima_K6[,,1]),
                                           colMeans(rbind(MFTS_BU_Level_F_state_err_arima_K6[,,1], MFTS_BU_Level_M_state_err_arima_K6[,,1])))

MFTS_BU_all_level_err_rmse_arima_K6 = cbind(MFTS_BU_Level_T_err_arima_K6[,2],
                                            rowMeans(cbind(MFTS_BU_Level_F_err_arima_K6[,2], MFTS_BU_Level_M_err_arima_K6[,2])),
                                            rowMeans(MFTS_BU_Level_T_R_err_arima_K6[,2,]),
                                            rowMeans(cbind(rowMeans(MFTS_BU_Level_F_R_err_arima_K6[,2,]), rowMeans(MFTS_BU_Level_M_R_err_arima_K6[,2,]))),
                                            colMeans(MFTS_BU_Level_T_state_err_arima_K6[,,2]),
                                            colMeans(rbind(MFTS_BU_Level_F_state_err_arima_K6[,,2], MFTS_BU_Level_M_state_err_arima_K6[,,2])))

colnames(MFTS_BU_all_level_err_mae_arima_K6) = colnames(MFTS_BU_all_level_err_rmse_arima_K6) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")


#######################
# fmethod = "comb_OLS"
#######################

MFTS_optim_Level_T_err_arima_K6 = MFTS_optim_Level_F_err_arima_K6 = MFTS_optim_Level_M_err_arima_K6 = matrix(NA, 10, 2)
MFTS_optim_Level_T_R_err_arima_K6 = MFTS_optim_Level_F_R_err_arima_K6 = MFTS_optim_Level_M_R_err_arima_K6 = array(NA, dim = c(10, 2, 8),
                                                                                                               dimnames = list(1:10, c("MAE", "RMSE"), region))

MFTS_optim_Level_T_state_err_arima_K6 = MFTS_optim_Level_F_state_err_arima_K6 = MFTS_optim_Level_M_state_err_arima_K6 = array(NA, dim = c(47, 10, 2),
                                                                                                                           dimnames = list(state, 1:10, c("MAE", "RMSE")))
for(ikw in 1:10)
{
    dum = BU_optim_err(ik = ikw, hier_method = "comb_OLS",
                       total_data_raw_rate_region = MFTS_region_forc_total_arima_K6,
                       female_data_raw_rate_region = MFTS_region_forc_female_arima_K6,
                       male_data_raw_rate_region = MFTS_region_forc_male_arima_K6,
                       female_data_raw_rate = MFTS_state_forc_female_arima_K6,
                       male_data_raw_rate = MFTS_state_forc_male_arima_K6,
                       total_data_raw_rate = MFTS_state_forc_total_arima_K6)

    ######
    # MAE
    ######

    # Total + Sex

    MFTS_optim_Level_T_err_arima_K6[ikw,1] = dum$mae_total_err
    MFTS_optim_Level_F_err_arima_K6[ikw,1] = dum$mae_female_err
    MFTS_optim_Level_M_err_arima_K6[ikw,1] = dum$mae_male_err

    # Region + Sex

    MFTS_optim_Level_T_R_err_arima_K6[ikw,1,] = dum$mae_total_R_err
    MFTS_optim_Level_F_R_err_arima_K6[ikw,1,] = dum$mae_female_R_err
    MFTS_optim_Level_M_R_err_arima_K6[ikw,1,] = dum$mae_male_R_err

    # Prefecture + Sex

    MFTS_optim_Level_T_state_err_arima_K6[,ikw,1]   = dum$mae_state_err
    MFTS_optim_Level_F_state_err_arima_K6[,ikw,1] = dum$mae_bottom_female_err
    MFTS_optim_Level_M_state_err_arima_K6[,ikw,1] = dum$mae_bottom_male_err

    #######
    # RMSE
    #######

    # Total + Sex

    MFTS_optim_Level_T_err_arima_K6[ikw,2] = dum$rmse_total_err
    MFTS_optim_Level_F_err_arima_K6[ikw,2] = dum$rmse_female_err
    MFTS_optim_Level_M_err_arima_K6[ikw,2] = dum$rmse_male_err

    # Region + Sex

    MFTS_optim_Level_T_R_err_arima_K6[ikw,2,] = dum$rmse_total_R_err
    MFTS_optim_Level_F_R_err_arima_K6[ikw,2,] = dum$rmse_female_R_err
    MFTS_optim_Level_M_R_err_arima_K6[ikw,2,] = dum$rmse_male_R_err

    # Prefecture + Sex

    MFTS_optim_Level_T_state_err_arima_K6[,ikw,2] = dum$rmse_state_err
    MFTS_optim_Level_F_state_err_arima_K6[,ikw,2] = dum$rmse_bottom_female_err
    MFTS_optim_Level_M_state_err_arima_K6[,ikw,2] = dum$rmse_bottom_male_err
    print(ikw); rm(ikw); rm(dum)
}

MFTS_optim_all_level_err_mae_arima_K6 = cbind(MFTS_optim_Level_T_err_arima_K6[,1],
                                              rowMeans(cbind(MFTS_optim_Level_F_err_arima_K6[,1], MFTS_optim_Level_M_err_arima_K6[,1])),
                                              rowMeans(MFTS_optim_Level_T_R_err_arima_K6[,1,]),
                                              rowMeans(cbind(rowMeans(MFTS_optim_Level_F_R_err_arima_K6[,1,]), rowMeans(MFTS_optim_Level_M_R_err_arima_K6[,1,]))),
                                              colMeans(MFTS_optim_Level_T_state_err_arima_K6[,,1]),
                                              colMeans(rbind(MFTS_optim_Level_F_state_err_arima_K6[,,1], MFTS_optim_Level_M_state_err_arima_K6[,,1])))

MFTS_optim_all_level_err_rmse_arima_K6 = cbind(MFTS_optim_Level_T_err_arima_K6[,2],
                                               rowMeans(cbind(MFTS_optim_Level_F_err_arima_K6[,2], MFTS_optim_Level_M_err_arima_K6[,2])),
                                               rowMeans(MFTS_optim_Level_T_R_err_arima_K6[,2,]),
                                               rowMeans(cbind(rowMeans(MFTS_optim_Level_F_R_err_arima_K6[,2,]), rowMeans(MFTS_optim_Level_M_R_err_arima_K6[,2,]))),
                                               colMeans(MFTS_optim_Level_T_state_err_arima_K6[,,2]),
                                               colMeans(rbind(MFTS_optim_Level_F_state_err_arima_K6[,,2], MFTS_optim_Level_M_state_err_arima_K6[,,2])))
colnames(MFTS_optim_all_level_err_mae_arima_K6) = colnames(MFTS_optim_all_level_err_rmse_arima_K6) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

#######################
# fmethod = "comb_GLS"
#######################

MFTS_optim_GLS_Level_T_err_arima_K6 = MFTS_optim_GLS_Level_F_err_arima_K6 = MFTS_optim_GLS_Level_M_err_arima_K6 = matrix(NA, 10, 2)
MFTS_optim_GLS_Level_T_R_err_arima_K6 = MFTS_optim_GLS_Level_F_R_err_arima_K6 = MFTS_optim_GLS_Level_M_R_err_arima_K6 = array(NA, dim = c(10, 2, 8),
                                                                                                                           dimnames = list(1:10, c("MAE", "RMSE"), region))

MFTS_optim_GLS_Level_T_state_err_arima_K6 = MFTS_optim_GLS_Level_F_state_err_arima_K6 = MFTS_optim_GLS_Level_M_state_err_arima_K6 = array(NA, dim = c(47,10,2),
                                                                                                                                       dimnames = list(state, 1:10, c("MAE", "RMSE")))
for(ikw in 1:10)
{
    dum = BU_optim_err(ik = ikw, hier_method = "comb_GLS",
                       total_data_raw_rate_region = MFTS_region_forc_total_arima_K6,
                       female_data_raw_rate_region = MFTS_region_forc_female_arima_K6,
                       male_data_raw_rate_region = MFTS_region_forc_male_arima_K6,
                       female_data_raw_rate = MFTS_state_forc_female_arima_K6,
                       male_data_raw_rate = MFTS_state_forc_male_arima_K6,
                       total_data_raw_rate = MFTS_state_forc_total_arima_K6)

    ######
    # MAE
    ######

    # Total + Sex

    MFTS_optim_GLS_Level_T_err_arima_K6[ikw,1] = dum$mae_total_err
    MFTS_optim_GLS_Level_F_err_arima_K6[ikw,1] = dum$mae_female_err
    MFTS_optim_GLS_Level_M_err_arima_K6[ikw,1] = dum$mae_male_err

    # Region + Sex

    MFTS_optim_GLS_Level_T_R_err_arima_K6[ikw,1,] = dum$mae_total_R_err
    MFTS_optim_GLS_Level_F_R_err_arima_K6[ikw,1,] = dum$mae_female_R_err
    MFTS_optim_GLS_Level_M_R_err_arima_K6[ikw,1,] = dum$mae_male_R_err

    # Prefecture + Sex

    MFTS_optim_GLS_Level_T_state_err_arima_K6[,ikw,1] = dum$mae_state_err
    MFTS_optim_GLS_Level_F_state_err_arima_K6[,ikw,1] = dum$mae_bottom_female_err
    MFTS_optim_GLS_Level_M_state_err_arima_K6[,ikw,1] = dum$mae_bottom_male_err

    #######
    # RMSE
    #######

    # Total + Sex

    MFTS_optim_GLS_Level_T_err_arima_K6[ikw,2] = dum$rmse_total_err
    MFTS_optim_GLS_Level_F_err_arima_K6[ikw,2] = dum$rmse_female_err
    MFTS_optim_GLS_Level_M_err_arima_K6[ikw,2] = dum$rmse_male_err

    # Region + Sex

    MFTS_optim_GLS_Level_T_R_err_arima_K6[ikw,2,] = dum$rmse_total_R_err
    MFTS_optim_GLS_Level_F_R_err_arima_K6[ikw,2,] = dum$rmse_female_R_err
    MFTS_optim_GLS_Level_M_R_err_arima_K6[ikw,2,] = dum$rmse_male_R_err

    # Prefecture + Sex

    MFTS_optim_GLS_Level_T_state_err_arima_K6[,ikw,2] = dum$rmse_state_err
    MFTS_optim_GLS_Level_F_state_err_arima_K6[,ikw,2] = dum$rmse_bottom_female_err
    MFTS_optim_GLS_Level_M_state_err_arima_K6[,ikw,2] = dum$rmse_bottom_male_err
    print(ikw); rm(ikw)
}

MFTS_optim_GLS_all_level_err_mae_arima_K6 = cbind(MFTS_optim_GLS_Level_T_err_arima_K6[,1],
                                                  rowMeans(cbind(MFTS_optim_GLS_Level_F_err_arima_K6[,1], MFTS_optim_GLS_Level_M_err_arima_K6[,1])),
                                                  rowMeans(MFTS_optim_GLS_Level_T_R_err_arima_K6[,1,]),
                                                  rowMeans(cbind(rowMeans(MFTS_optim_GLS_Level_F_R_err_arima_K6[,1,]), rowMeans(MFTS_optim_GLS_Level_M_R_err_arima_K6[,1,]))),
                                                  colMeans(MFTS_optim_GLS_Level_T_state_err_arima_K6[,,1]),
                                                  colMeans(rbind(MFTS_optim_GLS_Level_F_state_err_arima_K6[,,1], MFTS_optim_GLS_Level_M_state_err_arima_K6[,,1])))

MFTS_optim_GLS_all_level_err_rmse_arima_K6 = cbind(MFTS_optim_GLS_Level_T_err_arima_K6[,2],
                                                   rowMeans(cbind(MFTS_optim_GLS_Level_F_err_arima_K6[,2], MFTS_optim_GLS_Level_M_err_arima_K6[,2])),
                                                   rowMeans(MFTS_optim_GLS_Level_T_R_err_arima_K6[,2,]),
                                                   rowMeans(cbind(rowMeans(MFTS_optim_GLS_Level_F_R_err_arima_K6[,2,]), rowMeans(MFTS_optim_GLS_Level_M_R_err_arima_K6[,2,]))),
                                                   colMeans(MFTS_optim_GLS_Level_T_state_err_arima_K6[,,2]),
                                                   colMeans(rbind(MFTS_optim_Level_F_state_err_arima_K6[,,2], MFTS_optim_Level_M_state_err_arima_K6[,,2])))
colnames(MFTS_optim_GLS_all_level_err_mae_arima_K6) = colnames(MFTS_optim_GLS_all_level_err_rmse_arima_K6) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

#################
# summary output
#################

# Base

MFTS_ind_all_level_err_mae_arima_K6_summary = rbind(MFTS_ind_all_level_err_mae_arima_K6,
                                                    colMeans(MFTS_ind_all_level_err_mae_arima_K6),
                                                    apply(MFTS_ind_all_level_err_mae_arima_K6, 2, median))

# BU

MFTS_BU_all_level_err_mae_arima_K6_summary = rbind(MFTS_BU_all_level_err_mae_arima_K6,
                                                   colMeans(MFTS_BU_all_level_err_mae_arima_K6),
                                                   apply(MFTS_BU_all_level_err_mae_arima_K6, 2, median))

# OLS

MFTS_optim_all_level_err_mae_arima_K6_summary = rbind(MFTS_optim_all_level_err_mae_arima_K6,
                                                      colMeans(MFTS_optim_all_level_err_mae_arima_K6),
                                                      apply(MFTS_optim_all_level_err_mae_arima_K6, 2, median))

# GLS

MFTS_optim_GLS_all_level_err_mae_arima_K6_summary = rbind(MFTS_optim_GLS_all_level_err_mae_arima_K6,
                                                         colMeans(MFTS_optim_GLS_all_level_err_mae_arima_K6),
                                                         apply(MFTS_optim_GLS_all_level_err_mae_arima_K6, 2, median))

rownames(MFTS_ind_all_level_err_mae_arima_K6_summary) = rownames(MFTS_BU_all_level_err_mae_arima_K6_summary) =
rownames(MFTS_optim_all_level_err_mae_arima_K6_summary) = rownames(MFTS_optim_GLS_all_level_err_mae_arima_K6_summary) = c(1:10, "Mean", "Median")

###############################################################################
## Plot forecast accuracy by level of the group structure
## Methods: independent (base), bottom-up, OLS, GLS reconciliation
## Inputs are objects already in the workspace.
###############################################################################

levels_gs <- c("Total", "Sex", "Region", "Region + Sex",
               "Prefecture", "Prefecture + Sex")

## Coerce to a numeric matrix with consistent dimnames.
## Assumes rows are h = 1:10 followed by the Mean and Median summary rows,
## and 6 columns in the order given by levels_gs.
harmonise <- function(x)
{
    m <- as.matrix(x)
    storage.mode(m) <- "double"
    if (is.null(rownames(m)))
      rownames(m) <- c(seq_len(nrow(m) - 2L), "Mean", "Median")
    colnames(m) <- levels_gs
    m
}

## ---- 1. Collect the four objects -------------------------------------------
fc_MFTS_arima_K6 <- list(Independent = MFTS_ind_all_level_err_mae_arima_K6_summary,
                          `Bottom-up` = MFTS_BU_all_level_err_mae_arima_K6_summary,
                          OLS         = MFTS_optim_all_level_err_mae_arima_K6_summary,
                          GLS         = MFTS_optim_GLS_all_level_err_mae_arima_K6_summary)

fc_MFTS_arima_K6 <- lapply(fc_MFTS_arima_K6, harmonise)

h    <- 1:10                                  # forecast horizons
fc_MFTS_arima_K6_h <- lapply(fc_MFTS_arima_K6, function(m) m[seq_along(h), , drop = FALSE])

## Summary table (mean over horizons), handy for the paper
summ_MFTS_arima_K6 <- t(sapply(fc_MFTS_arima_K6, function(m) m["Mean", ]))
print(round(summ_MFTS_arima_K6, 5))

## ---- 2. Base graphics: one panel per level ---------------------------------
meth <- names(fc_MFTS_arima_EVR)
cols <- c("black", "#D55E00", "#0072B2", "#009E73")
pchs <- c(16, 17, 15, 18)
ltys <- c(1, 2, 3, 4)

dir.create("figures", showWarnings = FALSE)
pdf(file.path("figures", "MFTS_K6_arima_forecast_accuracy_by_level.pdf"), width = 11, height = 7)
op <- par(mfrow = c(2, 3), mar = c(4, 4.4, 2.6, 1))
for(j in seq_along(levels_gs))
{
  yy <- sapply(fc_MFTS_arima_K6_h, function(m) m[, j])      # 10 x 4
  matplot(h, yy, type = "b", col = cols, pch = pchs, lty = ltys, lwd = 1.8,
          xlab = "Forecast horizon (h)", ylab = "MAE",
          main = levels_gs[j], ylim = range(yy), xaxt = "n")
  axis(1, at = h)
  if(j == 1)
    legend("topleft", legend = meth, col = cols, pch = pchs, lty = ltys, lwd = 1.8, bty = "n", cex = 0.9)
}
par(op)
dev.off()
