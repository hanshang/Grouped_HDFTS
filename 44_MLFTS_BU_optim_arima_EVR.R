# ----------------------------------------------------------------------------
# 44_MLFTS_BU_optim_arima_EVR.R
#
# Reconcile the MLFTS base forecasts (ARIMA scores, components by the EVR
# criterion) bottom-up and by optimal combination (OLS and WLS), then
# tabulate forecast errors by level of the group structure.
#
# Sources: 24_Japan_S_mat.R, 25_UFTS_BU_optim_fun.R
# Writes:  figures/MLFTS_EVR_arima_forecast_accuracy_by_level.pdf
#
# Replication code for Shang, H. L., 'High-dimensional functional time
# series forecasting with a group structure'.
#
# Run with the working directory set to the code_submission/ folder.
# Scripts are numbered 01-51 in the order in which they must be run.
# See README.md for a one-line description of each.
# ----------------------------------------------------------------------------

source("24_Japan_S_mat.R")

source("25_UFTS_BU_optim_fun.R")

###################
# fmethod = "Base"
###################

# MAE

MLFTS_ind_all_level_err_mae_arima_EVR = cbind(Japan_T_fore_mae_arima_EVR,
                                              rowMeans(cbind(Japan_F_fore_mae_arima_EVR, Japan_M_fore_mae_arima_EVR)),
                                              colMeans(MLFTS_model_region_fun_EVR_arima_mae_T),
                                              colMeans(rbind(MLFTS_model_region_fun_EVR_arima_mae_F, MLFTS_model_region_fun_EVR_arima_mae_M)),
                                              colMeans(MLFTS_model_fun_EVR_arima_mae_T),
                                              colMeans(rbind(MLFTS_model_fun_EVR_arima_mae_F, MLFTS_model_fun_EVR_arima_mae_M)))
colnames(MLFTS_ind_all_level_err_mae_arima_EVR) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

# RMSE

MLFTS_ind_all_level_err_rmse_arima_EVR = cbind(Japan_T_fore_rmse_arima_EVR,
                                               rowMeans(cbind(Japan_F_fore_rmse_arima_EVR, Japan_M_fore_rmse_arima_EVR)),
                                               colMeans(MLFTS_model_region_fun_EVR_arima_rmse_T),
                                               colMeans(rbind(MLFTS_model_region_fun_EVR_arima_rmse_F, MLFTS_model_region_fun_EVR_arima_rmse_M)),
                                               colMeans(MLFTS_model_fun_EVR_arima_rmse_T),
                                               colMeans(rbind(MLFTS_model_fun_EVR_arima_rmse_F, MLFTS_model_fun_EVR_arima_rmse_M)))
colnames(MLFTS_ind_all_level_err_rmse_arima_EVR) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")


#################
# fmethod = "BU"
#################

MLFTS_BU_Level_T_err_arima_EVR = MLFTS_BU_Level_F_err_arima_EVR = MLFTS_BU_Level_M_err_arima_EVR = matrix(NA, 10, 2)
MLFTS_BU_Level_T_R_err_arima_EVR = MLFTS_BU_Level_F_R_err_arima_EVR = MLFTS_BU_Level_M_R_err_arima_EVR = array(NA, dim = c(10, 2, 8),
                                                                                                         dimnames = list(1:10, c("MAE", "RMSE"), region))
MLFTS_BU_Level_T_state_err_arima_EVR = MLFTS_BU_Level_F_state_err_arima_EVR = MLFTS_BU_Level_M_state_err_arima_EVR = array(NA, dim = c(47, 10, 2),
                                                                                                                     dimnames = list(state, 1:10, c("MAE", "RMSE")))
for(ikw in 1:10)
{
    dum = BU_optim_err(ik = ikw, hier_method = "BU",
                       total_data_raw_rate_region = MLFTS_region_forc_total_arima_EVR,
                       female_data_raw_rate_region = MLFTS_region_forc_female_arima_EVR,
                       male_data_raw_rate_region = MLFTS_region_forc_male_arima_EVR,

                       female_data_raw_rate = MLFTS_state_forc_female_arima_EVR,
                       male_data_raw_rate = MLFTS_state_forc_male_arima_EVR,
                       total_data_raw_rate = MLFTS_state_forc_total_arima_EVR)

    # Total + Sex

    MLFTS_BU_Level_T_err_arima_EVR[ikw,1] = dum$mae_total_err
    MLFTS_BU_Level_F_err_arima_EVR[ikw,1] = dum$mae_female_err
    MLFTS_BU_Level_M_err_arima_EVR[ikw,1] = dum$mae_male_err

    MLFTS_BU_Level_T_err_arima_EVR[ikw,2] = dum$rmse_total_err
    MLFTS_BU_Level_F_err_arima_EVR[ikw,2] = dum$rmse_female_err
    MLFTS_BU_Level_M_err_arima_EVR[ikw,2] = dum$rmse_male_err

    # Region + Sex

    MLFTS_BU_Level_T_R_err_arima_EVR[ikw,1,] = dum$mae_total_R_err
    MLFTS_BU_Level_F_R_err_arima_EVR[ikw,1,] = dum$mae_female_R_err
    MLFTS_BU_Level_M_R_err_arima_EVR[ikw,1,] = dum$mae_male_R_err

    MLFTS_BU_Level_T_R_err_arima_EVR[ikw,2,] = dum$rmse_total_R_err
    MLFTS_BU_Level_F_R_err_arima_EVR[ikw,2,] = dum$rmse_female_R_err
    MLFTS_BU_Level_M_R_err_arima_EVR[ikw,2,] = dum$rmse_male_R_err

    # Prefecture + Sex

    MLFTS_BU_Level_T_state_err_arima_EVR[,ikw,1] = dum$mae_state_err
    MLFTS_BU_Level_F_state_err_arima_EVR[,ikw,1] = dum$mae_bottom_female_err
    MLFTS_BU_Level_M_state_err_arima_EVR[,ikw,1] = dum$mae_bottom_male_err

    MLFTS_BU_Level_T_state_err_arima_EVR[,ikw,2] = dum$rmse_state_err
    MLFTS_BU_Level_F_state_err_arima_EVR[,ikw,2] = dum$rmse_bottom_female_err
    MLFTS_BU_Level_M_state_err_arima_EVR[,ikw,2] = dum$rmse_bottom_male_err

    print(ikw); rm(ikw); rm(dum)
}

MLFTS_BU_all_level_err_mae_arima_EVR = cbind(MLFTS_BU_Level_T_err_arima_EVR[,1],
                                             rowMeans(cbind(MLFTS_BU_Level_F_err_arima_EVR[,1], MLFTS_BU_Level_M_err_arima_EVR[,1])),
                                             rowMeans(MLFTS_BU_Level_T_R_err_arima_EVR[,1,]),
                                             rowMeans(cbind(rowMeans(MLFTS_BU_Level_F_R_err_arima_EVR[,1,]), rowMeans(MLFTS_BU_Level_M_R_err_arima_EVR[,1,]))),
                                             colMeans(MLFTS_BU_Level_T_state_err_arima_EVR[,,1]),
                                             colMeans(rbind(MLFTS_BU_Level_F_state_err_arima_EVR[,,1], MLFTS_BU_Level_M_state_err_arima_EVR[,,1])))

MLFTS_BU_all_level_err_rmse_arima_EVR = cbind(MLFTS_BU_Level_T_err_arima_EVR[,2],
                                              rowMeans(cbind(MLFTS_BU_Level_F_err_arima_EVR[,2], MLFTS_BU_Level_M_err_arima_EVR[,2])),
                                              rowMeans(MLFTS_BU_Level_T_R_err_arima_EVR[,2,]),
                                              rowMeans(cbind(rowMeans(MLFTS_BU_Level_F_R_err_arima_EVR[,2,]), rowMeans(MLFTS_BU_Level_M_R_err_arima_EVR[,2,]))),
                                              colMeans(MLFTS_BU_Level_T_state_err_arima_EVR[,,2]),
                                              colMeans(rbind(MLFTS_BU_Level_F_state_err_arima_EVR[,,2], MLFTS_BU_Level_M_state_err_arima_EVR[,,2])))
colnames(MLFTS_BU_all_level_err_mae_arima_EVR) = colnames(MLFTS_BU_all_level_err_rmse_arima_EVR) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")


#######################
# fmethod = "comb_OLS"
#######################

MLFTS_optim_Level_T_err_arima_EVR = MLFTS_optim_Level_F_err_arima_EVR = MLFTS_optim_Level_M_err_arima_EVR = matrix(NA, 10, 2)
MLFTS_optim_Level_T_R_err_arima_EVR = MLFTS_optim_Level_F_R_err_arima_EVR = MLFTS_optim_Level_M_R_err_arima_EVR = array(NA, dim = c(10, 2, 8),
                                                                                                                  dimnames = list(1:10, c("MAE", "RMSE"), region))

MLFTS_optim_Level_T_state_err_arima_EVR = MLFTS_optim_Level_F_state_err_arima_EVR = MLFTS_optim_Level_M_state_err_arima_EVR = array(NA, dim = c(47, 10, 2),
                                                                                                                              dimnames = list(state, 1:10, c("MAE", "RMSE")))
for(ikw in 1:10)
{
    dum = BU_optim_err(ik = ikw, hier_method = "comb_OLS",
                       total_data_raw_rate_region = MLFTS_region_forc_total_arima_EVR,
                       female_data_raw_rate_region = MLFTS_region_forc_female_arima_EVR,
                       male_data_raw_rate_region = MLFTS_region_forc_male_arima_EVR,
                       female_data_raw_rate = MLFTS_state_forc_female_arima_EVR,
                       male_data_raw_rate = MLFTS_state_forc_male_arima_EVR,
                       total_data_raw_rate = MLFTS_state_forc_total_arima_EVR)

    ######
    # MAE
    ######

    # Total + Sex

    MLFTS_optim_Level_T_err_arima_EVR[ikw,1] = dum$mae_total_err
    MLFTS_optim_Level_F_err_arima_EVR[ikw,1] = dum$mae_female_err
    MLFTS_optim_Level_M_err_arima_EVR[ikw,1] = dum$mae_male_err

    # Region + Sex

    MLFTS_optim_Level_T_R_err_arima_EVR[ikw,1,] = dum$mae_total_R_err
    MLFTS_optim_Level_F_R_err_arima_EVR[ikw,1,] = dum$mae_female_R_err
    MLFTS_optim_Level_M_R_err_arima_EVR[ikw,1,] = dum$mae_male_R_err

    # Prefecture + Sex

    MLFTS_optim_Level_T_state_err_arima_EVR[,ikw,1]   = dum$mae_state_err
    MLFTS_optim_Level_F_state_err_arima_EVR[,ikw,1] = dum$mae_bottom_female_err
    MLFTS_optim_Level_M_state_err_arima_EVR[,ikw,1] = dum$mae_bottom_male_err

    #######
    # RMSE
    #######

    # Total + Sex

    MLFTS_optim_Level_T_err_arima_EVR[ikw,2] = dum$rmse_total_err
    MLFTS_optim_Level_F_err_arima_EVR[ikw,2] = dum$rmse_female_err
    MLFTS_optim_Level_M_err_arima_EVR[ikw,2] = dum$rmse_male_err

    # Region + Sex

    MLFTS_optim_Level_T_R_err_arima_EVR[ikw,2,] = dum$rmse_total_R_err
    MLFTS_optim_Level_F_R_err_arima_EVR[ikw,2,] = dum$rmse_female_R_err
    MLFTS_optim_Level_M_R_err_arima_EVR[ikw,2,] = dum$rmse_male_R_err

    # Prefecture + Sex

    MLFTS_optim_Level_T_state_err_arima_EVR[,ikw,2] = dum$rmse_state_err
    MLFTS_optim_Level_F_state_err_arima_EVR[,ikw,2] = dum$rmse_bottom_female_err
    MLFTS_optim_Level_M_state_err_arima_EVR[,ikw,2] = dum$rmse_bottom_male_err
    print(ikw); rm(ikw); rm(dum)
}

MLFTS_optim_all_level_err_mae_arima_EVR = cbind(MLFTS_optim_Level_T_err_arima_EVR[,1],
                                                rowMeans(cbind(MLFTS_optim_Level_F_err_arima_EVR[,1], MLFTS_optim_Level_M_err_arima_EVR[,1])),
                                                rowMeans(MLFTS_optim_Level_T_R_err_arima_EVR[,1,]),
                                                rowMeans(cbind(rowMeans(MLFTS_optim_Level_F_R_err_arima_EVR[,1,]), rowMeans(MLFTS_optim_Level_M_R_err_arima_EVR[,1,]))),
                                                colMeans(MLFTS_optim_Level_T_state_err_arima_EVR[,,1]),
                                                colMeans(rbind(MLFTS_optim_Level_F_state_err_arima_EVR[,,1], MLFTS_optim_Level_M_state_err_arima_EVR[,,1])))

MLFTS_optim_all_level_err_rmse_arima_EVR = cbind(MLFTS_optim_Level_T_err_arima_EVR[,2],
                                                 rowMeans(cbind(MLFTS_optim_Level_F_err_arima_EVR[,2], MLFTS_optim_Level_M_err_arima_EVR[,2])),
                                                 rowMeans(MLFTS_optim_Level_T_R_err_arima_EVR[,2,]),
                                                 rowMeans(cbind(rowMeans(MLFTS_optim_Level_F_R_err_arima_EVR[,2,]), rowMeans(MLFTS_optim_Level_M_R_err_arima_EVR[,2,]))),
                                                 colMeans(MLFTS_optim_Level_T_state_err_arima_EVR[,,2]),
                                                 colMeans(rbind(MLFTS_optim_Level_F_state_err_arima_EVR[,,2], MLFTS_optim_Level_M_state_err_arima_EVR[,,2])))
colnames(MLFTS_optim_all_level_err_mae_arima_EVR) = colnames(MLFTS_optim_all_level_err_rmse_arima_EVR) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")


#######################
# fmethod = "comb_GLS"
#######################

MLFTS_optim_GLS_Level_T_err_arima_EVR = MLFTS_optim_GLS_Level_F_err_arima_EVR = MLFTS_optim_GLS_Level_M_err_arima_EVR = matrix(NA, 10, 2)
MLFTS_optim_GLS_Level_T_R_err_arima_EVR = MLFTS_optim_GLS_Level_F_R_err_arima_EVR = MLFTS_optim_GLS_Level_M_R_err_arima_EVR = array(NA, dim = c(10, 2, 8),
                                                                                                                              dimnames = list(1:10, c("MAE", "RMSE"), region))

MLFTS_optim_GLS_Level_T_state_err_arima_EVR = MLFTS_optim_GLS_Level_F_state_err_arima_EVR = MLFTS_optim_GLS_Level_M_state_err_arima_EVR = array(NA, dim = c(47,10,2),
                                                                                                                                          dimnames = list(state, 1:10, c("MAE", "RMSE")))
for(ikw in 1:10)
{
    dum = BU_optim_err(ik = ikw, hier_method = "comb_GLS",
                       total_data_raw_rate_region = MLFTS_region_forc_total_arima_EVR,
                       female_data_raw_rate_region = MLFTS_region_forc_female_arima_EVR,
                       male_data_raw_rate_region = MLFTS_region_forc_male_arima_EVR,
                       female_data_raw_rate = MLFTS_state_forc_female_arima_EVR,
                       male_data_raw_rate = MLFTS_state_forc_male_arima_EVR,
                       total_data_raw_rate = MLFTS_state_forc_total_arima_EVR)

    ######
    # MAE
    ######

    # Total + Sex

    MLFTS_optim_GLS_Level_T_err_arima_EVR[ikw,1] = dum$mae_total_err
    MLFTS_optim_GLS_Level_F_err_arima_EVR[ikw,1] = dum$mae_female_err
    MLFTS_optim_GLS_Level_M_err_arima_EVR[ikw,1] = dum$mae_male_err

    # Region + Sex

    MLFTS_optim_GLS_Level_T_R_err_arima_EVR[ikw,1,] = dum$mae_total_R_err
    MLFTS_optim_GLS_Level_F_R_err_arima_EVR[ikw,1,] = dum$mae_female_R_err
    MLFTS_optim_GLS_Level_M_R_err_arima_EVR[ikw,1,] = dum$mae_male_R_err

    # Prefecture + Sex

    MLFTS_optim_GLS_Level_T_state_err_arima_EVR[,ikw,1] = dum$mae_state_err
    MLFTS_optim_GLS_Level_F_state_err_arima_EVR[,ikw,1] = dum$mae_bottom_female_err
    MLFTS_optim_GLS_Level_M_state_err_arima_EVR[,ikw,1] = dum$mae_bottom_male_err

    #######
    # RMSE
    #######

    # Total + Sex

    MLFTS_optim_GLS_Level_T_err_arima_EVR[ikw,2] = dum$rmse_total_err
    MLFTS_optim_GLS_Level_F_err_arima_EVR[ikw,2] = dum$rmse_female_err
    MLFTS_optim_GLS_Level_M_err_arima_EVR[ikw,2] = dum$rmse_male_err

    # Region + Sex

    MLFTS_optim_GLS_Level_T_R_err_arima_EVR[ikw,2,] = dum$rmse_total_R_err
    MLFTS_optim_GLS_Level_F_R_err_arima_EVR[ikw,2,] = dum$rmse_female_R_err
    MLFTS_optim_GLS_Level_M_R_err_arima_EVR[ikw,2,] = dum$rmse_male_R_err

    # Prefecture + Sex

    MLFTS_optim_GLS_Level_T_state_err_arima_EVR[,ikw,2] = dum$rmse_state_err
    MLFTS_optim_GLS_Level_F_state_err_arima_EVR[,ikw,2] = dum$rmse_bottom_female_err
    MLFTS_optim_GLS_Level_M_state_err_arima_EVR[,ikw,2] = dum$rmse_bottom_male_err
    print(ikw); rm(ikw)
}

MLFTS_optim_GLS_all_level_err_mae_arima_EVR = cbind(MLFTS_optim_GLS_Level_T_err_arima_EVR[,1],
                                                    rowMeans(cbind(MLFTS_optim_GLS_Level_F_err_arima_EVR[,1], MLFTS_optim_GLS_Level_M_err_arima_EVR[,1])),
                                                    rowMeans(MLFTS_optim_GLS_Level_T_R_err_arima_EVR[,1,]),
                                                    rowMeans(cbind(rowMeans(MLFTS_optim_GLS_Level_F_R_err_arima_EVR[,1,]), rowMeans(MLFTS_optim_GLS_Level_M_R_err_arima_EVR[,1,]))),
                                                    colMeans(MLFTS_optim_GLS_Level_T_state_err_arima_EVR[,,1]),
                                                    colMeans(rbind(MLFTS_optim_GLS_Level_F_state_err_arima_EVR[,,1], MLFTS_optim_GLS_Level_M_state_err_arima_EVR[,,1])))

MLFTS_optim_GLS_all_level_err_rmse_arima_EVR = cbind(MLFTS_optim_GLS_Level_T_err_arima_EVR[,2],
                                                     rowMeans(cbind(MLFTS_optim_GLS_Level_F_err_arima_EVR[,2], MLFTS_optim_GLS_Level_M_err_arima_EVR[,2])),
                                                     rowMeans(MLFTS_optim_GLS_Level_T_R_err_arima_EVR[,2,]),
                                                     rowMeans(cbind(rowMeans(MLFTS_optim_GLS_Level_F_R_err_arima_EVR[,2,]), rowMeans(MLFTS_optim_GLS_Level_M_R_err_arima_EVR[,2,]))),
                                                     colMeans(MLFTS_optim_GLS_Level_T_state_err_arima_EVR[,,2]),
                                                     colMeans(rbind(MLFTS_optim_Level_F_state_err_arima_EVR[,,2], MLFTS_optim_Level_M_state_err_arima_EVR[,,2])))
colnames(MLFTS_optim_GLS_all_level_err_mae_arima_EVR) = colnames(MLFTS_optim_GLS_all_level_err_rmse_arima_EVR) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

#################
# summary output
#################

# Base

MLFTS_ind_all_level_err_mae_arima_EVR_summary = rbind(MLFTS_ind_all_level_err_mae_arima_EVR,
                                                      colMeans(MLFTS_ind_all_level_err_mae_arima_EVR),
                                                      apply(MLFTS_ind_all_level_err_mae_arima_EVR, 2, median))

# BU

MLFTS_BU_all_level_err_mae_arima_EVR_summary = rbind(MLFTS_BU_all_level_err_mae_arima_EVR,
                                                     colMeans(MLFTS_BU_all_level_err_mae_arima_EVR),
                                                     apply(MLFTS_BU_all_level_err_mae_arima_EVR, 2, median))

# OLS

MLFTS_optim_all_level_err_mae_arima_EVR_summary = rbind(MLFTS_optim_all_level_err_mae_arima_EVR,
                                                        colMeans(MLFTS_optim_all_level_err_mae_arima_EVR),
                                                        apply(MLFTS_optim_all_level_err_mae_arima_EVR, 2, median))

# GLS

MLFTS_optim_GLS_all_level_err_mae_arima_EVR_summary = rbind(MLFTS_optim_GLS_all_level_err_mae_arima_EVR,
                                                            colMeans(MLFTS_optim_GLS_all_level_err_mae_arima_EVR),
                                                            apply(MLFTS_optim_GLS_all_level_err_mae_arima_EVR, 2, median))

rownames(MLFTS_ind_all_level_err_mae_arima_EVR_summary) = rownames(MLFTS_BU_all_level_err_mae_arima_EVR_summary) =
rownames(MLFTS_optim_all_level_err_mae_arima_EVR_summary) = rownames(MLFTS_optim_GLS_all_level_err_mae_arima_EVR_summary) = c(1:10, "Mean", "Median")

####################################################################
## Plot forecast accuracy by level of the group structure
## Methods: independent (base), bottom-up, OLS, GLS reconciliation
## Inputs are objects already in the workspace.
####################################################################

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
fc_MLFTS_arima_EVR <- list(Independent = MLFTS_ind_all_level_err_mae_arima_EVR_summary,
                           `Bottom-up` = MLFTS_BU_all_level_err_mae_arima_EVR_summary,
                           OLS         = MLFTS_optim_all_level_err_mae_arima_EVR_summary,
                           GLS         = MLFTS_optim_GLS_all_level_err_mae_arima_EVR_summary)

fc_MLFTS_arima_EVR <- lapply(fc_MLFTS_arima_EVR, harmonise)

h    <- 1:10                                  # forecast horizons
fc_MLFTS_arima_EVR_h <- lapply(fc_MLFTS_arima_EVR, function(m) m[seq_along(h), , drop = FALSE])

## Summary table (mean over horizons), handy for the paper
summ_MLFTS_arima_EVR <- t(sapply(fc_MLFTS_arima_EVR, function(m) m["Mean", ]))
print(round(summ_MLFTS_arima_EVR, 5))

## ---- 2. Base graphics: one panel per level ---------------------------------
meth <- names(fc_MLFTS_arima_EVR)
cols <- c("black", "#D55E00", "#0072B2", "#009E73")
pchs <- c(16, 17, 15, 18)
ltys <- c(1, 2, 3, 4)

dir.create("figures", showWarnings = FALSE)
pdf(file.path("figures", "MLFTS_EVR_arima_forecast_accuracy_by_level.pdf"), width = 11, height = 7)
op <- par(mfrow = c(2, 3), mar = c(4, 4.4, 2.6, 1))
for(j in seq_along(levels_gs))
{
  yy <- sapply(fc_MLFTS_arima_EVR_h, function(m) m[, j])      # 10 x 4
  matplot(h, yy, type = "b", col = cols, pch = pchs, lty = ltys, lwd = 1.8,
          xlab = "Forecast horizon (h)", ylab = "MAE",
          main = levels_gs[j], ylim = range(yy), xaxt = "n")
  axis(1, at = h)
  if(j == 1)
    legend("topleft", legend = meth, col = cols, pch = pchs, lty = ltys, lwd = 1.8, bty = "n", cex = 0.9)
}
par(op)
dev.off()
