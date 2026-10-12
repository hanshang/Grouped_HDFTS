# ----------------------------------------------------------------------------
# 48_OWFANOVA_BU_optim_ets_K6.R
#
# Reconcile the one-way FANOVA + FFM base forecasts (ETS scores, K = 6
# components) bottom-up and by optimal combination (OLS and WLS), then
# tabulate forecast errors by level of the group structure.
#
# Sources: 24_Japan_S_mat.R, 25_UFTS_BU_optim_fun.R
# Writes:  figures/OWFANOVA_ets_forecast_accuracy_by_level.pdf
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

OWFANOVA_ind_all_level_err_mae_ets = cbind(Japan_T_fore_mae_ets_K6,
                                           rowMeans(cbind(Japan_F_fore_mae_ets_K6, Japan_M_fore_mae_ets_K6)),
                                           colMeans(OWFANOVA_FFM_region_ets_mae_total),
                                           colMeans(rbind(OWFANOVA_FFM_region_ets_mae_female, OWFANOVA_FFM_region_ets_mae_male)),
                                           colMeans(OWFANOVA_FFM_ets_mae_total),
                                           colMeans(rbind(OWFANOVA_FFM_ets_mae_female, OWFANOVA_FFM_ets_mae_male)))
colnames(OWFANOVA_ind_all_level_err_mae_ets) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

# RMSE

OWFANOVA_ind_all_level_err_rmse_ets = cbind(Japan_T_fore_rmse_ets_K6,
                                            rowMeans(cbind(Japan_F_fore_rmse_ets_K6, Japan_M_fore_rmse_ets_K6)),
                                            colMeans(OWFANOVA_FFM_region_ets_rmse_total),
                                            colMeans(rbind(OWFANOVA_FFM_region_ets_rmse_female, OWFANOVA_FFM_region_ets_rmse_male)),
                                            colMeans(OWFANOVA_FFM_ets_rmse_total),
                                            colMeans(rbind(OWFANOVA_FFM_ets_rmse_female, OWFANOVA_FFM_ets_rmse_male)))
colnames(OWFANOVA_ind_all_level_err_rmse_ets) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

#################
# fmethod = "BU"
#################

OWFANOVA_BU_Level_T_err_ets = OWFANOVA_BU_Level_F_err_ets = OWFANOVA_BU_Level_M_err_ets = matrix(NA, 10, 2)
OWFANOVA_BU_Level_T_R_err_ets = OWFANOVA_BU_Level_F_R_err_ets = OWFANOVA_BU_Level_M_R_err_ets = array(NA, dim = c(10, 2, 8),
                                                                                                            dimnames = list(1:10, c("MAE", "RMSE"), region))
OWFANOVA_BU_Level_T_state_err_ets = OWFANOVA_BU_Level_F_state_err_ets = OWFANOVA_BU_Level_M_state_err_ets = array(NA, dim = c(47, 10, 2),
                                                                                                                        dimnames = list(state, 1:10, c("MAE", "RMSE")))
for(ikw in 1:10)
{
    dum = BU_optim_err(ik = ikw, hier_method = "BU",
                       total_data_raw_rate_region = OWFANOVA_region_forc_total_ets,
                       female_data_raw_rate_region = OWFANOVA_region_forc_female_ets,
                       male_data_raw_rate_region = OWFANOVA_region_forc_male_ets,

                       female_data_raw_rate = OWFANOVA_state_forc_female_ets,
                       male_data_raw_rate = OWFANOVA_state_forc_male_ets,
                       total_data_raw_rate = OWFANOVA_state_forc_total_ets)

    # Total + Sex

    OWFANOVA_BU_Level_T_err_ets[ikw,1] = dum$mae_total_err
    OWFANOVA_BU_Level_F_err_ets[ikw,1] = dum$mae_female_err
    OWFANOVA_BU_Level_M_err_ets[ikw,1] = dum$mae_male_err

    OWFANOVA_BU_Level_T_err_ets[ikw,2] = dum$rmse_total_err
    OWFANOVA_BU_Level_F_err_ets[ikw,2] = dum$rmse_female_err
    OWFANOVA_BU_Level_M_err_ets[ikw,2] = dum$rmse_male_err

    # Region + Sex

    OWFANOVA_BU_Level_T_R_err_ets[ikw,1,] = dum$mae_total_R_err
    OWFANOVA_BU_Level_F_R_err_ets[ikw,1,] = dum$mae_female_R_err
    OWFANOVA_BU_Level_M_R_err_ets[ikw,1,] = dum$mae_male_R_err

    OWFANOVA_BU_Level_T_R_err_ets[ikw,2,] = dum$rmse_total_R_err
    OWFANOVA_BU_Level_F_R_err_ets[ikw,2,] = dum$rmse_female_R_err
    OWFANOVA_BU_Level_M_R_err_ets[ikw,2,] = dum$rmse_male_R_err

    # Prefecture + Sex

    OWFANOVA_BU_Level_T_state_err_ets[,ikw,1] = dum$mae_state_err
    OWFANOVA_BU_Level_F_state_err_ets[,ikw,1] = dum$mae_bottom_female_err
    OWFANOVA_BU_Level_M_state_err_ets[,ikw,1] = dum$mae_bottom_male_err

    OWFANOVA_BU_Level_T_state_err_ets[,ikw,2] = dum$rmse_state_err
    OWFANOVA_BU_Level_F_state_err_ets[,ikw,2] = dum$rmse_bottom_female_err
    OWFANOVA_BU_Level_M_state_err_ets[,ikw,2] = dum$rmse_bottom_male_err

    print(ikw); rm(ikw); rm(dum)
}

OWFANOVA_BU_all_level_err_mae_ets = cbind(OWFANOVA_BU_Level_T_err_ets[,1],
                                          rowMeans(cbind(OWFANOVA_BU_Level_F_err_ets[,1], OWFANOVA_BU_Level_M_err_ets[,1])),
                                          rowMeans(OWFANOVA_BU_Level_T_R_err_ets[,1,]),
                                          rowMeans(cbind(rowMeans(OWFANOVA_BU_Level_F_R_err_ets[,1,]), rowMeans(OWFANOVA_BU_Level_M_R_err_ets[,1,]))),
                                          colMeans(OWFANOVA_BU_Level_T_state_err_ets[,,1]),
                                          colMeans(rbind(OWFANOVA_BU_Level_F_state_err_ets[,,1], OWFANOVA_BU_Level_M_state_err_ets[,,1])))

OWFANOVA_BU_all_level_err_rmse_ets = cbind(OWFANOVA_BU_Level_T_err_ets[,2],
                                           rowMeans(cbind(OWFANOVA_BU_Level_F_err_ets[,2], OWFANOVA_BU_Level_M_err_ets[,2])),
                                           rowMeans(OWFANOVA_BU_Level_T_R_err_ets[,2,]),
                                           rowMeans(cbind(rowMeans(OWFANOVA_BU_Level_F_R_err_ets[,2,]), rowMeans(OWFANOVA_BU_Level_M_R_err_ets[,2,]))),
                                           colMeans(OWFANOVA_BU_Level_T_state_err_ets[,,2]),
                                           colMeans(rbind(OWFANOVA_BU_Level_F_state_err_ets[,,2], OWFANOVA_BU_Level_M_state_err_ets[,,2])))
colnames(OWFANOVA_BU_all_level_err_mae_ets) = colnames(OWFANOVA_BU_all_level_err_rmse_ets) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

#######################
# fmethod = "comb_OLS"
#######################

OWFANOVA_optim_Level_T_err_ets = OWFANOVA_optim_Level_F_err_ets = OWFANOVA_optim_Level_M_err_ets = matrix(NA, 10, 2)
OWFANOVA_optim_Level_T_R_err_ets = OWFANOVA_optim_Level_F_R_err_ets = OWFANOVA_optim_Level_M_R_err_ets = array(NA, dim = c(10, 2, 8),
                                                                                                               dimnames = list(1:10, c("MAE", "RMSE"), region))

OWFANOVA_optim_Level_T_state_err_ets = OWFANOVA_optim_Level_F_state_err_ets = OWFANOVA_optim_Level_M_state_err_ets = array(NA, dim = c(47, 10, 2),
                                                                                                                           dimnames = list(state, 1:10, c("MAE", "RMSE")))
for(ikw in 1:10)
{
    dum = BU_optim_err(ik = ikw, hier_method = "comb_OLS",
                       total_data_raw_rate_region = OWFANOVA_region_forc_total_ets,
                       female_data_raw_rate_region = OWFANOVA_region_forc_female_ets,
                       male_data_raw_rate_region = OWFANOVA_region_forc_male_ets,
                       female_data_raw_rate = OWFANOVA_state_forc_female_ets,
                       male_data_raw_rate = OWFANOVA_state_forc_male_ets,
                       total_data_raw_rate = OWFANOVA_state_forc_total_ets)

    ######
    # MAE
    ######

    # Total + Sex

    OWFANOVA_optim_Level_T_err_ets[ikw,1] = dum$mae_total_err
    OWFANOVA_optim_Level_F_err_ets[ikw,1] = dum$mae_female_err
    OWFANOVA_optim_Level_M_err_ets[ikw,1] = dum$mae_male_err

    # Region + Sex

    OWFANOVA_optim_Level_T_R_err_ets[ikw,1,] = dum$mae_total_R_err
    OWFANOVA_optim_Level_F_R_err_ets[ikw,1,] = dum$mae_female_R_err
    OWFANOVA_optim_Level_M_R_err_ets[ikw,1,] = dum$mae_male_R_err

    # Prefecture + Sex

    OWFANOVA_optim_Level_T_state_err_ets[,ikw,1] = dum$mae_state_err
    OWFANOVA_optim_Level_F_state_err_ets[,ikw,1] = dum$mae_bottom_female_err
    OWFANOVA_optim_Level_M_state_err_ets[,ikw,1] = dum$mae_bottom_male_err

    #######
    # RMSE
    #######

    # Total + Sex

    OWFANOVA_optim_Level_T_err_ets[ikw,2] = dum$rmse_total_err
    OWFANOVA_optim_Level_F_err_ets[ikw,2] = dum$rmse_female_err
    OWFANOVA_optim_Level_M_err_ets[ikw,2] = dum$rmse_male_err

    # Region + Sex

    OWFANOVA_optim_Level_T_R_err_ets[ikw,2,] = dum$rmse_total_R_err
    OWFANOVA_optim_Level_F_R_err_ets[ikw,2,] = dum$rmse_female_R_err
    OWFANOVA_optim_Level_M_R_err_ets[ikw,2,] = dum$rmse_male_R_err

    # Prefecture + Sex

    OWFANOVA_optim_Level_T_state_err_ets[,ikw,2] = dum$rmse_state_err
    OWFANOVA_optim_Level_F_state_err_ets[,ikw,2] = dum$rmse_bottom_female_err
    OWFANOVA_optim_Level_M_state_err_ets[,ikw,2] = dum$rmse_bottom_male_err
    print(ikw); rm(ikw); rm(dum)
}

OWFANOVA_optim_all_level_err_mae_ets = cbind(OWFANOVA_optim_Level_T_err_ets[,1],
                                             rowMeans(cbind(OWFANOVA_optim_Level_F_err_ets[,1], OWFANOVA_optim_Level_M_err_ets[,1])),
                                             rowMeans(OWFANOVA_optim_Level_T_R_err_ets[,1,]),
                                             rowMeans(cbind(rowMeans(OWFANOVA_optim_Level_F_R_err_ets[,1,]), rowMeans(OWFANOVA_optim_Level_M_R_err_ets[,1,]))),
                                             colMeans(OWFANOVA_optim_Level_T_state_err_ets[,,1]),
                                             colMeans(rbind(OWFANOVA_optim_Level_F_state_err_ets[,,1], OWFANOVA_optim_Level_M_state_err_ets[,,1])))

OWFANOVA_optim_all_level_err_rmse_ets = cbind(OWFANOVA_optim_Level_T_err_ets[,2],
                                              rowMeans(cbind(OWFANOVA_optim_Level_F_err_ets[,2], OWFANOVA_optim_Level_M_err_ets[,2])),
                                              rowMeans(OWFANOVA_optim_Level_T_R_err_ets[,2,]),
                                              rowMeans(cbind(rowMeans(OWFANOVA_optim_Level_F_R_err_ets[,2,]), rowMeans(OWFANOVA_optim_Level_M_R_err_ets[,2,]))),
                                              colMeans(OWFANOVA_optim_Level_T_state_err_ets[,,2]),
                                              colMeans(rbind(OWFANOVA_optim_Level_F_state_err_ets[,,2], OWFANOVA_optim_Level_M_state_err_ets[,,2])))
colnames(OWFANOVA_optim_all_level_err_mae_ets) = colnames(OWFANOVA_optim_all_level_err_rmse_ets) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

#######################
# fmethod = "comb_GLS"
#######################

OWFANOVA_optim_GLS_Level_T_err_ets = OWFANOVA_optim_GLS_Level_F_err_ets = OWFANOVA_optim_GLS_Level_M_err_ets = matrix(NA, 10, 2)
OWFANOVA_optim_GLS_Level_T_R_err_ets = OWFANOVA_optim_GLS_Level_F_R_err_ets = OWFANOVA_optim_GLS_Level_M_R_err_ets = array(NA, dim = c(10, 2, 8),
                                                                                                                           dimnames = list(1:10, c("MAE", "RMSE"), region))

OWFANOVA_optim_GLS_Level_T_state_err_ets = OWFANOVA_optim_GLS_Level_F_state_err_ets = OWFANOVA_optim_GLS_Level_M_state_err_ets = array(NA, dim = c(47,10,2),
                                                                                                                                       dimnames = list(state, 1:10, c("MAE", "RMSE")))
for(ikw in 1:10)
{
    dum = BU_optim_err(ik = ikw, hier_method = "comb_GLS",
                       total_data_raw_rate_region = OWFANOVA_region_forc_total_ets,
                       female_data_raw_rate_region = OWFANOVA_region_forc_female_ets,
                       male_data_raw_rate_region = OWFANOVA_region_forc_male_ets,
                       female_data_raw_rate = OWFANOVA_state_forc_female_ets,
                       male_data_raw_rate = OWFANOVA_state_forc_male_ets,
                       total_data_raw_rate = OWFANOVA_state_forc_total_ets)

    ######
    # MAE
    ######

    # Total + Sex

    OWFANOVA_optim_GLS_Level_T_err_ets[ikw,1] = dum$mae_total_err
    OWFANOVA_optim_GLS_Level_F_err_ets[ikw,1] = dum$mae_female_err
    OWFANOVA_optim_GLS_Level_M_err_ets[ikw,1] = dum$mae_male_err

    # Region + Sex

    OWFANOVA_optim_GLS_Level_T_R_err_ets[ikw,1,] = dum$mae_total_R_err
    OWFANOVA_optim_GLS_Level_F_R_err_ets[ikw,1,] = dum$mae_female_R_err
    OWFANOVA_optim_GLS_Level_M_R_err_ets[ikw,1,] = dum$mae_male_R_err

    # Prefecture + Sex

    OWFANOVA_optim_GLS_Level_T_state_err_ets[,ikw,1] = dum$mae_state_err
    OWFANOVA_optim_GLS_Level_F_state_err_ets[,ikw,1] = dum$mae_bottom_female_err
    OWFANOVA_optim_GLS_Level_M_state_err_ets[,ikw,1] = dum$mae_bottom_male_err

    #######
    # RMSE
    #######

    # Total + Sex

    OWFANOVA_optim_GLS_Level_T_err_ets[ikw,2] = dum$rmse_total_err
    OWFANOVA_optim_GLS_Level_F_err_ets[ikw,2] = dum$rmse_female_err
    OWFANOVA_optim_GLS_Level_M_err_ets[ikw,2] = dum$rmse_male_err

    # Region + Sex

    OWFANOVA_optim_GLS_Level_T_R_err_ets[ikw,2,] = dum$rmse_total_R_err
    OWFANOVA_optim_GLS_Level_F_R_err_ets[ikw,2,] = dum$rmse_female_R_err
    OWFANOVA_optim_GLS_Level_M_R_err_ets[ikw,2,] = dum$rmse_male_R_err

    # Prefecture + Sex

    OWFANOVA_optim_GLS_Level_T_state_err_ets[,ikw,2] = dum$rmse_state_err
    OWFANOVA_optim_GLS_Level_F_state_err_ets[,ikw,2] = dum$rmse_bottom_female_err
    OWFANOVA_optim_GLS_Level_M_state_err_ets[,ikw,2] = dum$rmse_bottom_male_err
    print(ikw); rm(ikw)
}

OWFANOVA_optim_GLS_all_level_err_mae_ets = cbind(OWFANOVA_optim_GLS_Level_T_err_ets[,1],
                                                 rowMeans(cbind(OWFANOVA_optim_GLS_Level_F_err_ets[,1], OWFANOVA_optim_GLS_Level_M_err_ets[,1])),
                                                 rowMeans(OWFANOVA_optim_GLS_Level_T_R_err_ets[,1,]),
                                                 rowMeans(cbind(rowMeans(OWFANOVA_optim_GLS_Level_F_R_err_ets[,1,]), rowMeans(OWFANOVA_optim_GLS_Level_M_R_err_ets[,1,]))),
                                                 colMeans(OWFANOVA_optim_GLS_Level_T_state_err_ets[,,1]),
                                                 colMeans(rbind(OWFANOVA_optim_GLS_Level_F_state_err_ets[,,1], OWFANOVA_optim_GLS_Level_M_state_err_ets[,,1])))

OWFANOVA_optim_GLS_all_level_err_rmse_ets = cbind(OWFANOVA_optim_GLS_Level_T_err_ets[,2],
                                                  rowMeans(cbind(OWFANOVA_optim_GLS_Level_F_err_ets[,2], OWFANOVA_optim_GLS_Level_M_err_ets[,2])),
                                                  rowMeans(OWFANOVA_optim_GLS_Level_T_R_err_ets[,2,]),
                                                  rowMeans(cbind(rowMeans(OWFANOVA_optim_GLS_Level_F_R_err_ets[,2,]), rowMeans(OWFANOVA_optim_GLS_Level_M_R_err_ets[,2,]))),
                                                  colMeans(OWFANOVA_optim_GLS_Level_T_state_err_ets[,,2]),
                                                  colMeans(rbind(OWFANOVA_optim_Level_F_state_err_ets[,,2], OWFANOVA_optim_Level_M_state_err_ets[,,2])))
colnames(OWFANOVA_optim_GLS_all_level_err_mae_ets) = colnames(OWFANOVA_optim_GLS_all_level_err_rmse_ets) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

#################
# summary output
#################

## MAFE

# Base

OWFANOVA_ind_all_level_err_mae_ets_summary = rbind(OWFANOVA_ind_all_level_err_mae_ets,
                                                   colMeans(OWFANOVA_ind_all_level_err_mae_ets),
                                                   apply(OWFANOVA_ind_all_level_err_mae_ets, 2, median))

# BU

OWFANOVA_BU_all_level_err_mae_ets_summary = rbind(OWFANOVA_BU_all_level_err_mae_ets,
                                                  colMeans(OWFANOVA_BU_all_level_err_mae_ets),
                                                  apply(OWFANOVA_BU_all_level_err_mae_ets, 2, median))

# OLS

OWFANOVA_optim_all_level_err_mae_ets_summary = rbind(OWFANOVA_optim_all_level_err_mae_ets,
                                                     colMeans(OWFANOVA_optim_all_level_err_mae_ets),
                                                     apply(OWFANOVA_optim_all_level_err_mae_ets, 2, median))

# GLS

OWFANOVA_optim_GLS_all_level_err_mae_ets_summary = rbind(OWFANOVA_optim_GLS_all_level_err_mae_ets,
                                                         colMeans(OWFANOVA_optim_GLS_all_level_err_mae_ets),
                                                         apply(OWFANOVA_optim_GLS_all_level_err_mae_ets, 2, median))

rownames(OWFANOVA_ind_all_level_err_mae_ets_summary) = rownames(OWFANOVA_BU_all_level_err_mae_ets_summary) =
rownames(OWFANOVA_optim_all_level_err_mae_ets_summary) = rownames(OWFANOVA_optim_GLS_all_level_err_mae_ets_summary) = c(1:10, "Mean", "Median")

## RMSFE

# Base

OWFANOVA_ind_all_level_err_rmse_ets_summary = rbind(OWFANOVA_ind_all_level_err_rmse_ets,
                                                    colMeans(OWFANOVA_ind_all_level_err_rmse_ets),
                                                    apply(OWFANOVA_ind_all_level_err_rmse_ets, 2, median))

# BU

OWFANOVA_BU_all_level_err_rmse_ets_summary = rbind(OWFANOVA_BU_all_level_err_rmse_ets,
                                                   colMeans(OWFANOVA_BU_all_level_err_rmse_ets),
                                                   apply(OWFANOVA_BU_all_level_err_rmse_ets, 2, median))

# OLS

OWFANOVA_optim_all_level_err_rmse_ets_summary = rbind(OWFANOVA_optim_all_level_err_rmse_ets,
                                                      colMeans(OWFANOVA_optim_all_level_err_rmse_ets),
                                                      apply(OWFANOVA_optim_all_level_err_rmse_ets, 2, median))

# GLS

OWFANOVA_optim_GLS_all_level_err_rmse_ets_summary = rbind(OWFANOVA_optim_GLS_all_level_err_rmse_ets,
                                                          colMeans(OWFANOVA_optim_GLS_all_level_err_rmse_ets),
                                                          apply(OWFANOVA_optim_GLS_all_level_err_rmse_ets, 2, median))

rownames(OWFANOVA_ind_all_level_err_rmse_ets_summary) = rownames(OWFANOVA_BU_all_level_err_rmse_ets_summary) =
rownames(OWFANOVA_optim_all_level_err_rmse_ets_summary) = rownames(OWFANOVA_optim_GLS_all_level_err_rmse_ets_summary) = c(1:10, "Mean", "Median")


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
fc_OWFANOVA_ets <- list(Independent = OWFANOVA_ind_all_level_err_mae_ets_summary,
                        `Bottom-up` = OWFANOVA_BU_all_level_err_mae_ets_summary,
                        OLS         = OWFANOVA_optim_all_level_err_mae_ets_summary,
                        GLS         = OWFANOVA_optim_GLS_all_level_err_mae_ets_summary)

fc_OWFANOVA_ets <- lapply(fc_OWFANOVA_ets, harmonise)

h    <- 1:10                                  # forecast horizons
fc_OWFANOVA_ets_h <- lapply(fc_OWFANOVA_ets, function(m) m[seq_along(h), , drop = FALSE])

## Summary table (mean over horizons), handy for the paper
summ_OWFANOVA_ets <- t(sapply(fc_OWFANOVA_ets, function(m) m["Mean", ]))
print(round(summ_OWFANOVA_ets, 5))

## ---- 2. Base graphics: one panel per level ---------------------------------
meth <- names(fc_OWFANOVA_ets)
cols <- c("black", "#D55E00", "#0072B2", "#009E73")
pchs <- c(16, 17, 15, 18)
ltys <- c(1, 2, 3, 4)

dir.create("figures", showWarnings = FALSE)
pdf(file.path("figures", "OWFANOVA_ets_forecast_accuracy_by_level.pdf"), width = 11, height = 7)
op <- par(mfrow = c(2, 3), mar = c(4, 4.4, 2.6, 1))
for(j in seq_along(levels_gs))
{
  yy <- sapply(fc_OWFANOVA_ets_h, function(m) m[, j])      # 10 x 4
  matplot(h, yy, type = "b", col = cols, pch = pchs, lty = ltys, lwd = 1.8,
          xlab = "Forecast horizon (h)", ylab = "MAE",
          main = levels_gs[j], ylim = range(yy), xaxt = "n")
  axis(1, at = h)
  if(j == 1)
    legend("topleft", legend = meth, col = cols, pch = pchs, lty = ltys, lwd = 1.8, bty = "n", cex = 0.9)
}
par(op)
dev.off()
