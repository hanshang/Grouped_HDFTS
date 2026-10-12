# ----------------------------------------------------------------------------
# 30_UFTS_BU_optim_arima_K6.R
#
# Reconcile the UFTS base forecasts (ARIMA scores, K = 6 components) bottom-
# up and by optimal combination (OLS and WLS), then tabulate forecast errors
# by level of the group structure.
#
# Sources: 25_UFTS_BU_optim_fun.R, 24_Japan_S_mat.R
# Writes:  figures/UFTS_K6_arima_forecast_accuracy_by_level.pdf
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

###################
# fmethod = "Base"
###################

# MAE

ind_all_level_err_mae_arima_K6 = cbind(Japan_T_fore_mae_arima_K6,
                                       rowMeans(cbind(Japan_F_fore_mae_arima_K6, Japan_M_fore_mae_arima_K6)),
                                       colMeans(res_region_T_mae_mat_arima_K6),
                                       colMeans(rbind(res_region_F_mae_mat_arima_K6, res_region_M_mae_mat_arima_K6)),
                                       colMeans(res_T_mae_mat_arima_K6),
                                       colMeans(rbind(res_F_mae_mat_arima_K6, res_M_mae_mat_arima_K6)))
colnames(ind_all_level_err_mae_arima_K6) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")
rownames(ind_all_level_err_mae_arima_K6) = 1:10

# RMSE

ind_all_level_err_rmse_arima_K6 = cbind(Japan_T_fore_rmse_arima_K6,
                                        rowMeans(cbind(Japan_F_fore_rmse_arima_K6, Japan_M_fore_rmse_arima_K6)),
                                        colMeans(res_region_T_rmse_mat_arima_K6),
                                        colMeans(rbind(res_region_F_rmse_mat_arima_K6, res_region_M_rmse_mat_arima_K6)),
                                        colMeans(res_T_rmse_mat_arima_K6),
                                        colMeans(rbind(res_F_rmse_mat_arima_K6, res_M_rmse_mat_arima_K6)))
colnames(ind_all_level_err_rmse_arima_K6) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")
rownames(ind_all_level_err_rmse_arima_K6) = 1:10

#################
# fmethod = "BU"
#################

BU_Level_T_err_arima_K6 = BU_Level_F_err_arima_K6 = BU_Level_M_err_arima_K6 = matrix(NA, 10, 2)
BU_Level_T_R_err_arima_K6 = BU_Level_F_R_err_arima_K6 = BU_Level_M_R_err_arima_K6 = array(NA, dim = c(10, 2, 8),
                                                                                    dimnames = list(1:10, c("MAE", "RMSE"), region))
BU_Level_T_state_err_arima_K6 = BU_Level_F_state_err_arima_K6 = BU_Level_M_state_err_arima_K6 = array(NA, dim = c(47, 10, 2),
                                                                                                dimnames = list(state, 1:10, c("MAE", "RMSE")))
for(ikw in 1:10)
{
    dum = BU_optim_err(ik = ikw, hier_method = "BU",
                       total_data_raw_rate_region = ind_region_forc_total_arima_K6,
                       female_data_raw_rate_region = ind_region_forc_female_arima_K6,
                       male_data_raw_rate_region = ind_region_forc_male_arima_K6,
                       female_data_raw_rate = ind_state_forc_female_arima_K6,
                       male_data_raw_rate = ind_state_forc_male_arima_K6, total_data_raw_rate = ind_state_forc_total_arima_K6)

    # Total + Sex

    BU_Level_T_err_arima_K6[ikw,1] = dum$mae_total_err
    BU_Level_F_err_arima_K6[ikw,1] = dum$mae_female_err
    BU_Level_M_err_arima_K6[ikw,1] = dum$mae_male_err

    BU_Level_T_err_arima_K6[ikw,2] = dum$rmse_total_err
    BU_Level_F_err_arima_K6[ikw,2] = dum$rmse_female_err
    BU_Level_M_err_arima_K6[ikw,2] = dum$rmse_male_err

    # Region + Sex

    BU_Level_T_R_err_arima_K6[ikw,1,] = dum$mae_total_R_err
    BU_Level_F_R_err_arima_K6[ikw,1,] = dum$mae_female_R_err
    BU_Level_M_R_err_arima_K6[ikw,1,] = dum$mae_male_R_err

    BU_Level_T_R_err_arima_K6[ikw,2,] = dum$rmse_total_R_err
    BU_Level_F_R_err_arima_K6[ikw,2,] = dum$rmse_female_R_err
    BU_Level_M_R_err_arima_K6[ikw,2,] = dum$rmse_male_R_err

    # Prefecture + Sex

    BU_Level_T_state_err_arima_K6[,ikw,1] = dum$mae_state_err
    BU_Level_F_state_err_arima_K6[,ikw,1] = dum$mae_bottom_female_err
    BU_Level_M_state_err_arima_K6[,ikw,1] = dum$mae_bottom_male_err

    BU_Level_T_state_err_arima_K6[,ikw,2] = dum$rmse_state_err
    BU_Level_F_state_err_arima_K6[,ikw,2] = dum$rmse_bottom_female_err
    BU_Level_M_state_err_arima_K6[,ikw,2] = dum$rmse_bottom_male_err
    print(ikw); rm(ikw); rm(dum)
}

BU_all_level_err_mae_arima_K6 = cbind(BU_Level_T_err_arima_K6[,1],
                                      rowMeans(cbind(BU_Level_F_err_arima_K6[,1], BU_Level_M_err_arima_K6[,1])),
                                      rowMeans(BU_Level_T_R_err_arima_K6[,1,]),
                                      rowMeans(cbind(rowMeans(BU_Level_F_R_err_arima_K6[,1,]), rowMeans(BU_Level_M_R_err_arima_K6[,1,]))),
                                      colMeans(BU_Level_T_state_err_arima_K6[,,1]),
                                      colMeans(rbind(BU_Level_F_state_err_arima_K6[,,1], BU_Level_M_state_err_arima_K6[,,1])))

BU_all_level_err_rmse_arima_K6 = cbind(BU_Level_T_err_arima_K6[,2],
                                       rowMeans(cbind(BU_Level_F_err_arima_K6[,2], BU_Level_M_err_arima_K6[,2])),
                                       rowMeans(BU_Level_T_R_err_arima_K6[,2,]),
                                       rowMeans(cbind(rowMeans(BU_Level_F_R_err_arima_K6[,2,]), rowMeans(BU_Level_M_R_err_arima_K6[,2,]))),
                                       colMeans(BU_Level_T_state_err_arima_K6[,,2]),
                                       colMeans(rbind(BU_Level_F_state_err_arima_K6[,,2], BU_Level_M_state_err_arima_K6[,,2])))
colnames(BU_all_level_err_mae_arima_K6) = colnames(BU_all_level_err_rmse_arima_K6) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

#######################
# fmethod = "comb_OLS"
#######################

optim_Level_T_err_arima_K6 = optim_Level_F_err_arima_K6 = optim_Level_M_err_arima_K6 = matrix(NA, 10, 2)
optim_Level_T_R_err_arima_K6 = optim_Level_F_R_err_arima_K6 = optim_Level_M_R_err_arima_K6 = array(NA, dim = c(10, 2, 8),
                                                                                             dimnames = list(1:10, c("MAE", "RMSE"), region))

optim_Level_T_state_err_arima_K6 = optim_Level_F_state_err_arima_K6 = optim_Level_M_state_err_arima_K6 = array(NA, dim = c(47, 10, 2),
                                                                                                         dimnames = list(state, 1:10, c("MAE", "RMSE")))
for(ikw in 1:10)
{
    dum = BU_optim_err(ik = ikw, hier_method = "comb_OLS",
                       total_data_raw_rate_region = ind_region_forc_total_arima_K6,
                       female_data_raw_rate_region = ind_region_forc_female_arima_K6,
                       male_data_raw_rate_region = ind_region_forc_male_arima_K6,
                       female_data_raw_rate = ind_state_forc_female_arima_K6,
                       male_data_raw_rate = ind_state_forc_male_arima_K6,
                       total_data_raw_rate = ind_state_forc_total_arima_K6)

    ######
    # MAE
    ######

    # Total + Sex

    optim_Level_T_err_arima_K6[ikw,1] = dum$mae_total_err
    optim_Level_F_err_arima_K6[ikw,1] = dum$mae_female_err
    optim_Level_M_err_arima_K6[ikw,1] = dum$mae_male_err

    # Region + Sex

    optim_Level_T_R_err_arima_K6[ikw,1,] = dum$mae_total_R_err
    optim_Level_F_R_err_arima_K6[ikw,1,] = dum$mae_female_R_err
    optim_Level_M_R_err_arima_K6[ikw,1,] = dum$mae_male_R_err

    # Prefecture + Sex

    optim_Level_T_state_err_arima_K6[,ikw,1]   = dum$mae_state_err
    optim_Level_F_state_err_arima_K6[,ikw,1] = dum$mae_bottom_female_err
    optim_Level_M_state_err_arima_K6[,ikw,1] = dum$mae_bottom_male_err

    #######
    # RMSE
    #######

    # Total + Sex

    optim_Level_T_err_arima_K6[ikw,2] = dum$rmse_total_err
    optim_Level_F_err_arima_K6[ikw,2] = dum$rmse_female_err
    optim_Level_M_err_arima_K6[ikw,2] = dum$rmse_male_err

    # Region + Sex

    optim_Level_T_R_err_arima_K6[ikw,2,] = dum$rmse_total_R_err
    optim_Level_F_R_err_arima_K6[ikw,2,] = dum$rmse_female_R_err
    optim_Level_M_R_err_arima_K6[ikw,2,] = dum$rmse_male_R_err

    # Prefecture + Sex

    optim_Level_T_state_err_arima_K6[,ikw,2] = dum$rmse_state_err
    optim_Level_F_state_err_arima_K6[,ikw,2] = dum$rmse_bottom_female_err
    optim_Level_M_state_err_arima_K6[,ikw,2] = dum$rmse_bottom_male_err
    print(ikw); rm(ikw); rm(dum)
}

optim_all_level_err_mae_arima_K6 = cbind(optim_Level_T_err_arima_K6[,1],
                                         rowMeans(cbind(optim_Level_F_err_arima_K6[,1], optim_Level_M_err_arima_K6[,1])),
                                         rowMeans(optim_Level_T_R_err_arima_K6[,1,]),
                                         rowMeans(cbind(rowMeans(optim_Level_F_R_err_arima_K6[,1,]), rowMeans(optim_Level_M_R_err_arima_K6[,1,]))),
                                         colMeans(optim_Level_T_state_err_arima_K6[,,1]),
                                         colMeans(rbind(optim_Level_F_state_err_arima_K6[,,1], optim_Level_M_state_err_arima_K6[,,1])))

optim_all_level_err_rmse_arima_K6 = cbind(optim_Level_T_err_arima_K6[,2],
                                          rowMeans(cbind(optim_Level_F_err_arima_K6[,2], optim_Level_M_err_arima_K6[,2])),
                                          rowMeans(optim_Level_T_R_err_arima_K6[,2,]),
                                          rowMeans(cbind(rowMeans(optim_Level_F_R_err_arima_K6[,2,]), rowMeans(optim_Level_M_R_err_arima_K6[,2,]))),
                                          colMeans(optim_Level_T_state_err_arima_K6[,,2]),
                                          colMeans(rbind(optim_Level_F_state_err_arima_K6[,,2], optim_Level_M_state_err_arima_K6[,,2])))
colnames(optim_all_level_err_mae_arima_K6) = colnames(optim_all_level_err_rmse_arima_K6) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

#######################
# fmethod = "comb_GLS"
#######################

optim_GLS_Level_T_err_arima_K6 = optim_GLS_Level_F_err_arima_K6 = optim_GLS_Level_M_err_arima_K6 = matrix(NA, 10, 2)
optim_GLS_Level_T_R_err_arima_K6 = optim_GLS_Level_F_R_err_arima_K6 = optim_GLS_Level_M_R_err_arima_K6 = array(NA, dim = c(10, 2, 8),
                                                                                                         dimnames = list(1:10, c("MAE", "RMSE"), region))

optim_GLS_Level_T_state_err_arima_K6 = optim_GLS_Level_F_state_err_arima_K6 = optim_GLS_Level_M_state_err_arima_K6 = array(NA, dim = c(47,10,2),
                                                                                                                     dimnames = list(state, 1:10, c("MAE", "RMSE")))
for(ikw in 1:10)
{
    dum = BU_optim_err(ik = ikw, hier_method = "comb_GLS",
                       total_data_raw_rate_region = ind_region_forc_total_arima_K6,
                       female_data_raw_rate_region = ind_region_forc_female_arima_K6,
                       male_data_raw_rate_region = ind_region_forc_male_arima_K6,
                       female_data_raw_rate = ind_state_forc_female_arima_K6,
                       male_data_raw_rate = ind_state_forc_male_arima_K6,
                       total_data_raw_rate = ind_state_forc_total_arima_K6)

    ######
    # MAE
    ######

    # Total + Sex

    optim_GLS_Level_T_err_arima_K6[ikw,1] = dum$mae_total_err
    optim_GLS_Level_F_err_arima_K6[ikw,1] = dum$mae_female_err
    optim_GLS_Level_M_err_arima_K6[ikw,1] = dum$mae_male_err

    # Region + Sex

    optim_GLS_Level_T_R_err_arima_K6[ikw,1,] = dum$mae_total_R_err
    optim_GLS_Level_F_R_err_arima_K6[ikw,1,] = dum$mae_female_R_err
    optim_GLS_Level_M_R_err_arima_K6[ikw,1,] = dum$mae_male_R_err

    # Prefecture + Sex

    optim_GLS_Level_T_state_err_arima_K6[,ikw,1] = dum$mae_state_err
    optim_GLS_Level_F_state_err_arima_K6[,ikw,1] = dum$mae_bottom_female_err
    optim_GLS_Level_M_state_err_arima_K6[,ikw,1] = dum$mae_bottom_male_err

    #######
    # RMSE
    #######

    # Total + Sex

    optim_GLS_Level_T_err_arima_K6[ikw,2] = dum$rmse_total_err
    optim_GLS_Level_F_err_arima_K6[ikw,2] = dum$rmse_female_err
    optim_GLS_Level_M_err_arima_K6[ikw,2] = dum$rmse_male_err

    # Region + Sex

    optim_GLS_Level_T_R_err_arima_K6[ikw,2,] = dum$rmse_total_R_err
    optim_GLS_Level_F_R_err_arima_K6[ikw,2,] = dum$rmse_female_R_err
    optim_GLS_Level_M_R_err_arima_K6[ikw,2,] = dum$rmse_male_R_err

    # Prefecture + Sex

    optim_GLS_Level_T_state_err_arima_K6[,ikw,2] = dum$rmse_state_err
    optim_GLS_Level_F_state_err_arima_K6[,ikw,2] = dum$rmse_bottom_female_err
    optim_GLS_Level_M_state_err_arima_K6[,ikw,2] = dum$rmse_bottom_male_err
    print(ikw); rm(ikw)
}

optim_GLS_all_level_err_mae_arima_K6 = cbind(optim_GLS_Level_T_err_arima_K6[,1],
                                             rowMeans(cbind(optim_GLS_Level_F_err_arima_K6[,1], optim_GLS_Level_M_err_arima_K6[,1])),
                                             rowMeans(optim_GLS_Level_T_R_err_arima_K6[,1,]),
                                             rowMeans(cbind(rowMeans(optim_GLS_Level_F_R_err_arima_K6[,1,]), rowMeans(optim_GLS_Level_M_R_err_arima_K6[,1,]))),
                                             colMeans(optim_GLS_Level_T_state_err_arima_K6[,,1]),
                                             colMeans(rbind(optim_GLS_Level_F_state_err_arima_K6[,,1], optim_GLS_Level_M_state_err_arima_K6[,,1])))

optim_GLS_all_level_err_rmse_arima_K6 = cbind(optim_GLS_Level_T_err_arima_K6[,2],
                                              rowMeans(cbind(optim_GLS_Level_F_err_arima_K6[,2], optim_GLS_Level_M_err_arima_K6[,2])),
                                              rowMeans(optim_GLS_Level_T_R_err_arima_K6[,2,]),
                                              rowMeans(cbind(rowMeans(optim_GLS_Level_F_R_err_arima_K6[,2,]), rowMeans(optim_GLS_Level_M_R_err_arima_K6[,2,]))),
                                              colMeans(optim_GLS_Level_T_state_err_arima_K6[,,2]),
                                              colMeans(rbind(optim_Level_F_state_err_arima_K6[,,2], optim_Level_M_state_err_arima_K6[,,2])))
colnames(optim_GLS_all_level_err_mae_arima_K6) = colnames(optim_GLS_all_level_err_rmse_arima_K6) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

#################
# summary output
#################

# Base

ind_all_level_err_mae_arima_K6_summary = rbind(ind_all_level_err_mae_arima_K6,
                                               colMeans(ind_all_level_err_mae_arima_K6),
                                               apply(ind_all_level_err_mae_arima_K6, 2, median))

# BU

BU_all_level_err_mae_arima_K6_summary = rbind(BU_all_level_err_mae_arima_K6,
                                              colMeans(BU_all_level_err_mae_arima_K6),
                                              apply(BU_all_level_err_mae_arima_K6, 2, median))

# OLS

optim_all_level_err_mae_arima_K6_summary = rbind(optim_all_level_err_mae_arima_K6,
                                                 colMeans(optim_all_level_err_mae_arima_K6),
                                                 apply(optim_all_level_err_mae_arima_K6, 2, median))

# GLS

optim_GLS_all_level_err_mae_arima_K6_summary = rbind(optim_GLS_all_level_err_mae_arima_K6,
                                                     colMeans(optim_GLS_all_level_err_mae_arima_K6),
                                                     apply(optim_GLS_all_level_err_mae_arima_K6, 2, median))

rownames(ind_all_level_err_mae_arima_K6_summary) = rownames(BU_all_level_err_mae_arima_K6_summary) =
rownames(optim_all_level_err_mae_arima_K6_summary) = rownames(optim_GLS_all_level_err_mae_arima_K6_summary) = c(1:10, "Mean", "Median")

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
fc_UFTS_arima_K6 <- list(Independent = ind_all_level_err_mae_arima_K6_summary,
                         `Bottom-up` = BU_all_level_err_mae_arima_K6_summary,
                         OLS         = optim_all_level_err_mae_arima_K6_summary,
                         GLS         = optim_GLS_all_level_err_mae_arima_K6_summary)

fc_UFTS_arima_K6 <- lapply(fc_UFTS_arima_K6, harmonise)

h    <- 1:10                                  # forecast horizons
fc_UFTS_arima_K6_h <- lapply(fc_UFTS_arima_K6, function(m) m[seq_along(h), , drop = FALSE])

## Summary table (mean over horizons), handy for the paper
summ_UFTS_arima_K6 <- t(sapply(fc_UFTS_arima_K6, function(m) m["Mean", ]))
print(round(summ_UFTS_arima_K6, 5))

## ---- 2. Base graphics: one panel per level ---------------------------------
meth <- names(fc_UFTS_arima_K6)
cols <- c("black", "#D55E00", "#0072B2", "#009E73")
pchs <- c(16, 17, 15, 18)
ltys <- c(1, 2, 3, 4)

dir.create("figures", showWarnings = FALSE)
pdf(file.path("figures", "UFTS_K6_arima_forecast_accuracy_by_level.pdf"), width = 11, height = 7)
op <- par(mfrow = c(2, 3), mar = c(4, 4.4, 2.6, 1))
for(j in seq_along(levels_gs))
{
    yy <- sapply(fc_UFTS_arima_K6_h, function(m) m[, j])      # 10 x 4
    matplot(h, yy, type = "b", col = cols, pch = pchs, lty = ltys, lwd = 1.8,
            xlab = "Forecast horizon (h)", ylab = "MAE",
            main = levels_gs[j], ylim = range(yy), xaxt = "n")
    axis(1, at = h)
    if(j == 1)
      legend("topleft", legend = meth, col = cols, pch = pchs, lty = ltys, lwd = 1.8, bty = "n", cex = 0.9)
}
par(op)
dev.off()
