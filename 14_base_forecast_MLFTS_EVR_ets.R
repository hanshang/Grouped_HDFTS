# ----------------------------------------------------------------------------
# 14_base_forecast_MLFTS_EVR_ets.R
#
# MLFTS base forecasts: ETS scores, components chosen by the EVR criterion.
#
# Replication code for Shang, H. L., 'High-dimensional functional time
# series forecasting with a group structure'.
#
# Run with the working directory set to the code_submission/ folder.
# Scripts are numbered 01-51 in the order in which they must be run.
# See README.md for a one-line description of each.
# ----------------------------------------------------------------------------

###########################
# subnational (prefecture)
###########################

# EVR

MLFTS_model_fun_EVR_ets_mae_F = MLFTS_model_fun_EVR_ets_rmse_F =
MLFTS_model_fun_EVR_ets_mae_M = MLFTS_model_fun_EVR_ets_rmse_M =
MLFTS_model_fun_EVR_ets_mae_T = MLFTS_model_fun_EVR_ets_rmse_T = matrix(NA, n_state, 10)
for(iwk in 1:10)
{
    # female

    dum = MLFTS_model_fun_err(data_var = state_smooth_F_array, test_data = state_F_array, horizon = iwk,
                              ncomp_selection_method = "EVR", fmethod = "ets")
    MLFTS_model_fun_EVR_ets_mae_F[,iwk] = dum$mae_err
    MLFTS_model_fun_EVR_ets_rmse_F[,iwk] = dum$rmse_err
    rm(dum)

    # male

    dum = MLFTS_model_fun_err(data_var = state_smooth_M_array, test_data = state_M_array, horizon = iwk,
                              ncomp_selection_method = "EVR", fmethod = "ets")
    MLFTS_model_fun_EVR_ets_mae_M[,iwk] = dum$mae_err
    MLFTS_model_fun_EVR_ets_rmse_M[,iwk] = dum$rmse_err
    rm(dum)

    # total

    dum = MLFTS_model_fun_err(data_var = state_smooth_T_array, test_data = state_T_array, horizon = iwk,
                              ncomp_selection_method = "EVR", fmethod = "ets")
    MLFTS_model_fun_EVR_ets_mae_T[,iwk] = dum$mae_err
    MLFTS_model_fun_EVR_ets_rmse_T[,iwk] = dum$rmse_err
    print(iwk); rm(iwk); rm(dum)
}
rownames(MLFTS_model_fun_EVR_ets_mae_F) = rownames(MLFTS_model_fun_EVR_ets_rmse_F) =
rownames(MLFTS_model_fun_EVR_ets_mae_M) = rownames(MLFTS_model_fun_EVR_ets_rmse_M) =
rownames(MLFTS_model_fun_EVR_ets_mae_T) = rownames(MLFTS_model_fun_EVR_ets_rmse_T) = state

colnames(MLFTS_model_fun_EVR_ets_mae_F) = colnames(MLFTS_model_fun_EVR_ets_rmse_F) =
colnames(MLFTS_model_fun_EVR_ets_mae_M) = colnames(MLFTS_model_fun_EVR_ets_rmse_M) =
colnames(MLFTS_model_fun_EVR_ets_mae_T) = colnames(MLFTS_model_fun_EVR_ets_rmse_T) = 1:10

MLFTS_model_fun_EVR_ets_overall = cbind(colMeans(MLFTS_model_fun_EVR_ets_mae_F),
                                        colMeans(MLFTS_model_fun_EVR_ets_rmse_F),

                                        colMeans(MLFTS_model_fun_EVR_ets_mae_M),
                                        colMeans(MLFTS_model_fun_EVR_ets_rmse_M),

                                        colMeans(MLFTS_model_fun_EVR_ets_mae_T),
                                        colMeans(MLFTS_model_fun_EVR_ets_rmse_T))
colnames(MLFTS_model_fun_EVR_ets_overall) = c("EVR (ets, F, mae)", "EVR (ets, F, rmse)",
                                              "EVR (ets, M, mae)", "EVR (ets, M, rmse)",
                                              "EVR (ets, T, mae)", "EVR (ets, T, rmse)")

#######################
# subnational (region)
#######################

# EVR

MLFTS_model_region_fun_EVR_ets_mae_F = MLFTS_model_region_fun_EVR_ets_rmse_F =
MLFTS_model_region_fun_EVR_ets_mae_M = MLFTS_model_region_fun_EVR_ets_rmse_M =
MLFTS_model_region_fun_EVR_ets_mae_T = MLFTS_model_region_fun_EVR_ets_rmse_T = matrix(NA, n_region, 10)
for(iwk in 1:10)
{
    # female

    dum = MLFTS_model_fun_err(data_var = region_smooth_F_array, test_data = region_F_array, horizon = iwk,
                              ncomp_selection_method = "EVR", fmethod = "ets")
    MLFTS_model_region_fun_EVR_ets_mae_F[,iwk] = dum$mae_err
    MLFTS_model_region_fun_EVR_ets_rmse_F[,iwk] = dum$rmse_err
    rm(dum)

    # male

    dum = MLFTS_model_fun_err(data_var = region_smooth_M_array, test_data = region_M_array, horizon = iwk,
                              ncomp_selection_method = "EVR", fmethod = "ets")
    MLFTS_model_region_fun_EVR_ets_mae_M[,iwk] = dum$mae_err
    MLFTS_model_region_fun_EVR_ets_rmse_M[,iwk] = dum$rmse_err
    rm(dum)

    # total

    dum = MLFTS_model_fun_err(data_var = region_smooth_T_array, test_data = region_T_array, horizon = iwk,
                              ncomp_selection_method = "EVR", fmethod = "ets")
    MLFTS_model_region_fun_EVR_ets_mae_T[,iwk] = dum$mae_err
    MLFTS_model_region_fun_EVR_ets_rmse_T[,iwk] = dum$rmse_err
    print(iwk); rm(iwk); rm(dum)
}
rownames(MLFTS_model_region_fun_EVR_ets_mae_F) = rownames(MLFTS_model_region_fun_EVR_ets_rmse_F) =
rownames(MLFTS_model_region_fun_EVR_ets_mae_M) = rownames(MLFTS_model_region_fun_EVR_ets_rmse_M) =
rownames(MLFTS_model_region_fun_EVR_ets_mae_T) = rownames(MLFTS_model_region_fun_EVR_ets_rmse_T) = region

colnames(MLFTS_model_region_fun_EVR_ets_mae_F) = colnames(MLFTS_model_region_fun_EVR_ets_rmse_F) =
colnames(MLFTS_model_region_fun_EVR_ets_mae_M) = colnames(MLFTS_model_region_fun_EVR_ets_rmse_M) =
colnames(MLFTS_model_region_fun_EVR_ets_mae_T) = colnames(MLFTS_model_region_fun_EVR_ets_rmse_T) = 1:10
