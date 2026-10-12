# ----------------------------------------------------------------------------
# 22_summary_results_arima.R
#
# Collect the ARIMA base-forecast errors into the appendix tables.
#
# Replication code for Shang, H. L., 'High-dimensional functional time
# series forecasting with a group structure'.
#
# Run with the working directory set to the code_submission/ folder.
# Scripts are numbered 01-51 in the order in which they must be run.
# See README.md for a one-line description of each.
# ----------------------------------------------------------------------------

############################
# bottom level (prefecture)
############################

## arima

bottom_level_mae_F_array_arima = bottom_level_rmse_F_array_arima =
bottom_level_mae_M_array_arima = bottom_level_rmse_M_array_arima =
bottom_level_mae_T_array_arima = bottom_level_rmse_T_array_arima = array(NA, dim = c(47, 10, 7),
                                                             dimnames = list(state, 1:10, c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "FANOVA+FFM",
                                                                                            "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)")))

# female

bottom_level_mae_F_array_arima[,,1] = res_F_mae_mat_arima_EVR
bottom_level_mae_F_array_arima[,,2] = MFTS_model_fun_EVR_arima_mae_F
bottom_level_mae_F_array_arima[,,3] = MLFTS_model_fun_EVR_arima_mae_F
bottom_level_mae_F_array_arima[,,4] = OWFANOVA_FFM_arima_mae_female
bottom_level_mae_F_array_arima[,,5] = res_F_mae_mat_arima_K6
bottom_level_mae_F_array_arima[,,6] = MFTS_model_fun_K6_arima_mae_F
bottom_level_mae_F_array_arima[,,7] = MLFTS_model_fun_K6_arima_mae_F


bottom_level_rmse_F_array_arima[,,1] = res_F_rmse_mat_arima_EVR
bottom_level_rmse_F_array_arima[,,2] = MFTS_model_fun_EVR_arima_rmse_F
bottom_level_rmse_F_array_arima[,,3] = MLFTS_model_fun_EVR_arima_rmse_F
bottom_level_rmse_F_array_arima[,,4] = OWFANOVA_FFM_arima_rmse_female
bottom_level_rmse_F_array_arima[,,5] = res_F_rmse_mat_arima_K6
bottom_level_rmse_F_array_arima[,,6] = MFTS_model_fun_K6_arima_rmse_F
bottom_level_rmse_F_array_arima[,,7] = MLFTS_model_fun_K6_arima_rmse_F


bottom_level_mae_F_array_arima_prefecture = rbind(apply(bottom_level_mae_F_array_arima, c(2, 3), mean),
                                                  apply(apply(bottom_level_mae_F_array_arima, c(2, 3), mean), 2, mean),
                                                  apply(apply(bottom_level_mae_F_array_arima, c(2, 3), mean), 2, median))

bottom_level_rmse_F_array_arima_prefecture = rbind(apply(bottom_level_rmse_F_array_arima, c(2, 3), mean),
                                                   apply(apply(bottom_level_rmse_F_array_arima, c(2, 3), mean), 2, mean),
                                                   apply(apply(bottom_level_rmse_F_array_arima, c(2, 3), mean), 2, median))

# male

bottom_level_mae_M_array_arima[,,1] = res_M_mae_mat_arima_EVR
bottom_level_mae_M_array_arima[,,2] = MFTS_model_fun_EVR_arima_mae_M
bottom_level_mae_M_array_arima[,,3] = MLFTS_model_fun_EVR_arima_mae_M
bottom_level_mae_M_array_arima[,,4] = OWFANOVA_FFM_arima_mae_male
bottom_level_mae_M_array_arima[,,5] = res_M_mae_mat_arima_K6
bottom_level_mae_M_array_arima[,,6] = MFTS_model_fun_K6_arima_mae_M
bottom_level_mae_M_array_arima[,,7] = MLFTS_model_fun_K6_arima_mae_M


bottom_level_rmse_M_array_arima[,,1] = res_M_rmse_mat_arima_EVR
bottom_level_rmse_M_array_arima[,,2] = MFTS_model_fun_EVR_arima_rmse_M
bottom_level_rmse_M_array_arima[,,3] = MLFTS_model_fun_EVR_arima_rmse_M
bottom_level_rmse_M_array_arima[,,4] = OWFANOVA_FFM_arima_rmse_male
bottom_level_rmse_M_array_arima[,,5] = res_M_rmse_mat_arima_K6
bottom_level_rmse_M_array_arima[,,6] = MFTS_model_fun_K6_arima_rmse_M
bottom_level_rmse_M_array_arima[,,7] = MLFTS_model_fun_K6_arima_rmse_M

bottom_level_mae_M_array_arima_prefecture = rbind(apply(bottom_level_mae_M_array_arima, c(2, 3), mean),
                                                  apply(apply(bottom_level_mae_M_array_arima, c(2, 3), mean), 2, mean),
                                                  apply(apply(bottom_level_mae_M_array_arima, c(2, 3), mean), 2, median))

bottom_level_rmse_M_array_arima_prefecture = rbind(apply(bottom_level_rmse_M_array_arima, c(2, 3), mean),
                                                   apply(apply(bottom_level_rmse_M_array_arima, c(2, 3), mean), 2, mean),
                                                   apply(apply(bottom_level_rmse_M_array_arima, c(2, 3), mean), 2, median))

# total

bottom_level_mae_T_array_arima[,,1] = res_T_mae_mat_arima_EVR
bottom_level_mae_T_array_arima[,,2] = MFTS_model_fun_EVR_arima_mae_T
bottom_level_mae_T_array_arima[,,3] = MLFTS_model_fun_EVR_arima_mae_T
bottom_level_mae_T_array_arima[,,4] = OWFANOVA_FFM_arima_mae_total
bottom_level_mae_T_array_arima[,,5] = res_T_mae_mat_arima_K6
bottom_level_mae_T_array_arima[,,6] = MFTS_model_fun_K6_arima_mae_T
bottom_level_mae_T_array_arima[,,7] = MLFTS_model_fun_K6_arima_mae_T

bottom_level_rmse_T_array_arima[,,1] = res_M_rmse_mat_arima_EVR
bottom_level_rmse_T_array_arima[,,2] = MFTS_model_fun_EVR_arima_rmse_M
bottom_level_rmse_T_array_arima[,,3] = MLFTS_model_fun_EVR_arima_rmse_M
bottom_level_rmse_T_array_arima[,,4] = OWFANOVA_FFM_arima_rmse_total
bottom_level_rmse_T_array_arima[,,5] = res_M_rmse_mat_arima_K6
bottom_level_rmse_T_array_arima[,,6] = MFTS_model_fun_K6_arima_rmse_M
bottom_level_rmse_T_array_arima[,,7] = MLFTS_model_fun_K6_arima_rmse_M

bottom_level_mae_T_array_arima_prefecture = rbind(apply(bottom_level_mae_T_array_arima, c(2, 3), mean),
                                                  apply(apply(bottom_level_mae_T_array_arima, c(2, 3), mean), 2, mean),
                                                  apply(apply(bottom_level_mae_T_array_arima, c(2, 3), mean), 2, median))

bottom_level_rmse_T_array_arima_prefecture = rbind(apply(bottom_level_rmse_T_array_arima, c(2, 3), mean),
                                                   apply(apply(bottom_level_rmse_T_array_arima, c(2, 3), mean), 2, mean),
                                                   apply(apply(bottom_level_rmse_T_array_arima, c(2, 3), mean), 2, median))

rownames(bottom_level_mae_F_array_arima_prefecture) = rownames(bottom_level_rmse_F_array_arima_prefecture) =
rownames(bottom_level_mae_M_array_arima_prefecture) = rownames(bottom_level_rmse_M_array_arima_prefecture) =
rownames(bottom_level_mae_T_array_arima_prefecture) = rownames(bottom_level_rmse_T_array_arima_prefecture) = c(1:10, "Mean", "Median")

# tables

xtable(cbind(rbind(bottom_level_mae_F_array_arima_prefecture[c(1,5,10:12),],
                   bottom_level_mae_M_array_arima_prefecture[c(1,5,10:12),],
                   bottom_level_mae_T_array_arima_prefecture[c(1,5,10:12),]),

              rbind(bottom_level_rmse_F_array_arima_prefecture[c(1,5,10:12),],
                     bottom_level_rmse_M_array_arima_prefecture[c(1,5,10:12),],
                     bottom_level_rmse_T_array_arima_prefecture[c(1,5,10:12),])) * 100, digits = 3)

#########################
## middle level (region)
#########################

middle_level_mae_F_array_arima = middle_level_rmse_F_array_arima =
middle_level_mae_M_array_arima = middle_level_rmse_M_array_arima =
middle_level_mae_T_array_arima = middle_level_rmse_T_array_arima = array(NA, dim = c(8, 10, 7),
                      dimnames = list(region, 1:10, c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "FANOVA+FFM",
                                                      "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)")))

# female

middle_level_mae_F_array_arima[,,1] = res_region_F_mae_mat_arima_EVR
middle_level_mae_F_array_arima[,,2] = MFTS_model_region_fun_EVR_arima_mae_F
middle_level_mae_F_array_arima[,,3] = MLFTS_model_region_fun_EVR_arima_mae_F
middle_level_mae_F_array_arima[,,4] = OWFANOVA_FFM_region_arima_mae_female
middle_level_mae_F_array_arima[,,5] = res_region_F_mae_mat_arima_K6
middle_level_mae_F_array_arima[,,6] = MFTS_model_region_fun_K6_arima_mae_F
middle_level_mae_F_array_arima[,,7] = MLFTS_model_region_fun_K6_arima_mae_F

middle_level_rmse_F_array_arima[,,1] = res_region_F_rmse_mat_arima_EVR
middle_level_rmse_F_array_arima[,,2] = MFTS_model_region_fun_EVR_arima_rmse_F
middle_level_rmse_F_array_arima[,,3] = MLFTS_model_region_fun_EVR_arima_rmse_F
middle_level_rmse_F_array_arima[,,4] = OWFANOVA_FFM_region_arima_rmse_female
middle_level_rmse_F_array_arima[,,5] = res_region_F_rmse_mat_arima_K6
middle_level_rmse_F_array_arima[,,6] = MFTS_model_region_fun_K6_arima_rmse_F
middle_level_rmse_F_array_arima[,,7] = MLFTS_model_region_fun_K6_arima_rmse_F

middle_level_mae_F_array_arima_region = rbind(apply(middle_level_mae_F_array_arima, c(2, 3), mean),
                                              apply(apply(middle_level_mae_F_array_arima, c(2, 3), mean), 2, mean),
                                              apply(apply(middle_level_mae_F_array_arima, c(2, 3), mean), 2, median))

middle_level_rmse_F_array_arima_region = rbind(apply(middle_level_rmse_F_array_arima, c(2, 3), mean),
                                               apply(apply(middle_level_rmse_F_array_arima, c(2, 3), mean), 2, mean),
                                               apply(apply(middle_level_rmse_F_array_arima, c(2, 3), mean), 2, median))

# male

middle_level_mae_M_array_arima[,,1] = res_region_M_mae_mat_arima_EVR
middle_level_mae_M_array_arima[,,2] = MFTS_model_region_fun_EVR_arima_mae_M
middle_level_mae_M_array_arima[,,3] = MLFTS_model_region_fun_EVR_arima_mae_M
middle_level_mae_M_array_arima[,,4] = OWFANOVA_FFM_region_arima_mae_male
middle_level_mae_M_array_arima[,,5] = res_region_M_mae_mat_arima_K6
middle_level_mae_M_array_arima[,,6] = MFTS_model_region_fun_K6_arima_mae_M
middle_level_mae_M_array_arima[,,7] = MLFTS_model_region_fun_K6_arima_mae_M


middle_level_rmse_M_array_arima[,,1] = res_region_M_rmse_mat_arima_EVR
middle_level_rmse_M_array_arima[,,2] = MFTS_model_region_fun_EVR_arima_rmse_M
middle_level_rmse_M_array_arima[,,3] = MLFTS_model_region_fun_EVR_arima_rmse_M
middle_level_rmse_M_array_arima[,,4] = OWFANOVA_FFM_region_arima_rmse_male
middle_level_rmse_M_array_arima[,,5] = res_region_M_rmse_mat_arima_K6
middle_level_rmse_M_array_arima[,,6] = MFTS_model_region_fun_K6_arima_rmse_M
middle_level_rmse_M_array_arima[,,7] = MLFTS_model_region_fun_K6_arima_rmse_M

middle_level_mae_M_array_arima_region = rbind(apply(middle_level_mae_M_array_arima, c(2, 3), mean),
                                              apply(apply(middle_level_mae_M_array_arima, c(2, 3), mean), 2, mean),
                                              apply(apply(middle_level_mae_M_array_arima, c(2, 3), mean), 2, median))

middle_level_rmse_M_array_arima_region = rbind(apply(middle_level_rmse_M_array_arima, c(2, 3), mean),
                                               apply(apply(middle_level_rmse_M_array_arima, c(2, 3), mean), 2, mean),
                                               apply(apply(middle_level_rmse_M_array_arima, c(2, 3), mean), 2, median))

# total

middle_level_mae_T_array_arima[,,1] = res_region_T_mae_mat_arima_EVR
middle_level_mae_T_array_arima[,,2] = MFTS_model_region_fun_EVR_arima_mae_T
middle_level_mae_T_array_arima[,,3] = MLFTS_model_region_fun_EVR_arima_mae_T
middle_level_mae_T_array_arima[,,4] = OWFANOVA_FFM_region_arima_mae_total
middle_level_mae_T_array_arima[,,5] = res_region_T_mae_mat_arima_K6
middle_level_mae_T_array_arima[,,6] = MFTS_model_region_fun_K6_arima_mae_T
middle_level_mae_T_array_arima[,,7] = MLFTS_model_region_fun_K6_arima_mae_T

middle_level_rmse_T_array_arima[,,1] = res_region_T_rmse_mat_arima_EVR
middle_level_rmse_T_array_arima[,,2] = MFTS_model_region_fun_EVR_arima_rmse_T
middle_level_rmse_T_array_arima[,,3] = MLFTS_model_region_fun_EVR_arima_rmse_T
middle_level_rmse_T_array_arima[,,4] = OWFANOVA_FFM_region_arima_rmse_total
middle_level_rmse_T_array_arima[,,5] = res_region_T_rmse_mat_arima_K6
middle_level_rmse_T_array_arima[,,6] = MFTS_model_region_fun_K6_arima_rmse_T
middle_level_rmse_T_array_arima[,,7] = MLFTS_model_region_fun_K6_arima_rmse_T

middle_level_mae_T_array_arima_region = rbind(apply(middle_level_mae_T_array_arima, c(2, 3), mean),
                                              apply(apply(middle_level_mae_T_array_arima, c(2, 3), mean), 2, mean),
                                              apply(apply(middle_level_mae_T_array_arima, c(2, 3), mean), 2, median))

middle_level_rmse_T_array_arima_region = rbind(apply(middle_level_rmse_T_array_arima, c(2, 3), mean),
                                               apply(apply(middle_level_rmse_T_array_arima, c(2, 3), mean), 2, mean),
                                               apply(apply(middle_level_rmse_T_array_arima, c(2, 3), mean), 2, median))

rownames(middle_level_mae_F_array_arima_region) = rownames(middle_level_rmse_F_array_arima_region) =
rownames(middle_level_mae_M_array_arima_region) = rownames(middle_level_rmse_M_array_arima_region) =
rownames(middle_level_mae_T_array_arima_region) = rownames(middle_level_rmse_T_array_arima_region) = c(1:10, "Mean", "Median")

# tables

xtable(cbind(rbind(middle_level_mae_F_array_arima_region[c(1,5,10:12),],
                   middle_level_mae_M_array_arima_region[c(1,5,10:12),],
                   middle_level_mae_T_array_arima_region[c(1,5,10:12),]),

             rbind(middle_level_rmse_F_array_arima_region[c(1,5,10:12),],
                   middle_level_rmse_M_array_arima_region[c(1,5,10:12),],
                   middle_level_rmse_T_array_arima_region[c(1,5,10:12),])) * 100, digits = 3)
