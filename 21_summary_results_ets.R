# ----------------------------------------------------------------------------
# 21_summary_results_ets.R
#
# Collect the ETS base-forecast errors into the tables reported in the
# paper.
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

## ets

bottom_level_mae_F_array = bottom_level_rmse_F_array =
bottom_level_mae_M_array = bottom_level_rmse_M_array =
bottom_level_mae_T_array = bottom_level_rmse_T_array = array(NA, dim = c(47, 10, 7),
          dimnames = list(state, 1:10, c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "FANOVA+FFM",
                                         "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)")))

# female

bottom_level_mae_F_array[,,1] = res_F_mae_mat_ets_EVR
bottom_level_mae_F_array[,,2] = MFTS_model_fun_EVR_ets_mae_F
bottom_level_mae_F_array[,,3] = MLFTS_model_fun_EVR_ets_mae_F
bottom_level_mae_F_array[,,4] = OWFANOVA_FFM_ets_mae_female
bottom_level_mae_F_array[,,5] = res_F_mae_mat_ets_K6
bottom_level_mae_F_array[,,6] = MFTS_model_fun_K6_ets_mae_F
bottom_level_mae_F_array[,,7] = MLFTS_model_fun_K6_ets_mae_F


bottom_level_rmse_F_array[,,1] = res_F_rmse_mat_ets_EVR
bottom_level_rmse_F_array[,,2] = MFTS_model_fun_EVR_ets_rmse_F
bottom_level_rmse_F_array[,,3] = MLFTS_model_fun_EVR_ets_rmse_F
bottom_level_rmse_F_array[,,4] = OWFANOVA_FFM_ets_rmse_female
bottom_level_rmse_F_array[,,5] = res_F_rmse_mat_ets_K6
bottom_level_rmse_F_array[,,6] = MFTS_model_fun_K6_ets_rmse_F
bottom_level_rmse_F_array[,,7] = MLFTS_model_fun_K6_ets_rmse_F


bottom_level_mae_F_array_prefecture = rbind(apply(bottom_level_mae_F_array, c(2, 3), mean),
                                            apply(apply(bottom_level_mae_F_array, c(2, 3), mean), 2, mean),
                                            apply(apply(bottom_level_mae_F_array, c(2, 3), mean), 2, median))

bottom_level_rmse_F_array_prefecture = rbind(apply(bottom_level_rmse_F_array, c(2, 3), mean),
                                             apply(apply(bottom_level_rmse_F_array, c(2, 3), mean), 2, mean),
                                             apply(apply(bottom_level_rmse_F_array, c(2, 3), mean), 2, median))

# male

bottom_level_mae_M_array[,,1] = res_M_mae_mat_ets_EVR
bottom_level_mae_M_array[,,2] = MFTS_model_fun_EVR_ets_mae_M
bottom_level_mae_M_array[,,3] = MLFTS_model_fun_EVR_ets_mae_M
bottom_level_mae_M_array[,,4] = OWFANOVA_FFM_ets_mae_male
bottom_level_mae_M_array[,,5] = res_M_mae_mat_ets_K6
bottom_level_mae_M_array[,,6] = MFTS_model_fun_K6_ets_mae_M
bottom_level_mae_M_array[,,7] = MLFTS_model_fun_K6_ets_mae_M


bottom_level_rmse_M_array[,,1] = res_M_rmse_mat_ets_EVR
bottom_level_rmse_M_array[,,2] = MFTS_model_fun_EVR_ets_rmse_M
bottom_level_rmse_M_array[,,3] = MLFTS_model_fun_EVR_ets_rmse_M
bottom_level_rmse_M_array[,,4] = OWFANOVA_FFM_ets_rmse_male
bottom_level_rmse_M_array[,,5] = res_M_rmse_mat_ets_K6
bottom_level_rmse_M_array[,,6] = MFTS_model_fun_K6_ets_rmse_M
bottom_level_rmse_M_array[,,7] = MLFTS_model_fun_K6_ets_rmse_M

bottom_level_mae_M_array_prefecture = rbind(apply(bottom_level_mae_M_array, c(2, 3), mean),
                                            apply(apply(bottom_level_mae_M_array, c(2, 3), mean), 2, mean),
                                            apply(apply(bottom_level_mae_M_array, c(2, 3), mean), 2, median))

bottom_level_rmse_M_array_prefecture = rbind(apply(bottom_level_rmse_M_array, c(2, 3), mean),
                                             apply(apply(bottom_level_rmse_M_array, c(2, 3), mean), 2, mean),
                                             apply(apply(bottom_level_rmse_M_array, c(2, 3), mean), 2, median))

# total

bottom_level_mae_T_array[,,1] = res_T_mae_mat_ets_EVR
bottom_level_mae_T_array[,,2] = MFTS_model_fun_EVR_ets_mae_T
bottom_level_mae_T_array[,,3] = MLFTS_model_fun_EVR_ets_mae_T
bottom_level_mae_T_array[,,4] = OWFANOVA_FFM_ets_mae_total
bottom_level_mae_T_array[,,5] = res_T_mae_mat_ets_K6
bottom_level_mae_T_array[,,6] = MFTS_model_fun_K6_ets_mae_T
bottom_level_mae_T_array[,,7] = MLFTS_model_fun_K6_ets_mae_T

bottom_level_rmse_T_array[,,1] = res_T_rmse_mat_ets_EVR
bottom_level_rmse_T_array[,,2] = MFTS_model_fun_EVR_ets_rmse_T
bottom_level_rmse_T_array[,,3] = MLFTS_model_fun_EVR_ets_rmse_T
bottom_level_rmse_T_array[,,4] = OWFANOVA_FFM_ets_rmse_total
bottom_level_rmse_T_array[,,5] = res_T_rmse_mat_ets_K6
bottom_level_rmse_T_array[,,6] = MFTS_model_fun_K6_ets_rmse_T
bottom_level_rmse_T_array[,,7] = MLFTS_model_fun_K6_ets_rmse_T

bottom_level_mae_T_array_prefecture = rbind(apply(bottom_level_mae_T_array, c(2, 3), mean),
                                            apply(apply(bottom_level_mae_T_array, c(2, 3), mean), 2, mean),
                                            apply(apply(bottom_level_mae_T_array, c(2, 3), mean), 2, median))

bottom_level_rmse_T_array_prefecture = rbind(apply(bottom_level_rmse_T_array, c(2, 3), mean),
                                             apply(apply(bottom_level_rmse_T_array, c(2, 3), mean), 2, mean),
                                             apply(apply(bottom_level_rmse_T_array, c(2, 3), mean), 2, median))

rownames(bottom_level_mae_F_array_prefecture) = rownames(bottom_level_rmse_F_array_prefecture) =
rownames(bottom_level_mae_M_array_prefecture) = rownames(bottom_level_rmse_M_array_prefecture) =
rownames(bottom_level_mae_T_array_prefecture) = rownames(bottom_level_rmse_T_array_prefecture) = c(1:10, "Mean", "Median")

####################
# graphical display
####################

par(mfrow = c(2, 3))
matplot(1:10, bottom_level_mae_F_array_prefecture[1:10,],  type = "l", xlab = "Horizon", ylab = "MAE",  main = "Female data")
matplot(1:10, bottom_level_rmse_F_array_prefecture[1:10,], type = "l", xlab = "Horizon", ylab = "RMSE", main = "Female data")
matplot(1:10, bottom_level_mae_M_array_prefecture[1:10,],  type = "l", xlab = "Horizon", ylab = "MAE",  main = "Male data")
matplot(1:10, bottom_level_rmse_M_array_prefecture[1:10,], type = "l", xlab = "Horizon", ylab = "RMSE", main = "Male data")
matplot(1:10, bottom_level_mae_T_array_prefecture[1:10,],  type = "l", xlab = "Horizon", ylab = "MAE",  main = "Total data")
matplot(1:10, bottom_level_rmse_T_array_prefecture[1:10,], type = "l", xlab = "Horizon", ylab = "RMSE", main = "Total data")

# MAE

colnames(bottom_level_mae_F_array_prefecture) =
colnames(bottom_level_mae_M_array_prefecture) =
colnames(bottom_level_mae_T_array_prefecture) = c("UFTS \n (EVR)", "MFTS \n (EVR)", "MLFTS \n (EVR)",
                                                  "FANOVA \n + FFM",
                                                  "UFTS \n (K=6)", "MFTS \n (K=6)", "MLFTS \n (K=6)")

savepdf("Fig_1_F_MAE", width = 12, height = 10, toplines = 0.8)
boxplot(bottom_level_mae_F_array_prefecture[1:10,], ylab = "MAFE", main = "Female",
        xaxt = "n",
        ylim = range(range(bottom_level_mae_F_array_prefecture[1:10,]),
                     range(bottom_level_mae_M_array_prefecture[1:10,]),
                     range(bottom_level_mae_T_array_prefecture[1:10,])))
axis(1, at = 1:10,
     labels = colnames(bottom_level_mae_F_array_prefecture)[1:10],
     mgp = c(4, 1.2, 0), cex.axis = 0.7)
dev.off()

savepdf("Fig_1_M_MAE", width = 12, height = 10, toplines = 0.8)
boxplot(bottom_level_mae_M_array_prefecture[1:10,], ylab = "", main = "Male",
        xaxt = "n",
        ylim = range(range(bottom_level_mae_F_array_prefecture[1:10,]),
                     range(bottom_level_mae_M_array_prefecture[1:10,]),
                     range(bottom_level_mae_T_array_prefecture[1:10,])))

axis(1, at = 1:10,
     labels = colnames(bottom_level_mae_M_array_prefecture)[1:10],
     mgp = c(4, 1.2, 0), cex.axis = 0.7)
dev.off()

savepdf("Fig_1_T_MAE", width = 12, height = 10, toplines = 0.8)
boxplot(bottom_level_mae_T_array_prefecture[1:10,], ylab = "", main = "Total",
        xaxt = "n",
        ylim = range(range(bottom_level_mae_F_array_prefecture[1:10,]),
                     range(bottom_level_mae_M_array_prefecture[1:10,]),
                     range(bottom_level_mae_T_array_prefecture[1:10,])))

axis(1, at = 1:10,
     labels = colnames(bottom_level_mae_T_array_prefecture)[1:10],
     mgp = c(4, 1.2, 0), cex.axis = 0.7)
dev.off()

# RMSE

colnames(bottom_level_rmse_F_array_prefecture) =
colnames(bottom_level_rmse_M_array_prefecture) =
colnames(bottom_level_rmse_T_array_prefecture) = c("UFTS \n (EVR)", "MFTS \n (EVR)", "MLFTS \n (EVR)",
                                                   "FANOVA \n + FFM",
                                                   "UFTS \n (K=6)", "MFTS \n (K=6)", "MLFTS \n (K=6)")

savepdf("Fig_1_F_RMSE", width = 12, height = 10, toplines = 0.8)
boxplot(bottom_level_rmse_F_array_prefecture[1:10,], ylab = "RMSE", main = "Female",
        xaxt = "n",
        ylim = range(range(bottom_level_rmse_F_array_prefecture[1:10,]),
                     range(bottom_level_rmse_M_array_prefecture[1:10,]),
                     range(bottom_level_rmse_T_array_prefecture[1:10,])))

axis(1, at = 1:10,
     labels = colnames(bottom_level_rmse_F_array_prefecture)[1:10],
     mgp = c(4, 1.2, 0), cex.axis = 0.7)
dev.off()

savepdf("Fig_1_M_RMSE", width = 12, height = 10, toplines = 0.8)
boxplot(bottom_level_rmse_M_array_prefecture[1:10,], ylab = "", main = "Male",
        xaxt = "n",
        ylim = range(range(bottom_level_rmse_F_array_prefecture[1:10,]),
                     range(bottom_level_rmse_M_array_prefecture[1:10,]),
                     range(bottom_level_rmse_T_array_prefecture[1:10,])))

axis(1, at = 1:10,
     labels = colnames(bottom_level_rmse_M_array_prefecture)[1:10],
     mgp = c(4, 1.2, 0), cex.axis = 0.7)
dev.off()

savepdf("Fig_1_T_RMSE", width = 12, height = 10, toplines = 0.8)
boxplot(bottom_level_rmse_T_array_prefecture[1:10,], ylab = "", main = "Total",
        xaxt = "n",
        ylim = range(range(bottom_level_rmse_F_array_prefecture[1:10,]),
                     range(bottom_level_rmse_M_array_prefecture[1:10,]),
                     range(bottom_level_rmse_T_array_prefecture[1:10,])))

axis(1, at = 1:10,
     labels = colnames(bottom_level_rmse_T_array_prefecture)[1:10],
     mgp = c(4, 1.2, 0), cex.axis = 0.7)
dev.off()

# tables

xtable(rbind(bottom_level_mae_F_array_prefecture[c(1,5,10:12),],
             bottom_level_mae_M_array_prefecture[c(1,5,10:12),],
             bottom_level_mae_T_array_prefecture[c(1,5,10:12),]) * 100, digits = 3)

xtable(rbind(bottom_level_rmse_F_array_prefecture[c(1,5,10:12),],
             bottom_level_rmse_M_array_prefecture[c(1,5,10:12),],
             bottom_level_rmse_T_array_prefecture[c(1,5,10:12),]) * 100, digits = 3)

apply(rbind(bottom_level_mae_F_array_prefecture[11:12,],
            bottom_level_mae_M_array_prefecture[11:12,],
            bottom_level_mae_T_array_prefecture[11:12,]), 1, which.min) # 2 6 7 7 7 7

apply(rbind(bottom_level_rmse_F_array_prefecture[11:12,],
            bottom_level_rmse_M_array_prefecture[11:12,],
            bottom_level_rmse_T_array_prefecture[11:12,]), 1, which.min) # 2 6 7 7 4 4

#########################
## middle level (region)
#########################

middle_level_mae_F_array = middle_level_rmse_F_array =
middle_level_mae_M_array = middle_level_rmse_M_array =
middle_level_mae_T_array = middle_level_rmse_T_array = array(NA, dim = c(8, 10, 7),
      dimnames = list(region, 1:10, c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "FANOVA+FFM",
                                      "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)")))

# female

middle_level_mae_F_array[,,1] = res_region_F_mae_mat_ets_EVR
middle_level_mae_F_array[,,2] = MFTS_model_region_fun_EVR_ets_mae_F
middle_level_mae_F_array[,,3] = MLFTS_model_region_fun_EVR_ets_mae_F
middle_level_mae_F_array[,,4] = OWFANOVA_FFM_region_ets_mae_female
middle_level_mae_F_array[,,5] = res_region_F_mae_mat_ets_K6
middle_level_mae_F_array[,,6] = MFTS_model_region_fun_K6_ets_mae_F
middle_level_mae_F_array[,,7] = MLFTS_model_region_fun_K6_ets_mae_F

middle_level_rmse_F_array[,,1] = res_region_F_rmse_mat_ets_EVR
middle_level_rmse_F_array[,,2] = MFTS_model_region_fun_EVR_ets_rmse_F
middle_level_rmse_F_array[,,3] = MLFTS_model_region_fun_EVR_ets_rmse_F
middle_level_rmse_F_array[,,4] = OWFANOVA_FFM_region_ets_rmse_female
middle_level_rmse_F_array[,,5] = res_region_F_rmse_mat_ets_K6
middle_level_rmse_F_array[,,6] = MFTS_model_region_fun_K6_ets_rmse_F
middle_level_rmse_F_array[,,7] = MLFTS_model_region_fun_K6_ets_rmse_F

middle_level_mae_F_array_region = rbind(apply(middle_level_mae_F_array, c(2, 3), mean),
                                        apply(apply(middle_level_mae_F_array, c(2, 3), mean), 2, mean),
                                        apply(apply(middle_level_mae_F_array, c(2, 3), mean), 2, median))

middle_level_rmse_F_array_region = rbind(apply(middle_level_rmse_F_array, c(2, 3), mean),
                                         apply(apply(middle_level_rmse_F_array, c(2, 3), mean), 2, mean),
                                         apply(apply(middle_level_rmse_F_array, c(2, 3), mean), 2, median))

# male

middle_level_mae_M_array[,,1] = res_region_M_mae_mat_ets_EVR
middle_level_mae_M_array[,,2] = MFTS_model_region_fun_EVR_ets_mae_M
middle_level_mae_M_array[,,3] = MLFTS_model_region_fun_EVR_ets_mae_M
middle_level_mae_M_array[,,4] = OWFANOVA_FFM_region_ets_mae_male
middle_level_mae_M_array[,,5] = res_region_M_mae_mat_ets_K6
middle_level_mae_M_array[,,6] = MFTS_model_region_fun_K6_ets_mae_M
middle_level_mae_M_array[,,7] = MLFTS_model_region_fun_K6_ets_mae_M


middle_level_rmse_M_array[,,1] = res_region_M_rmse_mat_ets_EVR
middle_level_rmse_M_array[,,2] = MFTS_model_region_fun_EVR_ets_rmse_M
middle_level_rmse_M_array[,,3] = MLFTS_model_region_fun_EVR_ets_rmse_M
middle_level_rmse_M_array[,,4] = OWFANOVA_FFM_region_ets_rmse_male
middle_level_rmse_M_array[,,5] = res_region_M_rmse_mat_ets_K6
middle_level_rmse_M_array[,,6] = MFTS_model_region_fun_K6_ets_rmse_M
middle_level_rmse_M_array[,,7] = MLFTS_model_region_fun_K6_ets_rmse_M

middle_level_mae_M_array_region = rbind(apply(middle_level_mae_M_array, c(2, 3), mean),
                                        apply(apply(middle_level_mae_M_array, c(2, 3), mean), 2, mean),
                                        apply(apply(middle_level_mae_M_array, c(2, 3), mean), 2, median))

middle_level_rmse_M_array_region = rbind(apply(middle_level_rmse_M_array, c(2, 3), mean),
                                         apply(apply(middle_level_rmse_M_array, c(2, 3), mean), 2, mean),
                                         apply(apply(middle_level_rmse_M_array, c(2, 3), mean), 2, median))

# total

middle_level_mae_T_array[,,1] = res_region_T_mae_mat_ets_EVR
middle_level_mae_T_array[,,2] = MFTS_model_region_fun_EVR_ets_mae_T
middle_level_mae_T_array[,,3] = MLFTS_model_region_fun_EVR_ets_mae_T
middle_level_mae_T_array[,,4] = OWFANOVA_FFM_region_ets_mae_total
middle_level_mae_T_array[,,5] = res_region_T_mae_mat_ets_K6
middle_level_mae_T_array[,,6] = MFTS_model_region_fun_K6_ets_mae_T
middle_level_mae_T_array[,,7] = MLFTS_model_region_fun_K6_ets_mae_T

middle_level_rmse_T_array[,,1] = res_region_T_rmse_mat_ets_EVR
middle_level_rmse_T_array[,,2] = MFTS_model_region_fun_EVR_ets_rmse_T
middle_level_rmse_T_array[,,3] = MLFTS_model_region_fun_EVR_ets_rmse_T
middle_level_rmse_T_array[,,4] = OWFANOVA_FFM_region_ets_rmse_total
middle_level_rmse_T_array[,,5] = res_region_T_rmse_mat_ets_K6
middle_level_rmse_T_array[,,6] = MFTS_model_region_fun_K6_ets_rmse_T
middle_level_rmse_T_array[,,7] = MLFTS_model_region_fun_K6_ets_rmse_T

middle_level_mae_T_array_region = rbind(apply(middle_level_mae_T_array, c(2, 3), mean),
                                        apply(apply(middle_level_mae_T_array, c(2, 3), mean), 2, mean),
                                        apply(apply(middle_level_mae_T_array, c(2, 3), mean), 2, median))

middle_level_rmse_T_array_region = rbind(apply(middle_level_rmse_T_array, c(2, 3), mean),
                                         apply(apply(middle_level_rmse_T_array, c(2, 3), mean), 2, mean),
                                         apply(apply(middle_level_rmse_T_array, c(2, 3), mean), 2, median))

rownames(middle_level_mae_F_array_region) = rownames(middle_level_rmse_F_array_region) =
rownames(middle_level_mae_M_array_region) = rownames(middle_level_rmse_M_array_region) =
rownames(middle_level_mae_T_array_region) = rownames(middle_level_rmse_T_array_region) = c(1:10, "Mean", "Median")

# MAFE

colnames(middle_level_mae_F_array_region) =
colnames(middle_level_mae_M_array_region) =
colnames(middle_level_mae_T_array_region) = c("UFTS \n (EVR)", "MFTS \n (EVR)", "MLFTS \n (EVR)",
                                              "FANOVA \n + FFM",
                                              "UFTS \n (K=6)", "MFTS \n (K=6)", "MLFTS \n (K=6)")

savepdf("Fig_2_F_MAE", width = 12, height = 10, toplines = 0.8)
boxplot(middle_level_mae_F_array_region[1:10,], ylab = "MAFE", main = "Female",
        xaxt = "n",
        ylim = range(range(middle_level_mae_F_array_region[1:10,]),
                     range(middle_level_mae_M_array_region[1:10,]),
                     range(middle_level_mae_T_array_region[1:10,])))

axis(1, at = 1:10,
     labels = colnames(middle_level_mae_F_array_region)[1:10],
     mgp = c(4, 1.2, 0), cex.axis = 0.7)
dev.off()

savepdf("Fig_2_M_MAE", width = 12, height = 10, toplines = 0.8)
boxplot(middle_level_mae_M_array_region[1:10,], ylab = "", main = "Male",
        xaxt = "n",
        ylim = range(range(middle_level_mae_F_array_region[1:10,]),
                     range(middle_level_mae_M_array_region[1:10,]),
                     range(middle_level_mae_T_array_region[1:10,])))

axis(1, at = 1:10,
     labels = colnames(middle_level_mae_M_array_region)[1:10],
     mgp = c(4, 1.2, 0), cex.axis = 0.7)
dev.off()

savepdf("Fig_2_T_MAE", width = 12, height = 10, toplines = 0.8)
boxplot(middle_level_mae_T_array_region[1:10,], ylab = "", main = "Total",
        xaxt = "n",
        ylim = range(range(middle_level_mae_F_array_region[1:10,]),
                     range(middle_level_mae_M_array_region[1:10,]),
                     range(middle_level_mae_T_array_region[1:10,])))

axis(1, at = 1:10,
     labels = colnames(middle_level_mae_T_array_region)[1:10],
     mgp = c(4, 1.2, 0), cex.axis = 0.7)
dev.off()

# RMSFE

colnames(middle_level_rmse_F_array_region) =
colnames(middle_level_rmse_M_array_region) =
colnames(middle_level_rmse_T_array_region) = c("UFTS \n (EVR)", "MFTS \n (EVR)", "MLFTS \n (EVR)",
                                               "FANOVA \n + FFM",
                                               "UFTS \n (K=6)", "MFTS \n (K=6)", "MLFTS \n (K=6)")

savepdf("Fig_2_F_RMSE", width = 12, height = 10, toplines = 0.8)
boxplot(middle_level_rmse_F_array_region[1:10,], ylab = "RMSE", main = "Female",
        xaxt = "n",
        ylim = range(range(middle_level_rmse_F_array_region[1:10,]),
                     range(middle_level_rmse_M_array_region[1:10,]),
                     range(middle_level_rmse_T_array_region[1:10,])))

axis(1, at = 1:10,
     labels = colnames(middle_level_rmse_F_array_region)[1:10],
     mgp = c(4, 1.2, 0), cex.axis = 0.7)
dev.off()

savepdf("Fig_2_M_RMSE", width = 12, height = 10, toplines = 0.8)
boxplot(middle_level_rmse_M_array_region[1:10,], ylab = "", main = "Male",
        xaxt = "n",
        ylim = range(range(middle_level_rmse_F_array_region[1:10,]),
                     range(middle_level_rmse_M_array_region[1:10,]),
                     range(middle_level_rmse_T_array_region[1:10,])))

axis(1, at = 1:10,
     labels = colnames(middle_level_rmse_M_array_region)[1:10],
     mgp = c(4, 1.2, 0), cex.axis = 0.7)
dev.off()

savepdf("Fig_2_T_RMSE", width = 12, height = 10, toplines = 0.8)
boxplot(middle_level_rmse_T_array_region[1:10,], ylab = "", main = "Total",
        xaxt = "n",
        ylim = range(range(middle_level_rmse_F_array_region[1:10,]),
                     range(middle_level_rmse_M_array_region[1:10,]),
                     range(middle_level_rmse_T_array_region[1:10,])))

axis(1, at = 1:10,
     labels = colnames(middle_level_rmse_T_array_region)[1:10],
     mgp = c(4, 1.2, 0), cex.axis = 0.7)
dev.off()

# tables

xtable(cbind(rbind(middle_level_mae_F_array_region[c(1,5,10:12),],
                   middle_level_mae_M_array_region[c(1,5,10:12),],
                   middle_level_mae_T_array_region[c(1,5,10:12),]),

             rbind(middle_level_rmse_F_array_region[c(1,5,10:12),],
                   middle_level_rmse_M_array_region[c(1,5,10:12),],
                   middle_level_rmse_T_array_region[c(1,5,10:12),])) * 100, digits = 3)

apply(rbind(middle_level_mae_F_array_region[11:12,],
            middle_level_mae_M_array_region[11:12,],
            middle_level_mae_T_array_region[11:12,]), 1, which.min) # 6 6 7 7 7 7

apply(rbind(middle_level_rmse_F_array_region[11:12,],
            middle_level_rmse_M_array_region[11:12,],
            middle_level_rmse_T_array_region[11:12,]), 1, which.min) # 7 7 7 7 7 7
