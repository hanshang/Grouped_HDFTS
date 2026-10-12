# ----------------------------------------------------------------------------
# 45_base_forecast_MLFTS_arima_K6.R
#
# Assemble the MLFTS base forecast values (ARIMA scores, K = 6 components)
# into the arrays consumed by the reconciliation script in this folder.
#
# Sources: 26_base_forecast_MLFTS_forecasts_fun.R, 01_load_packages.R, 03_auxiliary_point.R
#
# Replication code for Shang, H. L., 'High-dimensional functional time
# series forecasting with a group structure'.
#
# Run with the working directory set to the code_submission/ folder.
# Scripts are numbered 01-51 in the order in which they must be run.
# See README.md for a one-line description of each.
# ----------------------------------------------------------------------------

source("26_base_forecast_MLFTS_forecasts_fun.R")

source("01_load_packages.R")
source("03_auxiliary_point.R")

##############################
# store forecast values (EVR)
##############################

MLFTS_state_forc_female_arima_K6 = c("MLFTS_Hokkaido_forc_female_arima_K6", "MLFTS_Aomori_forc_female_arima_K6",
                                     "MLFTS_Iwate_forc_female_arima_K6", "MLFTS_Miyagi_forc_female_arima_K6", "MLFTS_Akita_forc_female_arima_K6",
                                     "MLFTS_Yamagata_forc_female_arima_K6", "MLFTS_Fukushima_forc_female_arima_K6", "MLFTS_Ibaraki_forc_female_arima_K6",
                                     "MLFTS_Tochigi_forc_female_arima_K6", "MLFTS_Gunma_forc_female_arima_K6", "MLFTS_Saitama_forc_female_arima_K6",
                                     "MLFTS_Chiba_forc_female_arima_K6", "MLFTS_Tokyo_forc_female_arima_K6", "MLFTS_Kanagawa_forc_female_arima_K6",
                                     "MLFTS_Niigata_forc_female_arima_K6", "MLFTS_Toyama_forc_female_arima_K6", "MLFTS_Ishikawa_forc_female_arima_K6",
                                     "MLFTS_Fukui_forc_female_arima_K6", "MLFTS_Yamanashi_forc_female_arima_K6", "MLFTS_Nagano_forc_female_arima_K6",
                                     "MLFTS_Gifu_forc_female_arima_K6", "MLFTS_Shizuoka_forc_female_arima_K6", "MLFTS_Aichi_forc_female_arima_K6",
                                     "MLFTS_Mie_forc_female_arima_K6", "MLFTS_Shiga_forc_female_arima_K6", "MLFTS_Kyoto_forc_female_arima_K6",
                                     "MLFTS_Osaka_forc_female_arima_K6", "MLFTS_Hyogo_forc_female_arima_K6", "MLFTS_Nara_forc_female_arima_K6",
                                     "MLFTS_Wakayama_forc_female_arima_K6", "MLFTS_Tottori_forc_female_arima_K6", "MLFTS_Shimane_forc_female_arima_K6",
                                     "MLFTS_Okayama_forc_female_arima_K6", "MLFTS_Hiroshima_forc_female_arima_K6", "MLFTS_Yamaguchi_forc_female_arima_K6",
                                     "MLFTS_Tokushima_forc_female_arima_K6", "MLFTS_Kagawa_forc_female_arima_K6", "MLFTS_Ehime_forc_female_arima_K6",
                                     "MLFTS_Kochi_forc_female_arima_K6", "MLFTS_Fukuoka_forc_female_arima_K6", "MLFTS_Saga_forc_female_arima_K6",
                                     "MLFTS_Nagasaki_forc_female_arima_K6", "MLFTS_Kumamoto_forc_female_arima_K6", "MLFTS_Oita_forc_female_arima_K6",
                                     "MLFTS_Miyazaki_forc_female_arima_K6", "MLFTS_Kagoshima_forc_female_arima_K6", "MLFTS_Okinawa_forc_female_arima_K6")

MLFTS_state_forc_male_arima_K6 = c("MLFTS_Hokkaido_forc_male_arima_K6", "MLFTS_Aomori_forc_male_arima_K6",
                                   "MLFTS_Iwate_forc_male_arima_K6", "MLFTS_Miyagi_forc_male_arima_K6", "MLFTS_Akita_forc_male_arima_K6",
                                   "MLFTS_Yamagata_forc_male_arima_K6", "MLFTS_Fukushima_forc_male_arima_K6", "MLFTS_Ibaraki_forc_male_arima_K6",
                                   "MLFTS_Tochigi_forc_male_arima_K6", "MLFTS_Gunma_forc_male_arima_K6", "MLFTS_Saitama_forc_male_arima_K6",
                                   "MLFTS_Chiba_forc_male_arima_K6", "MLFTS_Tokyo_forc_male_arima_K6", "MLFTS_Kanagawa_forc_male_arima_K6",
                                   "MLFTS_Niigata_forc_male_arima_K6", "MLFTS_Toyama_forc_male_arima_K6", "MLFTS_Ishikawa_forc_male_arima_K6",
                                   "MLFTS_Fukui_forc_male_arima_K6", "MLFTS_Yamanashi_forc_male_arima_K6", "MLFTS_Nagano_forc_male_arima_K6",
                                   "MLFTS_Gifu_forc_male_arima_K6", "MLFTS_Shizuoka_forc_male_arima_K6", "MLFTS_Aichi_forc_male_arima_K6",
                                   "MLFTS_Mie_forc_male_arima_K6", "MLFTS_Shiga_forc_male_arima_K6", "MLFTS_Kyoto_forc_male_arima_K6",
                                   "MLFTS_Osaka_forc_male_arima_K6", "MLFTS_Hyogo_forc_male_arima_K6", "MLFTS_Nara_forc_male_arima_K6",
                                   "MLFTS_Wakayama_forc_male_arima_K6", "MLFTS_Tottori_forc_male_arima_K6", "MLFTS_Shimane_forc_male_arima_K6",
                                   "MLFTS_Okayama_forc_male_arima_K6", "MLFTS_Hiroshima_forc_male_arima_K6", "MLFTS_Yamaguchi_forc_male_arima_K6",
                                   "MLFTS_Tokushima_forc_male_arima_K6", "MLFTS_Kagawa_forc_male_arima_K6", "MLFTS_Ehime_forc_male_arima_K6",
                                   "MLFTS_Kochi_forc_male_arima_K6", "MLFTS_Fukuoka_forc_male_arima_K6", "MLFTS_Saga_forc_male_arima_K6",
                                   "MLFTS_Nagasaki_forc_male_arima_K6", "MLFTS_Kumamoto_forc_male_arima_K6", "MLFTS_Oita_forc_male_arima_K6",
                                   "MLFTS_Miyazaki_forc_male_arima_K6", "MLFTS_Kagoshima_forc_male_arima_K6", "MLFTS_Okinawa_forc_male_arima_K6")

MLFTS_state_forc_total_arima_K6 = c("MLFTS_Hokkaido_forc_total_arima_K6", "MLFTS_Aomori_forc_total_arima_K6",
                                    "MLFTS_Iwate_forc_total_arima_K6", "MLFTS_Miyagi_forc_total_arima_K6", "MLFTS_Akita_forc_total_arima_K6",
                                    "MLFTS_Yamagata_forc_total_arima_K6", "MLFTS_Fukushima_forc_total_arima_K6", "MLFTS_Ibaraki_forc_total_arima_K6",
                                    "MLFTS_Tochigi_forc_total_arima_K6", "MLFTS_Gunma_forc_total_arima_K6", "MLFTS_Saitama_forc_total_arima_K6",
                                    "MLFTS_Chiba_forc_total_arima_K6", "MLFTS_Tokyo_forc_total_arima_K6", "MLFTS_Kanagawa_forc_total_arima_K6",
                                    "MLFTS_Niigata_forc_total_arima_K6", "MLFTS_Toyama_forc_total_arima_K6", "MLFTS_Ishikawa_forc_total_arima_K6",
                                    "MLFTS_Fukui_forc_total_arima_K6", "MLFTS_Yamanashi_forc_total_arima_K6", "MLFTS_Nagano_forc_total_arima_K6",
                                    "MLFTS_Gifu_forc_total_arima_K6", "MLFTS_Shizuoka_forc_total_arima_K6", "MLFTS_Aichi_forc_total_arima_K6",
                                    "MLFTS_Mie_forc_total_arima_K6", "MLFTS_Shiga_forc_total_arima_K6", "MLFTS_Kyoto_forc_total_arima_K6",
                                    "MLFTS_Osaka_forc_total_arima_K6", "MLFTS_Hyogo_forc_total_arima_K6", "MLFTS_Nara_forc_total_arima_K6",
                                    "MLFTS_Wakayama_forc_total_arima_K6", "MLFTS_Tottori_forc_total_arima_K6", "MLFTS_Shimane_forc_total_arima_K6",
                                    "MLFTS_Okayama_forc_total_arima_K6", "MLFTS_Hiroshima_forc_total_arima_K6", "MLFTS_Yamaguchi_forc_total_arima_K6",
                                    "MLFTS_Tokushima_forc_total_arima_K6", "MLFTS_Kagawa_forc_total_arima_K6", "MLFTS_Ehime_forc_total_arima_K6",
                                    "MLFTS_Kochi_forc_total_arima_K6", "MLFTS_Fukuoka_forc_total_arima_K6", "MLFTS_Saga_forc_total_arima_K6",
                                    "MLFTS_Nagasaki_forc_total_arima_K6", "MLFTS_Kumamoto_forc_total_arima_K6", "MLFTS_Oita_forc_total_arima_K6",
                                    "MLFTS_Miyazaki_forc_total_arima_K6", "MLFTS_Kagoshima_forc_total_arima_K6", "MLFTS_Okinawa_forc_total_arima_K6")

MLFTS_region_forc_female_arima_K6 = c("MLFTS_region_R1_forc_female_arima_K6", "MLFTS_region_R2_forc_female_arima_K6", "MLFTS_region_R3_forc_female_arima_K6",
                                      "MLFTS_region_R4_forc_female_arima_K6", "MLFTS_region_R5_forc_female_arima_K6", "MLFTS_region_R6_forc_female_arima_K6",
                                      "MLFTS_region_R7_forc_female_arima_K6", "MLFTS_region_R8_forc_female_arima_K6")

MLFTS_region_forc_male_arima_K6 = c("MLFTS_region_R1_forc_male_arima_K6", "MLFTS_region_R2_forc_male_arima_K6", "MLFTS_region_R3_forc_male_arima_K6",
                                    "MLFTS_region_R4_forc_male_arima_K6", "MLFTS_region_R5_forc_male_arima_K6", "MLFTS_region_R6_forc_male_arima_K6",
                                    "MLFTS_region_R7_forc_male_arima_K6", "MLFTS_region_R8_forc_male_arima_K6")

MLFTS_region_forc_total_arima_K6 = c("MLFTS_region_R1_forc_total_arima_K6", "MLFTS_region_R2_forc_total_arima_K6", "MLFTS_region_R3_forc_total_arima_K6",
                                     "MLFTS_region_R4_forc_total_arima_K6", "MLFTS_region_R5_forc_total_arima_K6", "MLFTS_region_R6_forc_total_arima_K6",
                                     "MLFTS_region_R7_forc_total_arima_K6", "MLFTS_region_R8_forc_total_arima_K6")

### ARIMA

## subnational (prefecture)

# EVR

# female

dum = MLFTS_model_fun_forecasts(data_var = state_smooth_F_array, ncomp_selection_method = "provide", fmethod = "arima")
for(ik in 1:n_state)
{
    assign(MLFTS_state_forc_female_arima_K6[ik], dum[,,ik,])
    rm(ik)
}
rm(dum)

# male

dum = MLFTS_model_fun_forecasts(data_var = state_smooth_M_array, ncomp_selection_method = "provide", fmethod = "arima")
for(ik in 1:n_state)
{
    assign(MLFTS_state_forc_male_arima_K6[ik], dum[,,ik,])
    rm(ik)
}
rm(dum)

# total

dum = MLFTS_model_fun_forecasts(data_var = state_smooth_T_array, ncomp_selection_method = "provide", fmethod = "arima")
for(ik in 1:n_state)
{
    assign(MLFTS_state_forc_total_arima_K6[ik], dum[,,ik,])
    rm(ik)
}
rm(dum)

## subnational (region)

# EVR

# female

dum = MLFTS_model_fun_forecasts(data_var = region_smooth_F_array, ncomp_selection_method = "provide", fmethod = "arima")
for(ik in 1:n_region)
{
    assign(MLFTS_region_forc_female_arima_K6[ik], dum[,,ik,])
    rm(ik)
}
rm(dum)

# male

dum = MLFTS_model_fun_forecasts(data_var = region_smooth_M_array, ncomp_selection_method = "provide", fmethod = "arima")
for(ik in 1:n_region)
{
    assign(MLFTS_region_forc_male_arima_K6[ik], dum[,,ik,])
    rm(ik)
}
rm(dum)

# total

dum = MLFTS_model_fun_forecasts(data_var = region_smooth_T_array, ncomp_selection_method = "provide", fmethod = "arima")
for(ik in 1:n_region)
{
    assign(MLFTS_region_forc_total_arima_K6[ik], dum[,,ik,])
    rm(ik)
}
rm(dum)


######################
# Japan national data
######################

Japan_F_fore_arima_K6 = Japan_M_fore_arima_K6 = Japan_T_fore_arima_K6 = array(NA, dim = c(10, 101, 10))
for(iw in 1:10)
{
    # females

    ind_dat = extract.years(Japan_demo_smooth_F, years = 1975:(2013 + iw))
    fun_forc = forecast(fdm(ind_dat, series = "Female", order = 6), h = 10, method = "arima")
    Japan_F_fore_arima_K6[,,iw] = t(fun_forc$rate$female)
    rm(ncomp)

    # males

    ind_dat = extract.years(Japan_demo_smooth_M, years = 1975:(2013 + iw))
    fun_forc = forecast(fdm(ind_dat, series = "Male", order = 6), h = 10, method = "arima")
    Japan_M_fore_arima_K6[,,iw] = t(fun_forc$rate$male)
    rm(ncomp)

    # total

    ind_dat = extract.years(Japan_demo_smooth_T, years = 1975:(2013 + iw))
    fun_forc = forecast(fdm(ind_dat, series = "Total", order = 6), h = 10, method = "arima")
    Japan_T_fore_arima_K6[,,iw] = t(fun_forc$rate$total)
    rm(iw); rm(ncomp)
}

###############
# MAE and RMSE
###############

Japan_F_fore_mae_arima_K6 = Japan_F_fore_rmse_arima_K6 =
Japan_M_fore_mae_arima_K6 = Japan_M_fore_rmse_arima_K6 =
Japan_T_fore_mae_arima_K6 = Japan_T_fore_rmse_arima_K6 = vector("numeric", 10)
for(k in 1:10)
{
    # females

    Japan_F_fore_mae_arima_K6[k]  = mae(Japan_F_fore_arima_K6[k,,1:(11 - k)],  extract.years(Japan_demo_F, years = (2014 + k):2024)$rate$Female)
    Japan_F_fore_rmse_arima_K6[k] = rmse(Japan_F_fore_arima_K6[k,,1:(11 - k)],  extract.years(Japan_demo_F, years = (2014 + k):2024)$rate$Female)

    # males

    Japan_M_fore_mae_arima_K6[k]  = mae(Japan_M_fore_arima_K6[k,,1:(11 - k)],  extract.years(Japan_demo_M, years = (2014 + k):2024)$rate$Male)
    Japan_M_fore_rmse_arima_K6[k] = rmse(Japan_M_fore_arima_K6[k,,1:(11 - k)],  extract.years(Japan_demo_M, years = (2014 + k):2024)$rate$Male)

    # totals

    Japan_T_fore_mae_arima_K6[k]  = mae(Japan_T_fore_arima_K6[k,,1:(11 - k)],  extract.years(Japan_demo_T, years = (2014 + k):2024)$rate$Total)
    Japan_T_fore_rmse_arima_K6[k] = rmse(Japan_T_fore_arima_K6[k,,1:(11 - k)],  extract.years(Japan_demo_T, years = (2014 + k):2024)$rate$Total)
    rm(k)
}
