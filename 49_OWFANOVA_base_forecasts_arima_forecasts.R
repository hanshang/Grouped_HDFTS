# ----------------------------------------------------------------------------
# 49_OWFANOVA_base_forecasts_arima_forecasts.R
#
# Assemble the one-way FANOVA + FFM base forecast values (ARIMA scores,
# components as configured) into the arrays consumed by the reconciliation
# script in this folder.
#
# Replication code for Shang, H. L., 'High-dimensional functional time
# series forecasting with a group structure'.
#
# Run with the working directory set to the code_submission/ folder.
# Scripts are numbered 01-51 in the order in which they must be run.
# See README.md for a one-line description of each.
# ----------------------------------------------------------------------------

################################################
# store forecast values obtained from the ARIMA
################################################

OWFANOVA_state_forc_female_arima = c("OWFANOVA_Hokkaido_forc_female_arima", "OWFANOVA_Aomori_forc_female_arima",
                                     "OWFANOVA_Iwate_forc_female_arima", "OWFANOVA_Miyagi_forc_female_arima", "OWFANOVA_Akita_forc_female_arima",
                                     "OWFANOVA_Yamagata_forc_female_arima", "OWFANOVA_Fukushima_forc_female_arima", "OWFANOVA_Ibaraki_forc_female_arima",
                                     "OWFANOVA_Tochigi_forc_female_arima", "OWFANOVA_Gunma_forc_female_arima", "OWFANOVA_Saitama_forc_female_arima",
                                     "OWFANOVA_Chiba_forc_female_arima", "OWFANOVA_Tokyo_forc_female_arima", "OWFANOVA_Kanagawa_forc_female_arima",
                                     "OWFANOVA_Niigata_forc_female_arima", "OWFANOVA_Toyama_forc_female_arima", "OWFANOVA_Ishikawa_forc_female_arima",
                                     "OWFANOVA_Fukui_forc_female_arima", "OWFANOVA_Yamanashi_forc_female_arima", "OWFANOVA_Nagano_forc_female_arima",
                                     "OWFANOVA_Gifu_forc_female_arima", "OWFANOVA_Shizuoka_forc_female_arima", "OWFANOVA_Aichi_forc_female_arima",
                                     "OWFANOVA_Mie_forc_female_arima", "OWFANOVA_Shiga_forc_female_arima", "OWFANOVA_Kyoto_forc_female_arima",
                                     "OWFANOVA_Osaka_forc_female_arima", "OWFANOVA_Hyogo_forc_female_arima", "OWFANOVA_Nara_forc_female_arima",
                                     "OWFANOVA_Wakayama_forc_female_arima", "OWFANOVA_Tottori_forc_female_arima", "OWFANOVA_Shimane_forc_female_arima",
                                     "OWFANOVA_Okayama_forc_female_arima", "OWFANOVA_Hiroshima_forc_female_arima", "OWFANOVA_Yamaguchi_forc_female_arima",
                                     "OWFANOVA_Tokushima_forc_female_arima", "OWFANOVA_Kagawa_forc_female_arima", "OWFANOVA_Ehime_forc_female_arima",
                                     "OWFANOVA_Kochi_forc_female_arima", "OWFANOVA_Fukuoka_forc_female_arima", "OWFANOVA_Saga_forc_female_arima",
                                     "OWFANOVA_Nagasaki_forc_female_arima", "OWFANOVA_Kumamoto_forc_female_arima", "OWFANOVA_Oita_forc_female_arima",
                                     "OWFANOVA_Miyazaki_forc_female_arima", "OWFANOVA_Kagoshima_forc_female_arima", "OWFANOVA_Okinawa_forc_female_arima")

OWFANOVA_state_forc_male_arima = c("OWFANOVA_Hokkaido_forc_male_arima", "OWFANOVA_Aomori_forc_male_arima",
                                   "OWFANOVA_Iwate_forc_male_arima", "OWFANOVA_Miyagi_forc_male_arima", "OWFANOVA_Akita_forc_male_arima",
                                   "OWFANOVA_Yamagata_forc_male_arima", "OWFANOVA_Fukushima_forc_male_arima", "OWFANOVA_Ibaraki_forc_male_arima",
                                   "OWFANOVA_Tochigi_forc_male_arima", "OWFANOVA_Gunma_forc_male_arima", "OWFANOVA_Saitama_forc_male_arima",
                                   "OWFANOVA_Chiba_forc_male_arima", "OWFANOVA_Tokyo_forc_male_arima", "OWFANOVA_Kanagawa_forc_male_arima",
                                   "OWFANOVA_Niigata_forc_male_arima", "OWFANOVA_Toyama_forc_male_arima", "OWFANOVA_Ishikawa_forc_male_arima",
                                   "OWFANOVA_Fukui_forc_male_arima", "OWFANOVA_Yamanashi_forc_male_arima", "OWFANOVA_Nagano_forc_male_arima",
                                   "OWFANOVA_Gifu_forc_male_arima", "OWFANOVA_Shizuoka_forc_male_arima", "OWFANOVA_Aichi_forc_male_arima",
                                   "OWFANOVA_Mie_forc_male_arima", "OWFANOVA_Shiga_forc_male_arima", "OWFANOVA_Kyoto_forc_male_arima",
                                   "OWFANOVA_Osaka_forc_male_arima", "OWFANOVA_Hyogo_forc_male_arima", "OWFANOVA_Nara_forc_male_arima",
                                   "OWFANOVA_Wakayama_forc_male_arima", "OWFANOVA_Tottori_forc_male_arima", "OWFANOVA_Shimane_forc_male_arima",
                                   "OWFANOVA_Okayama_forc_male_arima", "OWFANOVA_Hiroshima_forc_male_arima", "OWFANOVA_Yamaguchi_forc_male_arima",
                                   "OWFANOVA_Tokushima_forc_male_arima", "OWFANOVA_Kagawa_forc_male_arima", "OWFANOVA_Ehime_forc_male_arima",
                                   "OWFANOVA_Kochi_forc_male_arima", "OWFANOVA_Fukuoka_forc_male_arima", "OWFANOVA_Saga_forc_male_arima",
                                   "OWFANOVA_Nagasaki_forc_male_arima", "OWFANOVA_Kumamoto_forc_male_arima", "OWFANOVA_Oita_forc_male_arima",
                                   "OWFANOVA_Miyazaki_forc_male_arima", "OWFANOVA_Kagoshima_forc_male_arima", "OWFANOVA_Okinawa_forc_male_arima")

OWFANOVA_state_forc_total_arima = c("OWFANOVA_Hokkaido_forc_total_arima", "OWFANOVA_Aomori_forc_total_arima",
                                    "OWFANOVA_Iwate_forc_total_arima", "OWFANOVA_Miyagi_forc_total_arima", "OWFANOVA_Akita_forc_total_arima",
                                    "OWFANOVA_Yamagata_forc_total_arima", "OWFANOVA_Fukushima_forc_total_arima", "OWFANOVA_Ibaraki_forc_total_arima",
                                    "OWFANOVA_Tochigi_forc_total_arima", "OWFANOVA_Gunma_forc_total_arima", "OWFANOVA_Saitama_forc_total_arima",
                                    "OWFANOVA_Chiba_forc_total_arima", "OWFANOVA_Tokyo_forc_total_arima", "OWFANOVA_Kanagawa_forc_total_arima",
                                    "OWFANOVA_Niigata_forc_total_arima", "OWFANOVA_Toyama_forc_total_arima", "OWFANOVA_Ishikawa_forc_total_arima",
                                    "OWFANOVA_Fukui_forc_total_arima", "OWFANOVA_Yamanashi_forc_total_arima", "OWFANOVA_Nagano_forc_total_arima",
                                    "OWFANOVA_Gifu_forc_total_arima", "OWFANOVA_Shizuoka_forc_total_arima", "OWFANOVA_Aichi_forc_total_arima",
                                    "OWFANOVA_Mie_forc_total_arima", "OWFANOVA_Shiga_forc_total_arima", "OWFANOVA_Kyoto_forc_total_arima",
                                    "OWFANOVA_Osaka_forc_total_arima", "OWFANOVA_Hyogo_forc_total_arima", "OWFANOVA_Nara_forc_total_arima",
                                    "OWFANOVA_Wakayama_forc_total_arima", "OWFANOVA_Tottori_forc_total_arima", "OWFANOVA_Shimane_forc_total_arima",
                                    "OWFANOVA_Okayama_forc_total_arima", "OWFANOVA_Hiroshima_forc_total_arima", "OWFANOVA_Yamaguchi_forc_total_arima",
                                    "OWFANOVA_Tokushima_forc_total_arima", "OWFANOVA_Kagawa_forc_total_arima", "OWFANOVA_Ehime_forc_total_arima",
                                    "OWFANOVA_Kochi_forc_total_arima", "OWFANOVA_Fukuoka_forc_total_arima", "OWFANOVA_Saga_forc_total_arima",
                                    "OWFANOVA_Nagasaki_forc_total_arima", "OWFANOVA_Kumamoto_forc_total_arima", "OWFANOVA_Oita_forc_total_arima",
                                    "OWFANOVA_Miyazaki_forc_total_arima", "OWFANOVA_Kagoshima_forc_total_arima", "OWFANOVA_Okinawa_forc_total_arima")

OWFANOVA_region_forc_female_arima = c("OWFANOVA_region_R1_forc_female_arima", "OWFANOVA_region_R2_forc_female_arima", "OWFANOVA_region_R3_forc_female_arima",
                                      "OWFANOVA_region_R4_forc_female_arima", "OWFANOVA_region_R5_forc_female_arima", "OWFANOVA_region_R6_forc_female_arima",
                                      "OWFANOVA_region_R7_forc_female_arima", "OWFANOVA_region_R8_forc_female_arima")

OWFANOVA_region_forc_male_arima = c("OWFANOVA_region_R1_forc_male_arima", "OWFANOVA_region_R2_forc_male_arima", "OWFANOVA_region_R3_forc_male_arima",
                                    "OWFANOVA_region_R4_forc_male_arima", "OWFANOVA_region_R5_forc_male_arima", "OWFANOVA_region_R6_forc_male_arima",
                                    "OWFANOVA_region_R7_forc_male_arima", "OWFANOVA_region_R8_forc_male_arima")

OWFANOVA_region_forc_total_arima = c("OWFANOVA_region_R1_forc_total_arima", "OWFANOVA_region_R2_forc_total_arima", "OWFANOVA_region_R3_forc_total_arima",
                                     "OWFANOVA_region_R4_forc_total_arima", "OWFANOVA_region_R5_forc_total_arima", "OWFANOVA_region_R6_forc_total_arima",
                                     "OWFANOVA_region_R7_forc_total_arima", "OWFANOVA_region_R8_forc_total_arima")


# smooth_data_set:
# fore_method: "ets" or "arima"

OWFANOVA_fun_forecasts <- function(smooth_data_set, fore_method)
{
    n_age = dim(smooth_data_set)[1]
    n_year = dim(smooth_data_set)[2]
    n_state = dim(smooth_data_set)[3]

    forecast_val = array(NA, dim = c(10, n_age, n_state, 10))
    for(ij in 1:10)
    {
        forecast_val[,,,ij] = fore_rate_one_way_FANOVA(data_set = smooth_data_set[,1:(n_year - 11 + ij),],
                                                       fh = 10, fmethod = fore_method)
        rm(ij)
    }
    rm(n_state); rm(n_year); rm(n_age)
    return(forecast_val)
}

############################
## subnational (prefecture)
############################

# female

dum = OWFANOVA_fun_forecasts(smooth_data_set = state_smooth_F_array, fore_method = "arima")
for(ik in 1:n_state)
{
    assign(OWFANOVA_state_forc_female_arima[ik], dum[,,ik,])
    rm(ik)
}
rm(dum)

# male

dum = OWFANOVA_fun_forecasts(smooth_data_set = state_smooth_M_array, fore_method = "arima")
for(ik in 1:n_state)
{
    assign(OWFANOVA_state_forc_male_arima[ik], dum[,,ik,])
    rm(ik)
}
rm(dum)

# total

dum = OWFANOVA_fun_forecasts(smooth_data_set = state_smooth_T_array, fore_method = "arima")
for(ik in 1:n_state)
{
    assign(OWFANOVA_state_forc_total_arima[ik], dum[,,ik,])
    rm(ik)
}
rm(dum)

########################
## subnational (region)
########################

# female

dum = OWFANOVA_fun_forecasts(smooth_data_set = region_smooth_F_array, fore_method = "arima")
for(ik in 1:n_region)
{
    assign(OWFANOVA_region_forc_female_arima[ik], dum[,,ik,])
    rm(ik)
}
rm(dum)

# male

dum = OWFANOVA_fun_forecasts(smooth_data_set = region_smooth_M_array, fore_method = "arima")
for(ik in 1:n_region)
{
    assign(OWFANOVA_region_forc_male_arima[ik], dum[,,ik,])
    rm(ik)
}
rm(dum)

# total

dum = OWFANOVA_fun_forecasts(smooth_data_set = region_smooth_T_array, fore_method = "arima")
for(ik in 1:n_region)
{
    assign(OWFANOVA_region_forc_total_arima[ik], dum[,,ik,])
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
