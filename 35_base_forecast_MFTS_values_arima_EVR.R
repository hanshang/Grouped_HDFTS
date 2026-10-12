# ----------------------------------------------------------------------------
# 35_base_forecast_MFTS_values_arima_EVR.R
#
# Assemble the MFTS base forecast values (ARIMA scores, components by the
# EVR criterion) into the arrays consumed by the reconciliation script in
# this folder.
#
# Sources: 01_load_packages.R
#
# Replication code for Shang, H. L., 'High-dimensional functional time
# series forecasting with a group structure'.
#
# Run with the working directory set to the code_submission/ folder.
# Scripts are numbered 01-51 in the order in which they must be run.
# See README.md for a one-line description of each.
# ----------------------------------------------------------------------------

source("01_load_packages.R")

##############################
# store forecast values (EVR)
##############################

MFTS_state_forc_female_arima_EVR = c("MFTS_Hokkaido_forc_female_arima_EVR", "MFTS_Aomori_forc_female_arima_EVR",
                                     "MFTS_Iwate_forc_female_arima_EVR", "MFTS_Miyagi_forc_female_arima_EVR", "MFTS_Akita_forc_female_arima_EVR",
                                     "MFTS_Yamagata_forc_female_arima_EVR", "MFTS_Fukushima_forc_female_arima_EVR", "MFTS_Ibaraki_forc_female_arima_EVR",
                                     "MFTS_Tochigi_forc_female_arima_EVR", "MFTS_Gunma_forc_female_arima_EVR", "MFTS_Saitama_forc_female_arima_EVR",
                                     "MFTS_Chiba_forc_female_arima_EVR", "MFTS_Tokyo_forc_female_arima_EVR", "MFTS_Kanagawa_forc_female_arima_EVR",
                                     "MFTS_Niigata_forc_female_arima_EVR", "MFTS_Toyama_forc_female_arima_EVR", "MFTS_Ishikawa_forc_female_arima_EVR",
                                     "MFTS_Fukui_forc_female_arima_EVR", "MFTS_Yamanashi_forc_female_arima_EVR", "MFTS_Nagano_forc_female_arima_EVR",
                                     "MFTS_Gifu_forc_female_arima_EVR", "MFTS_Shizuoka_forc_female_arima_EVR", "MFTS_Aichi_forc_female_arima_EVR",
                                     "MFTS_Mie_forc_female_arima_EVR", "MFTS_Shiga_forc_female_arima_EVR", "MFTS_Kyoto_forc_female_arima_EVR",
                                     "MFTS_Osaka_forc_female_arima_EVR", "MFTS_Hyogo_forc_female_arima_EVR", "MFTS_Nara_forc_female_arima_EVR",
                                     "MFTS_Wakayama_forc_female_arima_EVR", "MFTS_Tottori_forc_female_arima_EVR", "MFTS_Shimane_forc_female_arima_EVR",
                                     "MFTS_Okayama_forc_female_arima_EVR", "MFTS_Hiroshima_forc_female_arima_EVR", "MFTS_Yamaguchi_forc_female_arima_EVR",
                                     "MFTS_Tokushima_forc_female_arima_EVR", "MFTS_Kagawa_forc_female_arima_EVR", "MFTS_Ehime_forc_female_arima_EVR",
                                     "MFTS_Kochi_forc_female_arima_EVR", "MFTS_Fukuoka_forc_female_arima_EVR", "MFTS_Saga_forc_female_arima_EVR",
                                     "MFTS_Nagasaki_forc_female_arima_EVR", "MFTS_Kumamoto_forc_female_arima_EVR", "MFTS_Oita_forc_female_arima_EVR",
                                     "MFTS_Miyazaki_forc_female_arima_EVR", "MFTS_Kagoshima_forc_female_arima_EVR", "MFTS_Okinawa_forc_female_arima_EVR")

MFTS_state_forc_male_arima_EVR = c("MFTS_Hokkaido_forc_male_arima_EVR", "MFTS_Aomori_forc_male_arima_EVR",
                                   "MFTS_Iwate_forc_male_arima_EVR", "MFTS_Miyagi_forc_male_arima_EVR", "MFTS_Akita_forc_male_arima_EVR",
                                   "MFTS_Yamagata_forc_male_arima_EVR", "MFTS_Fukushima_forc_male_arima_EVR", "MFTS_Ibaraki_forc_male_arima_EVR",
                                   "MFTS_Tochigi_forc_male_arima_EVR", "MFTS_Gunma_forc_male_arima_EVR", "MFTS_Saitama_forc_male_arima_EVR",
                                   "MFTS_Chiba_forc_male_arima_EVR", "MFTS_Tokyo_forc_male_arima_EVR", "MFTS_Kanagawa_forc_male_arima_EVR",
                                   "MFTS_Niigata_forc_male_arima_EVR", "MFTS_Toyama_forc_male_arima_EVR", "MFTS_Ishikawa_forc_male_arima_EVR",
                                   "MFTS_Fukui_forc_male_arima_EVR", "MFTS_Yamanashi_forc_male_arima_EVR", "MFTS_Nagano_forc_male_arima_EVR",
                                   "MFTS_Gifu_forc_male_arima_EVR", "MFTS_Shizuoka_forc_male_arima_EVR", "MFTS_Aichi_forc_male_arima_EVR",
                                   "MFTS_Mie_forc_male_arima_EVR", "MFTS_Shiga_forc_male_arima_EVR", "MFTS_Kyoto_forc_male_arima_EVR",
                                   "MFTS_Osaka_forc_male_arima_EVR", "MFTS_Hyogo_forc_male_arima_EVR", "MFTS_Nara_forc_male_arima_EVR",
                                   "MFTS_Wakayama_forc_male_arima_EVR", "MFTS_Tottori_forc_male_arima_EVR", "MFTS_Shimane_forc_male_arima_EVR",
                                   "MFTS_Okayama_forc_male_arima_EVR", "MFTS_Hiroshima_forc_male_arima_EVR", "MFTS_Yamaguchi_forc_male_arima_EVR",
                                   "MFTS_Tokushima_forc_male_arima_EVR", "MFTS_Kagawa_forc_male_arima_EVR", "MFTS_Ehime_forc_male_arima_EVR",
                                   "MFTS_Kochi_forc_male_arima_EVR", "MFTS_Fukuoka_forc_male_arima_EVR", "MFTS_Saga_forc_male_arima_EVR",
                                   "MFTS_Nagasaki_forc_male_arima_EVR", "MFTS_Kumamoto_forc_male_arima_EVR", "MFTS_Oita_forc_male_arima_EVR",
                                   "MFTS_Miyazaki_forc_male_arima_EVR", "MFTS_Kagoshima_forc_male_arima_EVR", "MFTS_Okinawa_forc_male_arima_EVR")

MFTS_state_forc_total_arima_EVR = c("MFTS_Hokkaido_forc_total_arima_EVR", "MFTS_Aomori_forc_total_arima_EVR",
                                    "MFTS_Iwate_forc_total_arima_EVR", "MFTS_Miyagi_forc_total_arima_EVR", "MFTS_Akita_forc_total_arima_EVR",
                                    "MFTS_Yamagata_forc_total_arima_EVR", "MFTS_Fukushima_forc_total_arima_EVR", "MFTS_Ibaraki_forc_total_arima_EVR",
                                    "MFTS_Tochigi_forc_total_arima_EVR", "MFTS_Gunma_forc_total_arima_EVR", "MFTS_Saitama_forc_total_arima_EVR",
                                    "MFTS_Chiba_forc_total_arima_EVR", "MFTS_Tokyo_forc_total_arima_EVR", "MFTS_Kanagawa_forc_total_arima_EVR",
                                    "MFTS_Niigata_forc_total_arima_EVR", "MFTS_Toyama_forc_total_arima_EVR", "MFTS_Ishikawa_forc_total_arima_EVR",
                                    "MFTS_Fukui_forc_total_arima_EVR", "MFTS_Yamanashi_forc_total_arima_EVR", "MFTS_Nagano_forc_total_arima_EVR",
                                    "MFTS_Gifu_forc_total_arima_EVR", "MFTS_Shizuoka_forc_total_arima_EVR", "MFTS_Aichi_forc_total_arima_EVR",
                                    "MFTS_Mie_forc_total_arima_EVR", "MFTS_Shiga_forc_total_arima_EVR", "MFTS_Kyoto_forc_total_arima_EVR",
                                    "MFTS_Osaka_forc_total_arima_EVR", "MFTS_Hyogo_forc_total_arima_EVR", "MFTS_Nara_forc_total_arima_EVR",
                                    "MFTS_Wakayama_forc_total_arima_EVR", "MFTS_Tottori_forc_total_arima_EVR", "MFTS_Shimane_forc_total_arima_EVR",
                                    "MFTS_Okayama_forc_total_arima_EVR", "MFTS_Hiroshima_forc_total_arima_EVR", "MFTS_Yamaguchi_forc_total_arima_EVR",
                                    "MFTS_Tokushima_forc_total_arima_EVR", "MFTS_Kagawa_forc_total_arima_EVR", "MFTS_Ehime_forc_total_arima_EVR",
                                    "MFTS_Kochi_forc_total_arima_EVR", "MFTS_Fukuoka_forc_total_arima_EVR", "MFTS_Saga_forc_total_arima_EVR",
                                    "MFTS_Nagasaki_forc_total_arima_EVR", "MFTS_Kumamoto_forc_total_arima_EVR", "MFTS_Oita_forc_total_arima_EVR",
                                    "MFTS_Miyazaki_forc_total_arima_EVR", "MFTS_Kagoshima_forc_total_arima_EVR", "MFTS_Okinawa_forc_total_arima_EVR")

MFTS_region_forc_female_arima_EVR = c("MFTS_region_R1_forc_female_arima_EVR", "MFTS_region_R2_forc_female_arima_EVR", "MFTS_region_R3_forc_female_arima_EVR",
                                      "MFTS_region_R4_forc_female_arima_EVR", "MFTS_region_R5_forc_female_arima_EVR", "MFTS_region_R6_forc_female_arima_EVR",
                                      "MFTS_region_R7_forc_female_arima_EVR", "MFTS_region_R8_forc_female_arima_EVR")

MFTS_region_forc_male_arima_EVR = c("MFTS_region_R1_forc_male_arima_EVR", "MFTS_region_R2_forc_male_arima_EVR", "MFTS_region_R3_forc_male_arima_EVR",
                                    "MFTS_region_R4_forc_male_arima_EVR", "MFTS_region_R5_forc_male_arima_EVR", "MFTS_region_R6_forc_male_arima_EVR",
                                    "MFTS_region_R7_forc_male_arima_EVR", "MFTS_region_R8_forc_male_arima_EVR")

MFTS_region_forc_total_arima_EVR = c("MFTS_region_R1_forc_total_arima_EVR", "MFTS_region_R2_forc_total_arima_EVR", "MFTS_region_R3_forc_total_arima_EVR",
                                     "MFTS_region_R4_forc_total_arima_EVR", "MFTS_region_R5_forc_total_arima_EVR", "MFTS_region_R6_forc_total_arima_EVR",
                                     "MFTS_region_R7_forc_total_arima_EVR", "MFTS_region_R8_forc_total_arima_EVR")

########################
# compute the forecasts
########################

# data_var: n_age x n_year x n_state
# ncomp_selection_method: EVR or K = 6
# fmethod: forecasting method, ets or arima

MFTS_model_fun_forecasts <- function(data_var, ncomp_selection_method, fmethod)
{
    n_age = dim(data_var)[1]
    n_year = dim(data_var)[2]
    n_state = dim(data_var)[3]

    dum_fore_array = array(NA, dim = c(10, n_age, n_state, 10))
    for(iw in 1:10)
    {
      dum = MFTS_model(data_var[,1:(n_year - 11 + iw),], ncomp_method = ncomp_selection_method,
                       fh = 10, fore_method = fmethod)
      dum_fore_array[,,,iw] = aperm(array(dum, dim = c(n_age, n_state, 10)), c(3, 1, 2))
      rm(iw); rm(dum)
    }
    return(dum_fore_array)
}

### ARIMA

## subnational (prefecture)

# EVR

# female

dum = MFTS_model_fun_forecasts(data_var = state_smooth_F_array, ncomp_selection_method = "EVR", fmethod = "arima")
for(ik in 1:n_state)
{
    assign(MFTS_state_forc_female_arima_EVR[ik], dum[,,ik,])
    rm(ik)
}
rm(dum)

# male

dum = MFTS_model_fun_forecasts(data_var = state_smooth_M_array, ncomp_selection_method = "EVR", fmethod = "arima")
for(ik in 1:n_state)
{
    assign(MFTS_state_forc_male_arima_EVR[ik], dum[,,ik,])
    rm(ik)
}
rm(dum)

# total

dum = MFTS_model_fun_forecasts(data_var = state_smooth_T_array, ncomp_selection_method = "EVR", fmethod = "arima")
for(ik in 1:n_state)
{
    assign(MFTS_state_forc_total_arima_EVR[ik], dum[,,ik,])
    rm(ik)
}
rm(dum)

## subnational (region)

# EVR

# female

dum = MFTS_model_fun_forecasts(data_var = region_smooth_F_array, ncomp_selection_method = "EVR", fmethod = "arima")
for(ik in 1:n_region)
{
    assign(MFTS_region_forc_female_arima_EVR[ik], dum[,,ik,])
    rm(ik)
}
rm(dum)

# male

dum = MFTS_model_fun_forecasts(data_var = region_smooth_M_array, ncomp_selection_method = "EVR", fmethod = "arima")
for(ik in 1:n_region)
{
    assign(MFTS_region_forc_male_arima_EVR[ik], dum[,,ik,])
    rm(ik)
}
rm(dum)

# total

dum = MFTS_model_fun_forecasts(data_var = region_smooth_T_array, ncomp_selection_method = "EVR", fmethod = "arima")
for(ik in 1:n_region)
{
    assign(MFTS_region_forc_total_arima_EVR[ik], dum[,,ik,])
    rm(ik)
}
rm(dum)
