# ----------------------------------------------------------------------------
# 06_base_forecast_UFTS_ets_EVR.R
#
# UFTS base forecasts: ETS scores, components chosen by the EVR criterion.
#
# Sources: 01_load_packages.R, 03_auxiliary_point.R, 04_base_forecast_UFTS_fun.R
#
# Replication code for Shang, H. L., 'High-dimensional functional time
# series forecasting with a group structure'.
#
# Run with the working directory set to the code_submission/ folder.
# Scripts are numbered 01-51 in the order in which they must be run.
# See README.md for a one-line description of each.
# ----------------------------------------------------------------------------

source("01_load_packages.R")
source("03_auxiliary_point.R")

source("04_base_forecast_UFTS_fun.R")

######
# MAE
######

ind_state_mae_female_ets_EVR = c("ind_Hokkaido_mae_female_ets_EVR", "ind_Aomori_mae_female_ets_EVR",
                                 "ind_Iwate_mae_female_ets_EVR", "ind_Miyagi_mae_female_ets_EVR", "ind_Akita_mae_female_ets_EVR",
                                 "ind_Yamagata_mae_female_ets_EVR", "ind_Fukushima_mae_female_ets_EVR", "ind_Ibaraki_mae_female_ets_EVR",
                                 "ind_Tochigi_mae_female_ets_EVR", "ind_Gunma_mae_female_ets_EVR", "ind_Saitama_mae_female_ets_EVR",
                                 "ind_Chiba_mae_female_ets_EVR", "ind_Tokyo_mae_female_ets_EVR", "ind_Kanagawa_mae_female_ets_EVR",
                                 "ind_Niigata_mae_female_ets_EVR", "ind_Toyama_mae_female_ets_EVR", "ind_Ishikawa_mae_female_ets_EVR",
                                 "ind_Fukui_mae_female_ets_EVR", "ind_Yamanashi_mae_female_ets_EVR", "ind_Nagano_mae_female_ets_EVR",
                                 "ind_Gifu_mae_female_ets_EVR", "ind_Shizuoka_mae_female_ets_EVR", "ind_Aichi_mae_female_ets_EVR",
                                 "ind_Mie_mae_female_ets_EVR", "ind_Shiga_mae_female_ets_EVR", "ind_Kyoto_mae_female_ets_EVR",
                                 "ind_Osaka_mae_female_ets_EVR", "ind_Hyogo_mae_female_ets_EVR", "ind_Nara_mae_female_ets_EVR",
                                 "ind_Wakayama_mae_female_ets_EVR", "ind_Tottori_mae_female_ets_EVR", "ind_Shimane_mae_female_ets_EVR",
                                 "ind_Okayama_mae_female_ets_EVR", "ind_Hiroshima_mae_female_ets_EVR", "ind_Yamaguchi_mae_female_ets_EVR",
                                 "ind_Tokushima_mae_female_ets_EVR", "ind_Kagawa_mae_female_ets_EVR", "ind_Ehime_mae_female_ets_EVR",
                                 "ind_Kochi_mae_female_ets_EVR", "ind_Fukuoka_mae_female_ets_EVR", "ind_Saga_mae_female_ets_EVR",
                                 "ind_Nagasaki_mae_female_ets_EVR", "ind_Kumamoto_mae_female_ets_EVR", "ind_Oita_mae_female_ets_EVR",
                                 "ind_Miyazaki_mae_female_ets_EVR", "ind_Kagoshima_mae_female_ets_EVR", "ind_Okinawa_mae_female_ets_EVR")

ind_state_mae_male_ets_EVR = c("ind_Hokkaido_mae_male_ets_EVR", "ind_Aomori_mae_male_ets_EVR",
                               "ind_Iwate_mae_male_ets_EVR", "ind_Miyagi_mae_male_ets_EVR", "ind_Akita_mae_male_ets_EVR",
                               "ind_Yamagata_mae_male_ets_EVR", "ind_Fukushima_mae_male_ets_EVR", "ind_Ibaraki_mae_male_ets_EVR",
                               "ind_Tochigi_mae_male_ets_EVR", "ind_Gunma_mae_male_ets_EVR", "ind_Saitama_mae_male_ets_EVR",
                               "ind_Chiba_mae_male_ets_EVR", "ind_Tokyo_mae_male_ets_EVR", "ind_Kanagawa_mae_male_ets_EVR",
                               "ind_Niigata_mae_male_ets_EVR", "ind_Toyama_mae_male_ets_EVR", "ind_Ishikawa_mae_male_ets_EVR",
                               "ind_Fukui_mae_male_ets_EVR", "ind_Yamanashi_mae_male_ets_EVR", "ind_Nagano_mae_male_ets_EVR",
                               "ind_Gifu_mae_male_ets_EVR", "ind_Shizuoka_mae_male_ets_EVR", "ind_Aichi_mae_male_ets_EVR",
                               "ind_Mie_mae_male_ets_EVR", "ind_Shiga_mae_male_ets_EVR", "ind_Kyoto_mae_male_ets_EVR",
                               "ind_Osaka_mae_male_ets_EVR", "ind_Hyogo_mae_male_ets_EVR", "ind_Nara_mae_male_ets_EVR",
                               "ind_Wakayama_mae_male_ets_EVR", "ind_Tottori_mae_male_ets_EVR", "ind_Shimane_mae_male_ets_EVR",
                               "ind_Okayama_mae_male_ets_EVR", "ind_Hiroshima_mae_male_ets_EVR", "ind_Yamaguchi_mae_male_ets_EVR",
                               "ind_Tokushima_mae_male_ets_EVR", "ind_Kagawa_mae_male_ets_EVR", "ind_Ehime_mae_male_ets_EVR",
                               "ind_Kochi_mae_male_ets_EVR", "ind_Fukuoka_mae_male_ets_EVR", "ind_Saga_mae_male_ets_EVR",
                               "ind_Nagasaki_mae_male_ets_EVR", "ind_Kumamoto_mae_male_ets_EVR", "ind_Oita_mae_male_ets_EVR",
                               "ind_Miyazaki_mae_male_ets_EVR", "ind_Kagoshima_mae_male_ets_EVR", "ind_Okinawa_mae_male_ets_EVR")

ind_state_mae_total_ets_EVR = c("ind_Hokkaido_mae_total_ets_EVR", "ind_Aomori_mae_total_ets_EVR",
                                "ind_Iwate_mae_total_ets_EVR", "ind_Miyagi_mae_total_ets_EVR", "ind_Akita_mae_total_ets_EVR",
                                "ind_Yamagata_mae_total_ets_EVR", "ind_Fukushima_mae_total_ets_EVR", "ind_Ibaraki_mae_total_ets_EVR",
                                "ind_Tochigi_mae_total_ets_EVR", "ind_Gunma_mae_total_ets_EVR", "ind_Saitama_mae_total_ets_EVR",
                                "ind_Chiba_mae_total_ets_EVR", "ind_Tokyo_mae_total_ets_EVR", "ind_Kanagawa_mae_total_ets_EVR",
                                "ind_Niigata_mae_total_ets_EVR", "ind_Toyama_mae_total_ets_EVR", "ind_Ishikawa_mae_total_ets_EVR",
                                "ind_Fukui_mae_total_ets_EVR", "ind_Yamanashi_mae_total_ets_EVR", "ind_Nagano_mae_total_ets_EVR",
                                "ind_Gifu_mae_total_ets_EVR", "ind_Shizuoka_mae_total_ets_EVR", "ind_Aichi_mae_total_ets_EVR",
                                "ind_Mie_mae_total_ets_EVR", "ind_Shiga_mae_total_ets_EVR", "ind_Kyoto_mae_total_ets_EVR",
                                "ind_Osaka_mae_total_ets_EVR", "ind_Hyogo_mae_total_ets_EVR", "ind_Nara_mae_total_ets_EVR",
                                "ind_Wakayama_mae_total_ets_EVR", "ind_Tottori_mae_total_ets_EVR", "ind_Shimane_mae_total_ets_EVR",
                                "ind_Okayama_mae_total_ets_EVR", "ind_Hiroshima_mae_total_ets_EVR", "ind_Yamaguchi_mae_total_ets_EVR",
                                "ind_Tokushima_mae_total_ets_EVR", "ind_Kagawa_mae_total_ets_EVR", "ind_Ehime_mae_total_ets_EVR",
                                "ind_Kochi_mae_total_ets_EVR", "ind_Fukuoka_mae_total_ets_EVR", "ind_Saga_mae_total_ets_EVR",
                                "ind_Nagasaki_mae_total_ets_EVR", "ind_Kumamoto_mae_total_ets_EVR", "ind_Oita_mae_total_ets_EVR",
                                "ind_Miyazaki_mae_total_ets_EVR", "ind_Kagoshima_mae_total_ets_EVR", "ind_Okinawa_mae_total_ets_EVR")

#######
# RMSE
#######

ind_state_rmse_female_ets_EVR = c("ind_Hokkaido_rmse_female_ets_EVR", "ind_Aomori_rmse_female_ets_EVR",
                                  "ind_Iwate_rmse_female_ets_EVR", "ind_Miyagi_rmse_female_ets_EVR", "ind_Akita_rmse_female_ets_EVR",
                                  "ind_Yamagata_rmse_female_ets_EVR", "ind_Fukushima_rmse_female_ets_EVR", "ind_Ibaraki_rmse_female_ets_EVR",
                                  "ind_Tochigi_rmse_female_ets_EVR", "ind_Gunma_rmse_female_ets_EVR", "ind_Saitama_rmse_female_ets_EVR",
                                  "ind_Chiba_rmse_female_ets_EVR", "ind_Tokyo_rmse_female_ets_EVR", "ind_Kanagawa_rmse_female_ets_EVR",
                                  "ind_Niigata_rmse_female_ets_EVR", "ind_Toyama_rmse_female_ets_EVR", "ind_Ishikawa_rmse_female_ets_EVR",
                                  "ind_Fukui_rmse_female_ets_EVR", "ind_Yamanashi_rmse_female_ets_EVR", "ind_Nagano_rmse_female_ets_EVR",
                                  "ind_Gifu_rmse_female_ets_EVR", "ind_Shizuoka_rmse_female_ets_EVR", "ind_Aichi_rmse_female_ets_EVR",
                                  "ind_Mie_rmse_female_ets_EVR", "ind_Shiga_rmse_female_ets_EVR", "ind_Kyoto_rmse_female_ets_EVR",
                                  "ind_Osaka_rmse_female_ets_EVR", "ind_Hyogo_rmse_female_ets_EVR", "ind_Nara_rmse_female_ets_EVR",
                                  "ind_Wakayama_rmse_female_ets_EVR", "ind_Tottori_rmse_female_ets_EVR", "ind_Shimane_rmse_female_ets_EVR",
                                  "ind_Okayama_rmse_female_ets_EVR", "ind_Hiroshima_rmse_female_ets_EVR", "ind_Yamaguchi_rmse_female_ets_EVR",
                                  "ind_Tokushima_rmse_female_ets_EVR", "ind_Kagawa_rmse_female_ets_EVR", "ind_Ehime_rmse_female_ets_EVR",
                                  "ind_Kochi_rmse_female_ets_EVR", "ind_Fukuoka_rmse_female_ets_EVR", "ind_Saga_rmse_female_ets_EVR",
                                  "ind_Nagasaki_rmse_female_ets_EVR", "ind_Kumamoto_rmse_female_ets_EVR", "ind_Oita_rmse_female_ets_EVR",
                                  "ind_Miyazaki_rmse_female_ets_EVR", "ind_Kagoshima_rmse_female_ets_EVR", "ind_Okinawa_rmse_female_ets_EVR")

ind_state_rmse_male_ets_EVR = c("ind_Hokkaido_rmse_male_ets_EVR", "ind_Aomori_rmse_male_ets_EVR",
                                "ind_Iwate_rmse_male_ets_EVR", "ind_Miyagi_rmse_male_ets_EVR", "ind_Akita_rmse_male_ets_EVR",
                                "ind_Yamagata_rmse_male_ets_EVR", "ind_Fukushima_rmse_male_ets_EVR", "ind_Ibaraki_rmse_male_ets_EVR",
                                "ind_Tochigi_rmse_male_ets_EVR", "ind_Gunma_rmse_male_ets_EVR", "ind_Saitama_rmse_male_ets_EVR",
                                "ind_Chiba_rmse_male_ets_EVR", "ind_Tokyo_rmse_male_ets_EVR", "ind_Kanagawa_rmse_male_ets_EVR",
                                "ind_Niigata_rmse_male_ets_EVR", "ind_Toyama_rmse_male_ets_EVR", "ind_Ishikawa_rmse_male_ets_EVR",
                                "ind_Fukui_rmse_male_ets_EVR", "ind_Yamanashi_rmse_male_ets_EVR", "ind_Nagano_rmse_male_ets_EVR",
                                "ind_Gifu_rmse_male_ets_EVR", "ind_Shizuoka_rmse_male_ets_EVR", "ind_Aichi_rmse_male_ets_EVR",
                                "ind_Mie_rmse_male_ets_EVR", "ind_Shiga_rmse_male_ets_EVR", "ind_Kyoto_rmse_male_ets_EVR",
                                "ind_Osaka_rmse_male_ets_EVR", "ind_Hyogo_rmse_male_ets_EVR", "ind_Nara_rmse_male_ets_EVR",
                                "ind_Wakayama_rmse_male_ets_EVR", "ind_Tottori_rmse_male_ets_EVR", "ind_Shimane_rmse_male_ets_EVR",
                                "ind_Okayama_rmse_male_ets_EVR", "ind_Hiroshima_rmse_male_ets_EVR", "ind_Yamaguchi_rmse_male_ets_EVR",
                                "ind_Tokushima_rmse_male_ets_EVR", "ind_Kagawa_rmse_male_ets_EVR", "ind_Ehime_rmse_male_ets_EVR",
                                "ind_Kochi_rmse_male_ets_EVR", "ind_Fukuoka_rmse_male_ets_EVR", "ind_Saga_rmse_male_ets_EVR",
                                "ind_Nagasaki_rmse_male_ets_EVR", "ind_Kumamoto_rmse_male_ets_EVR", "ind_Oita_rmse_male_ets_EVR",
                                "ind_Miyazaki_rmse_male_ets_EVR", "ind_Kagoshima_rmse_male_ets_EVR", "ind_Okinawa_rmse_male_ets_EVR")

ind_state_rmse_total_ets_EVR = c("ind_Hokkaido_rmse_total_ets_EVR", "ind_Aomori_rmse_total_ets_EVR",
                                 "ind_Iwate_rmse_total_ets_EVR", "ind_Miyagi_rmse_total_ets_EVR", "ind_Akita_rmse_total_ets_EVR",
                                 "ind_Yamagata_rmse_total_ets_EVR", "ind_Fukushima_rmse_total_ets_EVR", "ind_Ibaraki_rmse_total_ets_EVR",
                                 "ind_Tochigi_rmse_total_ets_EVR", "ind_Gunma_rmse_total_ets_EVR", "ind_Saitama_rmse_total_ets_EVR",
                                 "ind_Chiba_rmse_total_ets_EVR", "ind_Tokyo_rmse_total_ets_EVR", "ind_Kanagawa_rmse_total_ets_EVR",
                                 "ind_Niigata_rmse_total_ets_EVR", "ind_Toyama_rmse_total_ets_EVR", "ind_Ishikawa_rmse_total_ets_EVR",
                                 "ind_Fukui_rmse_total_ets_EVR", "ind_Yamanashi_rmse_total_ets_EVR", "ind_Nagano_rmse_total_ets_EVR",
                                 "ind_Gifu_rmse_total_ets_EVR", "ind_Shizuoka_rmse_total_ets_EVR", "ind_Aichi_rmse_total_ets_EVR",
                                 "ind_Mie_rmse_total_ets_EVR", "ind_Shiga_rmse_total_ets_EVR", "ind_Kyoto_rmse_total_ets_EVR",
                                 "ind_Osaka_rmse_total_ets_EVR", "ind_Hyogo_rmse_total_ets_EVR", "ind_Nara_rmse_total_ets_EVR",
                                 "ind_Wakayama_rmse_total_ets_EVR", "ind_Tottori_rmse_total_ets_EVR", "ind_Shimane_rmse_total_ets_EVR",
                                 "ind_Okayama_rmse_total_ets_EVR", "ind_Hiroshima_rmse_total_ets_EVR", "ind_Yamaguchi_rmse_total_ets_EVR",
                                 "ind_Tokushima_rmse_total_ets_EVR", "ind_Kagawa_rmse_total_ets_EVR", "ind_Ehime_rmse_total_ets_EVR",
                                 "ind_Kochi_rmse_total_ets_EVR", "ind_Fukuoka_rmse_total_ets_EVR", "ind_Saga_rmse_total_ets_EVR",
                                 "ind_Nagasaki_rmse_total_ets_EVR", "ind_Kumamoto_rmse_total_ets_EVR", "ind_Oita_rmse_total_ets_EVR",
                                 "ind_Miyazaki_rmse_total_ets_EVR", "ind_Kagoshima_rmse_total_ets_EVR", "ind_Okinawa_rmse_total_ets_EVR")

########################
# store forecast values
########################

ind_state_forc_female_ets_EVR = c("ind_Hokkaido_forc_female_ets_EVR", "ind_Aomori_forc_female_ets_EVR",
                                  "ind_Iwate_forc_female_ets_EVR", "ind_Miyagi_forc_female_ets_EVR", "ind_Akita_forc_female_ets_EVR",
                                  "ind_Yamagata_forc_female_ets_EVR", "ind_Fukushima_forc_female_ets_EVR", "ind_Ibaraki_forc_female_ets_EVR",
                                  "ind_Tochigi_forc_female_ets_EVR", "ind_Gunma_forc_female_ets_EVR", "ind_Saitama_forc_female_ets_EVR",
                                  "ind_Chiba_forc_female_ets_EVR", "ind_Tokyo_forc_female_ets_EVR", "ind_Kanagawa_forc_female_ets_EVR",
                                  "ind_Niigata_forc_female_ets_EVR", "ind_Toyama_forc_female_ets_EVR", "ind_Ishikawa_forc_female_ets_EVR",
                                  "ind_Fukui_forc_female_ets_EVR", "ind_Yamanashi_forc_female_ets_EVR", "ind_Nagano_forc_female_ets_EVR",
                                  "ind_Gifu_forc_female_ets_EVR", "ind_Shizuoka_forc_female_ets_EVR", "ind_Aichi_forc_female_ets_EVR",
                                  "ind_Mie_forc_female_ets_EVR", "ind_Shiga_forc_female_ets_EVR", "ind_Kyoto_forc_female_ets_EVR",
                                  "ind_Osaka_forc_female_ets_EVR", "ind_Hyogo_forc_female_ets_EVR", "ind_Nara_forc_female_ets_EVR",
                                  "ind_Wakayama_forc_female_ets_EVR", "ind_Tottori_forc_female_ets_EVR", "ind_Shimane_forc_female_ets_EVR",
                                  "ind_Okayama_forc_female_ets_EVR", "ind_Hiroshima_forc_female_ets_EVR", "ind_Yamaguchi_forc_female_ets_EVR",
                                  "ind_Tokushima_forc_female_ets_EVR", "ind_Kagawa_forc_female_ets_EVR", "ind_Ehime_forc_female_ets_EVR",
                                  "ind_Kochi_forc_female_ets_EVR", "ind_Fukuoka_forc_female_ets_EVR", "ind_Saga_forc_female_ets_EVR",
                                  "ind_Nagasaki_forc_female_ets_EVR", "ind_Kumamoto_forc_female_ets_EVR", "ind_Oita_forc_female_ets_EVR",
                                  "ind_Miyazaki_forc_female_ets_EVR", "ind_Kagoshima_forc_female_ets_EVR", "ind_Okinawa_forc_female_ets_EVR")

ind_state_forc_male_ets_EVR = c("ind_Hokkaido_forc_male_ets_EVR", "ind_Aomori_forc_male_ets_EVR",
                                "ind_Iwate_forc_male_ets_EVR", "ind_Miyagi_forc_male_ets_EVR", "ind_Akita_forc_male_ets_EVR",
                                "ind_Yamagata_forc_male_ets_EVR", "ind_Fukushima_forc_male_ets_EVR", "ind_Ibaraki_forc_male_ets_EVR",
                                "ind_Tochigi_forc_male_ets_EVR", "ind_Gunma_forc_male_ets_EVR", "ind_Saitama_forc_male_ets_EVR",
                                "ind_Chiba_forc_male_ets_EVR", "ind_Tokyo_forc_male_ets_EVR", "ind_Kanagawa_forc_male_ets_EVR",
                                "ind_Niigata_forc_male_ets_EVR", "ind_Toyama_forc_male_ets_EVR", "ind_Ishikawa_forc_male_ets_EVR",
                                "ind_Fukui_forc_male_ets_EVR", "ind_Yamanashi_forc_male_ets_EVR", "ind_Nagano_forc_male_ets_EVR",
                                "ind_Gifu_forc_male_ets_EVR", "ind_Shizuoka_forc_male_ets_EVR", "ind_Aichi_forc_male_ets_EVR",
                                "ind_Mie_forc_male_ets_EVR", "ind_Shiga_forc_male_ets_EVR", "ind_Kyoto_forc_male_ets_EVR",
                                "ind_Osaka_forc_male_ets_EVR", "ind_Hyogo_forc_male_ets_EVR", "ind_Nara_forc_male_ets_EVR",
                                "ind_Wakayama_forc_male_ets_EVR", "ind_Tottori_forc_male_ets_EVR", "ind_Shimane_forc_male_ets_EVR",
                                "ind_Okayama_forc_male_ets_EVR", "ind_Hiroshima_forc_male_ets_EVR", "ind_Yamaguchi_forc_male_ets_EVR",
                                "ind_Tokushima_forc_male_ets_EVR", "ind_Kagawa_forc_male_ets_EVR", "ind_Ehime_forc_male_ets_EVR",
                                "ind_Kochi_forc_male_ets_EVR", "ind_Fukuoka_forc_male_ets_EVR", "ind_Saga_forc_male_ets_EVR",
                                "ind_Nagasaki_forc_male_ets_EVR", "ind_Kumamoto_forc_male_ets_EVR", "ind_Oita_forc_male_ets_EVR",
                                "ind_Miyazaki_forc_male_ets_EVR", "ind_Kagoshima_forc_male_ets_EVR", "ind_Okinawa_forc_male_ets_EVR")

ind_state_forc_total_ets_EVR = c("ind_Hokkaido_forc_total_ets_EVR", "ind_Aomori_forc_total_ets_EVR",
                                 "ind_Iwate_forc_total_ets_EVR", "ind_Miyagi_forc_total_ets_EVR", "ind_Akita_forc_total_ets_EVR",
                                 "ind_Yamagata_forc_total_ets_EVR", "ind_Fukushima_forc_total_ets_EVR", "ind_Ibaraki_forc_total_ets_EVR",
                                 "ind_Tochigi_forc_total_ets_EVR", "ind_Gunma_forc_total_ets_EVR", "ind_Saitama_forc_total_ets_EVR",
                                 "ind_Chiba_forc_total_ets_EVR", "ind_Tokyo_forc_total_ets_EVR", "ind_Kanagawa_forc_total_ets_EVR",
                                 "ind_Niigata_forc_total_ets_EVR", "ind_Toyama_forc_total_ets_EVR", "ind_Ishikawa_forc_total_ets_EVR",
                                 "ind_Fukui_forc_total_ets_EVR", "ind_Yamanashi_forc_total_ets_EVR", "ind_Nagano_forc_total_ets_EVR",
                                 "ind_Gifu_forc_total_ets_EVR", "ind_Shizuoka_forc_total_ets_EVR", "ind_Aichi_forc_total_ets_EVR",
                                 "ind_Mie_forc_total_ets_EVR", "ind_Shiga_forc_total_ets_EVR", "ind_Kyoto_forc_total_ets_EVR",
                                 "ind_Osaka_forc_total_ets_EVR", "ind_Hyogo_forc_total_ets_EVR", "ind_Nara_forc_total_ets_EVR",
                                 "ind_Wakayama_forc_total_ets_EVR", "ind_Tottori_forc_total_ets_EVR", "ind_Shimane_forc_total_ets_EVR",
                                 "ind_Okayama_forc_total_ets_EVR", "ind_Hiroshima_forc_total_ets_EVR", "ind_Yamaguchi_forc_total_ets_EVR",
                                 "ind_Tokushima_forc_total_ets_EVR", "ind_Kagawa_forc_total_ets_EVR", "ind_Ehime_forc_total_ets_EVR",
                                 "ind_Kochi_forc_total_ets_EVR", "ind_Fukuoka_forc_total_ets_EVR", "ind_Saga_forc_total_ets_EVR",
                                 "ind_Nagasaki_forc_total_ets_EVR", "ind_Kumamoto_forc_total_ets_EVR", "ind_Oita_forc_total_ets_EVR",
                                 "ind_Miyazaki_forc_total_ets_EVR", "ind_Kagoshima_forc_total_ets_EVR", "ind_Okinawa_forc_total_ets_EVR")

ind_region_forc_female_ets_EVR = c("ind_region_R1_forc_female_ets_EVR", "ind_region_R2_forc_female_ets_EVR", "ind_region_R3_forc_female_ets_EVR",
                                   "ind_region_R4_forc_female_ets_EVR", "ind_region_R5_forc_female_ets_EVR", "ind_region_R6_forc_female_ets_EVR",
                                   "ind_region_R7_forc_female_ets_EVR", "ind_region_R8_forc_female_ets_EVR")

ind_region_forc_male_ets_EVR = c("ind_region_R1_forc_male_ets_EVR", "ind_region_R2_forc_male_ets_EVR", "ind_region_R3_forc_male_ets_EVR",
                                 "ind_region_R4_forc_male_ets_EVR", "ind_region_R5_forc_male_ets_EVR", "ind_region_R6_forc_male_ets_EVR",
                                 "ind_region_R7_forc_male_ets_EVR", "ind_region_R8_forc_male_ets_EVR")

ind_region_forc_total_ets_EVR = c("ind_region_R1_forc_total_ets_EVR", "ind_region_R2_forc_total_ets_EVR", "ind_region_R3_forc_total_ets_EVR",
                                  "ind_region_R4_forc_total_ets_EVR", "ind_region_R5_forc_total_ets_EVR", "ind_region_R6_forc_total_ets_EVR",
                                  "ind_region_R7_forc_total_ets_EVR", "ind_region_R8_forc_total_ets_EVR")

years = 1975:2024

## subnational results

# ets

res_F_mae_mat_ets_EVR = res_F_rmse_mat_ets_EVR =
res_M_mae_mat_ets_EVR = res_M_rmse_mat_ets_EVR =
res_T_mae_mat_ets_EVR = res_T_rmse_mat_ets_EVR = matrix(NA, n_state, 10)
for(iw in 1:n_state)
{
    # females

    res = ind_forecast_fun(data_var = state_smooth_F, index = iw, ncomp_method = "EVR", uni_fore_method = "ets")
    assign(ind_state_forc_female_ets_EVR[iw], res$fore_val)
    res_F_mae_mat_ets_EVR[iw,] = res$fore_mae
    res_F_rmse_mat_ets_EVR[iw,] = res$fore_rmse
    rm(res)

    # males

    res = ind_forecast_fun(data_var = state_smooth_M, index = iw, ncomp_method = "EVR", uni_fore_method = "ets")
    assign(ind_state_forc_male_ets_EVR[iw], res$fore_val)
    res_M_mae_mat_ets_EVR[iw,] = res$fore_mae
    res_M_rmse_mat_ets_EVR[iw,] = res$fore_rmse
    rm(res)

    # totals

    res = ind_forecast_fun(data_var = state_smooth_T, index = iw, ncomp_method = "EVR", uni_fore_method = "ets")
    assign(ind_state_forc_total_ets_EVR[iw], res$fore_val)
    res_T_mae_mat_ets_EVR[iw,] = res$fore_mae
    res_T_rmse_mat_ets_EVR[iw,] = res$fore_rmse
    rm(res); print(iw); rm(iw)
}
colnames(res_F_mae_mat_ets_EVR) = colnames(res_F_rmse_mat_ets_EVR) =
colnames(res_M_mae_mat_ets_EVR) = colnames(res_M_rmse_mat_ets_EVR) =
colnames(res_T_mae_mat_ets_EVR) = colnames(res_T_rmse_mat_ets_EVR) = 1:10

rownames(res_F_mae_mat_ets_EVR) = rownames(res_F_rmse_mat_ets_EVR) =
rownames(res_M_mae_mat_ets_EVR) = rownames(res_M_rmse_mat_ets_EVR) =
rownames(res_T_mae_mat_ets_EVR) = rownames(res_T_rmse_mat_ets_EVR) = state

## subnational results

# ets

n_region = length(region_smooth_F)

res_region_F_mae_mat_ets_EVR = res_region_F_rmse_mat_ets_EVR =
res_region_M_mae_mat_ets_EVR = res_region_M_rmse_mat_ets_EVR =
res_region_T_mae_mat_ets_EVR = res_region_T_rmse_mat_ets_EVR = matrix(NA, n_region, 10)
for(iw in 1:n_region)
{
    # females

    res = ind_forecast_fun(data_var = region_smooth_F, index = iw, ncomp_method = "EVR", uni_fore_method = "ets")
    assign(ind_region_forc_female_ets_EVR[iw], res$fore_val)
    res_region_F_mae_mat_ets_EVR[iw,] = res$fore_mae
    res_region_F_rmse_mat_ets_EVR[iw,] = res$fore_rmse
    rm(res)

    # males

    res = ind_forecast_fun(data_var = region_smooth_M, index = iw, ncomp_method = "EVR", uni_fore_method = "ets")
    assign(ind_region_forc_male_ets_EVR[iw], res$fore_val)
    res_region_M_mae_mat_ets_EVR[iw,] = res$fore_mae
    res_region_M_rmse_mat_ets_EVR[iw,] = res$fore_rmse
    rm(res)

    # totals

    res = ind_forecast_fun(data_var = region_smooth_T, index = iw, ncomp_method = "EVR", uni_fore_method = "ets")
    assign(ind_region_forc_total_ets_EVR[iw], res$fore_val)
    res_region_T_mae_mat_ets_EVR[iw,] = res$fore_mae
    res_region_T_rmse_mat_ets_EVR[iw,] = res$fore_rmse
    rm(res); print(iw); rm(iw)
}
rownames(res_region_F_mae_mat_ets_EVR) = rownames(res_region_F_rmse_mat_ets_EVR) =
rownames(res_region_M_mae_mat_ets_EVR) = rownames(res_region_M_rmse_mat_ets_EVR) =
rownames(res_region_T_mae_mat_ets_EVR) = rownames(res_region_T_rmse_mat_ets_EVR) = region

colnames(res_region_F_mae_mat_ets_EVR) = colnames(res_region_F_rmse_mat_ets_EVR) =
colnames(res_region_M_mae_mat_ets_EVR) = colnames(res_region_M_rmse_mat_ets_EVR) =
colnames(res_region_T_mae_mat_ets_EVR) = colnames(res_region_T_rmse_mat_ets_EVR) = 1:10


######################
# Japan national data
######################

Japan_F_fore_ets_EVR = Japan_M_fore_ets_EVR = Japan_T_fore_ets_EVR = array(NA, dim = c(10, 101, 10))
for(iw in 1:10)
{
    # females

    ind_dat = extract.years(Japan_demo_smooth_F, years = 1975:(2013 + iw))
    ncomp = select_K(tau = 10^-3, eigen(cov(t(ind_dat$rate$Female)))$values)
    fun_forc = forecast(fdm(ind_dat, series = "Female", order = ncomp), h = 10, method = "ets")
    Japan_F_fore_ets_EVR[,,iw] = t(fun_forc$rate$female)
    rm(ncomp)

    # males

    ind_dat = extract.years(Japan_demo_smooth_M, years = 1975:(2013 + iw))
    ncomp = select_K(tau = 10^-3, eigen(cov(t(ind_dat$rate$Male)))$values)
    fun_forc = forecast(fdm(ind_dat, series = "Male", order = ncomp), h = 10, method = "ets")
    Japan_M_fore_ets_EVR[,,iw] = t(fun_forc$rate$male)
    rm(ncomp)

    # total

    ind_dat = extract.years(Japan_demo_smooth_T, years = 1975:(2013 + iw))
    ncomp = select_K(tau = 10^-3, eigen(cov(t(ind_dat$rate$Total)))$values)
    fun_forc = forecast(fdm(ind_dat, series = "Total", order = ncomp), h = 10, method = "ets")
    Japan_T_fore_ets_EVR[,,iw] = t(fun_forc$rate$total)
    rm(iw); rm(ncomp)
}

# MAE and RMSE

Japan_F_fore_mae_ets_EVR = Japan_F_fore_rmse_ets_EVR =
Japan_M_fore_mae_ets_EVR = Japan_M_fore_rmse_ets_EVR =
Japan_T_fore_mae_ets_EVR = Japan_T_fore_rmse_ets_EVR = vector("numeric", 10)
for(k in 1:10)
{
    # females

    Japan_F_fore_mae_ets_EVR[k]  = mae(Japan_F_fore_ets_EVR[k,,1:(11 - k)],  extract.years(Japan_demo_F, years = (2014 + k):2024)$rate$Female)
    Japan_F_fore_rmse_ets_EVR[k] = rmse(Japan_F_fore_ets_EVR[k,,1:(11 - k)],  extract.years(Japan_demo_F, years = (2014 + k):2024)$rate$Female)

    # males

    Japan_M_fore_mae_ets_EVR[k]  = mae(Japan_M_fore_ets_EVR[k,,1:(11 - k)],  extract.years(Japan_demo_M, years = (2014 + k):2024)$rate$Male)
    Japan_M_fore_rmse_ets_EVR[k] = rmse(Japan_M_fore_ets_EVR[k,,1:(11 - k)],  extract.years(Japan_demo_M, years = (2014 + k):2024)$rate$Male)

    # totals

    Japan_T_fore_mae_ets_EVR[k]  = mae(Japan_T_fore_ets_EVR[k,,1:(11 - k)],  extract.years(Japan_demo_T, years = (2014 + k):2024)$rate$Total)
    Japan_T_fore_rmse_ets_EVR[k] = rmse(Japan_T_fore_ets_EVR[k,,1:(11 - k)],  extract.years(Japan_demo_T, years = (2014 + k):2024)$rate$Total)
    rm(k)
}
