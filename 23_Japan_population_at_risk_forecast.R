# ----------------------------------------------------------------------------
# 23_Japan_population_at_risk_forecast.R
#
# Forecast the exposure-to-risk ratios that populate the summing matrix at
# each forecast horizon.
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

################################################
# Forecast exposure-to-risk at the bottom-level
################################################

# top: exposure-to-risk for a given series at the bottom level
# bottom: exposure-to-risk for Japan total series
# year_horizon: forecast horizon

pop_fore_fun <- function(top, bottom, year_horizon)
{
    n_age = dim(top)[1]
    n_year = dim(top)[2]

    Japan_pop_fore = array(,dim = c(year_horizon, n_age, year_horizon))
    for(year in (n_year - 9):n_year)
    {
        Japan_pop_fore[1,2:n_age,(year - (n_year - 10))] = top[2:n_age,year]/bottom[2:n_age,year]

        # need forecasts birth

        ratio = top[1,1:year]/bottom[1,1:year]

        # forecasts birth

        # Japan_pop_fore[,1,(year-28)] =  forecast(ets(ratio), h = year_horizon)$mean

        Japan_pop_fore[,1,(year - (n_year - 10))] =  forecast(auto.arima(ratio), h = year_horizon)$mean

        for(ij in 2:year_horizon)
        {
          # x in year t is x+1 in year t+1

          Japan_pop_fore[ij,2:n_age,(year - (n_year - 10))] = Japan_pop_fore[ij-1, 1:(n_age - 1), (year - (n_year - 10))]
        }
    }
    return(Japan_pop_fore)
}

## Row 1

pop_ratio_P_F_to_T = c("pop_ratio_P1_F_to_T", "pop_ratio_P2_F_to_T", "pop_ratio_P3_F_to_T",
                       "pop_ratio_P4_F_to_T", "pop_ratio_P5_F_to_T", "pop_ratio_P6_F_to_T",
                       "pop_ratio_P7_F_to_T", "pop_ratio_P8_F_to_T", "pop_ratio_P9_F_to_T",
                       "pop_ratio_P10_F_to_T", "pop_ratio_P11_F_to_T", "pop_ratio_P12_F_to_T",
                       "pop_ratio_P13_F_to_T", "pop_ratio_P14_F_to_T", "pop_ratio_P15_F_to_T",
                       "pop_ratio_P16_F_to_T", "pop_ratio_P17_F_to_T", "pop_ratio_P18_F_to_T",
                       "pop_ratio_P19_F_to_T", "pop_ratio_P20_F_to_T", "pop_ratio_P21_F_to_T",
                       "pop_ratio_P22_F_to_T", "pop_ratio_P23_F_to_T", "pop_ratio_P24_F_to_T",
                       "pop_ratio_P25_F_to_T", "pop_ratio_P26_F_to_T", "pop_ratio_P27_F_to_T",
                       "pop_ratio_P28_F_to_T", "pop_ratio_P29_F_to_T", "pop_ratio_P30_F_to_T",
                       "pop_ratio_P31_F_to_T", "pop_ratio_P32_F_to_T", "pop_ratio_P33_F_to_T",
                       "pop_ratio_P34_F_to_T", "pop_ratio_P35_F_to_T", "pop_ratio_P36_F_to_T",
                       "pop_ratio_P37_F_to_T", "pop_ratio_P38_F_to_T", "pop_ratio_P39_F_to_T",
                       "pop_ratio_P40_F_to_T", "pop_ratio_P41_F_to_T", "pop_ratio_P42_F_to_T",
                       "pop_ratio_P43_F_to_T", "pop_ratio_P44_F_to_T", "pop_ratio_P45_F_to_T",
                       "pop_ratio_P46_F_to_T", "pop_ratio_P47_F_to_T")

pop_ratio_P_M_to_T = c("pop_ratio_P1_M_to_T",  "pop_ratio_P2_M_to_T",  "pop_ratio_P3_M_to_T",
                       "pop_ratio_P4_M_to_T",  "pop_ratio_P5_M_to_T",  "pop_ratio_P6_M_to_T",
                       "pop_ratio_P7_M_to_T",  "pop_ratio_P8_M_to_T",  "pop_ratio_P9_M_to_T",
                       "pop_ratio_P10_M_to_T", "pop_ratio_P11_M_to_T", "pop_ratio_P12_M_to_T",
                       "pop_ratio_P13_M_to_T", "pop_ratio_P14_M_to_T", "pop_ratio_P15_M_to_T",
                       "pop_ratio_P16_M_to_T", "pop_ratio_P17_M_to_T", "pop_ratio_P18_M_to_T",
                       "pop_ratio_P19_M_to_T", "pop_ratio_P20_M_to_T", "pop_ratio_P21_M_to_T",
                       "pop_ratio_P22_M_to_T", "pop_ratio_P23_M_to_T", "pop_ratio_P24_M_to_T",
                       "pop_ratio_P25_M_to_T", "pop_ratio_P26_M_to_T", "pop_ratio_P27_M_to_T",
                       "pop_ratio_P28_M_to_T", "pop_ratio_P29_M_to_T", "pop_ratio_P30_M_to_T",
                       "pop_ratio_P31_M_to_T", "pop_ratio_P32_M_to_T", "pop_ratio_P33_M_to_T",
                       "pop_ratio_P34_M_to_T", "pop_ratio_P35_M_to_T", "pop_ratio_P36_M_to_T",
                       "pop_ratio_P37_M_to_T", "pop_ratio_P38_M_to_T", "pop_ratio_P39_M_to_T",
                       "pop_ratio_P40_M_to_T", "pop_ratio_P41_M_to_T", "pop_ratio_P42_M_to_T",
                       "pop_ratio_P43_M_to_T", "pop_ratio_P44_M_to_T", "pop_ratio_P45_M_to_T",
                       "pop_ratio_P46_M_to_T", "pop_ratio_P47_M_to_T")

for(iw in 1:n_state)
{
    assign(pop_ratio_P_F_to_T[iw], pop_fore_fun(top = get(state[iw])$pop$female, bottom = Japan_demo_T$pop$Total, year_horizon = 10))
    assign(pop_ratio_P_M_to_T[iw], pop_fore_fun(top = get(state[iw])$pop$male,   bottom = Japan_demo_T$pop$Total, year_horizon = 10))
    rm(iw)
}

## Row 2

pop_ratio_P_F_to_F = c("pop_ratio_P1_F_to_F", "pop_ratio_P2_F_to_F", "pop_ratio_P3_F_to_F",
                       "pop_ratio_P4_F_to_F", "pop_ratio_P5_F_to_F", "pop_ratio_P6_F_to_F",
                       "pop_ratio_P7_F_to_F", "pop_ratio_P8_F_to_F", "pop_ratio_P9_F_to_F",
                       "pop_ratio_P10_F_to_F", "pop_ratio_P11_F_to_F", "pop_ratio_P12_F_to_F",
                       "pop_ratio_P13_F_to_F", "pop_ratio_P14_F_to_F", "pop_ratio_P15_F_to_F",
                       "pop_ratio_P16_F_to_F", "pop_ratio_P17_F_to_F", "pop_ratio_P18_F_to_F",
                       "pop_ratio_P19_F_to_F", "pop_ratio_P20_F_to_F", "pop_ratio_P21_F_to_F",
                       "pop_ratio_P22_F_to_F", "pop_ratio_P23_F_to_F", "pop_ratio_P24_F_to_F",
                       "pop_ratio_P25_F_to_F", "pop_ratio_P26_F_to_F", "pop_ratio_P27_F_to_F",
                       "pop_ratio_P28_F_to_F", "pop_ratio_P29_F_to_F", "pop_ratio_P30_F_to_F",
                       "pop_ratio_P31_F_to_F", "pop_ratio_P32_F_to_F", "pop_ratio_P33_F_to_F",
                       "pop_ratio_P34_F_to_F", "pop_ratio_P35_F_to_F", "pop_ratio_P36_F_to_F",
                       "pop_ratio_P37_F_to_F", "pop_ratio_P38_F_to_F", "pop_ratio_P39_F_to_F",
                       "pop_ratio_P40_F_to_F", "pop_ratio_P41_F_to_F", "pop_ratio_P42_F_to_F",
                       "pop_ratio_P43_F_to_F", "pop_ratio_P44_F_to_F", "pop_ratio_P45_F_to_F",
                       "pop_ratio_P46_F_to_F", "pop_ratio_P47_F_to_F")

## Row 3

pop_ratio_P_M_to_M = c("pop_ratio_P1_M_to_M",  "pop_ratio_P2_M_to_M",  "pop_ratio_P3_M_to_M",
                       "pop_ratio_P4_M_to_M",  "pop_ratio_P5_M_to_M",  "pop_ratio_P6_M_to_M",
                       "pop_ratio_P7_M_to_M",  "pop_ratio_P8_M_to_M",  "pop_ratio_P9_M_to_M",
                       "pop_ratio_P10_M_to_M", "pop_ratio_P11_M_to_M", "pop_ratio_P12_M_to_M",
                       "pop_ratio_P13_M_to_M", "pop_ratio_P14_M_to_M", "pop_ratio_P15_M_to_M",
                       "pop_ratio_P16_M_to_M", "pop_ratio_P17_M_to_M", "pop_ratio_P18_M_to_M",
                       "pop_ratio_P19_M_to_M", "pop_ratio_P20_M_to_M", "pop_ratio_P21_M_to_M",
                       "pop_ratio_P22_M_to_M", "pop_ratio_P23_M_to_M", "pop_ratio_P24_M_to_M",
                       "pop_ratio_P25_M_to_M", "pop_ratio_P26_M_to_M", "pop_ratio_P27_M_to_M",
                       "pop_ratio_P28_M_to_M", "pop_ratio_P29_M_to_M", "pop_ratio_P30_M_to_M",
                       "pop_ratio_P31_M_to_M", "pop_ratio_P32_M_to_M", "pop_ratio_P33_M_to_M",
                       "pop_ratio_P34_M_to_M", "pop_ratio_P35_M_to_M", "pop_ratio_P36_M_to_M",
                       "pop_ratio_P37_M_to_M", "pop_ratio_P38_M_to_M", "pop_ratio_P39_M_to_M",
                       "pop_ratio_P40_M_to_M", "pop_ratio_P41_M_to_M", "pop_ratio_P42_M_to_M",
                       "pop_ratio_P43_M_to_M", "pop_ratio_P44_M_to_M", "pop_ratio_P45_M_to_M",
                       "pop_ratio_P46_M_to_M", "pop_ratio_P47_M_to_M")

for(iw in 1:n_state)
{
    assign(pop_ratio_P_F_to_F[iw], pop_fore_fun(top = get(state[iw])$pop$female, bottom = Japan_demo_F$pop$Female, year_horizon = 10))
    assign(pop_ratio_P_M_to_M[iw], pop_fore_fun(top = get(state[iw])$pop$male,   bottom = Japan_demo_M$pop$Male, year_horizon = 10))
    rm(iw)
}

## Row 4

pop_ratio_P_F_to_R1_T = "pop_ratio_P1_F_to_R1_T"
pop_ratio_P_F_to_R2_T = c("pop_ratio_P2_F_to_R2_T", "pop_ratio_P3_F_to_R2_T", "pop_ratio_P4_F_to_R2_T",
                          "pop_ratio_P5_F_to_R2_T", "pop_ratio_P6_F_to_R2_T", "pop_ratio_P7_F_to_R2_T")
pop_ratio_P_F_to_R3_T = c("pop_ratio_P8_F_to_R3_T", "pop_ratio_P9_F_to_R3_T", "pop_ratio_P10_F_to_R3_T",
                          "pop_ratio_P11_F_to_R3_T", "pop_ratio_P12_F_to_R3_T", "pop_ratio_P13_F_to_R3_T",
                          "pop_ratio_P14_F_to_R3_T")
pop_ratio_P_F_to_R4_T = c("pop_ratio_P15_F_to_R4_T", "pop_ratio_P16_F_to_R4_T", "pop_ratio_P17_F_to_R4_T",
                          "pop_ratio_P18_F_to_R4_T", "pop_ratio_P19_F_to_R4_T", "pop_ratio_P20_F_to_R4_T",
                          "pop_ratio_P21_F_to_R4_T", "pop_ratio_P22_F_to_R4_T", "pop_ratio_P23_F_to_R4_T")
pop_ratio_P_F_to_R5_T = c("pop_ratio_P24_F_to_R5_T", "pop_ratio_P25_F_to_R5_T", "pop_ratio_P26_F_to_R5_T",
                          "pop_ratio_P27_F_to_R5_T", "pop_ratio_P28_F_to_R5_T", "pop_ratio_P29_F_to_R5_T",
                          "pop_ratio_P30_F_to_R5_T")
pop_ratio_P_F_to_R6_T = c("pop_ratio_P31_F_to_R6_T", "pop_ratio_P32_F_to_R6_T", "pop_ratio_P33_F_to_R6_T",
                          "pop_ratio_P34_F_to_R6_T", "pop_ratio_P35_F_to_R6_T")
pop_ratio_P_F_to_R7_T = c("pop_ratio_P36_F_to_R7_T", "pop_ratio_P37_F_to_R7_T", "pop_ratio_P38_F_to_R7_T",
                          "pop_ratio_P39_F_to_R7_T")
pop_ratio_P_F_to_R8_T = c("pop_ratio_P40_F_to_R8_T", "pop_ratio_P41_F_to_R8_T", "pop_ratio_P42_F_to_R8_T",
                          "pop_ratio_P43_F_to_R8_T", "pop_ratio_P44_F_to_R8_T", "pop_ratio_P45_F_to_R8_T",
                          "pop_ratio_P46_F_to_R8_T", "pop_ratio_P47_F_to_R8_T")

assign(pop_ratio_P_F_to_R1_T, pop_fore_fun(top = get(state[1])$pop$female, bottom = ind_R1$pop$total, year_horizon = 10))
for(iw in 2:7)
{
    assign(pop_ratio_P_F_to_R2_T[iw-1], pop_fore_fun(top = get(state[iw])$pop$female, bottom = ind_R2$pop$total, year_horizon = 10))
    rm(iw)
}
for(iw in 8:14)
{
    assign(pop_ratio_P_F_to_R3_T[iw-7], pop_fore_fun(top = get(state[iw])$pop$female, bottom = ind_R3$pop$total, year_horizon = 10))
    rm(iw)
}
for(iw in 15:23)
{
    assign(pop_ratio_P_F_to_R4_T[iw-14], pop_fore_fun(top = get(state[iw])$pop$female, bottom = ind_R4$pop$total, year_horizon = 10))
    rm(iw)
}
for(iw in 24:30)
{
    assign(pop_ratio_P_F_to_R5_T[iw-23], pop_fore_fun(top = get(state[iw])$pop$female, bottom = ind_R5$pop$total, year_horizon = 10))
    rm(iw)
}
for(iw in 31:35)
{
    assign(pop_ratio_P_F_to_R6_T[iw-30], pop_fore_fun(top = get(state[iw])$pop$female, bottom = ind_R6$pop$total, year_horizon = 10))
    rm(iw)
}
for(iw in 36:39)
{
    assign(pop_ratio_P_F_to_R7_T[iw-35], pop_fore_fun(top = get(state[iw])$pop$female, bottom = ind_R7$pop$total, year_horizon = 10))
    rm(iw)
}
for(iw in 40:47)
{
    assign(pop_ratio_P_F_to_R8_T[iw-39], pop_fore_fun(top = get(state[iw])$pop$female, bottom = ind_R8$pop$total, year_horizon = 10))
    rm(iw)
}


pop_ratio_P_M_to_R1_T = "pop_ratio_P1_M_to_R1_T"
pop_ratio_P_M_to_R2_T = c("pop_ratio_P2_M_to_R2_T", "pop_ratio_P3_M_to_R2_T", "pop_ratio_P4_M_to_R2_T",
                          "pop_ratio_P5_M_to_R2_T", "pop_ratio_P6_M_to_R2_T", "pop_ratio_P7_M_to_R2_T")
pop_ratio_P_M_to_R3_T = c("pop_ratio_P8_M_to_R3_T", "pop_ratio_P9_M_to_R3_T", "pop_ratio_P10_M_to_R3_T",
                          "pop_ratio_P11_M_to_R3_T", "pop_ratio_P12_M_to_R3_T", "pop_ratio_P13_M_to_R3_T",
                          "pop_ratio_P14_M_to_R3_T")
pop_ratio_P_M_to_R4_T = c("pop_ratio_P15_M_to_R4_T", "pop_ratio_P16_M_to_R4_T", "pop_ratio_P17_M_to_R4_T",
                          "pop_ratio_P18_M_to_R4_T", "pop_ratio_P19_M_to_R4_T", "pop_ratio_P20_M_to_R4_T",
                          "pop_ratio_P21_M_to_R4_T", "pop_ratio_P22_M_to_R4_T", "pop_ratio_P23_M_to_R4_T")
pop_ratio_P_M_to_R5_T = c("pop_ratio_P24_M_to_R5_T", "pop_ratio_P25_M_to_R5_T", "pop_ratio_P26_M_to_R5_T",
                          "pop_ratio_P27_M_to_R5_T", "pop_ratio_P28_M_to_R5_T", "pop_ratio_P29_M_to_R5_T",
                          "pop_ratio_P30_M_to_R5_T")
pop_ratio_P_M_to_R6_T = c("pop_ratio_P31_M_to_R6_T", "pop_ratio_P32_M_to_R6_T", "pop_ratio_P33_M_to_R6_T",
                          "pop_ratio_P34_M_to_R6_T", "pop_ratio_P35_M_to_R6_T")
pop_ratio_P_M_to_R7_T = c("pop_ratio_P36_M_to_R7_T", "pop_ratio_P37_M_to_R7_T", "pop_ratio_P38_M_to_R7_T",
                          "pop_ratio_P39_M_to_R7_T")
pop_ratio_P_M_to_R8_T = c("pop_ratio_P40_M_to_R8_T", "pop_ratio_P41_M_to_R8_T", "pop_ratio_P42_M_to_R8_T",
                          "pop_ratio_P43_M_to_R8_T", "pop_ratio_P44_M_to_R8_T", "pop_ratio_P45_M_to_R8_T",
                          "pop_ratio_P46_M_to_R8_T", "pop_ratio_P47_M_to_R8_T")

assign(pop_ratio_P_M_to_R1_T, pop_fore_fun(top = get(state[1])$pop$male, bottom = ind_R1$pop$total, year_horizon = 10))
for(iw in 2:7)
{
    assign(pop_ratio_P_M_to_R2_T[iw-1], pop_fore_fun(top = get(state[iw])$pop$male, bottom = ind_R2$pop$total, year_horizon = 10))
    rm(iw)
}
for(iw in 8:14)
{
    assign(pop_ratio_P_M_to_R3_T[iw-7], pop_fore_fun(top = get(state[iw])$pop$male, bottom = ind_R3$pop$total, year_horizon = 10))
    rm(iw)
}
for(iw in 15:23)
{
    assign(pop_ratio_P_M_to_R4_T[iw-14], pop_fore_fun(top = get(state[iw])$pop$male, bottom = ind_R4$pop$total, year_horizon = 10))
    rm(iw)
}
for(iw in 24:30)
{
    assign(pop_ratio_P_M_to_R5_T[iw-23], pop_fore_fun(top = get(state[iw])$pop$male, bottom = ind_R5$pop$total, year_horizon = 10))
    rm(iw)
}
for(iw in 31:35)
{
    assign(pop_ratio_P_M_to_R6_T[iw-30], pop_fore_fun(top = get(state[iw])$pop$male, bottom = ind_R6$pop$total, year_horizon = 10))
    rm(iw)
}
for(iw in 36:39)
{
    assign(pop_ratio_P_M_to_R7_T[iw-35], pop_fore_fun(top = get(state[iw])$pop$male, bottom = ind_R7$pop$total, year_horizon = 10))
    rm(iw)
}
for(iw in 40:47)
{
    assign(pop_ratio_P_M_to_R8_T[iw-39], pop_fore_fun(top = get(state[iw])$pop$male, bottom = ind_R8$pop$total, year_horizon = 10))
    rm(iw)
}

## `Row' 5

pop_ratio_P_F_to_R1_F = "pop_ratio_P1_F_to_R1_F"
pop_ratio_P_F_to_R2_F = c("pop_ratio_P2_F_to_R2_F", "pop_ratio_P3_F_to_R2_F", "pop_ratio_P4_F_to_R2_F",
                          "pop_ratio_P5_F_to_R2_F", "pop_ratio_P6_F_to_R2_F", "pop_ratio_P7_F_to_R2_F")
pop_ratio_P_F_to_R3_F = c("pop_ratio_P8_F_to_R3_F", "pop_ratio_P9_F_to_R3_F", "pop_ratio_P10_F_to_R3_F",
                          "pop_ratio_P11_F_to_R3_F", "pop_ratio_P12_F_to_R3_F", "pop_ratio_P13_F_to_R3_F",
                          "pop_ratio_P14_F_to_R3_F")
pop_ratio_P_F_to_R4_F = c("pop_ratio_P15_F_to_R4_F", "pop_ratio_P16_F_to_R4_F", "pop_ratio_P17_F_to_R4_F",
                          "pop_ratio_P18_F_to_R4_F", "pop_ratio_P19_F_to_R4_F", "pop_ratio_P20_F_to_R4_F",
                          "pop_ratio_P21_F_to_R4_F", "pop_ratio_P22_F_to_R4_F", "pop_ratio_P23_F_to_R4_F")
pop_ratio_P_F_to_R5_F = c("pop_ratio_P24_F_to_R5_F", "pop_ratio_P25_F_to_R5_F", "pop_ratio_P26_F_to_R5_F",
                          "pop_ratio_P27_F_to_R5_F", "pop_ratio_P28_F_to_R5_F", "pop_ratio_P29_F_to_R5_F",
                          "pop_ratio_P30_F_to_R5_F")
pop_ratio_P_F_to_R6_F = c("pop_ratio_P31_F_to_R6_F", "pop_ratio_P32_F_to_R6_F", "pop_ratio_P33_F_to_R6_F",
                          "pop_ratio_P34_F_to_R6_F", "pop_ratio_P35_F_to_R6_F")
pop_ratio_P_F_to_R7_F = c("pop_ratio_P36_F_to_R7_F", "pop_ratio_P37_F_to_R7_F", "pop_ratio_P38_F_to_R7_F",
                          "pop_ratio_P39_F_to_R7_F")
pop_ratio_P_F_to_R8_F = c("pop_ratio_P40_F_to_R8_F", "pop_ratio_P41_F_to_R8_F", "pop_ratio_P42_F_to_R8_F",
                          "pop_ratio_P43_F_to_R8_F", "pop_ratio_P44_F_to_R8_F", "pop_ratio_P45_F_to_R8_F",
                          "pop_ratio_P46_F_to_R8_F", "pop_ratio_P47_F_to_R8_F")

assign(pop_ratio_P_F_to_R1_F, pop_fore_fun(top = get(state[1])$pop$female, bottom = ind_R1$pop$female, year_horizon = 10))
for(iw in 2:7)
{
    assign(pop_ratio_P_F_to_R2_F[iw-1], pop_fore_fun(top = get(state[iw])$pop$female, bottom = ind_R2$pop$female, year_horizon = 10))
    rm(iw)
}
for(iw in 8:14)
{
    assign(pop_ratio_P_F_to_R3_F[iw-7], pop_fore_fun(top = get(state[iw])$pop$female, bottom = ind_R3$pop$female, year_horizon = 10))
    rm(iw)
}
for(iw in 15:23)
{
    assign(pop_ratio_P_F_to_R4_F[iw-14], pop_fore_fun(top = get(state[iw])$pop$female, bottom = ind_R4$pop$female, year_horizon = 10))
    rm(iw)
}
for(iw in 24:30)
{
    assign(pop_ratio_P_F_to_R5_F[iw-23], pop_fore_fun(top = get(state[iw])$pop$female, bottom = ind_R5$pop$female, year_horizon = 10))
    rm(iw)
}
for(iw in 31:35)
{
    assign(pop_ratio_P_F_to_R6_F[iw-30], pop_fore_fun(top = get(state[iw])$pop$female, bottom = ind_R6$pop$female, year_horizon = 10))
    rm(iw)
}
for(iw in 36:39)
{
    assign(pop_ratio_P_F_to_R7_F[iw-35], pop_fore_fun(top = get(state[iw])$pop$female, bottom = ind_R7$pop$female, year_horizon = 10))
    rm(iw)
}
for(iw in 40:47)
{
    assign(pop_ratio_P_F_to_R8_F[iw-39], pop_fore_fun(top = get(state[iw])$pop$female, bottom = ind_R8$pop$female, year_horizon = 10))
    rm(iw)
}

## Row 6

pop_ratio_P_M_to_R1_M = "pop_ratio_P1_M_to_R1_M"
pop_ratio_P_M_to_R2_M = c("pop_ratio_P2_M_to_R2_M", "pop_ratio_P3_M_to_R2_M", "pop_ratio_P4_M_to_R2_M",
                          "pop_ratio_P5_M_to_R2_M", "pop_ratio_P6_M_to_R2_M", "pop_ratio_P7_M_to_R2_M")
pop_ratio_P_M_to_R3_M = c("pop_ratio_P8_M_to_R3_M", "pop_ratio_P9_M_to_R3_M", "pop_ratio_P10_M_to_R3_M",
                          "pop_ratio_P11_M_to_R3_M", "pop_ratio_P12_M_to_R3_M", "pop_ratio_P13_M_to_R3_M",
                          "pop_ratio_P14_M_to_R3_M")
pop_ratio_P_M_to_R4_M = c("pop_ratio_P15_M_to_R4_M", "pop_ratio_P16_M_to_R4_M", "pop_ratio_P17_M_to_R4_M",
                          "pop_ratio_P18_M_to_R4_M", "pop_ratio_P19_M_to_R4_M", "pop_ratio_P20_M_to_R4_M",
                          "pop_ratio_P21_M_to_R4_M", "pop_ratio_P22_M_to_R4_M", "pop_ratio_P23_M_to_R4_M")
pop_ratio_P_M_to_R5_M = c("pop_ratio_P24_M_to_R5_M", "pop_ratio_P25_M_to_R5_M", "pop_ratio_P26_M_to_R5_M",
                          "pop_ratio_P27_M_to_R5_M", "pop_ratio_P28_M_to_R5_M", "pop_ratio_P29_M_to_R5_M",
                          "pop_ratio_P30_M_to_R5_M")
pop_ratio_P_M_to_R6_M = c("pop_ratio_P31_M_to_R6_M", "pop_ratio_P32_M_to_R6_M", "pop_ratio_P33_M_to_R6_M",
                          "pop_ratio_P34_M_to_R6_M", "pop_ratio_P35_M_to_R6_M")
pop_ratio_P_M_to_R7_M = c("pop_ratio_P36_M_to_R7_M", "pop_ratio_P37_M_to_R7_M", "pop_ratio_P38_M_to_R7_M",
                          "pop_ratio_P39_M_to_R7_M")
pop_ratio_P_M_to_R8_M = c("pop_ratio_P40_M_to_R8_M", "pop_ratio_P41_M_to_R8_M", "pop_ratio_P42_M_to_R8_M",
                          "pop_ratio_P43_M_to_R8_M", "pop_ratio_P44_M_to_R8_M", "pop_ratio_P45_M_to_R8_M",
                          "pop_ratio_P46_M_to_R8_M", "pop_ratio_P47_M_to_R8_M")

assign(pop_ratio_P_M_to_R1_M, pop_fore_fun(top = get(state[1])$pop$male, bottom = ind_R1$pop$male, year_horizon = 10))
for(iw in 2:7)
{
    assign(pop_ratio_P_M_to_R2_M[iw-1], pop_fore_fun(top = get(state[iw])$pop$male, bottom = ind_R2$pop$male, year_horizon = 10))
    rm(iw)
}
for(iw in 8:14)
{
    assign(pop_ratio_P_M_to_R3_M[iw-7], pop_fore_fun(top = get(state[iw])$pop$male, bottom = ind_R3$pop$male, year_horizon = 10))
    rm(iw)
}
for(iw in 15:23)
{
    assign(pop_ratio_P_M_to_R4_M[iw-14], pop_fore_fun(top = get(state[iw])$pop$male, bottom = ind_R4$pop$male, year_horizon = 10))
    rm(iw)
}
for(iw in 24:30)
{
    assign(pop_ratio_P_M_to_R5_M[iw-23], pop_fore_fun(top = get(state[iw])$pop$male, bottom = ind_R5$pop$male, year_horizon = 10))
    rm(iw)
}
for(iw in 31:35)
{
    assign(pop_ratio_P_M_to_R6_M[iw-30], pop_fore_fun(top = get(state[iw])$pop$male, bottom = ind_R6$pop$male, year_horizon = 10))
    rm(iw)
}
for(iw in 36:39)
{
    assign(pop_ratio_P_M_to_R7_M[iw-35], pop_fore_fun(top = get(state[iw])$pop$male, bottom = ind_R7$pop$male, year_horizon = 10))
    rm(iw)
}
for(iw in 40:47)
{
    assign(pop_ratio_P_M_to_R8_M[iw-39], pop_fore_fun(top = get(state[iw])$pop$male, bottom = ind_R8$pop$male, year_horizon = 10))
    rm(iw)
}

## Row 7

pop_ratio_P_F_to_P_T = c("pop_ratio_P1_F_to_P1_T", "pop_ratio_P2_F_to_P2_T", "pop_ratio_P3_F_to_P3_T",
                         "pop_ratio_P4_F_to_P4_T", "pop_ratio_P5_F_to_P5_T", "pop_ratio_P6_F_to_P6_T",
                         "pop_ratio_P7_F_to_P7_T", "pop_ratio_P8_F_to_P8_T", "pop_ratio_P9_F_to_P9_T",
                         "pop_ratio_P10_F_to_P10_T", "pop_ratio_P11_F_to_P11_T", "pop_ratio_P12_F_to_P12_T",
                         "pop_ratio_P13_F_to_P13_T", "pop_ratio_P14_F_to_P14_T",
                         "pop_ratio_P15_F_to_P15_T", "pop_ratio_P16_F_to_P16_T", "pop_ratio_P17_F_to_P17_T",
                         "pop_ratio_P18_F_to_P18_T", "pop_ratio_P19_F_to_P19_T", "pop_ratio_P20_F_to_P20_T",
                         "pop_ratio_P21_F_to_P21_T", "pop_ratio_P22_F_to_P22_T", "pop_ratio_P23_F_to_P23_T",
                         "pop_ratio_P24_F_to_P24_T", "pop_ratio_P25_F_to_P25_T", "pop_ratio_P26_F_to_P26_T",
                         "pop_ratio_P27_F_to_P27_T", "pop_ratio_P28_F_to_P28_T", "pop_ratio_P29_F_to_P29_T",
                         "pop_ratio_P30_F_to_P30_T", "pop_ratio_P31_F_to_P31_T", "pop_ratio_P32_F_to_P32_T",
                         "pop_ratio_P33_F_to_P33_T", "pop_ratio_P34_F_to_P34_T", "pop_ratio_P35_F_to_P35_T",
                         "pop_ratio_P36_F_to_P36_T", "pop_ratio_P37_F_to_P37_T", "pop_ratio_P38_F_to_P38_T",
                         "pop_ratio_P39_F_to_P39_T", "pop_ratio_P40_F_to_P40_T", "pop_ratio_P41_F_to_P41_T",
                         "pop_ratio_P42_F_to_P42_T", "pop_ratio_P43_F_to_P43_T", "pop_ratio_P44_F_to_P44_T",
                         "pop_ratio_P45_F_to_P45_T", "pop_ratio_P46_F_to_P46_T", "pop_ratio_P47_F_to_P47_T")

pop_ratio_P_M_to_P_T = c("pop_ratio_P1_M_to_P1_T", "pop_ratio_P2_M_to_P2_T", "pop_ratio_P3_M_to_P3_T",
                         "pop_ratio_P4_M_to_P4_T", "pop_ratio_P5_M_to_P5_T", "pop_ratio_P6_M_to_P6_T",
                         "pop_ratio_P7_M_to_P7_T", "pop_ratio_P8_M_to_P8_T", "pop_ratio_P9_M_to_P9_T",
                         "pop_ratio_P10_M_to_P10_T", "pop_ratio_P11_M_to_P11_T", "pop_ratio_P12_M_to_P12_T",
                         "pop_ratio_P13_M_to_P13_T", "pop_ratio_P14_M_to_P14_T",
                         "pop_ratio_P15_M_to_P15_T", "pop_ratio_P16_M_to_P16_T", "pop_ratio_P17_M_to_P17_T",
                         "pop_ratio_P18_M_to_P18_T", "pop_ratio_P19_M_to_P19_T", "pop_ratio_P20_M_to_P20_T",
                         "pop_ratio_P21_M_to_P21_T", "pop_ratio_P22_M_to_P22_T", "pop_ratio_P23_M_to_P23_T",
                         "pop_ratio_P24_M_to_P24_T", "pop_ratio_P25_M_to_P25_T", "pop_ratio_P26_M_to_P26_T",
                         "pop_ratio_P27_M_to_P27_T", "pop_ratio_P28_M_to_P28_T", "pop_ratio_P29_M_to_P29_T",
                         "pop_ratio_P30_M_to_P30_T", "pop_ratio_P31_M_to_P31_T", "pop_ratio_P32_M_to_P32_T",
                         "pop_ratio_P33_M_to_P33_T", "pop_ratio_P34_M_to_P34_T", "pop_ratio_P35_M_to_P35_T",
                         "pop_ratio_P36_M_to_P36_T", "pop_ratio_P37_M_to_P37_T", "pop_ratio_P38_M_to_P38_T",
                         "pop_ratio_P39_M_to_P39_T", "pop_ratio_P40_M_to_P40_T", "pop_ratio_P41_M_to_P41_T",
                         "pop_ratio_P42_M_to_P42_T", "pop_ratio_P43_M_to_P43_T", "pop_ratio_P44_M_to_P44_T",
                         "pop_ratio_P45_M_to_P45_T", "pop_ratio_P46_M_to_P46_T", "pop_ratio_P47_M_to_P47_T")

for(iw in 1:n_state)
{
    assign(pop_ratio_P_F_to_P_T[iw], pop_fore_fun(top = get(state[iw])$pop$female, bottom = get(state[iw])$pop$total, year_horizon = 10))
    assign(pop_ratio_P_M_to_P_T[iw], pop_fore_fun(top = get(state[iw])$pop$male,   bottom = get(state[iw])$pop$total, year_horizon = 10))
    rm(iw)
}
