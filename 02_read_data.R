# ----------------------------------------------------------------------------
# 02_read_data.R
#
# Download Japanese Mortality Database rates and exposures, write the plain-
# text copies in read_data/, and build the demogdata objects used throughout
# (national, 8 regions, 47 prefectures; female, male, total).
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

# load R packages

source("01_load_packages.R")

# The raw text copies are written to, and read back from, a read_data/
# sub-folder, which this script creates on first run.
project_root = getwd()
data_dir = file.path(project_root, "read_data")
dir.create(data_dir, showWarnings = FALSE)
setwd(data_dir)


# national level

data_rate = readJMDweb(prefID = "00", item = "Mx_1x1", fixup = FALSE)
data_pop  = readJMDweb(prefID = "00", item = "Exposures_1x1", fixup = FALSE)

data_years = unique(data_rate$Year)
data_ages = unique(data_rate$Age)

n_years = length(data_years)
n_ages = length(data_ages)

ages = 0:100

# create demogdata from 1975 to 2024 for ages 0 to 100

Japan_demo_F = extract.years(extract.ages(demogdata(data = matrix(data_rate$Female, n_ages, n_years),
                                                    pop = matrix(data_pop$Female, n_ages, n_years),
                                                    ages = 0:110, years = data_years,
                                                    type = "mortality", label = "Japan", name = "Female"), ages), 1975:tail(data_years,1))

Japan_demo_M = extract.years(extract.ages(demogdata(data = matrix(data_rate$Male, n_ages, n_years),
                                                    pop = matrix(data_pop$Male, n_ages, n_years),
                                                    ages = 0:110, years = data_years,
                                                    type = "mortality", label = "Japan", name = "Male"), ages), 1975:tail(data_years,1))

Japan_demo_T = extract.years(extract.ages(demogdata(data = matrix(data_rate$Total, n_ages, n_years),
                                                    pop = matrix(data_pop$Total, n_ages, n_years),
                                                    ages = 0:110, years = data_years,
                                                    type = "mortality", label = "Japan", name = "Total"), ages), 1975:tail(data_years,1))

Japan_demo_smooth_F = smooth.demogdata(Japan_demo_F)
Japan_demo_smooth_M = smooth.demogdata(Japan_demo_M)
Japan_demo_smooth_T = smooth.demogdata(Japan_demo_T)

rm(data_years); rm(data_ages); rm(n_years); rm(n_ages); rm(data_rate); rm(data_pop)

# subnational level

state = c("Hokkaido", "Aomori", "Iwate", "Miyagi", "Akita", "Yamagata", "Fukushima",
          "Ibaraki", "Tochigi", "Gunma", "Saitama", "Chiba", "Tokyo", "Kanagawa", "Niigata",
          "Toyama", "Ishikawa", "Fukui", "Yamanashi", "Nagano", "Gifu", "Shizuoka", "Aichi",
          "Mie", "Shiga", "Kyoto", "Osaka", "Hyogo", "Nara", "Wakayama", "Tottori", "Shimane",
          "Okayama", "Hiroshima", "Yamaguchi", "Tokushima", "Kagawa", "Ehime", "Kochi",
          "Fukuoka", "Saga", "Nagasaki", "Kumamoto", "Oita", "Miyazaki", "Kagoshima", "Okinawa")

state_smooth = c("Hokkaido_smooth", "Aomori_smooth", "Iwate_smooth",
                 "Miyagi_smooth", "Akita_smooth", "Yamagata_smooth", "Fukushima_smooth",
                 "Ibaraki_smooth", "Tochigi_smooth", "Gunma_smooth", "Saitama_smooth",
                 "Chiba_smooth", "Tokyo_smooth", "Kanagawa_smooth", "Niigata_smooth",
                 "Toyama_smooth", "Ishikawa_smooth", "Fukui_smooth", "Yamanashi_smooth",
                 "Nagano_smooth", "Gifu_smooth", "Shizuoka_smooth", "Aichi_smooth",
                 "Mie_smooth", "Shiga_smooth", "Kyoto_smooth", "Osaka_smooth", "Hyogo_smooth",
                 "Nara_smooth", "Wakayama_smooth", "Tottori_smooth", "Shimane_smooth",
                 "Okayama_smooth", "Hiroshima_smooth", "Yamaguchi_smooth", "Tokushima_smooth",
                 "Kagawa_smooth", "Ehime_smooth", "Kochi_smooth", "Fukuoka_smooth", "Saga_smooth",
                 "Nagasaki_smooth", "Kumamoto_smooth", "Oita_smooth", "Miyazaki_smooth",
                 "Kagoshima_smooth", "Okinawa_smooth")

n_state = length(state)
state_T <- paste0(state, "_T")
state_M <- paste0(state, "_M")
state_F <- paste0(state, "_F")
state_smooth_T = paste0(state_smooth, "_T")
state_smooth_M = paste0(state_smooth, "_M")
state_smooth_F = paste0(state_smooth, "_F")

for(ik in 1:n_state)
{
    if(ik < 10)
    {
        ik_str <- paste0("0", ik)
    }
    else
    {
        ik_str <- as.character(ik)
    }
    data_rate = readJMDweb(prefID = ik_str, item = "Mx_1x1", fixup = FALSE)
    data_pop  = readJMDweb(prefID = ik_str, item = "Exposures_1x1", fixup = FALSE)

    data_years = unique(data_rate$Year)
    data_ages = unique(data_rate$Age)

    n_years = length(data_years)
    n_ages = length(data_ages)

    # read mortality rate

    data_rate_T = matrix(data_rate$Total, n_ages, n_years)
    data_rate_M = matrix(data_rate$Male, n_ages, n_years)
    data_rate_F = matrix(data_rate$Female, n_ages, n_years)

    data_rate_combo = cbind(rep(data_years, each = n_ages), rep(data_ages, n_years), data_rate$Female, data_rate$Male, data_rate$Total)
    colnames(data_rate_combo) = c("Year", "Age", "Female", "Male", "Total")
    write.table(data_rate_combo, file = paste(state[ik],"_rate.txt",sep=""), quote = FALSE, row.names = FALSE, na = ".")

    # read population at risk

    data_pop_T = matrix(data_pop$Total, n_ages, n_years)
    data_pop_M = matrix(data_pop$Male, n_ages, n_years)
    data_pop_F = matrix(data_pop$Female, n_ages, n_years)

    data_pop_combo = cbind(rep(data_years, n_ages), rep(data_ages, n_years), data_pop$Female, data_pop$Male, data_pop$Total)
    colnames(data_pop_combo) = c("Year", "Age", "Female", "Male", "Total")
    write.table(data_pop_combo, file = paste(state[ik],"_expo.txt",sep=""), quote = FALSE, row.names = FALSE, na = ".")

    # create demogdata

    data_demo_T = extract.years(extract.ages(demogdata(data = data_rate_T, pop = data_pop_T, ages = 0:110, years = data_years,
                                                       type = "mortality", label = state[ik], name = "Total"), 0:100), 1975:tail(data_years,1))

    data_demo_M = extract.years(extract.ages(demogdata(data = data_rate_M, pop = data_pop_M, ages = 0:110, years = data_years,
                                                       type = "mortality", label = state[ik], name = "Male"), 0:100), 1975:tail(data_years,1))

    data_demo_F = extract.years(extract.ages(demogdata(data = data_rate_F, pop = data_pop_F, ages = 0:110, years = data_years,
                                                       type = "mortality", label = state[ik], name = "Female"), 0:100), 1975:tail(data_years,1))

    # read demogdata

    data_demo_combo = extract.years(extract.ages(read.demogdata(file = paste(state[ik], "_rate.txt", sep = ""),
                                     popfile = paste(state[ik], "_expo.txt", sep = ""),
                                     type = "mortality", label = state[ik], skip = 0), 0:100), 1975:tail(data_years,1))

    assign(state[ik],   data_demo_combo)
    assign(state_T[ik], data_demo_T)
    assign(state_M[ik], data_demo_M)
    assign(state_F[ik], data_demo_F)

    # smooth demogdata

    data_demo_smooth_T = smooth.demogdata(data_demo_T)
    data_demo_smooth_M = smooth.demogdata(data_demo_M)
    data_demo_smooth_F = smooth.demogdata(data_demo_F)

    assign(state_smooth[ik], smooth.demogdata(data_demo_combo))
    assign(state_smooth_T[ik], data_demo_smooth_T)
    assign(state_smooth_M[ik], data_demo_smooth_M)
    assign(state_smooth_F[ik], data_demo_smooth_F)

    print(ik); rm(ik); rm(data_demo_F); rm(data_demo_M); rm(data_demo_T);
    rm(data_demo_smooth_F); rm(data_demo_smooth_M); rm(data_demo_smooth_T);
    rm(data_demo_combo); rm(data_pop_combo); rm(data_rate_combo)
    rm(data_pop_T); rm(data_pop_F); rm(data_pop_M); rm(data_rate_F); rm(data_rate_M); rm(data_rate_T)
}

state_F_array = state_M_array = state_T_array =
state_smooth_F_array = state_smooth_M_array = state_smooth_T_array = array(NA, dim = c(101, length(1975:2024), n_state))

state_F_list = state_M_list = state_T_list =
state_smooth_F_list = state_smooth_M_list = state_smooth_T_list = list()
for(ik in 1:n_state)
{
    state_T_list[[ik]] = state_T_array[,,ik] = get(state_T[[ik]])$rate$Total
    state_M_list[[ik]] = state_M_array[,,ik] = get(state_M[[ik]])$rate$Male
    state_F_list[[ik]] = state_F_array[,,ik] = get(state_F[[ik]])$rate$Female

    state_smooth_T_list[[ik]] = state_smooth_T_array[,,ik] = get(state_smooth_T[[ik]])$rate$Total
    state_smooth_M_list[[ik]] = state_smooth_M_array[,,ik] = get(state_smooth_M[[ik]])$rate$Male
    state_smooth_F_list[[ik]] = state_smooth_F_array[,,ik] = get(state_smooth_F[[ik]])$rate$Female
    rm(ik)
}

# M first, F second into an array format

# state_combo_array = state_smooth_combo_array = array(NA, c(101, 50, (47 * 2)),
#                                                     dimnames = list(0:100, 1975:2024, c(rep("M", 47), rep("F", 47))))

# state_combo_array[,,1:47] = state_M_array
# state_combo_array[,,48:(47 * 2)] = state_F_array

# state_smooth_combo_array[,,1:47] = state_smooth_M_array
# state_smooth_combo_array[,,48:(47 * 2)] = state_smooth_F_array

############################
# from prefecture to region
############################

## R1 (one prefecture)

ind_R1 = Hokkaido
ind_R1_smooth = Hokkaido_smooth

## R2 (6 prefectures)

# death counts for females

Aomori_count_female = Aomori$pop$female * Aomori$rate$female
Iwate_count_female = Iwate$pop$female * Iwate$rate$female
Miyagi_count_female = Miyagi$pop$female * Miyagi$rate$female
Akita_count_female = Akita$pop$female * Akita$rate$female
Yamagata_count_female = Yamagata$pop$female * Yamagata$rate$female
Fukushima_count_female = Fukushima$pop$female * Fukushima$rate$female

# death counts for males

Aomori_count_male = Aomori$pop$male * Aomori$rate$male
Iwate_count_male = Iwate$pop$male * Iwate$rate$male
Miyagi_count_male = Miyagi$pop$male * Miyagi$rate$male
Akita_count_male = Akita$pop$male * Akita$rate$male
Yamagata_count_male = Yamagata$pop$male * Yamagata$rate$male
Fukushima_count_male = Fukushima$pop$male * Fukushima$rate$male

# death counts for total

Aomori_count_total = Aomori$pop$total * Aomori$rate$total
Iwate_count_total = Iwate$pop$total * Iwate$rate$total
Miyagi_count_total = Miyagi$pop$total * Miyagi$rate$total
Akita_count_total = Akita$pop$total * Akita$rate$total
Yamagata_count_total = Yamagata$pop$total * Yamagata$rate$total
Fukushima_count_total = Fukushima$pop$total * Fukushima$rate$total

# region rates

ind_R2_female_count = Aomori$pop$female + Iwate$pop$female + Miyagi$pop$female + Akita$pop$female + Yamagata$pop$female + Fukushima$pop$female
ind_R2_female_rate = (Aomori_count_female + Iwate_count_female + Miyagi_count_female + Akita_count_female + Yamagata_count_female + Fukushima_count_female)/ind_R2_female_count

ind_R2_male_count = Aomori$pop$male + Iwate$pop$male + Miyagi$pop$male + Akita$pop$male + Yamagata$pop$male + Fukushima$pop$male
ind_R2_male_rate = (Aomori_count_male + Iwate_count_male + Miyagi_count_male + Akita_count_male + Yamagata_count_male + Fukushima_count_male)/ind_R2_male_count

ind_R2_total_count = Aomori$pop$total + Iwate$pop$total + Miyagi$pop$total + Akita$pop$total + Yamagata$pop$total + Fukushima$pop$total
ind_R2_total_rate = (Aomori_count_total + Iwate_count_total + Miyagi_count_total + Akita_count_total + Yamagata_count_total + Fukushima_count_total)/ind_R2_total_count

ind_R2_rate_summary = cbind(rep(1975:2024, 101), rep(0:100, 50), as.numeric(ind_R2_female_rate), as.numeric(ind_R2_male_rate), as.numeric(ind_R2_total_rate))
colnames(ind_R2_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R2_rate_summary, "ind_R2_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE, na = ".")

ind_R2_count_summary = cbind(rep(1975:2024, 101), rep(0:100, 50), as.numeric(ind_R2_female_count), as.numeric(ind_R2_male_count), as.numeric(ind_R2_total_count))
colnames(ind_R2_count_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R2_count_summary, "ind_R2_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE, na = ".")

ind_R2 = read.demogdata("ind_R2_rate.txt", "ind_R2_count.txt", type="mortality", label = "R2", skip=0)
ind_R2_smooth = smooth.demogdata(ind_R2)

## R3 (7 prefectures)

# death counts for females

Ibaraki_count_female = Ibaraki$pop$female * Ibaraki$rate$female
Tochigi_count_female = Tochigi$pop$female * Tochigi$rate$female
Gunma_count_female = Gunma$pop$female * Gunma$rate$female
Saitama_count_female = Saitama$pop$female * Saitama$rate$female
Chiba_count_female = Chiba$pop$female * Chiba$rate$female
Tokyo_count_female = Tokyo$pop$female * Tokyo$rate$female
Kanagawa_count_female = Kanagawa$pop$female * Kanagawa$rate$female

# death counts for males

Ibaraki_count_male = Ibaraki$pop$male * Ibaraki$rate$male
Tochigi_count_male = Tochigi$pop$male * Tochigi$rate$male
Gunma_count_male = Gunma$pop$male * Gunma$rate$male
Saitama_count_male = Saitama$pop$male * Saitama$rate$male
Chiba_count_male = Chiba$pop$male * Chiba$rate$male
Tokyo_count_male = Tokyo$pop$male * Tokyo$rate$male
Kanagawa_count_male = Kanagawa$pop$male * Kanagawa$rate$male

# death counts for total

Ibaraki_count_total = Ibaraki$pop$total * Ibaraki$rate$total
Tochigi_count_total = Tochigi$pop$total * Tochigi$rate$total
Gunma_count_total = Gunma$pop$total * Gunma$rate$total
Saitama_count_total = Saitama$pop$total * Saitama$rate$total
Chiba_count_total = Chiba$pop$total * Chiba$rate$total
Tokyo_count_total = Tokyo$pop$total * Tokyo$rate$total
Kanagawa_count_total = Kanagawa$pop$total * Kanagawa$rate$total

# region rate

ind_R3_female_count = Ibaraki$pop$female + Tochigi$pop$female + Gunma$pop$female + Saitama$pop$female + Chiba$pop$female + Tokyo$pop$female + Kanagawa$pop$female
ind_R3_female_rate = (Ibaraki_count_female + Tochigi_count_female + Gunma_count_female + Saitama_count_female + Chiba_count_female + Tokyo_count_female + Kanagawa_count_female)/ind_R3_female_count

ind_R3_male_count = Ibaraki$pop$male + Tochigi$pop$male + Gunma$pop$male + Saitama$pop$male + Chiba$pop$male + Tokyo$pop$male + Kanagawa$pop$male
ind_R3_male_rate = (Ibaraki_count_male + Tochigi_count_male + Gunma_count_male + Saitama_count_male + Chiba_count_male + Tokyo_count_male + Kanagawa_count_male)/ind_R3_male_count

ind_R3_total_count = Ibaraki$pop$total + Tochigi$pop$total + Gunma$pop$total + Saitama$pop$total + Chiba$pop$total + Tokyo$pop$total + Kanagawa$pop$total
ind_R3_total_rate = (Ibaraki_count_total + Tochigi_count_total + Gunma_count_total + Saitama_count_total + Chiba_count_total + Tokyo_count_total + Kanagawa_count_total)/ind_R3_total_count

ind_R3_rate_summary = cbind(rep(1975:2024,101), rep(0:100,50), as.numeric(ind_R3_female_rate), as.numeric(ind_R3_male_rate), as.numeric(ind_R3_total_rate))
colnames(ind_R3_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R3_rate_summary, "ind_R3_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE, na = ".")

ind_R3_count_summary = cbind(rep(1975:2024,101), rep(0:100,50), as.numeric(ind_R3_female_count), as.numeric(ind_R3_male_count), as.numeric(ind_R3_total_count))
colnames(ind_R3_count_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R3_count_summary, "ind_R3_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE, na = ".")

ind_R3 = read.demogdata("ind_R3_rate.txt", "ind_R3_count.txt", type="mortality", label = "R3", skip=0)
ind_R3_smooth = smooth.demogdata(ind_R3)

## R4 (9 prefectures)

# death counts for females

Niigata_count_female = Niigata$pop$female * Niigata$rate$female
Toyama_count_female = Toyama$pop$female * Toyama$rate$female
Ishikawa_count_female = Ishikawa$pop$female * Ishikawa$rate$female
Fukui_count_female = Fukui$pop$female * Fukui$rate$female
Yamanashi_count_female = Yamanashi$pop$female * Yamanashi$rate$female
Nagano_count_female = Nagano$pop$female * Nagano$rate$female
Gifu_count_female = Gifu$pop$female * Gifu$rate$female
Shizuoka_count_female = Shizuoka$pop$female * Shizuoka$rate$female
Aichi_count_female = Aichi$pop$female * Aichi$rate$female

# death counts for males

Niigata_count_male = Niigata$pop$male * Niigata$rate$male
Toyama_count_male = Toyama$pop$male * Toyama$rate$male
Ishikawa_count_male = Ishikawa$pop$male * Ishikawa$rate$male
Fukui_count_male = Fukui$pop$male * Fukui$rate$male
Yamanashi_count_male = Yamanashi$pop$male * Yamanashi$rate$male
Nagano_count_male = Nagano$pop$male * Nagano$rate$male
Gifu_count_male = Gifu$pop$male * Gifu$rate$male
Shizuoka_count_male = Shizuoka$pop$male * Shizuoka$rate$male
Aichi_count_male = Aichi$pop$male * Aichi$rate$male

# death counts for totals

Niigata_count_total = Niigata$pop$total * Niigata$rate$total
Toyama_count_total = Toyama$pop$total * Toyama$rate$total
Ishikawa_count_total = Ishikawa$pop$total * Ishikawa$rate$total
Fukui_count_total = Fukui$pop$total * Fukui$rate$total
Yamanashi_count_total = Yamanashi$pop$total * Yamanashi$rate$total
Nagano_count_total = Nagano$pop$total * Nagano$rate$total
Gifu_count_total = Gifu$pop$total * Gifu$rate$total
Shizuoka_count_total = Shizuoka$pop$total * Shizuoka$rate$total
Aichi_count_total = Aichi$pop$total * Aichi$rate$total

# region rate

ind_R4_female_count = Niigata$pop$female + Toyama$pop$female + Ishikawa$pop$female + Fukui$pop$female + Yamanashi$pop$female + Nagano$pop$female + Gifu$pop$female + Shizuoka$pop$female + Aichi$pop$female
ind_R4_female_rate = (Niigata_count_female + Toyama_count_female + Ishikawa_count_female + Fukui_count_female + Yamanashi_count_female + Nagano_count_female + Gifu_count_female + Shizuoka_count_female + Aichi_count_female)/ind_R4_female_count

ind_R4_male_count = Niigata$pop$male + Toyama$pop$male + Ishikawa$pop$male + Fukui$pop$male + Yamanashi$pop$male + Nagano$pop$male + Gifu$pop$male + Shizuoka$pop$male + Aichi$pop$male
ind_R4_male_rate = (Niigata_count_male + Toyama_count_male + Ishikawa_count_male + Fukui_count_male + Yamanashi_count_male + Nagano_count_male + Gifu_count_male + Shizuoka_count_male + Aichi_count_male)/ind_R4_male_count

ind_R4_total_count = Niigata$pop$total + Toyama$pop$total + Ishikawa$pop$total + Fukui$pop$total + Yamanashi$pop$total + Nagano$pop$total + Gifu$pop$total + Shizuoka$pop$total + Aichi$pop$total
ind_R4_total_rate = (Niigata_count_total + Toyama_count_total + Ishikawa_count_total + Fukui_count_total + Yamanashi_count_total + Nagano_count_total + Gifu_count_total + Shizuoka_count_total + Aichi_count_total)/ind_R4_total_count

ind_R4_rate_summary = cbind(rep(1975:2024,101), rep(0:100,50), as.numeric(ind_R4_female_rate), as.numeric(ind_R4_male_rate), as.numeric(ind_R4_total_rate))
colnames(ind_R4_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R4_rate_summary, "ind_R4_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE, na = ".")

ind_R4_count_summary = cbind(rep(1975:2024,101), rep(0:100,50), as.numeric(ind_R4_female_count), as.numeric(ind_R4_male_count), as.numeric(ind_R4_total_count))
colnames(ind_R4_count_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R4_count_summary, "ind_R4_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE, na = ".")

ind_R4 = read.demogdata("ind_R4_rate.txt", "ind_R4_count.txt", type="mortality", label = "R4", skip=0)
ind_R4_smooth = smooth.demogdata(ind_R4)

## R5 (7 prefectures)

# death counts for females

Mie_count_female = Mie$pop$female * Mie$rate$female
Shiga_count_female = Shiga$pop$female * Shiga$rate$female
Kyoto_count_female = Kyoto$pop$female * Kyoto$rate$female
Osaka_count_female = Osaka$pop$female * Osaka$rate$female
Hyogo_count_female = Hyogo$pop$female * Hyogo$rate$female
Nara_count_female  = Nara$pop$female * Nara$rate$female
Wakayama_count_female = Wakayama$pop$female * Wakayama$rate$female

# death counts for males

Mie_count_male = Mie$pop$male * Mie$rate$male
Shiga_count_male = Shiga$pop$male * Shiga$rate$male
Kyoto_count_male = Kyoto$pop$male * Kyoto$rate$male
Osaka_count_male = Osaka$pop$male * Osaka$rate$male
Hyogo_count_male = Hyogo$pop$male * Hyogo$rate$male
Nara_count_male  = Nara$pop$male * Nara$rate$male
Wakayama_count_male = Wakayama$pop$male * Wakayama$rate$male

# death counts for totals

Mie_count_total = Mie$pop$total * Mie$rate$total
Shiga_count_total = Shiga$pop$total * Shiga$rate$total
Kyoto_count_total = Kyoto$pop$total * Kyoto$rate$total
Osaka_count_total = Osaka$pop$total * Osaka$rate$total
Hyogo_count_total = Hyogo$pop$total * Hyogo$rate$total
Nara_count_total  = Nara$pop$total * Nara$rate$total
Wakayama_count_total = Wakayama$pop$total * Wakayama$rate$total

# region rate

ind_R5_female_count = Mie$pop$female + Shiga$pop$female + Kyoto$pop$female + Osaka$pop$female + Hyogo$pop$female + Nara$pop$female + Wakayama$pop$female
ind_R5_female_rate = (Mie_count_female + Shiga_count_female + Kyoto_count_female + Osaka_count_female + Hyogo_count_female + Nara_count_female + Wakayama_count_female)/ind_R5_female_count

ind_R5_male_count = Mie$pop$male + Shiga$pop$male + Kyoto$pop$male + Osaka$pop$male + Hyogo$pop$male + Nara$pop$male + Wakayama$pop$male
ind_R5_male_rate = (Mie_count_male + Shiga_count_male + Kyoto_count_male + Osaka_count_male + Hyogo_count_male + Nara_count_male + Wakayama_count_male)/ind_R5_male_count

ind_R5_total_count = Mie$pop$total + Shiga$pop$total + Kyoto$pop$total + Osaka$pop$total + Hyogo$pop$total + Nara$pop$total + Wakayama$pop$total
ind_R5_total_rate = (Mie_count_total + Shiga_count_total + Kyoto_count_total + Osaka_count_total + Hyogo_count_total + Nara_count_total + Wakayama_count_total)/ind_R5_total_count

ind_R5_rate_summary = cbind(rep(1975:2024,101), rep(0:100,50), as.numeric(ind_R5_female_rate), as.numeric(ind_R5_male_rate), as.numeric(ind_R5_total_rate))
colnames(ind_R5_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R5_rate_summary, "ind_R5_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE, na = ".")

ind_R5_count_summary = cbind(rep(1975:2024,101), rep(0:100,50), as.numeric(ind_R5_female_count), as.numeric(ind_R5_male_count), as.numeric(ind_R5_total_count))
colnames(ind_R5_count_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R5_count_summary, "ind_R5_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE, na = ".")

ind_R5 = read.demogdata("ind_R5_rate.txt", "ind_R5_count.txt", type="mortality", label = "R5", skip=0)
ind_R5_smooth = smooth.demogdata(ind_R5)

## R6 (5 prefectures)

# death counts for females

Tottori_count_female = Tottori$pop$female * Tottori$rate$female
Shimane_count_female = Shimane$pop$female * Shimane$rate$female
Okayama_count_female = Okayama$pop$female * Okayama$rate$female
Hiroshima_count_female = Hiroshima$pop$female * Hiroshima$rate$female
Yamaguchi_count_female = Yamaguchi$pop$female * Yamaguchi$rate$female

# death counts for males

Tottori_count_male = Tottori$pop$male * Tottori$rate$male
Shimane_count_male = Shimane$pop$male * Shimane$rate$male
Okayama_count_male = Okayama$pop$male * Okayama$rate$male
Hiroshima_count_male = Hiroshima$pop$male * Hiroshima$rate$male
Yamaguchi_count_male = Yamaguchi$pop$male * Yamaguchi$rate$male

# death counts for totals

Tottori_count_total = Tottori$pop$total * Tottori$rate$total
Shimane_count_total = Shimane$pop$total * Shimane$rate$total
Okayama_count_total = Okayama$pop$total * Okayama$rate$total
Hiroshima_count_total = Hiroshima$pop$total * Hiroshima$rate$total
Yamaguchi_count_total = Yamaguchi$pop$total * Yamaguchi$rate$total

# region rates

ind_R6_female_count = Tottori$pop$female + Shimane$pop$female + Okayama$pop$female + Hiroshima$pop$female + Yamaguchi$pop$female
ind_R6_female_rate = (Tottori_count_female + Shimane_count_female + Okayama_count_female + Hiroshima_count_female + Yamaguchi_count_female)/ind_R6_female_count

ind_R6_male_count = Tottori$pop$male + Shimane$pop$male + Okayama$pop$male + Hiroshima$pop$male + Yamaguchi$pop$male
ind_R6_male_rate  = (Tottori_count_male + Shimane_count_male + Okayama_count_male + Hiroshima_count_male + Yamaguchi_count_male)/ind_R6_male_count

ind_R6_total_count = Tottori$pop$total + Shimane$pop$total + Okayama$pop$total + Hiroshima$pop$total + Yamaguchi$pop$total
ind_R6_total_rate  = (Tottori_count_total + Shimane_count_total + Okayama_count_total + Hiroshima_count_total + Yamaguchi_count_total)/ind_R6_total_count

ind_R6_rate_summary = cbind(rep(1975:2024,101), rep(0:100,50), as.numeric(ind_R6_female_rate), as.numeric(ind_R6_male_rate), as.numeric(ind_R6_total_rate))
colnames(ind_R6_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R6_rate_summary, "ind_R6_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE, na = ".")

ind_R6_count_summary = cbind(rep(1975:2024,101), rep(0:100,50), as.numeric(ind_R6_female_count), as.numeric(ind_R6_male_count), as.numeric(ind_R6_total_count))
colnames(ind_R6_count_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R6_count_summary, "ind_R6_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE, na = ".")

ind_R6 = read.demogdata("ind_R6_rate.txt", "ind_R6_count.txt", type="mortality", label = "R6", skip=0)
ind_R6_smooth = smooth.demogdata(ind_R6)

## R7 (4 prefectures)

# death counts for females

Tokushima_count_female = Tokushima$pop$female * Tokushima$rate$female
Kagawa_count_female = Kagawa$pop$female * Kagawa$rate$female
Ehime_count_female = Ehime$pop$female * Ehime$rate$female
Kochi_count_female = Kochi$pop$female * Kochi$rate$female

# death counts for males

Tokushima_count_male = Tokushima$pop$male * Tokushima$rate$male
Kagawa_count_male = Kagawa$pop$male * Kagawa$rate$male
Ehime_count_male = Ehime$pop$male * Ehime$rate$male
Kochi_count_male = Kochi$pop$male * Kochi$rate$male

# death counts for totals

Tokushima_count_total = Tokushima$pop$total * Tokushima$rate$total
Kagawa_count_total = Kagawa$pop$total * Kagawa$rate$total
Ehime_count_total = Ehime$pop$total * Ehime$rate$total
Kochi_count_total = Kochi$pop$total * Kochi$rate$total

# region rate

ind_R7_female_count = Tokushima$pop$female + Kagawa$pop$female + Ehime$pop$female + Kochi$pop$female
ind_R7_female_rate  = (Tokushima_count_female + Kagawa_count_female + Ehime_count_female + Kochi_count_female)/ind_R7_female_count

ind_R7_male_count = Tokushima$pop$male + Kagawa$pop$male + Ehime$pop$male + Kochi$pop$male
ind_R7_male_rate  = (Tokushima_count_male + Kagawa_count_male + Ehime_count_male + Kochi_count_male)/ind_R7_male_count

ind_R7_total_count = Tokushima$pop$total + Kagawa$pop$total + Ehime$pop$total + Kochi$pop$total
ind_R7_total_rate  = (Tokushima_count_total + Kagawa_count_total + Ehime_count_total + Kochi_count_total)/ind_R7_total_count

ind_R7_rate_summary = cbind(rep(1975:2024,101), rep(0:100,50), as.numeric(ind_R7_female_rate), as.numeric(ind_R7_male_rate), as.numeric(ind_R7_total_rate))
colnames(ind_R7_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R7_rate_summary, "ind_R7_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE, na = ".")

ind_R7_count_summary = cbind(rep(1975:2024,101), rep(0:100,50), as.numeric(ind_R7_female_count), as.numeric(ind_R7_male_count), as.numeric(ind_R7_total_count))
colnames(ind_R7_count_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R7_count_summary, "ind_R7_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE, na = ".")

ind_R7 = read.demogdata("ind_R7_rate.txt", "ind_R7_count.txt", type="mortality", label = "R7", skip=0)
ind_R7_smooth = smooth.demogdata(ind_R7)

## R8 (8 prefectures)

# death counts for females

Fukuoka_count_female = Fukuoka$pop$female * Fukuoka$rate$female
Saga_count_female = Saga$pop$female * Saga$rate$female
Nagasaki_count_female = Nagasaki$pop$female * Nagasaki$rate$female
Kumamoto_count_female = Kumamoto$pop$female * Kumamoto$rate$female
Oita_count_female = Oita$pop$female * Oita$rate$female
Miyazaki_count_female = Miyazaki$pop$female * Miyazaki$rate$female
Kagoshima_count_female = Kagoshima$pop$female * Kagoshima$rate$female
Okinawa_count_female = Okinawa$pop$female * Okinawa$rate$female

# death counts for males

Fukuoka_count_male = Fukuoka$pop$male * Fukuoka$rate$male
Saga_count_male = Saga$pop$male * Saga$rate$male
Nagasaki_count_male = Nagasaki$pop$male * Nagasaki$rate$male
Kumamoto_count_male = Kumamoto$pop$male * Kumamoto$rate$male
Oita_count_male = Oita$pop$male * Oita$rate$male
Miyazaki_count_male = Miyazaki$pop$male * Miyazaki$rate$male
Kagoshima_count_male = Kagoshima$pop$male * Kagoshima$rate$male
Okinawa_count_male = Okinawa$pop$male * Okinawa$rate$male

# death counts for totals

Fukuoka_count_total = Fukuoka$pop$total * Fukuoka$rate$total
Saga_count_total = Saga$pop$total * Saga$rate$total
Nagasaki_count_total = Nagasaki$pop$total * Nagasaki$rate$total
Kumamoto_count_total = Kumamoto$pop$total * Kumamoto$rate$total
Oita_count_total = Oita$pop$total * Oita$rate$total
Miyazaki_count_total = Miyazaki$pop$total * Miyazaki$rate$total
Kagoshima_count_total = Kagoshima$pop$total * Kagoshima$rate$total
Okinawa_count_total = Okinawa$pop$total * Okinawa$rate$total

# region rate

ind_R8_female_count = Fukuoka$pop$female + Saga$pop$female + Nagasaki$pop$female + Kumamoto$pop$female + Oita$pop$female + Miyazaki$pop$female + Kagoshima$pop$female + Okinawa$pop$female
ind_R8_female_rate = (Fukuoka_count_female + Saga_count_female + Nagasaki_count_female + Kumamoto_count_female + Oita_count_female + Miyazaki_count_female + Kagoshima_count_female + Okinawa_count_female)/ind_R8_female_count

ind_R8_male_count = Fukuoka$pop$male + Saga$pop$male + Nagasaki$pop$male + Kumamoto$pop$male + Oita$pop$male + Miyazaki$pop$male + Kagoshima$pop$male + Okinawa$pop$male
ind_R8_male_rate = (Fukuoka_count_male + Saga_count_male + Nagasaki_count_male + Kumamoto_count_male + Oita_count_male + Miyazaki_count_male + Kagoshima_count_male + Okinawa_count_male)/ind_R8_male_count

ind_R8_total_count = Fukuoka$pop$total + Saga$pop$total + Nagasaki$pop$total + Kumamoto$pop$total + Oita$pop$total + Miyazaki$pop$total + Kagoshima$pop$total + Okinawa$pop$total
ind_R8_total_rate = (Fukuoka_count_total + Saga_count_total + Nagasaki_count_total + Kumamoto_count_total + Oita_count_total + Miyazaki_count_total + Kagoshima_count_total + Okinawa_count_total)/ind_R8_total_count

ind_R8_rate_summary = cbind(rep(1975:2024,101), rep(0:100,50), as.numeric(ind_R8_female_rate), as.numeric(ind_R8_male_rate), as.numeric(ind_R8_total_rate))
colnames(ind_R8_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R8_rate_summary, "ind_R8_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE, na = ".")

ind_R8_count_summary = cbind(rep(1975:2024,101), rep(0:100,50), as.numeric(ind_R8_female_count), as.numeric(ind_R8_male_count), as.numeric(ind_R8_total_count))
colnames(ind_R8_count_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R8_count_summary, "ind_R8_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE, na = ".")

ind_R8 = read.demogdata("ind_R8_rate.txt", "ind_R8_count.txt", type="mortality", label = "R8", skip=0)
ind_R8_smooth = smooth.demogdata(ind_R8)

region = c("ind_R1", "ind_R2", "ind_R3", "ind_R4", "ind_R5", "ind_R6", "ind_R7", "ind_R8")
region_smooth = c("ind_R1_smooth", "ind_R2_smooth", "ind_R3_smooth", "ind_R4_smooth", "ind_R5_smooth",
                  "ind_R6_smooth", "ind_R7_smooth", "ind_R8_smooth")


region_T <- paste0(region, "_T")
region_M <- paste0(region, "_M")
region_F <- paste0(region, "_F")
region_smooth_T = paste0(region_smooth, "_T")
region_smooth_M = paste0(region_smooth, "_M")
region_smooth_F = paste0(region_smooth, "_F")

n_region = 8

for(ik in 1:n_region)
{
    assign(region_F[ik], demogdata(data = get(region[ik])$rate$female, pop = get(region[ik])$pop$female, ages = ages,
                                   years = 1975:2024, type = "mortality", label = region[ik], name = "Female"))

    assign(region_M[ik], demogdata(data = get(region[ik])$rate$male, pop = get(region[ik])$pop$male, ages = ages,
                                   years = 1975:2024, type = "mortality", label = region[ik], name = "Male"))

    assign(region_T[ik], demogdata(data = get(region[ik])$rate$total, pop = get(region[ik])$pop$total, ages = ages,
                                   years = 1975:2024, type = "mortality", label = region[ik], name = "Total"))

    # smooth

    assign(region_smooth_F[ik], demogdata(data = get(region_smooth[ik])$rate$female, pop = get(region_smooth[ik])$pop$female, ages = ages,
                                   years = 1975:2024, type = "mortality", label = region[ik], name = "Female"))

    assign(region_smooth_M[ik], demogdata(data = get(region_smooth[ik])$rate$male, pop = get(region_smooth[ik])$pop$male, ages = ages,
                                   years = 1975:2024, type = "mortality", label = region[ik], name = "Male"))

    assign(region_smooth_T[ik], demogdata(data = get(region_smooth[ik])$rate$total, pop = get(region_smooth[ik])$pop$total, ages = ages,
                                   years = 1975:2024, type = "mortality", label = region[ik], name = "Total"))
    print(ik); rm(ik)
}

region_F_array = region_M_array = region_T_array =
region_smooth_F_array = region_smooth_M_array = region_smooth_T_array = array(NA, dim = c(101, length(1975:2024), n_region))

region_F_list = region_M_list = region_T_list =
region_smooth_F_list = region_smooth_M_list = region_smooth_T_list = list()
for(ik in 1:n_region)
{
    region_T_list[[ik]] = region_T_array[,,ik] = get(region_T[ik])$rate$Total
    region_M_list[[ik]] = region_M_array[,,ik] = get(region_M[ik])$rate$Male
    region_F_list[[ik]] = region_F_array[,,ik] = get(region_F[ik])$rate$Female

    region_smooth_T_list[[ik]] = region_smooth_T_array[,,ik] = get(region_smooth_T[ik])$rate$Total
    region_smooth_M_list[[ik]] = region_smooth_M_array[,,ik] = get(region_smooth_M[ik])$rate$Male
    region_smooth_F_list[[ik]] = region_smooth_F_array[,,ik] = get(region_smooth_F[ik])$rate$Female
    rm(ik)
}


# Restore the project folder as the working directory.
setwd(project_root)
