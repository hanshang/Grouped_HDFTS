# ----------------------------------------------------------------------------
# 24_Japan_S_mat.R
#
# Build the summing matrix S_t for the Japanese group structure (168 series:
# national, sex, region, region x sex, prefecture, prefecture x sex) from
# the exposure-to-risk ratios.
#
# Replication code for Shang, H. L., 'High-dimensional functional time
# series forecasting with a group structure'.
#
# Run with the working directory set to the code_submission/ folder.
# Scripts are numbered 01-51 in the order in which they must be run.
# See README.md for a one-line description of each.
# ----------------------------------------------------------------------------

###########################################################
# Construct summing matrix (S) by age and forecast horizon
###########################################################

Smat_fun <- function(kj, age, no_prefecture = 47)
{
    level_0 = level_1_a = level_1_b = matrix(,2*no_prefecture,(11-kj))
    for(iw in 1:no_prefecture)
    {
        # Level 0

        level_0[(2*iw-1),] = get(pop_ratio_P_F_to_T[iw])[kj,age,1:(11-kj)]
        level_0[(2*iw),]   = get(pop_ratio_P_M_to_T[iw])[kj,age,1:(11-kj)]

        # Level 1 (disaggregate by sex)

        level_1_a[(2*iw-1),] = get(pop_ratio_P_F_to_F[iw])[kj,age,1:(11-kj)]
        level_1_a[(2*iw),]   = rep(0,(11-kj))

        level_1_b[(2*iw-1),] = rep(0,(11-kj))
        level_1_b[(2*iw),]   = get(pop_ratio_P_M_to_M[iw])[kj,age,1:(11-kj)]
    }
    level_2_a_1 = level_2_a_2 = level_2_a_3 = level_2_a_4 = level_2_a_5 =
    level_2_a_6 = level_2_a_7 = level_2_a_8 = matrix(,2*no_prefecture,(11-kj))

    ###################################
    # Level 2 (disaggregate by region)
    ###################################

    # R1

    level_2_a_1[1,] = get(pop_ratio_P_F_to_R1_T[1])[kj,age,1:(11-kj)]
    level_2_a_1[2,] = get(pop_ratio_P_M_to_R1_T[1])[kj,age,1:(11-kj)]
    level_2_a_1[3:(2*no_prefecture),] = matrix(rep(0,(11-kj)*92), ncol=(11-kj))

    # R2

    level_2_a_2[1:2, ] = matrix(rep(0,(11-kj)*2), ncol=(11-kj))
    for(iw in 1:6)
    {
        level_2_a_2[(2*iw+1),] = get(pop_ratio_P_F_to_R2_T[iw])[kj,age,1:(11-kj)]
        level_2_a_2[(2*iw+2),] = get(pop_ratio_P_M_to_R2_T[iw])[kj,age,1:(11-kj)]
    }
    level_2_a_2[15:(2*no_prefecture),] = matrix(rep(0,(11-kj)*80), ncol=(11-kj))

    # R3

    level_2_a_3[1:14,] = matrix(rep(0,(11-kj)*14), ncol=(11-kj))
    for(iw in 1:7)
    {
        level_2_a_3[(2*(iw+6)+1),] = get(pop_ratio_P_F_to_R3_T[iw])[kj,age,1:(11-kj)]
        level_2_a_3[(2*(iw+6)+2),] = get(pop_ratio_P_M_to_R3_T[iw])[kj,age,1:(11-kj)]
    }
    level_2_a_3[29:(2*no_prefecture),] = matrix(rep(0,(11-kj)*66), ncol=(11-kj))

    # R4

    level_2_a_4[1:28,] = matrix(rep(0,(11-kj)*28), ncol=(11-kj))
    for(iw in 1:9)
    {
        level_2_a_4[(2*(iw+13)+1),] = get(pop_ratio_P_F_to_R4_T[iw])[kj,age,1:(11-kj)]
        level_2_a_4[(2*(iw+13)+2),] = get(pop_ratio_P_M_to_R4_T[iw])[kj,age,1:(11-kj)]
    }
    level_2_a_4[47:(2*no_prefecture),] = matrix(rep(0,(11-kj)*48), ncol=(11-kj))

    # R5

    level_2_a_5[1:46,] = matrix(rep(0,(11-kj)*46), ncol=(11-kj))
    for(iw in 1:7)
    {
        level_2_a_5[(2*(iw+22)+1),] = get(pop_ratio_P_F_to_R5_T[iw])[kj,age,1:(11-kj)]
        level_2_a_5[(2*(iw+22)+2),] = get(pop_ratio_P_M_to_R5_T[iw])[kj,age,1:(11-kj)]
    }
    level_2_a_5[61:(2*no_prefecture),] = matrix(rep(0,(11-kj)*34), ncol=(11-kj))

    # R6

    level_2_a_6[1:60,] = matrix(rep(0,(11-kj)*60), ncol=(11-kj))
    for(iw in 1:5)
    {
        level_2_a_6[(2*(iw+29)+1),] = get(pop_ratio_P_F_to_R6_T[iw])[kj,age,1:(11-kj)]
        level_2_a_6[(2*(iw+29)+2),] = get(pop_ratio_P_M_to_R6_T[iw])[kj,age,1:(11-kj)]
    }
    level_2_a_6[71:(2*no_prefecture),] = matrix(rep(0,(11-kj)*24), ncol=(11-kj))

    # R7

    level_2_a_7[1:70,] = matrix(rep(0,(11-kj)*70), ncol=(11-kj))
    for(iw in 1:4)
    {
        level_2_a_7[(2*(iw+34)+1),] = get(pop_ratio_P_F_to_R7_T[iw])[kj,age,1:(11-kj)]
        level_2_a_7[(2*(iw+34)+2),] = get(pop_ratio_P_M_to_R7_T[iw])[kj,age,1:(11-kj)]
    }
    level_2_a_7[79:(2*no_prefecture),] = matrix(rep(0,(11-kj)*16), ncol=(11-kj))

    # R8

    level_2_a_8[1:78,] = matrix(rep(0,(11-kj)*78), ncol=(11-kj))
    for(iw in 1:8)
    {
        level_2_a_8[(2*(iw+38)+1),] = get(pop_ratio_P_F_to_R8_T[iw])[kj,age,1:(11-kj)]
        level_2_a_8[(2*(iw+38)+2),] = get(pop_ratio_P_M_to_R8_T[iw])[kj,age,1:(11-kj)]
    }

    level_2_b_1 = level_2_b_2 = level_2_b_3 = level_2_b_4 = level_2_b_5 = level_2_b_6 =
    level_2_b_7 = level_2_b_8 = matrix(,(2*no_prefecture),(11-kj))

    ##################################################
    # Level 2 (disaggregate by region + sex (female))
    ##################################################

    # R1

    level_2_b_1[1,] = get(pop_ratio_P_F_to_R1_F[1])[kj,age,1:(11-kj)]
    level_2_b_1[2:(2*no_prefecture),] = matrix(rep(0,(11-kj)*93), ncol=(11-kj))

    # R2

    level_2_b_2[1:2, ] = matrix(rep(0,(11-kj)*2), ncol=(11-kj))
    for(iw in 1:6)
    {
        level_2_b_2[(2*iw+1),] = get(pop_ratio_P_F_to_R2_F[iw])[kj,age,1:(11-kj)]
        level_2_b_2[(2*iw+2),] = rep(0, (11-kj))
    }
    level_2_b_2[15:(2*no_prefecture),] = matrix(rep(0,(11-kj)*80), ncol=(11-kj))

    # R3

    level_2_b_3[1:14,] = matrix(rep(0,(11-kj)*14), ncol=(11-kj))
    for(iw in 1:7)
    {
        level_2_b_3[(2*(iw+6)+1),] = get(pop_ratio_P_F_to_R3_F[iw])[kj,age,1:(11-kj)]
        level_2_b_3[(2*(iw+6)+2),] = rep(0, (11-kj))
    }
    level_2_b_3[29:(2*no_prefecture),] = matrix(rep(0,(11-kj)*66), ncol=(11-kj))

    # R4

    level_2_b_4[1:28,] = matrix(rep(0,(11-kj)*28), ncol=(11-kj))
    for(iw in 1:9)
    {
        level_2_b_4[(2*(iw+13)+1),] = get(pop_ratio_P_F_to_R4_F[iw])[kj,age,1:(11-kj)]
        level_2_b_4[(2*(iw+13)+2),] = rep(0, (11-kj))
    }
    level_2_b_4[47:(2*no_prefecture),] = matrix(rep(0,(11-kj)*48), ncol=(11-kj))

    # R5

    level_2_b_5[1:46,] = matrix(rep(0,(11-kj)*46), ncol=(11-kj))
    for(iw in 1:7)
    {
        level_2_b_5[(2*(iw+22)+1),] = get(pop_ratio_P_F_to_R5_F[iw])[kj,age,1:(11-kj)]
        level_2_b_5[(2*(iw+22)+2),] = rep(0, (11-kj))
    }
    level_2_b_5[61:(2*no_prefecture),] = matrix(rep(0,(11-kj)*34), ncol=(11-kj))

    # R6

    level_2_b_6[1:60,] = matrix(rep(0,(11-kj)*60), ncol=(11-kj))
    for(iw in 1:5)
    {
        level_2_b_6[(2*(iw+29)+1),] = get(pop_ratio_P_F_to_R6_F[iw])[kj,age,1:(11-kj)]
        level_2_b_6[(2*(iw+29)+2),] = rep(0, (11-kj))
    }
    level_2_b_6[71:(2*no_prefecture),] = matrix(rep(0,(11-kj)*24), ncol=(11-kj))

    # R7

    level_2_b_7[1:70,] = matrix(rep(0,(11-kj)*70), ncol=(11-kj))
    for(iw in 1:4)
    {
        level_2_b_7[(2*(iw+34)+1),] = get(pop_ratio_P_F_to_R7_F[iw])[kj,age,1:(11-kj)]
        level_2_b_7[(2*(iw+34)+2),] = rep(0, (11-kj))
    }
    level_2_b_7[79:(2*no_prefecture),] = matrix(rep(0,(11-kj)*16), ncol=(11-kj))

    # R8

    level_2_b_8[1:78,] = matrix(rep(0,(11-kj)*78), ncol=(11-kj))
    for(iw in 1:8)
    {
        level_2_b_8[(2*(iw+38)+1),] = get(pop_ratio_P_F_to_R8_F[iw])[kj,age,1:(11-kj)]
        level_2_b_8[(2*(iw+38)+2),] = rep(0, (11-kj))
    }

    level_2_c_1 = level_2_c_2 = level_2_c_3 = level_2_c_4 = level_2_c_5 = level_2_c_6 =
    level_2_c_7 = level_2_c_8 = matrix(,(2*no_prefecture),(11-kj))

    ################################################
    # Level 2 (disaggregate by region + sex (male))
    ################################################

    # R1

    level_2_c_1[1,] = rep(0,(11-kj))
    level_2_c_1[2,] = get(pop_ratio_P_M_to_R1_M[1])[kj,age,1:(11-kj)]
    level_2_c_1[3:(2*no_prefecture),] = matrix(rep(0,(11-kj)*92), ncol=(11-kj))

    # R2

    level_2_c_2[1:2, ] = matrix(rep(0,(11-kj)*2), ncol=(11-kj))
    for(iw in 1:6)
    {
        level_2_c_2[(2*iw+1),] = rep(0,(11-kj))
        level_2_c_2[(2*iw+2),] = get(pop_ratio_P_M_to_R2_M[iw])[kj,age,1:(11-kj)]
    }
    level_2_c_2[15:(2*no_prefecture),] = matrix(rep(0,(11-kj)*80), ncol=(11-kj))

    # R3

    level_2_c_3[1:14,] = matrix(rep(0,(11-kj)*14), ncol=(11-kj))
    for(iw in 1:7)
    {
        level_2_c_3[(2*(iw+6)+1),] = rep(0,(11-kj))
        level_2_c_3[(2*(iw+6)+2),] = get(pop_ratio_P_M_to_R3_M[iw])[kj,age,1:(11-kj)]
    }
    level_2_c_3[29:(2*no_prefecture),] = matrix(rep(0,(11-kj)*66), ncol=(11-kj))

    # R4

    level_2_c_4[1:28,] = matrix(rep(0,(11-kj)*28), ncol=(11-kj))
    for(iw in 1:9)
    {
        level_2_c_4[(2*(iw+13)+1),] = rep(0,(11-kj))
        level_2_c_4[(2*(iw+13)+2),] = get(pop_ratio_P_M_to_R4_M[iw])[kj,age,1:(11-kj)]
    }
    level_2_c_4[47:(2*no_prefecture),] = matrix(rep(0,(11-kj)*48), ncol=(11-kj))

    # R5

    level_2_c_5[1:46,] = matrix(rep(0,(11-kj)*46), ncol=(11-kj))
    for(iw in 1:7)
    {
        level_2_c_5[(2*(iw+22)+1),] = rep(0,(11-kj))
        level_2_c_5[(2*(iw+22)+2),] = get(pop_ratio_P_M_to_R5_M[iw])[kj,age,1:(11-kj)]
    }
    level_2_c_5[61:(2*no_prefecture),] = matrix(rep(0,(11-kj)*34), ncol=(11-kj))

    # R6

    level_2_c_6[1:60,] = matrix(rep(0,(11-kj)*60), ncol=(11-kj))
    for(iw in 1:5)
    {
        level_2_c_6[(2*(iw+29)+1),] = rep(0,(11-kj))
        level_2_c_6[(2*(iw+29)+2),] = get(pop_ratio_P_M_to_R6_M[iw])[kj,age,1:(11-kj)]
    }
    level_2_c_6[71:(2*no_prefecture),] = matrix(rep(0,(11-kj)*24), ncol=(11-kj))

    # R7

    level_2_c_7[1:70,] = matrix(rep(0,(11-kj)*70), ncol=(11-kj))
    for(iw in 1:4)
    {
        level_2_c_7[(2*(iw+34)+1),] = rep(0,(11-kj))
        level_2_c_7[(2*(iw+34)+2),] = get(pop_ratio_P_M_to_R7_M[iw])[kj,age,1:(11-kj)]
    }
    level_2_c_7[79:(2*no_prefecture),] = matrix(rep(0,(11-kj)*16), ncol=(11-kj))

    # R8

    level_2_c_8[1:78,] = matrix(rep(0,(11-kj)*78), ncol=(11-kj))
    for(iw in 1:8)
    {
        level_2_c_8[(2*(iw+38)+1),] = rep(0,(11-kj))
        level_2_c_8[(2*(iw+38)+2),] = get(pop_ratio_P_M_to_R8_M[iw])[kj,age,1:(11-kj)]
    }

    ###################################################
    # Level 3 (disaggregate by prefecture + sex (male))
    ###################################################

    level_3 = array(0,dim = c((2*no_prefecture),(11-kj),47))
    for(iw in 1:47)
    {
        level_3[2*iw-1,,iw] = get(pop_ratio_P_F_to_P_T[iw])[kj,age,1:(11-kj)]
        level_3[2*iw,,iw]   = get(pop_ratio_P_M_to_P_T[iw])[kj,age,1:(11-kj)]
    }

    S_mat = array(, dim = c(168, (2*no_prefecture), (11-kj)))
    for(ik in 1:(11-kj))
    {
        S_mat[,,ik] = rbind(level_0[,ik],     level_1_a[,ik],   level_1_b[,ik],   level_2_a_1[,ik],
                            level_2_a_2[,ik], level_2_a_3[,ik], level_2_a_4[,ik], level_2_a_5[,ik],
                            level_2_a_6[,ik], level_2_a_7[,ik], level_2_a_8[,ik], level_2_b_1[,ik],
                            level_2_b_2[,ik], level_2_b_3[,ik], level_2_b_4[,ik], level_2_b_5[,ik],
                            level_2_b_6[,ik], level_2_b_7[,ik], level_2_b_8[,ik], level_2_c_1[,ik],
                            level_2_c_2[,ik], level_2_c_3[,ik], level_2_c_4[,ik], level_2_c_5[,ik],
                            level_2_c_6[,ik], level_2_c_7[,ik], level_2_c_8[,ik], t(level_3[,ik,]),
                            diag(94))
    }
    return(S_mat)
}
