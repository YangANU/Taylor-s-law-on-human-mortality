##################################
# Summary of results into Table 1
##################################

## Temporal TL

# adjust R-squared
R_squared_temporal = matrix(0, nrow = 3, ncol = 3)
rownames(R_squared_temporal) = c("total", "female", "male")
colnames(R_squared_temporal) = c("classic", "quadratic", "cubic")

R_squared_temporal["total",] = c(mean(total_classic_R_squared_prefecture), mean(total_quadratic_R_squared_prefecture), mean(total_TL_R_squared_prefecture))
R_squared_temporal["female",] = c(mean(female_classic_R_squared_prefecture), mean(female_quadratic_R_squared_prefecture), mean(female_TL_R_squared_prefecture))
R_squared_temporal["male",] = c(mean(male_classic_R_squared_prefecture), mean(male_quadratic_R_squared_prefecture), mean(male_TL_R_squared_prefecture))

# BIC
BIC_temporal =  matrix(0, nrow = 3, ncol = 3)
rownames(BIC_temporal) = c("total", "female", "male")
colnames(BIC_temporal) = c("classic", "quadratic", "cubic")

BIC_temporal["total",] = c(mean(total_classic_BIC), mean(total_quadratic_BIC), mean(total_cubic_BIC))
BIC_temporal["female",] = c(mean(female_classic_BIC), mean(female_quadratic_BIC), mean(female_cubic_BIC))
BIC_temporal["male",] = c(mean(male_classic_BIC), mean(male_quadratic_BIC), mean(male_cubic_BIC))

# b coefficient
b_coef_temporal = matrix(0, nrow = 3, ncol = 3)
rownames(b_coef_temporal) = c("total", "female", "male")
colnames(b_coef_temporal) = c("classic", "quadratic", "cubic")

b_coef_temporal["total",] = c(sum(total_classic_p[2,] < 0.05), sum(total_quadratic_p[2,] < 0.05), sum(total_TL_p[2,] < 0.05))
b_coef_temporal["female",] = c(sum(female_classic_p[2,] < 0.05), sum(female_quadratic_p[2,] < 0.05), sum(female_TL_p[2,] < 0.05))
b_coef_temporal["male",] = c(sum(male_classic_p[2,] < 0.05), sum(male_quadratic_p[2,] < 0.05), sum(male_TL_p[2,] < 0.05))

# c coefficient
c_coef_temporal = matrix(0, nrow = 3, ncol = 2)
rownames(c_coef_temporal) = c("total", "female", "male")
colnames(c_coef_temporal) = c("quadratic", "cubic")
c_coef_temporal["total",] = c(sum(total_quadratic_p[3,] < 0.05), sum(total_TL_p[3,] < 0.05))
c_coef_temporal["female",] = c(sum(female_quadratic_p[3,] < 0.05), sum(female_TL_p[3,] < 0.05))
c_coef_temporal["male",] = c(sum(male_quadratic_p[3,] < 0.05), sum(male_TL_p[3,] < 0.05))

# d coefficient
d_coef_temporal = matrix(0, nrow = 3, ncol = 1)
rownames(d_coef_temporal) = c("total", "female", "male")
d_coef_temporal = c(sum(total_TL_p[4,] < 0.05), sum(female_TL_p[4,] < 0.05), sum(male_TL_p[4,] < 0.05))

# b coefficient (Bonferroni)
b_coef_temporal_bonferroni = matrix(0, nrow = 3, ncol = 3)
rownames(b_coef_temporal_bonferroni) = c("total", "female", "male")
colnames(b_coef_temporal_bonferroni) = c("classic", "quadratic", "cubic")

b_coef_temporal_bonferroni["total",] = c(sum(total_classic_p[2,] < 0.05/47), sum(total_quadratic_p[2,] < 0.05/47), sum(total_TL_p[2,] < 0.05/47))
b_coef_temporal_bonferroni["female",] = c(sum(female_classic_p[2,] < 0.05), sum(female_quadratic_p[2,] < 0.05/47), sum(female_TL_p[2,] < 0.05/47))
b_coef_temporal_bonferroni["male",] = c(sum(male_classic_p[2,] < 0.05/47), sum(male_quadratic_p[2,] < 0.05/47), sum(male_TL_p[2,] < 0.05/47))

# c coefficient (Bonferroni)
c_coef_temporal_bonferroni = matrix(0, nrow = 3, ncol = 2)
rownames(c_coef_temporal_bonferroni) = c("total", "female", "male")
colnames(c_coef_temporal_bonferroni) = c("quadratic", "cubic")
c_coef_temporal_bonferroni["total",] = c(sum(total_quadratic_p[3,] < 0.05/47), sum(total_TL_p[3,] < 0.05/47))
c_coef_temporal_bonferroni["female",] = c(sum(female_quadratic_p[3,] < 0.05/47), sum(female_TL_p[3,] < 0.05/47))
c_coef_temporal_bonferroni["male",] = c(sum(male_quadratic_p[3,] < 0.05/47), sum(male_TL_p[3,] < 0.05/47))

# d coefficient (Bonferroni)
d_coef_temporal_bonferroni = matrix(0, nrow = 3, ncol = 1)
rownames(d_coef_temporal_bonferroni) = c("total", "female", "male")
d_coef_temporal_bonferroni = c(sum(total_TL_p[4,] < 0.05/47), sum(female_TL_p[4,] < 0.05/47), sum(male_TL_p[4,] < 0.05/47))

## Spatial TL

# adjust R-squared
R_squared_spatial = matrix(0, nrow = 3, ncol = 3)
rownames(R_squared_spatial) = c("total", "female", "male")
colnames(R_squared_spatial) = c("classic", "quadratic", "cubic")

R_squared_spatial["total",] = c(mean(spatial_total_classic_R_squared_prefecture), mean(spatial_total_quadratic_R_squared_prefecture), mean(spatial_total_TL_R_squared_prefecture))
R_squared_spatial["female",] = c(mean(spatial_female_classic_R_squared_prefecture), mean(spatial_female_quadratic_R_squared_prefecture), mean(spatial_female_TL_R_squared_prefecture))
R_squared_spatial["male",] = c(mean(spatial_male_classic_R_squared_prefecture), mean(spatial_male_quadratic_R_squared_prefecture), mean(spatial_male_TL_R_squared_prefecture))

BIC_spatial =  matrix(0, nrow = 3, ncol = 3)
rownames(BIC_spatial) = c("total", "female", "male")
colnames(BIC_spatial) = c("classic", "quadratic", "cubic")

BIC_spatial["total",] = c(mean(spatial_total_classic_BIC), mean(spatial_total_quadratic_BIC), mean(spatial_total_cubic_BIC))
BIC_spatial["female",] = c(mean(spatial_female_classic_BIC), mean(spatial_female_quadratic_BIC), mean(spatial_female_cubic_BIC))
BIC_spatial["male",] = c(mean(spatial_male_classic_BIC), mean(spatial_male_quadratic_BIC), mean(spatial_male_cubic_BIC))


# b coefficient
b_coef_spatial = matrix(0, nrow = 3, ncol = 3)
rownames(b_coef_spatial) = c("total", "female", "male")
colnames(b_coef_spatial) = c("classic", "quadratic", "cubic")

b_coef_spatial["total",] = c(sum(spatial_total_classic_p[2,] < 0.05), sum(spatial_total_quadratic_p[2,] < 0.05), sum(spatial_total_TL_p[2,] < 0.05))
b_coef_spatial["female",] = c(sum(spatial_female_classic_p[2,] < 0.05), sum(spatial_female_quadratic_p[2,] < 0.05), sum(spatial_female_TL_p[2,] < 0.05))
b_coef_spatial["male",] = c(sum(spatial_male_classic_p[2,] < 0.05), sum(spatial_male_quadratic_p[2,] < 0.05), sum(spatial_male_TL_p[2,] < 0.05))

# c coefficient
c_coef_spatial = matrix(0, nrow = 3, ncol = 2)
rownames(c_coef_spatial) = c("total", "female", "male")
colnames(c_coef_spatial) = c("quadratic", "cubic")
c_coef_spatial["total",] = c(sum(spatial_total_quadratic_p[3,] < 0.05), sum(spatial_total_TL_p[3,] < 0.05))
c_coef_spatial["female",] = c(sum(spatial_female_quadratic_p[3,] < 0.05), sum(spatial_female_TL_p[3,] < 0.05))
c_coef_spatial["male",] = c(sum(spatial_male_quadratic_p[3,] < 0.05), sum(spatial_male_TL_p[3,] < 0.05))

# d coefficient
d_coef_spatial = matrix(0, nrow = 3, ncol = 1)
rownames(d_coef_spatial) = c("total", "female", "male")
d_coef_spatial = c(sum(spatial_total_TL_p[4,] < 0.05), sum(spatial_female_TL_p[4,] < 0.05), sum(spatial_male_TL_p[4,] < 0.05))

# b coefficient (Bonferroni)
b_coef_spatial_bonferroni = matrix(0, nrow = 3, ncol = 3)
rownames(b_coef_spatial_bonferroni) = c("total", "female", "male")
colnames(b_coef_spatial_bonferroni) = c("classic", "quadratic", "cubic")

b_coef_spatial_bonferroni["total",] = c(sum(spatial_total_classic_p[2,] < 0.05/47), sum(spatial_total_quadratic_p[2,] < 0.05/47), sum(spatial_total_TL_p[2,] < 0.05/47))
b_coef_spatial_bonferroni["female",] = c(sum(spatial_female_classic_p[2,] < 0.05), sum(spatial_female_quadratic_p[2,] < 0.05/47), sum(spatial_female_TL_p[2,] < 0.05/47))
b_coef_spatial_bonferroni["male",] = c(sum(spatial_male_classic_p[2,] < 0.05/47), sum(spatial_male_quadratic_p[2,] < 0.05/47), sum(spatial_male_TL_p[2,] < 0.05/47))

# c coefficient (Bonferroni)
c_coef_spatial_bonferroni = matrix(0, nrow = 3, ncol = 2)
rownames(c_coef_spatial_bonferroni) = c("total", "female", "male")
colnames(c_coef_spatial_bonferroni) = c("quadratic", "cubic")
c_coef_spatial_bonferroni["total",] = c(sum(spatial_total_quadratic_p[3,] < 0.05/47), sum(spatial_total_TL_p[3,] < 0.05/47))
c_coef_spatial_bonferroni["female",] = c(sum(spatial_female_quadratic_p[3,] < 0.05/47), sum(spatial_female_TL_p[3,] < 0.05/47))
c_coef_spatial_bonferroni["male",] = c(sum(spatial_male_quadratic_p[3,] < 0.05/47), sum(spatial_male_TL_p[3,] < 0.05/47))

# d coefficient (Bonferroni)
d_coef_spatial_bonferroni = matrix(0, nrow = 3, ncol = 1)
rownames(d_coef_spatial_bonferroni) = c("total", "female", "male")
d_coef_spatial_bonferroni = c(sum(spatial_total_TL_p[4,] < 0.05/47), sum(spatial_female_TL_p[4,] < 0.05/47), sum(spatial_male_TL_p[4,] < 0.05/47))







