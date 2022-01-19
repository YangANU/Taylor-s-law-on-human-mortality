##############
# Temporal TL
##############

#########################################
# 95% confidence of Hampel loss function
#########################################

k_hampel = 0.9016085

psi.hampel_95 = function (u, a = 1.5*k_hampel, b = 3.5*k_hampel, c = 8*k_hampel, deriv = 0) 
{
  U <- pmin(abs(u) + 1e-50, c)
  if (!deriv) 
    return(ifelse(U <= a, U, ifelse(U <= b, a, a * (c - U)/(c - 
                                                              b)))/U)
  ifelse(abs(u) <= c, ifelse(U <= a, 1, ifelse(U <= b, 0, -a/(c - 
                                                                b))), 0)
}

# plot temporal mean and variance

# female

temporal_female_mean = apply(female_data[,,2:48], c(1, 3), mean)
temporal_female_var  = apply(female_data[,,2:48], c(1, 3), var)

rownames(temporal_female_mean) = rownames(temporal_female_var) = 0:100
colnames(temporal_female_mean) = colnames(temporal_female_var) = state[2:48]

plot(fts(0:100, log(temporal_female_mean, base = 10)), xlab = "Age", ylab = "Female mortality")
plot(fts(0:100, log(temporal_female_var,  base = 10)), xlab = "Age", ylab = "Female mortality")

# male

temporal_male_mean = apply(male_data[,,2:48], c(1, 3), mean)
temporal_male_var  = apply(male_data[,,2:48], c(1, 3), var)

rownames(temporal_male_mean) = rownames(temporal_male_var) = 0:100
colnames(temporal_male_mean) = colnames(temporal_male_var) = state[2:48]

plot(fts(0:100, log(temporal_male_mean, base = 10)), xlab = "Age", ylab = "Male mortality")
plot(fts(0:100, log(temporal_male_var,  base = 10)), xlab = "Age", ylab = "Male mortality")

# total

temporal_total_mean = apply(total_data[,,2:48], c(1, 3), mean)
temporal_total_var  = apply(total_data[,,2:48], c(1, 3), var)

rownames(temporal_total_mean) = rownames(temporal_total_var) = 0:100
colnames(temporal_total_mean) = colnames(temporal_total_var) = state[2:48]

plot(fts(0:100, log(temporal_total_mean, base = 10)), xlab = "Age", ylab = "Total mortality")
plot(fts(0:100, log(temporal_total_var,  base = 10)), xlab = "Age", ylab = "Total mortality")


# identify outliers

female_TL_mahalanobis = male_TL_mahalanobis = total_TL_mahalanobis = matrix(NA, nrow = 101, ncol = 47)
rownames(female_TL_mahalanobis) = rownames(male_TL_mahalanobis) = rownames(total_TL_mahalanobis) = 0:100
colnames(female_TL_mahalanobis) = colnames(male_TL_mahalanobis) = colnames(total_TL_mahalanobis) = state[-1]

for(ij in 1:47)
{
  female_select = cbind(log(temporal_female_var[,ij], base = 10), log(temporal_female_mean[,ij], base = 10))
  female_TL_mahalanobis[,ij] = mahalanobis(x = female_select, center = colMeans(female_select), cov = cov(female_select))
  
  male_select = cbind(log(temporal_male_var[,ij], base = 10), log(temporal_male_mean[,ij], base = 10))
  male_TL_mahalanobis[!is.na(temporal_male_mean[,ij]),ij] = mahalanobis(x = na.omit(male_select), center = colMeans(na.omit(male_select)), cov = cov(na.omit(male_select)))
  
  total_select = cbind(log(temporal_total_var[,ij], base = 10), log(temporal_total_mean[,ij], base = 10))
  total_TL_mahalanobis[,ij] = mahalanobis(x = total_select, center = colMeans(total_select), cov = cov(total_select))
}

female_TL_outliers = male_TL_outliers = total_TL_outliers = 
  female_TL_mahalanobis_outliers = male_TL_mahalanobis_outliers = total_TL_mahalanobis_outliers = 
  female_TL_rstudent_outliers = male_TL_rstudent_outliers = total_TL_rstudent_outliers = list()

for(ij in 1:47)
{
  female_select = data.frame(log_mean = log10(temporal_female_mean[,ij]), log_var = log10(temporal_long_run_female_var[,ij]))
  female_TL_mahalanobis_outliers[[ij]] = female_select[female_TL_mahalanobis[,ij] > qchisq(p = 0.95, df = 2), , drop = FALSE]
  female_TL_rstudent_outliers[[ij]] = female_select[abs(rstudent(lm(log10(temporal_long_run_female_var[,ij]) ~ log10(temporal_female_mean[,ij])))) > 2, , drop = FALSE]
  outlier_age = as.numeric(intersect(row.names(female_TL_mahalanobis_outliers[[ij]]), row.names(female_TL_rstudent_outliers[[ij]])))
  female_TL_outliers[[ij]] = female_select[outlier_age+1,]
  
  male_select = data.frame(log_mean = log10(temporal_male_mean[,ij]), log_var = log10(temporal_long_run_male_var[,ij]))
  male_TL_mahalanobis_outliers[[ij]] = male_select[male_TL_mahalanobis[,ij] > qchisq(p = 0.95, df = 2), , drop = FALSE]
  male_TL_rstudent_outliers[[ij]] = male_select[abs(rstudent(lm(log10(temporal_long_run_male_var[,ij]) ~ log10(temporal_male_mean[,ij])))) > 2, , drop = FALSE]
  outlier_age = as.numeric(intersect(row.names(male_TL_mahalanobis_outliers[[ij]]), row.names(male_TL_rstudent_outliers[[ij]])))
  male_TL_outliers[[ij]] = male_select[outlier_age+1,]
  
  total_select = data.frame(log_mean = log10(temporal_total_mean[,ij]), log_var = log10(temporal_long_run_total_var[,ij]))
  total_TL_mahalanobis_outliers[[ij]] = total_select[total_TL_mahalanobis[,ij] > qchisq(p = 0.95, df = 2), , drop = FALSE]
  total_TL_rstudent_outliers[[ij]] = total_select[abs(rstudent(lm(log10(temporal_long_run_total_var[,ij]) ~ log10(temporal_total_mean[,ij])))) > 2, , drop = FALSE]
  outlier_age = as.numeric(intersect(row.names(total_TL_mahalanobis_outliers[[ij]]), row.names(total_TL_rstudent_outliers[[ij]])))
  total_TL_outliers[[ij]] = total_select[outlier_age+1,]
}

names(female_TL_outliers) = names(male_TL_outliers) = names(total_TL_outliers) = 
  names(female_TL_mahalanobis_outliers) = names(male_TL_mahalanobis_outliers) = names(total_TL_mahalanobis_outliers) =
  names(female_TL_rstudent_outliers) = names(male_TL_rstudent_outliers) = names(total_TL_rstudent_outliers) = state[-1]

sum(unlist(lapply(female_TL_mahalanobis_outliers, nrow)))
sum(unlist(lapply(female_TL_outliers, nrow)))

unlist(lapply(female_TL_outliers, nrow))
unlist(lapply(male_TL_outliers, nrow))
unlist(lapply(total_TL_outliers, nrow))

#############################
# Temporal long-run variance
#############################

library(sandwich)
library(MASS)

temporal_long_run_female_var = apply(female_data[,,2:48], c(1, 3), lrvar, type = "Andrews")
temporal_long_run_male_var = apply(male_data[,,2:48], c(1, 3), lrvar, type = "Andrews")
temporal_long_run_total_var = apply(total_data[,,2:48], c(1, 3), lrvar, type = "Andrews")


# female
female_long_run_TL_slope_prefecture = female_long_run_TL_hampel_slope_prefecture = female_long_run_TL_bisquare_slope_prefecture = vector("numeric", 47)

female_long_run_TL_R_squared_prefecture = female_long_run_TL_hampel_R_squared_prefecture = female_long_run_TL_bisquare_R_squared_prefecture = vector("numeric", 47)

for(ij in 1:47)
{
  OLS_model = lm(log(temporal_long_run_female_var[,ij], base = 10) ~ log(temporal_female_mean[,ij], base = 10))
  female_long_run_TL_slope_prefecture[ij] = OLS_model$coefficients[2]
  female_long_run_TL_R_squared_prefecture[ij] = summary(OLS_model)$r.squared
  
  hampel_model = rlm(log(temporal_long_run_female_var[,ij], base = 10) ~ log(temporal_female_mean[,ij], base = 10), psi = psi.hampel_95, maxit = 500)
  female_long_run_TL_hampel_slope_prefecture[ij] = hampel_model$coefficients[2]
  female_long_run_TL_hampel_R_squared_prefecture[ij] = 1 - sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2)
  
  bisquare_model = rlm(log(temporal_long_run_female_var[,ij], base = 10) ~ log(temporal_female_mean[,ij], base = 10), psi = psi.bisquare, maxit = 500)
  female_long_run_TL_bisquare_slope_prefecture[ij] = bisquare_model$coefficients[2]
  female_long_run_TL_bisquare_R_squared_prefecture[ij] = 1 - sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2)
}

savepdf("female_long_run_TL_temporal", width = 14, height = 10, toplines = 0.8)
plot(1:47, female_long_run_TL_slope_prefecture, xlab = "Prefecture ordered geographically by North to South", 
     ylab = "Taylor's Law slope parameter estimates", ylim = c(1.6, 2.2))
#,main = expression(paste(log[10], "(temporal long-run variance) versus ", log[10], "(temporal mean)")))
points(1:47, female_long_run_TL_hampel_slope_prefecture, col = 2, pch = 2)
points(1:47, female_long_run_TL_bisquare_slope_prefecture, col = 4, pch = 4)
legend("topright", c("TL", "RTL (Hampel weight)", "RTL (Bisquare weight)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 1)
dev.off()

# male

male_long_run_TL_slope_prefecture = male_long_run_TL_hampel_slope_prefecture = male_long_run_TL_bisquare_slope_prefecture = vector("numeric", 47)

male_long_run_TL_R_squared_prefecture = male_long_run_TL_hampel_R_squared_prefecture = male_long_run_TL_bisquare_R_squared_prefecture = vector("numeric", 47)

for(ij in 1:47)
{
  OLS_model = lm(log(temporal_long_run_male_var[,ij], base = 10) ~ log(temporal_male_mean[,ij], base = 10))
  male_long_run_TL_slope_prefecture[ij] = OLS_model$coefficients[2]
  male_long_run_TL_R_squared_prefecture[ij] = summary(OLS_model)$r.squared
  
  hampel_model = rlm(log(temporal_long_run_male_var[,ij], base = 10) ~ log(temporal_male_mean[,ij], base = 10), psi = psi.hampel, maxit = 200)
  male_long_run_TL_hampel_slope_prefecture[ij] = hampel_model$coefficients[2]
  male_long_run_TL_hampel_R_squared_prefecture[ij] = 1 - sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2)
  
  bisquare_model = rlm(log(temporal_long_run_male_var[,ij], base = 10) ~ log(temporal_male_mean[,ij], base = 10), psi = psi.bisquare, maxit = 200)
  male_long_run_TL_bisquare_slope_prefecture[ij] = bisquare_model$coefficients[2]
  male_long_run_TL_bisquare_R_squared_prefecture[ij] = 1 - sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2)
}

savepdf("male_long_run_TL_temporal", width = 14, height = 10, toplines = 0.8)
plot(1:47, male_long_run_TL_slope_prefecture, xlab = "Prefecture ordered geographically by North to South", ylab = "Taylor's Law slope parameter estimates", ylim = c(1.6, 2.2))
# , main = expression(paste(log[10], "(temporal long-run variance) versus ", log[10], "(temporal mean)")))
points(1:47, male_long_run_TL_hampel_slope_prefecture, col = 2, pch = 2)
points(1:47, male_long_run_TL_bisquare_slope_prefecture, col = 4, pch = 4)
#legend("topright", c("LM", "RLM (Hampel weight)", "RLM (Bisquare weight)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 0.8)
dev.off()

# total

total_long_run_TL_slope_prefecture = total_long_run_TL_hampel_slope_prefecture = total_long_run_TL_bisquare_slope_prefecture = vector("numeric", 47)

total_long_run_TL_R_squared_prefecture = total_long_run_TL_hampel_R_squared_prefecture = total_long_run_TL_bisquare_R_squared_prefecture = vector("numeric", 47)

for(ij in 1:47)
{
  OLS_model = lm(log(temporal_long_run_total_var[,ij], base = 10) ~ log(temporal_total_mean[,ij], base = 10))
  total_long_run_TL_slope_prefecture[ij] = OLS_model$coefficients[2]
  total_long_run_TL_R_squared_prefecture[ij] = summary(OLS_model)$r.squared
  
  hampel_model = rlm(log(temporal_long_run_total_var[,ij], base = 10) ~ log(temporal_total_mean[,ij], base = 10), psi = psi.hampel, maxit = 200)
  total_long_run_TL_hampel_slope_prefecture[ij] = hampel_model$coefficients[2]
  total_long_run_TL_hampel_R_squared_prefecture = 1 - sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2)
  
  bisquare_model = rlm(log(temporal_long_run_total_var[,ij], base = 10) ~ log(temporal_total_mean[,ij], base = 10), psi = psi.bisquare, maxit = 200)
  total_long_run_TL_bisquare_slope_prefecture[ij] = bisquare_model$coefficients[2]
  total_long_run_TL_bisquare_R_squared_prefecture[ij] = 1 - sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2)
}

savepdf("total_long_run_TL_temporal", width = 14, height = 10, toplines = 0.8)
plot(1:47, total_long_run_TL_slope_prefecture, xlab = "Prefecture ordered geographically by North to South", ylab = "Taylor's Law slope parameter estimates", ylim = c(1.6, 2.2))
#, main = expression(paste(log[10], "(temporal long-run variance) versus ", log[10], "(temporal mean)")))
points(1:47, total_long_run_TL_hampel_slope_prefecture, col = 2, pch = 2)
points(1:47, total_long_run_TL_bisquare_slope_prefecture, col = 4, pch = 4)
#legend("topright", c("LM", "RLM (Hampel weight)", "RLM (Bisquare weight)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 0.8)
dev.off()


savepdf("female_2015", width = 14, height = 10, toplines = 0.8)
matplot(log10(female_data_smooth[,41,2:48]), lty = 1, type = "l", col = "grey", xlab = "Age", ylab = "Mortality rate (logarithm base 10)", main = "")
lines(log10(female_data_smooth[,41,41]), lwd = 2, col = 3)
lines(log10(female_data_smooth[,41,23]), lwd = 2, col = 2)
lines(log10(female_data_smooth[,41,24]), lwd = 2, col = 4)
legend("topleft", lty = c(1,1,1), col = c(2,4,3), c("Prefecture 22 Shizuoka", "Prefecture 23 Aichi", "Prefecture 40 Fukuoka"), lwd = c(2,2,2))
dev.off()


savepdf("female_2016", width = 14, height = 10, toplines = 0.8)
matplot(log10(female_data_smooth[,42,2:48]), lty = 1, type = "l", col = "grey", xlab = "Age", ylab = "Mortality rate (logarithm base 10)", main = "")
lines(log10(female_data_smooth[,42,41]), lwd = 2, col = 3)
lines(log10(female_data_smooth[,42,23]), lwd = 2, col = 2)
lines(log10(female_data_smooth[,42,24]), lwd = 2, col = 4)
#legend("topleft", lty = c(1,1,1), col = c(2,4,3), c("22 Shizuoka", "23 Aichi", "40 Fukuoka"), lwd = c(2,2,2))
dev.off()

savepdf("female_2017", width = 14, height = 10, toplines = 0.8)
matplot(log10(female_data_smooth[,43,2:48]), lty = 1, type = "l", col = "grey", xlab = "Age", ylab = "Mortality rate (logarithm base 10)", main = "")
lines(log10(female_data_smooth[,43,41]), lwd = 2, col = 3)
lines(log10(female_data_smooth[,43,23]), lwd = 2, col = 2)
lines(log10(female_data_smooth[,43,24]), lwd = 2, col = 4)
#legend("topleft", lty = c(1,1,1), col = c(2,4,3), c("22 Shizuoka", "23 Aichi", "40 Fukuoka"), lwd = c(2,2,2))
dev.off()

savepdf("female_2018", width = 14, height = 10, toplines = 0.8)
matplot(log10(female_data_smooth[,44,2:48]), lty = 1, type = "l", col = "grey", xlab = "Age", ylab = "Mortality rate (logarithm base 10)", main = "")
lines(log10(female_data_smooth[,44,41]), lwd = 2, col = 3)
lines(log10(female_data_smooth[,44,23]), lwd = 2, col = 2)
lines(log10(female_data_smooth[,44,24]), lwd = 2, col = 4)
#legend("topleft", lty = c(1,1,1), col = c(2,4,3), c("22 Shizuoka", "23 Aichi", "40 Fukuoka"), lwd = c(2,2,2))
dev.off()


#########################################
# Taylor's law slope parameter estimates
#########################################
library(MASS)
library(qpcR)
library(tidyverse)
library(plot3D)

find_p_value = function(t_values, df)
{
  n = length(t_values)
  out = rep(0, n)
  for(i in 1:n)
  {
    if(t_values[i] < 0)
    {
      out[i] = pt(q = t_values[i], df = df, lower.tail = TRUE)
    } else {
      out[i] = pt(q = t_values[i], df = df, lower.tail = FALSE)
    }
  }
  return(out)
}


# female

# 1: benchmark of the classic OLS TL

female_classic = female_classic_hampel = female_classic_bisquare = matrix(NA, nrow = 2, ncol = 47)
female_classic_p = female_classic_hampel_p = female_classic_bisquare_p = matrix(NA, nrow = 2, ncol = 47)
female_classic_R_squared_prefecture = female_classic_hampel_R_squared_prefecture = female_classic_bisquare_R_squared_prefecture = vector("numeric", 47)

for(ij in 1:47)
{
  if(length(female_TL_outliers[[ij]]) > 0)
  {
    age_select = setdiff(age_all, as.numeric(rownames(female_TL_outliers[[ij]])))
    y = log(temporal_long_run_female_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_female_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_long_run_female_var[,ij], base = 10)
    x = log(temporal_female_mean[,ij], base = 10)
  }
  
  OLS_model = lm(y ~ x)
  female_classic[,ij] = OLS_model$coefficients
  female_classic_p[,ij] = summary(OLS_model)$coef[,4]
  female_classic_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  
  hampel_model = rlm(y ~ x, psi = psi.hampel, maxit = 200)
  female_classic_hampel[,ij] = hampel_model$coefficients
  female_classic_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  female_classic_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x, psi = psi.bisquare, maxit = 200)
  female_classic_bisquare[,ij] = bisquare_model$coefficients
  female_classic_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  female_classic_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
}

# 2: benchmark of quadratic OLS TL

female_quadratic = female_quadratic_hampel = female_quadratic_bisquare = matrix(NA, nrow = 3, ncol = 47)
female_quadratic_p = female_quadratic_hampel_p = female_quadratic_bisquare_p = matrix(NA, nrow = 3, ncol = 47)
female_quadratic_R_squared_prefecture = female_quadratic_hampel_R_squared_prefecture = female_quadratic_bisquare_R_squared_prefecture = vector("numeric", 47)

for(ij in 1:47)
{
  if(length(female_TL_outliers[[ij]]) > 0)
  {
    age_select = setdiff(age_all, as.numeric(rownames(female_TL_outliers[[ij]])))
    y = log(temporal_long_run_female_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_female_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_long_run_female_var[,ij], base = 10)
    x = log(temporal_female_mean[,ij], base = 10)
  }
  
  OLS_model = lm(y ~ x + I(x^2))
  female_quadratic[,ij] = OLS_model$coefficients
  female_quadratic_p[,ij] = summary(OLS_model)$coef[,4]
  female_quadratic_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  
  hampel_model = rlm(y ~ x + I(x^2), psi = psi.hampel, maxit = 200)
  female_quadratic_hampel[,ij] = hampel_model$coefficients
  female_quadratic_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  female_quadratic_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x + I(x^2), psi = psi.bisquare, maxit = 200)
  female_quadratic_bisquare[,ij] = bisquare_model$coefficients
  female_quadratic_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  female_quadratic_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
}

# 3: cubic TL with OLS estimation

female_TL_coef = female_TL_hampel_coef = female_TL_bisquare_coef = matrix(NA, nrow = 4, ncol = 47)
female_TL_p = female_TL_hampel_p = female_TL_bisquare_p = matrix(NA, nrow = 4, ncol = 47)
female_TL_R_squared_prefecture = female_TL_hampel_R_squared_prefecture = female_TL_bisquare_R_squared_prefecture = vector("numeric", 47)

female_fitted = female_bisquare_fitted = matrix(NA, nrow = 101, ncol = 47)

for(ij in 1:47)
{
  if(length(female_TL_outliers[[ij]]) > 0)
  {
    age_select = setdiff(age_all, as.numeric(rownames(female_TL_outliers[[ij]])))
    y = log(temporal_long_run_female_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_female_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_long_run_female_var[,ij], base = 10)
    x = log(temporal_female_mean[,ij], base = 10)
  }
  
  y = log(temporal_long_run_female_var[,ij], base = 10)
  x = log(temporal_female_mean[,ij], base = 10)
  
  
  OLS_model = lm(y ~ x + I(x^2) + I(x^3))
  female_TL_coef[,ij] = OLS_model$coefficients
  female_TL_p[,ij] = summary(OLS_model)$coef[,4]
  female_TL_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  female_fitted[,ij] = OLS_model$fitted.values
  
  hampel_model = rlm(y ~ x + I(x^2) + I(x^3), psi = psi.hampel_95, maxit = 200)
  female_TL_hampel_coef[,ij] = hampel_model$coefficients
  female_TL_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  female_TL_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x + I(x^2) + I(x^3), psi = psi.bisquare, maxit = 500)
  female_TL_bisquare_coef[,ij] = bisquare_model$coefficients
  female_TL_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  female_TL_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  female_bisquare_fitted[,ij] = bisquare_model$fitted.values
}

## BIC
female_classic_BIC = female_quadratic_BIC = female_cubic_BIC = vector("numeric", 47)
for(ij in 1:47)
{
  if(length(female_TL_outliers[[ij]]) > 0)
  {
    age_select = setdiff(age_all, as.numeric(rownames(female_TL_outliers[[ij]])))
    y = log(temporal_long_run_female_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_female_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_long_run_female_var[,ij], base = 10)
    x = log(temporal_female_mean[,ij], base = 10)
  }
  
  y = log(temporal_long_run_female_var[,ij], base = 10)
  x = log(temporal_female_mean[,ij], base = 10)
  
  OLS_model = lm(y ~ x)
  female_classic_BIC[ij] = BIC(OLS_model)
  
  OLS_model_quadratic = lm(y ~ x + I(x^2))
  female_quadratic_BIC[ij] = BIC(OLS_model_quadratic)
  
  OLS_model_cubic = lm(y ~ x + I(x^2) + I(x^3))
  female_cubic_BIC[ij] = BIC(OLS_model_cubic)
}

# male

# benchmark of the classic OLS TL

male_classic = male_classic_hampel = male_classic_bisquare = matrix(NA, nrow = 2, ncol = 47)
male_classic_p = male_classic_hampel_p = male_classic_bisquare_p = matrix(NA, nrow = 2, ncol = 47)
male_classic_R_squared_prefecture = male_classic_hampel_R_squared_prefecture = male_classic_bisquare_R_squared_prefecture = vector("numeric", 47)

for(ij in 1:47)
{
  if(length(male_TL_outliers[[ij]]) > 0)
  {
    age_select = setdiff(age_all, as.numeric(rownames(male_TL_outliers[[ij]])))
    y = log(temporal_long_run_male_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_male_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_long_run_male_var[,ij], base = 10)
    x = log(temporal_male_mean[,ij], base = 10)
  }

  y = log(temporal_long_run_male_var[,ij], base = 10)
  x = log(temporal_male_mean[,ij], base = 10)
  
  OLS_model = lm(y ~ x)
  male_classic[,ij] = OLS_model$coefficients
  male_classic_p[,ij] = summary(OLS_model)$coef[,4]
  male_classic_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  
  hampel_model = rlm(y ~ x, psi = psi.hampel, maxit = 2000)
  male_classic_hampel[,ij] = hampel_model$coefficients
  male_classic_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  male_classic_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x, psi = psi.bisquare, maxit = 2000)
  male_classic_bisquare[,ij] = bisquare_model$coefficients
  male_classic_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  male_classic_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
}

# benchmark of quadratic OLS TL

male_quadratic = male_quadratic_hampel = male_quadratic_bisquare = matrix(NA, nrow = 3, ncol = 47)
male_quadratic_p = male_quadratic_hampel_p = male_quadratic_bisquare_p = matrix(NA, nrow = 3, ncol = 47)
male_quadratic_R_squared_prefecture = male_quadratic_hampel_R_squared_prefecture = male_quadratic_bisquare_R_squared_prefecture = vector("numeric", 47)

for(ij in 1:47)
{
  if(length(male_TL_outliers[[ij]]) > 0)
  {
    age_select = setdiff(age_all, as.numeric(rownames(male_TL_outliers[[ij]])))
    y = log(temporal_long_run_male_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_male_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_long_run_male_var[,ij], base = 10)
    x = log(temporal_male_mean[,ij], base = 10)
  }
  
  y = log(temporal_long_run_male_var[,ij], base = 10)
  x = log(temporal_male_mean[,ij], base = 10)
  
  OLS_model = lm(y ~ x + I(x^2))
  male_quadratic[,ij] = OLS_model$coefficients
  male_quadratic_p[,ij] = summary(OLS_model)$coef[,4]
  male_quadratic_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  
  hampel_model = rlm(y ~ x + I(x^2), psi = psi.hampel, maxit = 2000)
  male_quadratic_hampel[,ij] = hampel_model$coefficients
  male_quadratic_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  male_quadratic_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x + I(x^2), psi = psi.bisquare, maxit = 2000)
  male_quadratic_bisquare[,ij] = bisquare_model$coefficients
  male_quadratic_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  male_quadratic_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
}


# cubic TL with OLS estimation

male_TL_coef = male_TL_hampel_coef = male_TL_bisquare_coef = matrix(NA, nrow = 4, ncol = 47)
male_TL_p = male_TL_hampel_p = male_TL_bisquare_p = matrix(NA, nrow = 4, ncol = 47)
male_TL_R_squared_prefecture = male_TL_hampel_R_squared_prefecture = male_TL_bisquare_R_squared_prefecture = vector("numeric", 47)

male_fitted = male_bisquare_fitted = matrix(NA, nrow = 101, ncol = 47)

for(ij in 1:47)
{
  if(length(male_TL_outliers[[ij]]) > 0)
  {
    age_select = setdiff(age_all, as.numeric(rownames(male_TL_outliers[[ij]])))
    y = log(temporal_long_run_male_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_male_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_long_run_male_var[,ij], base = 10)
    x = log(temporal_male_mean[,ij], base = 10)
  }
  
  y = log(temporal_long_run_male_var[,ij], base = 10)
  x = log(temporal_male_mean[,ij], base = 10)
  
  OLS_model = lm(y ~ x + I(x^2) + I(x^3))
  male_TL_coef[,ij] = OLS_model$coefficients
  male_TL_p[,ij] = summary(OLS_model)$coef[,4]
  male_TL_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  male_fitted[!is.na(x),ij] = OLS_model$fitted.values
  
  hampel_model = rlm(y ~ x + I(x^2) + I(x^3), psi = psi.hampel_95, maxit = 10000)
  male_TL_hampel_coef[,ij] = hampel_model$coefficients
  male_TL_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  male_TL_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x + I(x^2) + I(x^3), psi = psi.bisquare, maxit = 10000)
  male_TL_bisquare_coef[,ij] = bisquare_model$coefficients
  male_TL_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  male_TL_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  male_bisquare_fitted[!is.na(x),ij] = bisquare_model$fitted.values
}


## BIC
male_classic_BIC = male_quadratic_BIC = male_cubic_BIC = vector("numeric", 47)
for(ij in 1:47)
{
  if(length(male_TL_outliers[[ij]]) > 0)
  {
    age_select = setdiff(age_all, as.numeric(rownames(male_TL_outliers[[ij]])))
    y = log(temporal_long_run_male_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_male_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_long_run_male_var[,ij], base = 10)
    x = log(temporal_male_mean[,ij], base = 10)
  }
  
  y = log(temporal_long_run_male_var[,ij], base = 10)
  x = log(temporal_male_mean[,ij], base = 10)
  
  OLS_model = lm(y ~ x)
  male_classic_BIC[ij] = BIC(OLS_model)
  
  OLS_model_quadratic = lm(y ~ x + I(x^2))
  male_quadratic_BIC[ij] = BIC(OLS_model_quadratic)
  
  OLS_model_cubic = lm(y ~ x + I(x^2) + I(x^3))
  male_cubic_BIC[ij] = BIC(OLS_model_cubic)
}

##########

# total

# benchmark of the classic OLS TL

total_classic = total_classic_hampel = total_classic_bisquare = matrix(NA, nrow = 2, ncol = 47)
total_classic_p = total_classic_hampel_p = total_classic_bisquare_p = matrix(NA, nrow = 2, ncol = 47)
total_classic_R_squared_prefecture = total_classic_hampel_R_squared_prefecture = total_classic_bisquare_R_squared_prefecture = vector("numeric", 47)

for(ij in 1:47)
{
  if(length(total_TL_outliers[[ij]]) > 0)
  {
    age_select = setdiff(age_all, as.numeric(rownames(total_TL_outliers[[ij]])))
    y = log(temporal_long_run_total_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_total_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_long_run_total_var[,ij], base = 10)
    x = log(temporal_total_mean[,ij], base = 10)
  }
  
  y = log(temporal_long_run_total_var[,ij], base = 10)
  x = log(temporal_total_mean[,ij], base = 10)
  
  OLS_model = lm(y ~ x)
  total_classic[,ij] = OLS_model$coefficients
  total_classic_p[,ij] = summary(OLS_model)$coef[,4]
  total_classic_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  
  hampel_model = rlm(y ~ x, psi = psi.hampel, maxit = 200)
  total_classic_hampel[,ij] = hampel_model$coefficients
  total_classic_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  total_classic_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x, psi = psi.bisquare, maxit = 200)
  total_classic_bisquare[,ij] = bisquare_model$coefficients
  total_classic_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  total_classic_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
}

# benchmark of quadratic OLS TL

total_quadratic = total_quadratic_hampel = total_quadratic_bisquare = matrix(NA, nrow = 3, ncol = 47)
total_quadratic_p = total_quadratic_hampel_p = total_quadratic_bisquare_p = matrix(NA, nrow = 3, ncol = 47)
total_quadratic_R_squared_prefecture = total_quadratic_hampel_R_squared_prefecture = total_quadratic_bisquare_R_squared_prefecture = vector("numeric", 47)

for(ij in 1:47)
{
  if(length(total_TL_outliers[[ij]]) > 0)
  {
    age_select = setdiff(age_all, as.numeric(rownames(total_TL_outliers[[ij]])))
    y = log(temporal_long_run_total_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_total_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_long_run_total_var[,ij], base = 10)
    x = log(temporal_total_mean[,ij], base = 10)
  }
  
  y = log(temporal_long_run_total_var[,ij], base = 10)
  x = log(temporal_total_mean[,ij], base = 10)
  
  OLS_model = lm(y ~ x + I(x^2))
  total_quadratic[,ij] = OLS_model$coefficients
  total_quadratic_p[,ij] = summary(OLS_model)$coef[,4]
  total_quadratic_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  
  hampel_model = rlm(y ~ x + I(x^2), psi = psi.hampel, maxit = 200)
  total_quadratic_hampel[,ij] = hampel_model$coefficients
  total_quadratic_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  total_quadratic_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x + I(x^2), psi = psi.bisquare, maxit = 200)
  total_quadratic_bisquare[,ij] = bisquare_model$coefficients
  total_quadratic_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  total_quadratic_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
}

# cubic TL

total_TL_coef = total_TL_hampel_coef = total_TL_bisquare_coef = matrix(NA, nrow = 4, ncol = 47)
total_TL_p = total_TL_hampel_p = total_TL_bisquare_p = matrix(NA, nrow = 4, ncol = 47)
total_TL_R_squared_prefecture = total_TL_hampel_R_squared_prefecture = total_TL_bisquare_R_squared_prefecture = vector("numeric", 47)

total_fitted = total_bisquare_fitted = matrix(NA, nrow = 101, ncol = 47)

for(ij in 1:47)
{
  if(length(total_TL_outliers[[ij]]) > 0)
  {
    age_select = setdiff(age_all, as.numeric(rownames(total_TL_outliers[[ij]])))
    y = log(temporal_long_run_total_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_total_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_long_run_total_var[,ij], base = 10)
    x = log(temporal_total_mean[,ij], base = 10)
  }
  
  y = log(temporal_long_run_total_var[,ij], base = 10)
  x = log(temporal_total_mean[,ij], base = 10)
  
  OLS_model = lm(y ~ x + I(x^2) + I(x^3))
  total_TL_coef[,ij] = OLS_model$coefficients
  total_TL_p[,ij] = summary(OLS_model)$coef[,4]
  total_TL_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  total_fitted[,ij] = OLS_model$fitted.values
  
  hampel_model = rlm(y ~ x + I(x^2) + I(x^3), psi = psi.hampel_95, maxit = 500)
  total_TL_hampel_coef[,ij] = hampel_model$coefficients
  total_TL_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  total_TL_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x + I(x^2) + I(x^3), psi = psi.bisquare, maxit = 500)
  total_TL_bisquare_coef[,ij] = bisquare_model$coefficients
  total_TL_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  total_TL_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  total_bisquare_fitted[,ij] = bisquare_model$fitted.values
}

## BIC
total_classic_BIC = total_quadratic_BIC = total_cubic_BIC = vector("numeric", 47)
for(ij in 1:47)
{
  if(length(total_TL_outliers[[ij]]) > 0)
  {
    age_select = setdiff(age_all, as.numeric(rownames(total_TL_outliers[[ij]])))
    y = log(temporal_long_run_total_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_total_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_long_run_total_var[,ij], base = 10)
    x = log(temporal_total_mean[,ij], base = 10)
  }
  
  y = log(temporal_long_run_total_var[,ij], base = 10)
  x = log(temporal_total_mean[,ij], base = 10)
  
  OLS_model = lm(y ~ x)
  total_classic_BIC[ij] = BIC(OLS_model)
  
  OLS_model_quadratic = lm(y ~ x + I(x^2))
  total_quadratic_BIC[ij] = BIC(OLS_model_quadratic)
  
  OLS_model_cubic = lm(y ~ x + I(x^2) + I(x^3))
  total_cubic_BIC[ij] = BIC(OLS_model_cubic)
}

###########################
# Order 3 TL scatter plots
###########################

for(ij in 1:47)
{
  # female
  age_select = setdiff(age_all, as.numeric(rownames(female_TL_outliers[[ij]])))
  y_female = log(temporal_long_run_female_var[age_all %in% age_select,ij], base = 10)
  x_female = log(temporal_female_mean[age_all %in% age_select,ij], base = 10)
  
  OLS_model_female = lm(y_female ~ x_female + I(x_female^2) + I(x_female^3))
  newdata_female = data.frame(x_female = seq(from = min(x_female)-0.1, to = max(x_female)+0.2, by = 0.01))
  pred_female = predict(OLS_model_female, newdata_female, interval = "predict")
  
  savepdf(paste("prediction_female", ij, sep = "_"), width = 20, height = 20, toplines = 0.8)
  par(mar = c(5,7,5,3))
  plot(x = log10(temporal_female_mean[,ij]), y = log10(temporal_long_run_female_var[,ij]), xlim = c(min(log10(temporal_female_mean[,ij]))-0.2, max(log10(temporal_female_mean[,ij]))+0.2), ylim = c(min(pred_female[,2]) - 0.2, max(pred_female[,3]+0.2)), xlab = expression(paste(log[10], "(Mean)")), ylab = expression(paste(log[10], "(Variance)")), cex.lab = 2, cex.axis = 2, col = rainbow(101, end = 0.9))
  if(nrow(female_TL_outliers[[ij]]) > 0)
  {
    points(female_TL_outliers[[ij]][,1], female_TL_outliers[[ij]][,2], pch = 21, bg = 3:(nrow(female_TL_outliers[[ij]])+2), col = "black", cex = 2)
    legend("topleft", col = 3:(nrow(female_TL_outliers[[ij]])+2), pch = 16, legend = rownames(female_TL_outliers[[ij]]), cex = 2)
  }
  title(main = state[ij+1], line = 1, cex.main = 3, font.main= 2)
  lines(x = newdata_female$x, y = pred_female[,1], col = 2, lwd = 2)
  polygon(c(rev(newdata_female$x), newdata_female$x), c(rev(pred_female[,3]), pred_female[,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
  dev.off()
  
  # male
  age_select = setdiff(age_all, as.numeric(rownames(male_TL_outliers[[ij]])))
  y_male = log(temporal_long_run_male_var[age_all %in% age_select,ij], base = 10)
  x_male = log(temporal_male_mean[age_all %in% age_select,ij], base = 10)
  
  OLS_model_male = lm(y_male ~ x_male + I(x_male^2) + I(x_male^3))
  newdata_male = data.frame(x_male = seq(from = min(x_male, na.rm = T)-0.1, to = max(x_male, na.rm = T)+0.2, by = 0.01))
  pred_male = predict(OLS_model_male, newdata_male, interval = "predict")
  
  savepdf(paste("prediction_male", ij, sep = "_"), width = 20, height = 20, toplines = 0.8)
  par(mar = c(5,7,5,3))
  plot(x = log10(temporal_male_mean[,ij]), y = log10(temporal_long_run_male_var[,ij]), xlim = c(min(log10(temporal_male_mean[,ij]), na.rm = T)-0.2, max(log10(temporal_male_mean[,ij]), na.rm = T)+0.2), ylim = c(min(pred_male[,2]) - 0.2, max(pred_male[,3]+0.2)), xlab = expression(paste(log[10], "(Mean)")), ylab = expression(paste(log[10], "(Variance)")), cex.lab = 2, cex.axis = 2, col = rainbow(101, end = 0.9))
  if(nrow(male_TL_outliers[[ij]]) > 0)
  {
    points(x = male_TL_outliers[[ij]][,1], y = male_TL_outliers[[ij]][,2], pch = 21, bg = 3:(nrow(male_TL_outliers[[ij]])+2), col = "black", cex = 2)
    legend("topleft", col = 3:(nrow(male_TL_outliers[[ij]])+2), pch = 16, legend = rownames(male_TL_outliers[[ij]]), cex = 2)
  }
  title(main = state[ij+1], line = 1, cex.main = 3, font.main= 2)
  lines(x = newdata_male$x, y = pred_male[,1], col = 2, lwd = 2)
  polygon(c(rev(newdata_male$x), newdata_male$x), c(rev(pred_male[,3]), pred_male[,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
  dev.off()
  
  # total
  age_select = setdiff(age_all, as.numeric(rownames(total_TL_outliers[[ij]])))
  y_total = log(temporal_long_run_total_var[age_all %in% age_select,ij], base = 10)
  x_total = log(temporal_total_mean[age_all %in% age_select,ij], base = 10)
  
  OLS_model_total = lm(y_total ~ x_total + I(x_total^2) + I(x_total^3))
  newdata_total = data.frame(x_total = seq(from = min(x_total)-0.1, to = max(x_total)+0.2, by = 0.01))
  pred_total = predict(OLS_model_total, newdata_total, interval = "predict")
  
  savepdf(paste("prediction_total", ij, sep = "_"), width = 20, height = 20, toplines = 0.8)
  par(mar = c(5,7,5,3))
  plot(x = log10(temporal_total_mean[,ij]), y = log10(temporal_long_run_total_var[,ij]), xlim = c(min(log10(temporal_total_mean[,ij]))-0.2, max(log10(temporal_total_mean[,ij]))+0.2), ylim = c(min(pred_total[,2]) - 0.2, max(pred_total[,3]+0.2)), xlab = expression(paste(log[10], "(Mean)")), ylab = expression(paste(log[10], "(Variance)")), cex.lab = 2, cex.axis = 2, col = rainbow(101, end = 0.9))
  if(nrow(total_TL_outliers[[ij]]) > 0)
  {
    points(total_TL_outliers[[ij]][,1], total_TL_outliers[[ij]][,2], pch = 21, bg = 3:(nrow(total_TL_outliers[[ij]])+2), col = "black", cex = 2)
    legend("topleft", col = 3:(nrow(total_TL_outliers[[ij]])+2), pch = 16, legend = rownames(total_TL_outliers[[ij]]), cex = 2)
  }
  title(main = state[ij+1], line = 1, cex.main = 3, font.main= 2)
  lines(x = newdata_total$x, y = pred_total[,1], col = 2, lwd = 2)
  polygon(c(rev(newdata_total$x), newdata_total$x), c(rev(pred_total[,3]), pred_total[,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
  dev.off()
  
}

################################
# Plots of temporal coeffcients
################################

# female

savepdf("Fig_5b", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(1:47, female_TL_coef[1,], xlab = "", ylab = "", ylim = c(-4.3, -2.9))
points(1:47, female_TL_hampel_coef[1,], col = 2, pch = 2)
points(1:47, female_TL_bisquare_coef[1,], col = 4, pch = 4)
dev.off()

savepdf("Fig_5e", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(1:47, female_TL_coef[2,], xlab = "", ylab = "", ylim = c(-2.7, 0.1))
points(1:47, female_TL_hampel_coef[2,], col = 2, pch = 2)
points(1:47, female_TL_bisquare_coef[2,], col = 4, pch = 4)
dev.off()

savepdf("Fig_5h", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(1:47, female_TL_coef[3,], xlab = "", ylab = "", ylim = c(-2.6, -1))
points(1:47, female_TL_hampel_coef[3,], col = 2, pch = 2)
points(1:47, female_TL_bisquare_coef[3,], col = 4, pch = 4)
dev.off()

savepdf("Fig_5k", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(1:47, female_TL_coef[4,], xlab = "Prefecture ordered geographically by North to South", ylab = "", ylim = c(-0.41,-0.17))
points(1:47, female_TL_hampel_coef[4,], col = 2, pch = 2)
points(1:47, female_TL_bisquare_coef[4,], col = 4, pch = 4)
dev.off()

# male

savepdf("Fig_5c", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(1:47, male_TL_coef[1,], xlab = "", ylab = "", ylim = c(-4.5, -2.1))
points(1:47, male_TL_hampel_coef[1,], col = 2, pch = 2)
points(1:47, male_TL_bisquare_coef[1,], col = 4, pch = 4)
dev.off()

savepdf("Fig_5f", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(1:47, male_TL_coef[2,], xlab = "", ylab = "", ylim = c(-2.5,2.9))
points(1:47, male_TL_hampel_coef[2,], col = 2, pch = 2)
points(1:47, male_TL_bisquare_coef[2,], col = 4, pch = 4)
dev.off()

savepdf("Fig_5i", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(1:47, male_TL_coef[3,], xlab = "", ylab = "", ylim = c(-2.3,0.4))
points(1:47, male_TL_hampel_coef[3,], col = 2, pch = 2)
points(1:47, male_TL_bisquare_coef[3,], col = 4, pch = 4)
dev.off()

savepdf("Fig_5l", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(1:47, male_TL_coef[4,], xlab = "Prefecture ordered geographically by North to South", ylab = "", ylim = c(-0.34,0.02))
points(1:47, male_TL_hampel_coef[4,], col = 2, pch = 2)
points(1:47, male_TL_bisquare_coef[4,], col = 4, pch = 4)
dev.off()

# total

savepdf("Fig_5a", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(1:47, (total_TL_coef[1,]), xlab = "", ylab = expression(log[10](hat(a[3]))), main = "", ylim = c(-4.1, -2), cex.lab = 1.5)
points(1:47, (total_TL_hampel_coef[1,]), col = 2, pch = 2)
points(1:47, (total_TL_bisquare_coef[1,]), col = 4, pch = 4)
legend("topleft", c("OLS", "Robust (Hampel)", "Robust (Bisquare)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 1)
dev.off()

savepdf("Fig_5d", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(1:47, total_TL_coef[2,], xlab = "", ylab = expression(hat(b)[3]),  main = "", cex.lab = 1.5)
points(1:47, total_TL_hampel_coef[2,], col = 2, pch = 2)
points(1:47, total_TL_bisquare_coef[2,], col = 4, pch = 4)
dev.off()

savepdf("Fig_5g", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(1:47, total_TL_coef[3,], xlab = "", ylab = expression(hat(c)[3]), main = "", cex.lab = 1.5)
points(1:47, total_TL_hampel_coef[3,], col = 2, pch = 2)
points(1:47, total_TL_bisquare_coef[3,], col = 4, pch = 4)
dev.off()

savepdf("Fig_5j", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(1:47, total_TL_coef[4,], xlab = "", 
     ylab = expression(hat(d)[3]), ylim = c(-0.4, -0.07), main = "", cex.lab = 1.5)
title(xlab = "Prefecture ordered geographically by North to South")
points(1:47, total_TL_hampel_coef[4,], col = 2, pch = 2)
points(1:47, total_TL_bisquare_coef[4,], col = 4, pch = 4)
dev.off()


################################################
# Plots of temporal mean and temporal variance
###############################################

sort(abs(female_TL_bisquare_coef[4,]-female_TL_coef[4,]), index.return = T, decreasing = T)$ix
sort(abs(male_TL_bisquare_coef[4,]-male_TL_coef[4,]), index.return = T, decreasing = T)$ix
sort(male_TL_coef[4,]-male_TL_bisquare_coef[4,], index.return = T, decreasing = T)$ix
sort(abs(total_TL_bisquare_coef[4,]-total_TL_coef[4,]), index.return = T, decreasing = T)$ix

## temporal variance

savepdf("total_var_temporal", width = 14, height = 10, toplines = 0.8)
matplot(log10(temporal_long_run_total_var), lty = 1, type = "l", col = "grey", ylab = expression(paste(log[10], "(Temporal Variance)")), xlab = "", ylim = c(-9,0))
lines(log10(temporal_long_run_total_var[,33]), lwd = 2, col = 1)
lines(log10(temporal_long_run_total_var[,6]), lwd = 2, col = 2)
lines(log10(temporal_long_run_total_var[,42]), lwd = 2, col = 3)
lines(log10(temporal_long_run_total_var[,20]), lwd = 2, col = 4)
lines(log10(temporal_long_run_total_var[,32]), lwd = 2, col = 5)
dev.off()

savepdf("female_var_temporal", width = 14, height = 10, toplines = 0.8)
matplot(log10(temporal_long_run_female_var), lty = 1, type = "l", col = "grey", ylab = "", xlab = "", ylim = c(-9,0))
lines(log10(temporal_long_run_female_var[,46]), lwd = 2, col = "brown")
lines(log10(temporal_long_run_female_var[,42]), lwd = 2, col = 3)
lines(log10(temporal_long_run_female_var[,27]), lwd = 2, col = 2)
lines(log10(temporal_long_run_female_var[,30]), lwd = 2, col = "aquamarine")
lines(log10(temporal_long_run_female_var[,40]), lwd = 2, col = "deepskyblue2")
dev.off()


savepdf("male_var_temporal", width = 14, height = 10, toplines = 0.8)
matplot(log10(temporal_long_run_male_var), lty = 1, type = "l", col = "grey", ylab = "", xlab = "", ylim = c(-10,-0.5))
lines(log10(temporal_long_run_male_var[,28]), lwd = 2, col = "deepskyblue2")
lines(log10(temporal_long_run_male_var[,3]), lwd = 2, col = "deeppink")
lines(log10(temporal_long_run_male_var[,4]), lwd = 2, col = "darkorange")
# lines(log10(temporal_long_run_male_var[,43]), lwd = 2, col = "darkgreen")
# lines(log10(temporal_long_run_male_var[,44]), lwd = 2, col = "darkviolet")
dev.off()

## temporal mean

savepdf("total_mean_temporal", width = 14, height = 10, toplines = 0.8)
matplot(log10(temporal_total_mean), lty = 1, type = "l", col = "grey", ylab = expression(paste(log[10], "(Temporal Mean)")), xlab = "Age", ylim = c(-4.2,0))
lines(log10(temporal_total_mean[,20]), lwd = 2, col = 1)
lines(log10(temporal_total_mean[,14]), lwd = 2, col = 2)
lines(log10(temporal_total_mean[,11]), lwd = 2, col = 3)
lines(log10(temporal_total_mean[,29]), lwd = 2, col = 4)
lines(log10(temporal_total_mean[,16]), lwd = 2, col = 5)
dev.off()

savepdf("female_mean_temporal", width = 14, height = 10, toplines = 0.8)
matplot(log10(temporal_female_mean), lty = 1, type = "l", col = "grey", ylab = "", xlab = "Age", ylim = c(-4.2,0))
lines(log10(temporal_female_mean[,20]), lwd = 2, col = 1)
lines(log10(temporal_female_mean[,11]), lwd = 2, col = "brown")
lines(log10(temporal_female_mean[,14]), lwd = 2, col = 2)
lines(log10(temporal_female_mean[,13]), lwd = 2, col = "aquamarine")
lines(log10(temporal_female_mean[,4]), lwd = 2, col = "deepskyblue2")
dev.off()


savepdf("male_mean_temporal", width = 14, height = 10, toplines = 0.8)
matplot(log10(temporal_male_mean), lty = 1, type = "l", col = "grey", ylab = "", xlab = "Age", ylim = c(-4.2,0))
lines(log10(temporal_male_mean[,28]), lwd = 2, col = "deepskyblue2")
lines(log10(temporal_male_mean[,3]), lwd = 2, col = "deeppink")
lines(log10(temporal_male_mean[,29]), lwd = 2, col = "darkorange")
# lines(log10(temporal_male_mean[,23]), lwd = 2, col = "darkgreen")
# lines(log10(temporal_male_mean[,44]), lwd = 2, col = "darkviolet")
dev.off()

########################################################
# Plots of differences between OLS and robust estimates
########################################################

# savepdf("total_fitted_diff", width = 14, height = 10, toplines = 0.8)
# matplot(total_bisquare_fitted - total_fitted, lty = 1, type = "l", col = "grey", ylim = c(-0.3,0.1), ylab = "Difference in fitted values", xlab = "")
# lines((total_bisquare_fitted - total_fitted)[,20], lwd = 2, col = 1)
# lines((total_bisquare_fitted - total_fitted)[,14], lwd = 2, col = 2)
# lines((total_bisquare_fitted - total_fitted)[,11], lwd = 2, col = 3)
# lines((total_bisquare_fitted - total_fitted)[,29], lwd = 2, col = 4)
# lines((total_bisquare_fitted - total_fitted)[,16], lwd = 2, col = 5)
# legend("bottomleft", lty = c(1,1,1,1,1), col = c(1,2,3,4,5), c("Nagano", "Kanagawa", "Saitama", "Nara", "Toyama"), lwd = c(2,2,2,2,2))
# dev.off()
# 
# savepdf("female_fitted_diff", width = 14, height = 10, toplines = 0.8)
# matplot(female_bisquare_fitted - female_fitted, lty = 1, type = "l", col = "grey", ylim = c(-0.35,0.25), ylab = "", xlab = "")
# lines((female_bisquare_fitted - female_fitted)[,20], lwd = 2, col = 1)
# lines((female_bisquare_fitted - female_fitted)[,11], lwd = 2, col = "brown")
# lines((female_bisquare_fitted - female_fitted)[,14], lwd = 2, col = 2)
# lines((female_bisquare_fitted - female_fitted)[,13], lwd = 2, col = "aquamarine")
# lines((female_bisquare_fitted - female_fitted)[,4], lwd = 2, col = "deepskyblue2")
# legend("bottomleft", lty = c(1,1,1,1,1), col = c(1, "brown", 2, "aquamarine", "deepskyblue2"), c("Nagano", "Saitama", "Kanagawa", "Tokyo", "Miyagi"), lwd = c(2,2,2,2,2))
# dev.off()
# 
# savepdf("male_fitted_diff", width = 14, height = 10, toplines = 0.8)
# matplot(male_bisquare_fitted - male_fitted, lty = 1, type = "l", col = "grey", ylim = c(-0.4,0.08), ylab = "", xlab = "")
# lines((male_bisquare_fitted - male_fitted)[,28], lwd = 2, col = 1)
# lines((male_bisquare_fitted - male_fitted)[,3], lwd = 2, col = 2)
# lines((male_bisquare_fitted - male_fitted)[,15], lwd = 2, col = 4)
# # lines((male_bisquare_fitted - male_fitted)[,24], lwd = 2, col = "darkorange")
# # lines((male_bisquare_fitted - male_fitted)[,23], lwd = 2, col = "darkgreen")
# # lines((male_bisquare_fitted - male_fitted)[,44], lwd = 2, col = "darkviolet")
# legend("bottomleft", lty = c(1,1,1,1,1), col = c("deepskyblue2", "deeppink", "darkorange", "darkgreen", "darkviolet"), c("Miyagi", "Chiba", "Mie", "Aichi", "Oita"), lwd = c(2,2,2,2,2))
# dev.off()

####################

savepdf("male_fitted_diff", width = 14, height = 10, toplines = 0.8)
par(mar = c(4,4,2,2))
matplot(male_bisquare_fitted[,c(28,3)], col = c(1,2), lty = c(1,1), lwd = c(2,2), type = "l", ylim = c(-9.5, -3), ylab = "Fitted Long-run Temporal Variances", xlab = "Age", cex.lab = 1.5)
lines(male_fitted[,28], lwd = 2, col = 1, lty = 2)
lines(male_fitted[,3], lwd = 2, col = 2, lty = 2)
legend("topleft", lty = c(2,1,2,1), col = c(1,1,2,2), c("Hyogo OLS", "Hyogo Bisquare", "Iwate OLS", "Iwate Bisquare"), lwd = c(2,2,2,2))
dev.off()

savepdf("male_mean_temporal", width = 14, height = 10, toplines = 0.8)
par(mar = c(4,4,2,2))
matplot(log10(temporal_male_mean), lty = 1, type = "l", col = "grey",  xlab = "Age", ylim = c(-4.2,0), ylab = expression(paste(log[10], "(Long-run Temporal Mean)")), cex.lab = 1.5)
lines(log10(temporal_male_mean[,28]), lwd = 2, col = 1)
lines(log10(temporal_male_mean[,3]), lwd = 2, col = 2)
dev.off()

savepdf("male_var_temporal", width = 14, height = 10, toplines = 0.8)
par(mar = c(4,4,2,2))
matplot(log10(temporal_long_run_male_var), lty = 1, type = "l", col = "grey", xlab = "Age", ylim = c(-10,-0.5), ylab = expression(paste(log[10], "(Long-run Temporal Variance)")), cex.lab = 1.5)
lines(log10(temporal_long_run_male_var[,28]), lwd = 2, col = 1)
lines(log10(temporal_long_run_male_var[,3]), lwd = 2, col = 2)
dev.off()











