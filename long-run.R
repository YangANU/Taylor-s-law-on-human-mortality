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
  
  hampel_model = rlm(log(temporal_long_run_female_var[,ij], base = 10) ~ log(temporal_female_mean[,ij], base = 10), psi = psi.hampel, maxit = 500)
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