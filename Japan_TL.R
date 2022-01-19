################
# Rainbow plots
################

setwd("/Volumes/MY PASSPORT/Dropbox/Todos/Taylor_Law_JHMD/tex/TL_JMD/plots")
savepdf("Japan_female", width = 12, height = 10, toplines = 1)
plot(Japan, series = "female")
dev.off()

savepdf("Japan_male", width = 12, height = 10, toplines = 1)
plot(Japan, series = "male")
dev.off()

savepdf("Japan_total", width = 12, height = 10, toplines = 1)
plot(Japan, series = "total")
dev.off()

savepdf("Japan_smooth", width = 12, height = 10, toplines = 1)
plot(Japan_smooth, series = "total")
dev.off()

savepdf("Fukushima_smooth", width = 12, height = 10, toplines = 1)
plot(Fukushima_smooth, series = "total")
dev.off()

savepdf("Miyagi_smooth", width = 12, height = 10, toplines = 1)
plot(Miyagi_smooth, series = "total")
dev.off()

savepdf("Iwate_smooth", width = 12, height = 10, toplines = 1)
plot(Iwate_smooth, series = "total")
dev.off()


##############
# Image plots
##############

require(RColorBrewer)

# ratio between each prefecture and total 

prefecture_total = prefecture_female = prefecture_male = array(, dim = c(101, 44, 47))
for(iw in 2:48)
{
    gettotal <- get(state[iw])$rate$total
    gettotal[gettotal==0] <- NA
    getmale <- get(state[iw])$rate$male
    getmale[getmale==0] <- NA
    getfemale <- get(state[iw])$rate$female
    getfemale[getfemale==0] <- NA
    prefecture_total[,,iw-1]  = log(gettotal/Japan$rate$total)
    prefecture_female[,,iw-1] = log(getfemale/Japan$rate$female) 
    prefecture_male[,,iw-1]   = log(getmale/Japan$rate$male)
}  

# raw (averaged over age and prefecture)

total_rate_raw = apply(prefecture_total, c(1,3), mean, na.rm = TRUE)
female_rate_raw = apply(prefecture_female, c(1,3), mean, na.rm = TRUE)
male_rate_raw = apply(prefecture_male, c(1,3), mean, na.rm = TRUE)
colnames(male_rate_raw) = colnames(female_rate_raw) = colnames(total_rate_raw) = state[2:48]

total_rate_raw_arrange = total_rate_raw[,47:1]
female_rate_raw_arrange = female_rate_raw[,47:1]
male_rate_raw_arrange = male_rate_raw[,47:1]

# raw (averaged over year and prefecture)

total_rate_raw_year  = apply(prefecture_total, c(2,3), mean, na.rm = TRUE)
female_rate_raw_year = apply(prefecture_female, c(2,3), mean, na.rm = TRUE)
male_rate_raw_year   = apply(prefecture_male, c(2,3), mean, na.rm = TRUE)
colnames(male_rate_raw_year) = colnames(female_rate_raw_year) = colnames(total_rate_raw_year) = state[2:48]

total_rate_raw_arrange_year = total_rate_raw_year[,47:1]
female_rate_raw_arrange_year = female_rate_raw_year[,47:1]
male_rate_raw_arrange_year = male_rate_raw_year[,47:1]

# raw (averaged over year and age)
total_rate_raw_prefecture = apply(prefecture_total, c(1,2), mean, na.rm = TRUE)
female_rate_raw_prefecture = apply(prefecture_female, c(1,2), mean, na.rm = TRUE)
male_rate_raw_prefecture = apply(prefecture_male, c(1,2), mean, na.rm = TRUE)
colnames(total_rate_raw_prefecture) = colnames(female_rate_raw_prefecture) = colnames(male_rate_raw_prefecture) = 1975:2018

require(RColorBrewer)
par(mfrow = c(2, 3))
image(0:100, 1:47, total_rate_raw_arrange, col = brewer.pal(7, "RdBu"), xlab = "Age", ylab = "Prefecture", main = "Total", zlim = c(-1, 1)) 
box()
image(0:100, 1:47, female_rate_raw_arrange, col = brewer.pal(7, "RdBu"), xlab = "Age", ylab = "", main = "Female", zlim = c(-1, 1))
box()
image(0:100, 1:47, male_rate_raw_arrange, col = brewer.pal(7, "RdBu"), xlab = "Age", ylab = "", main = "Male", zlim = c(-1, 1))
box()

image(1975:2018, 1:47, total_rate_raw_arrange_year, col = brewer.pal(7, "RdBu"), xlab = "Year", ylab = "Prefecture", zlim = c(-1, 1))
box()
image(1975:2018, 1:47, female_rate_raw_arrange_year, col = brewer.pal(7, "RdBu"), xlab = "Year", ylab = "", zlim = c(-1, 1))
box()
image(1975:2018, 1:47, male_rate_raw_arrange_year, col = brewer.pal(7, "RdBu"), xlab = "Year", ylab = "", zlim = c(-1, 1))
box()

ytick = c(1, 7, 17, 27 ,37, 47)
ytick_print = c(47, 40, 30, 20, 10, 1)

savepdf("image_plot_1", width = 22, height = 7, toplines = 0.8, pointsize = 12)
par(mfrow = c(1,3))
image(0:100, 1:47, total_rate_raw_arrange, col = brewer.pal(7, "RdBu"), xlab = "Age", ylab = "Prefecture", main = "Total", zlim = c(-1, 1),yaxt="n")
axis(side=2, at=ytick, labels = FALSE)
text(par("usr")[1], ytick, labels = ytick_print, pos = 2, xpd = TRUE)
box()
image(0:100, 1:47, female_rate_raw_arrange, col = brewer.pal(7, "RdBu"), xlab = "Age", ylab = "", main = "Female", zlim = c(-1, 1), yaxt="n")
axis(side=2, at=ytick, labels = FALSE)
text(par("usr")[1], ytick, labels = ytick_print, pos = 2, xpd = TRUE)
box()
image(0:100, 1:47, male_rate_raw_arrange, col = brewer.pal(7, "RdBu"), xlab = "Age", ylab = "", main = "Male", zlim = c(-1, 1), yaxt="n")
axis(side=2, at=ytick, labels = FALSE)
text(par("usr")[1], ytick, labels = ytick_print, pos = 2, xpd = TRUE)
box()
dev.off()

savepdf("image_plot_2", width = 22, height = 7, toplines = 0.8, pointsize = 12)
par(mfrow = c(1,3))
image(1975:2018, 1:47, total_rate_raw_arrange_year, col = brewer.pal(7, "RdBu"), xlab = "Year", ylab = "Prefecture", zlim = c(-1, 1), yaxt="n")
axis(side=2, at=ytick, labels = FALSE)
text(par("usr")[1], ytick, labels = ytick_print, pos = 2, xpd = TRUE)
box()
image(1975:2018, 1:47, female_rate_raw_arrange_year, col = brewer.pal(7, "RdBu"), xlab = "Year", ylab = "", zlim = c(-1, 1), yaxt="n")
axis(side=2, at=ytick, labels = FALSE)
text(par("usr")[1], ytick, labels = ytick_print, pos = 2, xpd = TRUE)
box()
image(1975:2018, 1:47, male_rate_raw_arrange_year, col = brewer.pal(7, "RdBu"), xlab = "Year", ylab = "", zlim = c(-1, 1), yaxt="n")
axis(side=2, at=ytick, labels = FALSE)
text(par("usr")[1], ytick, labels = ytick_print, pos = 2, xpd = TRUE)
box()
dev.off()

savepdf("image_plot_3", width = 22, height = 7, toplines = 0.8, pointsize = 12)
par(mfrow = c(1,3))
image(0:100, 1975:2018, total_rate_raw_prefecture, col = brewer.pal(7, "RdBu"), xlab = "Age", ylab = "Year", main = "Total", zlim = c(-1, 1))
image(0:100, 1975:2018, female_rate_raw_prefecture, col = brewer.pal(7, "RdBu"), xlab = "Age", ylab = "", main = "Female", zlim = c(-1, 1))
image(0:100, 1975:2018, male_rate_raw_prefecture, col = brewer.pal(7, "RdBu"), xlab = "Age", ylab = "", main = "Male", zlim = c(-1, 1))
dev.off()


#############
# Taylor law
#############

# across prefectures

n_age = 101
age_all = 0:100
year = 1975:2018
n_year = length(year)
n_prefecture = 47

total_data = male_data = female_data = total_data_smooth = male_data_smooth = female_data_smooth = array(NA, dim = c(n_age, n_year, n_prefecture+1))
for(ik in 1:(n_prefecture + 1))
{
    female_data[,,ik] = get(state[ik])$rate$female
    male_data[,,ik]   = get(state[ik])$rate$male
    total_data[,,ik]  = get(state[ik])$rate$total
    
    female_data_smooth[,,ik] = get(state_smooth[ik])$rate$female
    male_data_smooth[,,ik]   = get(state_smooth[ik])$rate$male
    total_data_smooth[,,ik]  = get(state_smooth[ik])$rate$total
    print(ik)
}

##################################
# plot temporal mean and variance
##################################

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
  female_select = data.frame(log_var = log10(temporal_female_mean[,ij]), log_mean = log10(temporal_female_var[,ij]))
  female_TL_mahalanobis_outliers[[ij]] = female_select[female_TL_mahalanobis[,ij] > qchisq(p = 0.95, df = 2), , drop = FALSE]
  female_TL_rstudent_outliers[[ij]] = female_select[abs(rstudent(lm(log10(temporal_female_var[,ij]) ~ log10(temporal_female_mean[,ij])))) > 2, , drop = FALSE]
  outlier_age = as.numeric(intersect(row.names(female_TL_mahalanobis_outliers[[ij]]), row.names(female_TL_rstudent_outliers[[ij]])))
  female_TL_outliers[[ij]] = female_select[outlier_age+1,]
  
  male_select = data.frame(log_var = log10(temporal_male_mean[,ij]), log_mean = log10(temporal_male_var[,ij]))
  male_TL_mahalanobis_outliers[[ij]] = male_select[male_TL_mahalanobis[,ij] > qchisq(p = 0.95, df = 2), , drop = FALSE]
  male_TL_rstudent_outliers[[ij]] = male_select[abs(rstudent(lm(log10(temporal_male_var[,ij]) ~ log10(temporal_male_mean[,ij])))) > 2, , drop = FALSE]
  outlier_age = as.numeric(intersect(row.names(male_TL_mahalanobis_outliers[[ij]]), row.names(male_TL_rstudent_outliers[[ij]])))
  male_TL_outliers[[ij]] = male_select[outlier_age+1,]
  
  total_select = data.frame(log_var = log10(temporal_total_mean[,ij]), log_mean = log10(temporal_total_var[,ij]))
  total_TL_mahalanobis_outliers[[ij]] = total_select[total_TL_mahalanobis[,ij] > qchisq(p = 0.95, df = 2), , drop = FALSE]
  total_TL_rstudent_outliers[[ij]] = total_select[abs(rstudent(lm(log10(temporal_total_var[,ij]) ~ log10(temporal_total_mean[,ij])))) > 2, , drop = FALSE]
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

# benchmark of the classic OLS TL

female_classic = female_classic_hampel = female_classic_bisquare = matrix(NA, nrow = 2, ncol = 47)
female_classic_p = female_classic_hampel_p = female_classic_bisquare_p = matrix(NA, nrow = 2, ncol = 47)
female_classic_R_squared_prefecture = female_classic_hampel_R_squared_prefecture = female_classic_bisquare_R_squared_prefecture = vector("numeric", 47)

for(ij in 1:47)
{
  if(length(female_TL_outliers[[ij]]) > 0)
  {
    age_select = setdiff(age_all, as.numeric(rownames(female_TL_outliers[[ij]])))
    y = log(temporal_female_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_female_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_female_var[,ij], base = 10)
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

# benchmark of quadratic OLS TL

female_quadratic = female_quadratic_hampel = female_quadratic_bisquare = matrix(NA, nrow = 3, ncol = 47)
female_quadratic_p = female_quadratic_hampel_p = female_quadratic_bisquare_p = matrix(NA, nrow = 3, ncol = 47)
female_quadratic_R_squared_prefecture = female_quadratic_hampel_R_squared_prefecture = female_quadratic_bisquare_R_squared_prefecture = vector("numeric", 47)

for(ij in 1:47)
{
  if(length(female_TL_outliers[[ij]]) > 0)
  {
    age_select = setdiff(age_all, as.numeric(rownames(female_TL_outliers[[ij]])))
    y = log(temporal_female_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_female_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_female_var[,ij], base = 10)
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

# cubic TL with OLS estimation

female_TL_coef = female_TL_hampel_coef = female_TL_bisquare_coef = matrix(NA, nrow = 4, ncol = 47)
female_TL_p = female_TL_hampel_p = female_TL_bisquare_p = matrix(NA, nrow = 4, ncol = 47)

female_TL_R_squared_prefecture = female_TL_hampel_R_squared_prefecture = female_TL_bisquare_R_squared_prefecture = vector("numeric", 47)

for(ij in 1:47)
{
  if(length(female_TL_outliers[[ij]]) > 0)
  {
    age_select = setdiff(age_all, as.numeric(rownames(female_TL_outliers[[ij]])))
    y = log(temporal_female_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_female_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_female_var[,ij], base = 10)
    x = log(temporal_female_mean[,ij], base = 10)
  }
  
  OLS_model = lm(y ~ x + I(x^2) + I(x^3))
  female_TL_coef[,ij] = OLS_model$coefficients
  female_TL_p[,ij] = summary(OLS_model)$coef[,4]
  female_TL_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared

  hampel_model = rlm(y ~ x + I(x^2) + I(x^3), psi = psi.hampel, maxit = 200)
  female_TL_hampel_coef[,ij] = hampel_model$coefficients
  female_TL_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  female_TL_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x + I(x^2) + I(x^3), psi = psi.bisquare, maxit = 200)
  female_TL_bisquare_coef[,ij] = bisquare_model$coefficients
  female_TL_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  female_TL_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
}

###################
savepdf("female_TL_temporal_order1", width = 12, height = 10, toplines = 0.8)
plot(1:47, female_TL_coef[2,], xlab = "", 
     ylab = "", ylim = c(-0.9, 1.55))
     #,main = expression(paste(log[10], "(temporal variance) versus ", log[10], "(temporal mean)")))
points(1:47, female_TL_hampel_coef[2,], col = 2, pch = 2)
points(1:47, female_TL_bisquare_coef[2,], col = 4, pch = 4)
#legend("topright", c("LM", "RLM (Hampel weight)", "RLM (Bisquare weight)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 1)
dev.off()

savepdf("female_TL_temporal_order2", width = 12, height = 10, toplines = 0.8)
plot(1:47, female_TL_coef[3,], xlab = "", 
     ylab = "", ylim = c(-1.6,-0.3))
#,main = expression(paste(log[10], "(temporal variance) versus ", log[10], "(temporal mean)")))
points(1:47, female_TL_hampel_coef[3,], col = 2, pch = 2)
points(1:47, female_TL_bisquare_coef[3,], col = 4, pch = 4)
dev.off()

savepdf("female_TL_temporal_order3", width = 12, height = 10, toplines = 0.8)
plot(1:47, female_TL_coef[4,], xlab = "Prefecture ordered geographically by North to South", 
     ylab = "", ylim = c(-0.26,-0.06))
#,main = expression(paste(log[10], "(temporal variance) versus ", log[10], "(temporal mean)")))
points(1:47, female_TL_hampel_coef[4,], col = 2, pch = 2)
points(1:47, female_TL_bisquare_coef[4,], col = 4, pch = 4)
dev.off()
#######################

savepdf("female_TL_temporal", width = 30, height = 20, toplines = 0.8)
par(mar = c(3, 3, 3, 3))
scatter3D(x = female_TL_coef[2,], y = female_TL_coef[3,], z = female_TL_coef[4,], colvar = 1:47, col = gg2.col(47), bty = "g", pch = 3, phi = 0, theta = 30, ticktype = "detailed", xlim = c(-1.4,2), ylim = c(-1.5, -0.3), zlim = c(-0.3, -0.05), colkey = list(side = 2, at = 1:47, width = 0.6, length = 0.9, dist = -0.15), clab = c("Prefecture","Index"), xlab = "", ylab = "", zlab = "")

text(0.25, -0.5, adj = c(-1,0.2), expression(paste(log[10], "(Mean)")^2),srt = 50, cex = 0.8)
text(-0.2, -0.45, adj = c(0.7,0), expression(paste(log[10], "(Mean)")),srt = -15, cex = 1)
text(-0.5, 0, adj = c(0.5,-0.2), expression(paste(log[10], "(Mean)")^3),srt = 90, cex = 1)

scatter3D(x = female_TL_hampel_coef[2,], y = female_TL_hampel_coef[3,], z = female_TL_hampel_coef[4,], colvar = NULL, col = gg2.col(47, alpha = 0.3), add = TRUE, pch = 16)

scatter3D(x = female_TL_bisquare_coef[2,], y = female_TL_bisquare_coef[3,], z = female_TL_bisquare_coef[4,], colvar = NULL, col = gg2.col(47, alpha = 0.3), add = TRUE, pch = 17)

legend("right", legend = c("LM", "(Classic TL)", "RLM","(Hampel weight)", "RLM", "(Bisquare weight)"), pch = c(3, NA, 16, NA, 17, NA), horiz = FALSE, inset = 0.05)
dev.off()


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
    y = log(temporal_male_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_male_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_male_var[,ij], base = 10)
    x = log(temporal_male_mean[,ij], base = 10)
  }
  
  OLS_model = lm(y ~ x)
  male_classic[,ij] = OLS_model$coefficients
  male_classic_p[,ij] = summary(OLS_model)$coef[,4]
  male_classic_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  
  hampel_model = rlm(y ~ x, psi = psi.hampel, maxit = 200)
  male_classic_hampel[,ij] = hampel_model$coefficients
  male_classic_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  male_classic_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x, psi = psi.bisquare, maxit = 200)
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
    y = log(temporal_male_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_male_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_male_var[,ij], base = 10)
    x = log(temporal_male_mean[,ij], base = 10)
  }
  
  OLS_model = lm(y ~ x + I(x^2))
  male_quadratic[,ij] = OLS_model$coefficients
  male_quadratic_p[,ij] = summary(OLS_model)$coef[,4]
  male_quadratic_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  
  hampel_model = rlm(y ~ x + I(x^2), psi = psi.hampel, maxit = 200)
  male_quadratic_hampel[,ij] = hampel_model$coefficients
  male_quadratic_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  male_quadratic_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x + I(x^2), psi = psi.bisquare, maxit = 200)
  male_quadratic_bisquare[,ij] = bisquare_model$coefficients
  male_quadratic_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  male_quadratic_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
}


# cubic TL with OLS estimation

male_TL_coef = male_TL_hampel_coef = male_TL_bisquare_coef = matrix(NA, nrow = 4, ncol = 47)
male_TL_p = male_TL_hampel_p = male_TL_bisquare_p = matrix(NA, nrow = 4, ncol = 47)

male_TL_R_squared_prefecture = male_TL_hampel_R_squared_prefecture = male_TL_bisquare_R_squared_prefecture = vector("numeric", 47)

for(ij in 1:47)
{
  if(length(male_TL_outliers[[ij]]) > 0)
  {
    age_select = setdiff(age_all, as.numeric(rownames(male_TL_outliers[[ij]])))
    y = log(temporal_male_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_male_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_male_var[,ij], base = 10)
    x = log(temporal_male_mean[,ij], base = 10)
  }
  
  OLS_model = lm(y ~ x + I(x^2) + I(x^3))
  male_TL_coef[,ij] = OLS_model$coefficients
  male_TL_p[,ij] = summary(OLS_model)$coef[,4]
  male_TL_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  
  hampel_model = rlm(y ~ x + I(x^2) + I(x^3), psi = psi.hampel, maxit = 200)
  male_TL_hampel_coef[,ij] = hampel_model$coefficients
  male_TL_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  male_TL_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x + I(x^2) + I(x^3), psi = psi.bisquare, maxit = 200)
  male_TL_bisquare_coef[,ij] = bisquare_model$coefficients
  male_TL_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  male_TL_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
}

savepdf("male_TL_temporal_order1", width = 12, height = 10, toplines = 0.8)
plot(1:47, male_TL_coef[2,], xlab = "", 
     ylab = "", ylim = c(0.2,3.1))
#,main = expression(paste(log[10], "(temporal variance) versus ", log[10], "(temporal mean)")))
points(1:47, male_TL_hampel_coef[2,], col = 2, pch = 2)
points(1:47, male_TL_bisquare_coef[2,], col = 4, pch = 4)
#legend("topright", c("LM", "RLM (Hampel weight)", "RLM (Bisquare weight)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 1)
dev.off()

savepdf("male_TL_temporal_order2", width = 12, height = 10, toplines = 0.8)
plot(1:47, male_TL_coef[3,], xlab = "", 
     ylab = "", ylim = c(-1,0.5))
#,main = expression(paste(log[10], "(temporal variance) versus ", log[10], "(temporal mean)")))
points(1:47, male_TL_hampel_coef[3,], col = 2, pch = 2)
points(1:47, male_TL_bisquare_coef[3,], col = 4, pch = 4)
dev.off()

savepdf("male_TL_temporal_order3", width = 12, height = 10, toplines = 0.8)
plot(1:47, male_TL_coef[4,], xlab = "Prefecture ordered geographically by North to South", 
     ylab = "", ylim = c(-0.17,0.05))
#,main = expression(paste(log[10], "(temporal variance) versus ", log[10], "(temporal mean)")))
points(1:47, male_TL_hampel_coef[4,], col = 2, pch = 2)
points(1:47, male_TL_bisquare_coef[4,], col = 4, pch = 4)
dev.off()


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
    y = log(temporal_total_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_total_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_total_var[,ij], base = 10)
    x = log(temporal_total_mean[,ij], base = 10)
  }
  
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
    y = log(temporal_total_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_total_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_total_var[,ij], base = 10)
    x = log(temporal_total_mean[,ij], base = 10)
  }
  
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

# cubic TL with OLS estimation

total_TL_coef = total_TL_hampel_coef = total_TL_bisquare_coef = matrix(NA, nrow = 4, ncol = 47)
total_TL_p = total_TL_hampel_p = total_TL_bisquare_p = matrix(NA, nrow = 4, ncol = 47)

total_TL_R_squared_prefecture = total_TL_hampel_R_squared_prefecture = total_TL_bisquare_R_squared_prefecture = vector("numeric", 47)

for(ij in 1:47)
{
  if(length(total_TL_outliers[[ij]]) > 0)
  {
    age_select = setdiff(age_all, as.numeric(rownames(total_TL_outliers[[ij]])))
    y = log(temporal_total_var[age_all %in% age_select,ij], base = 10)
    x = log(temporal_total_mean[age_all %in% age_select,ij], base = 10)
  } else {
    y = log(temporal_total_var[,ij], base = 10)
    x = log(temporal_total_mean[,ij], base = 10)
  }
  
  OLS_model = lm(y ~ x + I(x^2) + I(x^3))
  total_TL_coef[,ij] = OLS_model$coefficients
  total_TL_p[,ij] = summary(OLS_model)$coef[,4]
  total_TL_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  
  hampel_model = rlm(y ~ x + I(x^2) + I(x^3), psi = psi.hampel, maxit = 200)
  total_TL_hampel_coef[,ij] = hampel_model$coefficients
  total_TL_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  total_TL_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x + I(x^2) + I(x^3), psi = psi.bisquare, maxit = 200)
  total_TL_bisquare_coef[,ij] = bisquare_model$coefficients
  total_TL_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  total_TL_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
}

savepdf("total_TL_temporal_order1", width = 12, height = 10, toplines = 0.8)
plot(1:47, total_TL_coef[2,], xlab = "", 
     ylab = expression(paste(log[10], "(Temporal Mean) estimates")), ylim = c(-0.5,3), main = "")
#,main = expression(paste(log[10], "(temporal variance) versus ", log[10], "(temporal mean)")))
points(1:47, total_TL_hampel_coef[2,], col = 2, pch = 2)
points(1:47, total_TL_bisquare_coef[2,], col = 4, pch = 4)
legend("topleft", c("LM", "RLM (Hampel weight)", "RLM (Bisquare weight)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 1)
dev.off()

savepdf("total_TL_temporal_order2", width = 12, height = 10, toplines = 0.8)
plot(1:47, total_TL_coef[3,], xlab = "", 
     ylab = expression(paste(log[10]^2, "(Temporal Mean) estimates")), ylim = c(-1.3,0.1), main = "")
#,main = expression(paste(log[10], "(temporal variance) versus ", log[10], "(temporal mean)")))
points(1:47, total_TL_hampel_coef[3,], col = 2, pch = 2)
points(1:47, total_TL_bisquare_coef[3,], col = 4, pch = 4)
dev.off()

savepdf("total_TL_temporal_order3", width = 12, height = 10, toplines = 0.8)
plot(1:47, total_TL_coef[4,], xlab = "Prefecture ordered geographically by North to South", 
     ylab = expression(paste(log[10]^3, "(Temporal Mean) estimates")), ylim = c(-0.21, -0.03), main = "")
#,main = expression(paste(log[10], "(temporal variance) versus ", log[10], "(temporal mean)")))
points(1:47, total_TL_hampel_coef[4,], col = 2, pch = 2)
points(1:47, total_TL_bisquare_coef[4,], col = 4, pch = 4)
dev.off()

#################################
# plot spatial mean and variance
#################################

# female

spatial_female_mean = apply(female_data[,,2:48], c(1, 2), mean)
spatial_female_var  = apply(female_data[,,2:48], c(1, 2), var)

rownames(spatial_female_mean) = rownames(spatial_female_var) = 0:100
colnames(spatial_female_mean) = colnames(spatial_female_var) = year

plot(fts(0:100, log(spatial_female_mean, base = 10)), xlab = "Age", ylab = "Female mortality")
plot(fts(0:100, log(spatial_female_var,  base = 10)), xlab = "Age", ylab = "Female mortality")

# male

spatial_male_mean = apply(male_data[,,2:48], c(1, 2), mean)
spatial_male_var  = apply(male_data[,,2:48], c(1, 2), var)

rownames(spatial_male_mean) = rownames(spatial_male_var) = 0:100
colnames(spatial_male_mean) = colnames(spatial_male_var) = year

plot(fts(0:100, log(spatial_male_mean, base = 10)), xlab = "Age", ylab = "Male mortality")
plot(fts(0:100, log(spatial_male_var,  base = 10)), xlab = "Age", ylab = "Male mortality")

# total

spatial_total_mean = apply(total_data[,,2:48], c(1, 2), mean)
spatial_total_var  = apply(total_data[,,2:48], c(1, 2), var)

rownames(spatial_total_mean) = rownames(spatial_total_var) = 0:100
colnames(spatial_total_mean) = colnames(spatial_total_var) = year

plot(fts(0:100, log(spatial_total_mean, base = 10)), xlab = "Age", ylab = "Total mortality")
plot(fts(0:100, log(spatial_total_var,  base = 10)), xlab = "Age", ylab = "Total mortality")


#########################################
# Taylor's law slope parameter estimates
#########################################

# female ver 1

female_TL_slope_dynamic = female_TL_hampel_slope_dynamic = female_TL_bisquare_slope_dynamic = vector("numeric", 43)

female_TL_R_squared_dynamic = female_TL_hampel_R_squared_dynamic = female_TL_bisquare_R_squared_dynamic = vector("numeric", 43)

for(ij in 1:n_year)
{
  OLS_model = lm(log(spatial_female_var[,ij], base = 10) ~ log(spatial_female_mean[,ij], base = 10))
  female_TL_slope_dynamic[ij] = OLS_model$coefficients[2]
  female_TL_R_squared_dynamic[ij] = summary(OLS_model)$r.squared
  
  hampel_model = rlm(log(spatial_female_var[,ij], base = 10) ~ log(spatial_female_mean[,ij], base = 10), psi = psi.hampel, maxit = 200)
  female_TL_hampel_slope_dynamic[ij] = hampel_model$coefficients[2]
  female_TL_hampel_R_squared_dynamic[ij] = 1 - sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2)
  
  bisquare_model = rlm(log(spatial_female_var[,ij], base = 10) ~ log(spatial_female_mean[,ij], base = 10), psi = psi.bisquare, maxit = 200)
  female_TL_bisquare_slope_dynamic[ij] = bisquare_model$coefficients[2]
  female_TL_bisquare_R_squared_dynamic[ij] = 1 - sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2)
}

female_spatial_iid = lm(female_TL_slope_dynamic~year)
female_spatial_hampel_iid = lm(female_TL_hampel_slope_dynamic~year)
female_spatial_bisquare_iid = lm(female_TL_bisquare_slope_dynamic~year)

savepdf("female_TL_spatial", width = 12, height = 10, toplines = 0.8)
plot(1975:2017, female_TL_slope_dynamic, xlab = "Year", 
     ylab = "Taylor's Law slope parameter estimates", ylim = c(1.05, 1.8))
     #, main = expression(paste(log[10], "(spatial variance) versus ", log[10], "(spatial mean)")))
points(1975:2017, female_TL_hampel_slope_dynamic, col = 2, pch = 2)
points(1975:2017, female_TL_bisquare_slope_dynamic, col = 4, pch = 4)
abline(female_spatial_iid)
abline(female_spatial_hampel_iid, col = 2)
abline(female_spatial_bisquare_iid, col = 4)
legend("topright", c("LM", "RLM (Hampel weight)", "RLM (Bisquare weight)"), col = c(1, 2, 4), pch = c(1, 2, 4), lty = c(1,1,1), cex = 1.2)
dev.off()


AICc(lm(log(spatial_female_var[,ij], base = 10) ~ poly(log(spatial_female_mean[,ij], base = 10), 1)))
AICc(lm(log(spatial_female_var[,ij], base = 10) ~ poly(log(spatial_female_mean[,ij], base = 10), 2)))
AICc(lm(log(spatial_female_var[,ij], base = 10) ~ poly(log(spatial_female_mean[,ij], base = 10), 3)))
AICc(lm(log(spatial_female_var[,ij], base = 10) ~ poly(log(spatial_female_mean[,ij], base = 10), 4)))
AICc(lm(log(spatial_female_var[,ij], base = 10) ~ poly(log(spatial_female_mean[,ij], base = 10), 5)))
AICc(lm(log(spatial_female_var[,ij], base = 10) ~ poly(log(spatial_female_mean[,ij], base = 10), 6)))
AICc(lm(log(spatial_female_var[,ij], base = 10) ~ poly(log(spatial_female_mean[,ij], base = 10), 7)))

# female ver 2

female_TL_slope_dynamic_v2 = female_TL_hampel_slope_dynamic_v2 = female_TL_bisquare_slope_dynamic_v2 = vector("numeric", 43)
female_TL_R_squared_dynamic_v2 = female_TL_hampel_R_squared_dynamic_v2 = female_TL_bisquare_R_squared_dynamic_v2 = vector("numeric", 43)

for(ij in 1:n_year)
{
  OLS_model = lm(log(spatial_female_var[,ij], base = 10) ~ log(Japan$rate$female[,ij], base = 10))
  female_TL_slope_dynamic_v2[ij] = OLS_model$coefficients[2]
  female_TL_R_squared_dynamic_v2[ij] = summary(OLS_model)$r.squared
  
  hampel_model = rlm(log(spatial_female_var[,ij], base = 10) ~ log(Japan$rate$female[,ij], base = 10), psi = psi.hampel, maxit = 200)
  female_TL_hampel_slope_dynamic_v2[ij] = hampel_model$coefficients[2]
  female_TL_hampel_R_squared_dynamic_v2[ij] = 1 - sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2)
  
  bisquare_model = rlm(log(spatial_female_var[,ij], base = 10) ~ log(Japan$rate$female[,ij], base = 10), psi = psi.bisquare, maxit = 200)
  female_TL_bisquare_slope_dynamic_v2[ij] = bisquare_model$coefficients[2]
  female_TL_bisquare_R_squared_dynamic_v2[ij] = 1 - sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2)
}

savepdf("female_TL_spatial_v2", width = 12, height = 10, toplines = 0.8)
plot(1975:2017, female_TL_slope_dynamic_v2, xlab = "Year", 
     ylab = "Taylor's Law slope parameter estimates", ylim = c(1.05, 1.8))
     #, main = expression(paste(log[10], "(spatial variance) versus ", log[10], "(spatial mean)")))
points(1975:2017, female_TL_hampel_slope_dynamic_v2, col = 2, pch = 2)
points(1975:2017, female_TL_bisquare_slope_dynamic_v2, col = 4, pch = 4)
legend("topright", c("LM", "RLM (Hampel weight)", "RLM (Bisquare weight)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 1.2)
dev.off()

# male ver 1

male_TL_slope_dynamic = male_TL_hampel_slope_dynamic = male_TL_bisquare_slope_dynamic = vector("numeric", 43)

male_TL_R_squared_dynamic = male_TL_hampel_R_squared_dynamic = male_TL_bisquare_R_squared_dynamic = vector("numeric", 43)

for(ij in 1:n_year)
{
  OLS_model = lm(log(spatial_male_var[,ij], base = 10) ~ log(spatial_male_mean[,ij], base = 10))
  male_TL_slope_dynamic[ij] = OLS_model$coefficients[2]
  male_TL_R_squared_dynamic[ij] = summary(OLS_model)$r.squared
  
  hampel_model = rlm(log(spatial_male_var[,ij], base = 10) ~ log(spatial_male_mean[,ij], base = 10), psi = psi.hampel, maxit = 200)
  male_TL_hampel_slope_dynamic[ij] = hampel_model$coefficients[2]
  male_TL_hampel_R_squared_dynamic[ij] = 1 - sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2)
  
  bisquare_model = rlm(log(spatial_male_var[,ij], base = 10) ~ log(spatial_male_mean[,ij], base = 10), psi = psi.bisquare, maxit = 200)
  male_TL_bisquare_slope_dynamic[ij] = bisquare_model$coefficients[2]
  male_TL_bisquare_R_squared_dynamic[ij] = 1 - sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2)
}

male_spatial_iid = lm(male_TL_slope_dynamic~year)
male_spatial_hampel_iid = lm(male_TL_hampel_slope_dynamic~year)
male_spatial_bisquare_iid = lm(male_TL_bisquare_slope_dynamic~year)

savepdf("male_TL_spatial", width = 12, height = 10, toplines = 0.8)
plot(1975:2017,   male_TL_slope_dynamic, xlab = "Year", 
     ylab = "Taylor's Law slope parameter estimates", ylim = c(1.05, 1.8))
     #, main = expression(paste(log[10], "(spatial variance) versus ", log[10], "(spatial mean)")))
points(1975:2017, male_TL_hampel_slope_dynamic, col = 2, pch = 2)
points(1975:2017, male_TL_bisquare_slope_dynamic, col = 4, pch = 4)
abline(male_spatial_iid)
abline(male_spatial_hampel_iid, col = 2)
abline(male_spatial_bisquare_iid, col = 4)
# legend("topright", c("LM", "RLM (Hampel weight)", "RLM (Bisquare weight)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 0.8)
dev.off()

# male ver 2

male_TL_slope_dynamic_v2 = male_TL_hampel_slope_dynamic_v2 = male_TL_bisquare_slope_dynamic_v2 = vector("numeric", 43)
male_TL_R_squared_dynamic_v2 = male_TL_hampel_R_squared_dynamic_v2 = male_TL_bisquare_R_squared_dynamic_v2 = vector("numeric", 43)

for(ij in 1:n_year)
{
  OLS_model = lm(log(spatial_male_var[,ij], base = 10) ~ log(Japan$rate$male[,ij], base = 10))
  male_TL_slope_dynamic_v2[ij] = OLS_model$coefficients[2]
  male_TL_R_squared_dynamic_v2[ij] = summary(OLS_model)$r.squared
  
  hampel_model = rlm(log(spatial_male_var[,ij], base = 10) ~ log(Japan$rate$male[,ij], base = 10), psi = psi.hampel, maxit = 200)
  male_TL_hampel_slope_dynamic_v2[ij] = hampel_model$coefficients[2]
  male_TL_hampel_R_squared_dynamic_v2[ij] = 1 - sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2)
  
  bisquare_model = rlm(log(spatial_male_var[,ij], base = 10) ~ log(Japan$rate$male[,ij], base = 10), psi = psi.bisquare, maxit = 200)
  male_TL_bisquare_slope_dynamic_v2[ij] = bisquare_model$coefficients[2]
  male_TL_bisquare_R_squared_dynamic_v2[ij] = 1 - sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2)
}

savepdf("male_TL_spatial_v2", width = 12, height = 10, toplines = 0.8)
plot(1975:2017, male_TL_slope_dynamic_v2, xlab = "Year", 
     ylab = "Taylor's Law slope parameter estimates", ylim = c(1.05, 1.8))
     #, main = expression(paste(log[10], "(spatial variance) versus ", log[10], "(spatial mean)")))
points(1975:2017, male_TL_hampel_slope_dynamic_v2, col = 2, pch = 2)
points(1975:2017, male_TL_bisquare_slope_dynamic_v2, col = 4, pch = 4)
#legend("topright", c("LM", "RLM (Hampel weight)", "RLM (Bisquare weight)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 0.8)
dev.off()


# total ver 1

total_TL_slope_dynamic = total_TL_hampel_slope_dynamic = total_TL_bisquare_slope_dynamic = vector("numeric", 43)

total_TL_R_squared_dynamic = total_TL_hampel_R_squared_dynamic = total_TL_bisquare_R_squared_dynamic = vector("numeric", 43)


for(ij in 1:n_year)
{
  OLS_model = lm(log(spatial_total_var[,ij], base = 10) ~ log(spatial_total_mean[,ij], base = 10))
  total_TL_slope_dynamic[ij] = OLS_model$coefficients[2]
  total_TL_R_squared_dynamic[ij] = summary(OLS_model)$r.squared
  
  hampel_model = rlm(log(spatial_total_var[,ij], base = 10) ~ log(spatial_total_mean[,ij], base = 10), psi = psi.hampel, maxit = 200)
  total_TL_hampel_slope_dynamic[ij] = hampel_model$coefficients[2]
  total_TL_hampel_R_squared_dynamic[ij] = 1 - sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2)
  
  bisquare_model = rlm(log(spatial_total_var[,ij], base = 10) ~ log(spatial_total_mean[,ij], base = 10), psi = psi.bisquare, maxit = 200)
  total_TL_bisquare_slope_dynamic[ij] = bisquare_model$coefficients[2]
  total_TL_bisquare_R_squared_dynamic[ij] = 1 - sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2)
}

total_spatial_iid = lm(total_TL_slope_dynamic~year)
total_spatial_hampel_iid = lm(total_TL_hampel_slope_dynamic~year)
total_spatial_bisquare_iid = lm(total_TL_bisquare_slope_dynamic~year)

savepdf("total_TL_spatial", width = 12, height = 10, toplines = 0.8)
plot(1975:2017,   total_TL_slope_dynamic, xlab = "Year", 
     ylab = "Taylor's Law slope parameter estimates", ylim = c(1.05, 1.8))
     #, main = expression(paste(log[10], "(spatial variance) versus ", log[10], "(spatial mean)")))
points(1975:2017, total_TL_hampel_slope_dynamic, col = 2, pch = 2)
points(1975:2017, total_TL_bisquare_slope_dynamic, col = 4, pch = 4)
abline(total_spatial_iid)
abline(total_spatial_hampel_iid, col = 2)
abline(total_spatial_bisquare_iid, col = 4)
# legend("topright", c("LM", "RLM (Hampel weight)", "RLM (Bisquare weight)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 0.8)
dev.off()

# total ver 2

total_TL_slope_dynamic_v2 = total_TL_hampel_slope_dynamic_v2 = total_TL_bisquare_slope_dynamic_v2 = vector("numeric", 43)
total_TL_R_squared_dynamic_v2 = total_TL_hampel_R_squared_dynamic_v2 = total_TL_bisquare_R_squared_dynamic_v2 = vector("numeric", 43)

for(ij in 1:n_year)
{
  OLS_model = lm(log(spatial_total_var[,ij], base = 10) ~ log(Japan$rate$total[,ij], base = 10))
  total_TL_slope_dynamic_v2[ij] = OLS_model$coefficients[2]
  total_TL_R_squared_dynamic_v2[ij] = summary(OLS_model)$r.squared
  
  hampel_model = rlm(log(spatial_total_var[,ij], base = 10) ~ log(Japan$rate$total[,ij], base = 10), psi = psi.hampel, maxit = 200)
  total_TL_hampel_slope_dynamic_v2[ij] = hampel_model$coefficients[2]
  total_TL_hampel_R_squared_dynamic_v2[ij] = 1 - sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2)
  
  bisquare_model = rlm(log(spatial_total_var[,ij], base = 10) ~ log(Japan$rate$total[,ij], base = 10), psi = psi.bisquare, maxit = 200)
  total_TL_bisquare_slope_dynamic_v2[ij] = bisquare_model$coefficients[2]
  total_TL_bisquare_R_squared_dynamic_v2[ij] = 1 - sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2)
}

savepdf("total_TL_spatial_v2", width = 12, height = 10, toplines = 0.8)
plot(1975:2017, total_TL_slope_dynamic_v2, xlab = "Year", 
     ylab = "Taylor's Law slope parameter estimates", ylim = c(1.05, 1.8))
     #, main = expression(paste(log[10], "(spatial variance) versus ", log[10], "(spatial mean)")))
points(1975:2017, total_TL_hampel_slope_dynamic_v2, col = 2, pch = 2)
points(1975:2017, total_TL_bisquare_slope_dynamic_v2, col = 4, pch = 4)
#legend("topright", c("LM", "RLM (Hampel weight)", "RLM (Bisquare weight)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 0.8)
dev.off()


#####################
# Spatial covariance
#####################

library(NipponMap)
library(spdep)
library(geosphere)

# binary weights matrix
shp = system.file("shapes/jpn.shp", package = "NipponMap")[1]
Japan_shp = read_sf(shp)
adjacency_weights = poly2nb(Japan_shp, row.names = Japan_shp$name)
weights_matrix = nb2mat(adjacency_weights, style= "B", zero.policy = TRUE)


# distance-related weight matrix

lonlat_mat = Japan_prefecture_city_population[2:48,c("Longitude", "Latitude")]
dist_mat = distm(lonlat_mat)
weights_matrix_2 = 1/dist_mat
diag(weights_matrix_2) = 0

# mean values weighted by distance to Tokyo
# spatial_mean = function(y_array, w_mat, ref_ind)
# {
#   ratios = 1/w_mat[ref_ind,]
#   ratios[ref_ind] = 1
#   
#   ws = ratios/sum(ratios)
#   
#   res_temp = matrix(0, nrow = dim(y_array)[1], ncol = dim(y_array)[2])
#   for(ij in 1:nrow(res_temp))
#   {
#     for(it in 1:ncol(res_temp))
#     {
#       res_temp[ij,it] = mean(y_array[ij,it,]*ws, na.rm = TRUE)
#     }
#   }
# }


spatial_var = function(y, w_mat)
{
  if(length(y) != nrow(w_mat))
  {
    warnings("uncomfortable matrix")
  }
  
  if(sum(is.na(y))>0)
  {
    ind = which(is.na(y))
    y = na.omit(y)
    w_mat = w_mat[-ind,-ind]
  }
  
  yi_yj = sapply(y, "-", y)
  weighted_cov = w_mat*yi_yj^2
  return(sum(weighted_cov/2)/sum(w_mat))
}


# Distance-weighted spatial covariance

spatial_dist_female_var = spatial_dist_male_var = spatial_dist_total_var = matrix(0, nrow = n_age, ncol = n_year)
for(ij in 1:n_age)
{
  for(it in 1:n_year)
  {
    spatial_dist_female_var[ij,it] = spatial_var(y = female_data[ij,it,2:48], w_mat = weights_matrix_2)
    spatial_dist_male_var[ij,it] = spatial_var(y = male_data[ij,it,2:48], w_mat = weights_matrix_2)
    spatial_dist_total_var[ij,it] = spatial_var(y = total_data[ij,it,2:48], w_mat = weights_matrix_2)
  }
}


savepdf("spatial_female_mean", width = 12, height = 10, toplines = 0.8)
plot(fts(0:100, log(spatial_female_mean, base = 10)), xlab = "Age", ylab = "log(Spatial Mean)")
lines(fts(0:100, log(spatial_female_mean[,37], base = 10)), lwd = 2)
dev.off()

savepdf("spatial_female_var", width = 12, height = 10, toplines = 0.8)
plot(fts(0:100, log(spatial_dist_female_var, base = 10)), xlab = "Age", ylab = "log(Spatial Variance)")
lines(fts(0:100, log(spatial_dist_female_var[,37], base = 10)), lwd = 2)
dev.off()

female_example_df1 = data.frame(mean = spatial_female_mean[,36], var = spatial_dist_female_var[,36], age = 0:100)
female_example_m1 = lm(log10(spatial_dist_female_var[,36])~log10(spatial_female_mean[,36]))

female_example_df2 = data.frame(mean = spatial_female_mean[,37], var = spatial_dist_female_var[,37], age = 0:100)
female_example_m2 = lm(log10(spatial_dist_female_var[,37])~log10(spatial_female_mean[,37]))

female_example_df3 = data.frame(mean = spatial_female_mean[,38], var = spatial_dist_female_var[,38], age = 0:100)
female_example_m3 = lm(log10(spatial_dist_female_var[,38])~log10(spatial_female_mean[,38]))

female_example_df_all = data.frame(mean = as.vector(spatial_female_mean[,36:38]), var = as.vector(spatial_dist_female_var[,36:38]), Age = rep(0:100, 3), Year = rep(c("2010", "2011", "2012"), each = 101)) 



library(patchwork)

sp1 = ggplot(as.data.frame(female_example_df1), aes(x = mean, y = var, color = age)) + geom_point()  + scale_color_gradientn(colours = rainbow(101, end = 0.85)) + theme(legend.position = "none") + scale_x_continuous(name="Spatial Mean", limits=c(0.00001, 1), trans = "log10") + scale_y_continuous(name="Spatial Variance", limits=c(1e-12, 1e-6), trans = "log10") + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + ggtitle("Female series 2010") + theme(plot.title = element_text(hjust = 0.5))

sp2 = ggplot(as.data.frame(female_example_df2), aes(x = mean, y = var, color = age)) + geom_point()  + scale_color_gradientn(colours = rainbow(101, end = 0.85)) + theme(legend.position = "none")  + scale_x_continuous(name="Spatial Mean", limits=c(0.00001, 1), trans = "log10") + scale_y_continuous(name="", limits=c(1e-12, 1e-6), trans = "log10") + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + ggtitle("Female series 2011") + theme(plot.title = element_text(hjust = 0.5))

sp3 = ggplot(as.data.frame(female_example_df3), aes(x = mean, y = var, color = age)) + geom_point()  + scale_color_gradientn(colours = rainbow(101, end = 0.85))  + scale_x_continuous(name="Spatial Mean", limits=c(0.00001, 1), trans = "log10") + scale_y_continuous(name="", limits=c(1e-12, 1e-6), trans = "log10") + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + ggtitle("Female series 2012") + theme(plot.title = element_text(hjust = 0.5))


sp4 = ggplot(as.data.frame(female_example_df_all), aes(x = mean, y = var, color = Age,  linetype = Year, group = Year)) + geom_point(aes(x = mean, y = var, color = Age, shape = Year))  + scale_x_continuous(name="Spatial Mean", limits=c(1e-5, 1), trans = "log10") + scale_y_continuous(name="Spatial Variance", limits=c(1e-12, 1e-6), trans = "log10") + geom_smooth(method=lm, se=FALSE, fullrange=TRUE, size=0.5) + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + ggtitle("Female series 2010 to 2012") + theme(plot.title = element_text(hjust = 0.5))+ scale_color_gradientn(colours = rainbow(101, end = 0.85))

sp4

savepdf("spatial_female_2010to2012", width = 24, height = 20, toplines = 0.8)
sp4
dev.off()


par(mfrow=c(3,1))
plot(log(spatial_female_mean[,36], base = 10), log(spatial_dist_female_var[,36], base = 10), ylim = c(-12,-6))
abline(lm(log(spatial_dist_female_var[,36], base = 10)~log(spatial_female_mean[,36], base = 10)))
plot(log(spatial_female_mean[,37], base = 10), log(spatial_dist_female_var[,37], base = 10), ylim = c(-12,-6))
abline(lm(log(spatial_dist_female_var[,37], base = 10)~log(spatial_female_mean[,37], base = 10)))
plot(log(spatial_female_mean[,38], base = 10), log(spatial_dist_female_var[,38], base = 10), ylim = c(-12,-6))
abline(lm(log(spatial_dist_female_var[,38], base = 10)~log(spatial_female_mean[,38], base = 10)))


plot(log(spatial_female_mean[,36], base = 10), log(spatial_dist_female_var[,36], base = 10), ylim = c(-12,-6), xlim = c(-5, 0), col = rainbow(101, end = 0.85), lwd = 1.5, xaxt = "n", xlab = "Spatial Mean", ylab = "Spatial Variance")
points(log(spatial_female_mean[,37], base = 10), log(spatial_dist_female_var[,37], base = 10), col = rainbow(101, end = 0.85), lwd = 1.5, pch = 17)
points(log(spatial_female_mean[,38], base = 10), log(spatial_dist_female_var[,38], base = 10), col = rainbow(101, end = 0.85), lwd = 1.5, pch = 3)
axis(1, at = c(-5, -4, -3, -2, -1, 0), labels = c(1e-5, 1e-4, 0.001, 0.01, 0.1, 1))
abline(female_example_m1, col = 1, lty = 1, lwd = 1.5)
abline(female_example_m2, col = 1, lty = 2, lwd = 1.5)
abline(female_example_m3, col = 1, lty = 3, lwd = 1.5)


# female ver 1

female_TL_slope_spatial = female_TL_hampel_slope_spatial = female_TL_bisquare_slope_spatial = vector("numeric", 43)
female_TL_R_squared_spatial = female_TL_hampel_R_squared_spatial = female_TL_bisquare_R_squared_spatial = vector("numeric", 43)

for(ij in 1:n_year)
{
  OLS_model = lm(log(spatial_dist_female_var[,ij], base = 10) ~ log(spatial_female_mean[,ij], base = 10))
  female_TL_slope_spatial[ij] = OLS_model$coefficients[2]
  female_TL_R_squared_spatial[ij] = summary(OLS_model)$r.squared
  
  hampel_model = rlm(log(spatial_dist_female_var[,ij], base = 10) ~ log(spatial_female_mean[,ij], base = 10), psi = psi.hampel, maxit = 200)
  female_TL_hampel_slope_spatial[ij] = hampel_model$coefficients[2]
  female_TL_hampel_R_squared_spatial[ij] = 1 - sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2)
  
  bisquare_model = rlm(log(spatial_dist_female_var[,ij], base = 10) ~ log(spatial_female_mean[,ij], base = 10), psi = psi.bisquare, maxit = 200)
  female_TL_bisquare_slope_spatial[ij] = bisquare_model$coefficients[2]
  female_TL_bisquare_R_squared_spatial[ij] = 1 - sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2)
}

female_spatial_distance = lm(female_TL_slope_spatial~year)
female_spatial_hampel_distance = lm(female_TL_hampel_slope_spatial~year)
female_spatial_bisquare_distance = lm(female_TL_bisquare_slope_spatial~year)

savepdf("female_TL_spatial_non_iid", width = 12, height = 10, toplines = 0.8)
plot(1975:2017, female_TL_slope_spatial, xlab = "Year", 
     ylab = "Taylor's Law slope parameter estimates", ylim = c(1.05, 1.8))
     #, main = expression(paste(log[10], "(spatial variance) versus ", log[10], "(spatial mean)")))
points(1975:2017, female_TL_hampel_slope_spatial, col = 2, pch = 2)
points(1975:2017, female_TL_bisquare_slope_spatial, col = 4, pch = 4)
abline(female_spatial_distance)
abline(female_spatial_hampel_distance, col = 2)
abline(female_spatial_bisquare_distance, col = 4)
# legend("topright", c("TL", "RTL (Hampel weight)", "RTL (Bisquare weight)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 1)
dev.off()


# female ver 2

female_TL_slope_spatial_v2 = female_TL_hampel_slope_spatial_v2 = female_TL_bisquare_slope_spatial_v2 = vector("numeric", 43)
female_TL_R_squared_spatial_v2 = female_TL_hampel_R_squared_spatial_v2 = female_TL_bisquare_R_squared_spatial_v2 = vector("numeric", 43)

for(ij in 1:n_year)
{
  OLS_model = lm(log(spatial_dist_female_var[,ij], base = 10) ~ log(Japan$rate$female[,ij], base = 10))
  female_TL_slope_spatial_v2[ij] = OLS_model$coefficients[2]
  female_TL_R_squared_spatial_v2[ij] = summary(OLS_model)$r.squared
  
  hampel_model = rlm(log(spatial_dist_female_var[,ij], base = 10) ~ log(Japan$rate$female[,ij], base = 10), psi = psi.hampel, maxit = 200)
  female_TL_hampel_slope_spatial_v2[ij] = hampel_model$coefficients[2]
  female_TL_hampel_R_squared_spatial_v2[ij] = 1 - sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2)
  
  bisquare_model = rlm(log(spatial_dist_female_var[,ij], base = 10) ~ log(Japan$rate$female[,ij], base = 10), psi = psi.bisquare, maxit = 200)
  female_TL_bisquare_slope_spatial_v2[ij] = bisquare_model$coefficients[2]
  female_TL_bisquare_R_squared_spatial_v2[ij] = 1 - sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2)
}

savepdf("female_TL_spatial_v2", width = 12, height = 10, toplines = 0.8)
plot(1975:2017, female_TL_slope_spatial_v2, xlab = "Year", 
     ylab = "Taylor's Law slope parameter estimates", ylim = c(1.05, 1.8))
     #,  main = expression(paste(log[10], "(spatial variance) versus ", log[10], "(spatial mean)")))
points(1975:2017, female_TL_hampel_slope_spatial_v2, col = 2, pch = 2)
points(1975:2017, female_TL_bisquare_slope_spatial_v2, col = 4, pch = 4)
legend("topright", c("LM", "RLM (Hampel weight)", "RLM (Bisquare weight)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 1.2)
dev.off()


# male ver 1

male_TL_slope_spatial = male_TL_hampel_slope_spatial = male_TL_bisquare_slope_spatial = vector("numeric", 43)
male_TL_R_squared_spatial = male_TL_hampel_R_squared_spatial = male_TL_bisquare_R_squared_spatial = vector("numeric", 43)

for(ij in 1:n_year)
{
  OLS_model = lm(log(spatial_dist_male_var[,ij], base = 10) ~ log(spatial_male_mean[,ij], base = 10))
  male_TL_slope_spatial[ij] = OLS_model$coefficients[2]
  male_TL_R_squared_spatial[ij] = summary(OLS_model)$r.squared
  
  hampel_model = rlm(log(spatial_dist_male_var[,ij], base = 10) ~ log(spatial_male_mean[,ij], base = 10), psi = psi.hampel, maxit = 200)
  male_TL_hampel_slope_spatial[ij] = hampel_model$coefficients[2]
  male_TL_hampel_R_squared_spatial[ij] = 1 - sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2)
  
  bisquare_model = rlm(log(spatial_dist_male_var[,ij], base = 10) ~ log(spatial_male_mean[,ij], base = 10), psi = psi.bisquare, maxit = 200)
  male_TL_bisquare_slope_spatial[ij] = bisquare_model$coefficients[2]
  male_TL_bisquare_R_squared_spatial[ij] = 1 - sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2)
}

male_spatial_distance = lm(male_TL_slope_spatial~year)
male_spatial_hampel_distance = lm(male_TL_hampel_slope_spatial~year)
male_spatial_bisquare_distance = lm(male_TL_bisquare_slope_spatial~year)

savepdf("male_TL_spatial_non_iid", width = 12, height = 10, toplines = 0.8)
plot(1975:2017, male_TL_slope_spatial, xlab = "Year", 
     ylab = "Taylor's Law slope parameter estimates", ylim = c(1.05, 1.8))
    #, main = expression(paste(log[10], "(spatial variance) versus ", log[10], "(spatial mean)")))
points(1975:2017, male_TL_hampel_slope_spatial, col = 2, pch = 2)
points(1975:2017, male_TL_bisquare_slope_spatial, col = 4, pch = 4)
abline(male_spatial_distance)
abline(male_spatial_hampel_distance, col = 2)
abline(male_spatial_bisquare_distance, col = 4)
#legend("topright", c("LM", "RLM (Hampel weight)", "RLM (Bisquare weight)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 0.8)
dev.off()


# male ver 2

male_TL_slope_spatial_v2 = male_TL_hampel_slope_spatial_v2 = male_TL_bisquare_slope_spatial_v2 = vector("numeric", 43)
male_TL_R_squared_spatial_v2 = male_TL_hampel_R_squared_spatial_v2 = male_TL_bisquare_R_squared_spatial_v2 = vector("numeric", 43)

for(ij in 1:n_year)
{
  OLS_model = lm(log(spatial_dist_male_var[,ij], base = 10) ~ log(Japan$rate$male[,ij], base = 10))
  male_TL_slope_spatial_v2[ij] = OLS_model$coefficients[2]
  male_TL_R_squared_spatial_v2[ij] = summary(OLS_model)$r.squared
  
  hampel_model = rlm(log(spatial_dist_male_var[,ij], base = 10) ~ log(Japan$rate$male[,ij], base = 10), psi = psi.hampel, maxit = 200)
  male_TL_hampel_slope_spatial_v2[ij] = hampel_model$coefficients[2]
  male_TL_hampel_R_squared_spatial_v2[ij] = 1 - sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2)
  
  bisquare_model = rlm(log(spatial_dist_male_var[,ij], base = 10) ~ log(Japan$rate$male[,ij], base = 10), psi = psi.bisquare, maxit = 200)
  male_TL_bisquare_slope_spatial_v2[ij] = bisquare_model$coefficients[2]
  male_TL_bisquare_R_squared_spatial_v2[ij] = 1 - sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2)
}


savepdf("male_TL_spatial_v2", width = 12, height = 10, toplines = 0.8)
plot(1975:2017, male_TL_slope_spatial_v2, xlab = "Year", 
     ylab = "Taylor's Law slope parameter estimates", ylim = c(1.05, 1.8))
     #, main = expression(paste(log[10], "(spatial variance) versus ", log[10], "(spatial mean)")))
points(1975:2017, male_TL_hampel_slope_spatial_v2, col = 2, pch = 2)
points(1975:2017, male_TL_bisquare_slope_spatial_v2, col = 4, pch = 4)
#legend("topright", c("LM", "RLM (Hampel weight)", "RLM (Bisquare weight)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 0.8)
dev.off()


# total ver 1

total_TL_slope_spatial = total_TL_hampel_slope_spatial = total_TL_bisquare_slope_spatial = vector("numeric", 43)
total_TL_R_squared_spatial = total_TL_hampel_R_squared_spatial = total_TL_bisquare_R_squared_spatial = vector("numeric", 43)

for(ij in 1:n_year)
{
  OLS_model = lm(log(spatial_dist_total_var[,ij], base = 10) ~ log(spatial_total_mean[,ij], base = 10))
  total_TL_slope_spatial[ij] = OLS_model$coefficients[2]
  total_TL_R_squared_spatial[ij] = summary(OLS_model)$r.squared
  
  hampel_model = rlm(log(spatial_dist_total_var[,ij], base = 10) ~ log(spatial_total_mean[,ij], base = 10), psi = psi.hampel, maxit = 200)
  total_TL_hampel_slope_spatial[ij] = hampel_model$coefficients[2]
  total_TL_hampel_R_squared_spatial[ij] = 1 - sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2)
  
  bisquare_model = rlm(log(spatial_dist_total_var[,ij], base = 10) ~ log(spatial_total_mean[,ij], base = 10), psi = psi.bisquare, maxit = 200)
  total_TL_bisquare_slope_spatial[ij] = bisquare_model$coefficients[2]
  total_TL_bisquare_R_squared_spatial[ij] = 1 - sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2)
}

total_spatial_distance = lm(total_TL_slope_spatial~year)
total_spatial_hampel_distance = lm(total_TL_hampel_slope_spatial~year)
total_spatial_bisquare_distance = lm(total_TL_bisquare_slope_spatial~year)

savepdf("total_TL_spatial_non_iid", width = 12, height = 10, toplines = 0.8)
plot(1975:2017, total_TL_slope_spatial, xlab = "Year", 
     ylab = "Taylor's Law slope parameter estimates", ylim = c(1.05, 1.8))
     #, main = expression(paste(log[10], "(spatial variance) versus ", log[10], "(spatial mean)")))
points(1975:2017, total_TL_hampel_slope_spatial, col = 2, pch = 2)
points(1975:2017, total_TL_bisquare_slope_spatial, col = 4, pch = 4)
abline(total_spatial_distance)
abline(total_spatial_hampel_distance, col = 2)
abline(total_spatial_bisquare_distance, col = 4)
legend("topright", c("LM", "RLM (Hampel weight)", "RLM (Bisquare weight)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 1)
dev.off()


# total ver 2

total_TL_slope_spatial_v2 = total_TL_hampel_slope_spatial_v2 = total_TL_bisquare_slope_spatial_v2 = vector("numeric", 43)
total_TL_R_squared_spatial_v2 = total_TL_hampel_R_squared_spatial_v2 = total_TL_bisquare_R_squared_spatial_v2 = vector("numeric", 43)

for(ij in 1:n_year)
{
  OLS_model = lm(log(spatial_dist_total_var[,ij], base = 10) ~ log(Japan$rate$total[,ij], base = 10))
  total_TL_slope_spatial_v2[ij] = OLS_model$coefficients[2]
  total_TL_R_squared_spatial_v2[ij] = summary(OLS_model)$r.squared
  
  hampel_model = rlm(log(spatial_dist_total_var[,ij], base = 10) ~ log(Japan$rate$total[,ij], base = 10), psi = psi.hampel, maxit = 200)
  total_TL_hampel_slope_spatial_v2[ij] = hampel_model$coefficients[2]
  total_TL_hampel_R_squared_spatial_v2[ij] = 1 - sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2)
  
  bisquare_model = rlm(log(spatial_dist_total_var[,ij], base = 10) ~ log(Japan$rate$total[,ij], base = 10), psi = psi.bisquare, maxit = 200)
  total_TL_bisquare_slope_spatial_v2[ij] = bisquare_model$coefficients[2]
  total_TL_bisquare_R_squared_spatial_v2[ij] = 1 - sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2)
}


savepdf("total_TL_spatial_v2", width = 12, height = 10, toplines = 0.8)
plot(1975:2017, total_TL_slope_spatial_v2, xlab = "Year", 
     ylab = "Taylor's Law slope parameter estimates", ylim = c(1.05, 1.8))
     #, main = expression(paste(log[10], "(spatial variance) versus ", log[10], "(spatial mean)")))
points(1975:2017, total_TL_hampel_slope_spatial_v2, col = 2, pch = 2)
points(1975:2017, total_TL_bisquare_slope_spatial_v2, col = 4, pch = 4)
#legend("topright", c("LM", "RLM (Hampel weight)", "RLM (Bisquare weight)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 0.8)
dev.off()


############################################
# K-means clustering of TL slope parameters
############################################
library(factoextra)

# Temporal TL

# female

state_prefecture_female_df = data.frame(a_linear = female_TL_coef[1,], b_linear = female_TL_coef[2,], c_linear = female_TL_coef[3,], d_linear = female_TL_coef[4,], row.names = state[-1])

# Select the number of clusters by K-means
savepdf("wss_female", width = 12, height = 10, toplines = 0.8)
fviz_nbclust_plot(state_prefecture_female_df, kmeans, method = "wss")
dev.off()

savepdf("silhouette_female", width = 12, height = 10, toplines = 0.8)
fviz_nbclust_plot(state_prefecture_female_df, kmeans, method = "silhouette")
dev.off()

kmeans_female = kmeans(state_prefecture_female_df, centers = 2, nstart = 25)
kmeans_female_plot = fviz_cluster(kmeans_female, data = state_prefecture_female_df, main = "")

savepdf("cluster_female", width = 12, height = 10, toplines = 0.8)
plot(kmeans_female_plot)
dev.off()

k_members_female = data.frame(Region = attributes(kmeans_female$cluster)$names, Cluster_female = as.vector(kmeans_female$cluster))


# male

state_prefecture_male_df = data.frame(a_linear = male_TL_coef[1,], b_linear = male_TL_coef[2,], c_linear = male_TL_coef[3,], d_linear = male_TL_coef[4,], row.names = state[-1])

# Select the number of clusters by K-means
savepdf("wss_male", width = 12, height = 10, toplines = 0.8)
fviz_nbclust_plot(state_prefecture_male_df, kmeans, method = "wss")
dev.off()

savepdf("silhouette_male", width = 12, height = 10, toplines = 0.8)
fviz_nbclust(state_prefecture_male_df, kmeans, method = "silhouette")
dev.off()

kmeans_male = kmeans(state_prefecture_male_df, centers = 2, nstart = 25)
kmeans_male_plot = fviz_cluster(kmeans_male, data = state_prefecture_male_df, main = "")
plot(kmeans_male_plot)

savepdf("cluster_male", width = 12, height = 10, toplines = 0.8)
plot(kmeans_male_plot)
dev.off()

k_members_male = data.frame(Region = attributes(kmeans_male$cluster)$names, Cluster_male = as.vector(kmeans_male$cluster))

# total

state_prefecture_total_df = data.frame(a_linear = total_TL_coef[1,], b_linear = total_TL_coef[2,], c_linear = total_TL_coef[3,], d_linear = total_TL_coef[4,], row.names = state[-1])

# Select the number of clusters by K-means
savepdf("wss_total", width = 12, height = 10, toplines = 0.8)
fviz_nbclust_plot(state_prefecture_total_df, kmeans, method = "wss")
dev.off()

savepdf("silhouette_total", width = 12, height = 10, toplines = 0.8)
fviz_nbclust(state_prefecture_total_df, kmeans, method = "silhouette")
dev.off()

kmeans_total = kmeans(state_prefecture_total_df, centers = 2, nstart = 25)
kmeans_total_plot = fviz_cluster(kmeans_total, data = state_prefecture_total_df, main = "")
plot(kmeans_total_plot)

savepdf("cluster_total", width = 12, height = 10, toplines = 0.8)
plot(kmeans_total_plot)
dev.off()

k_members_total = data.frame(Region = attributes(kmeans_total$cluster)$names, Cluster_total = as.vector(kmeans_total$cluster))

k_members_all = list(k_members_total, k_members_female, k_members_male) %>% reduce(left_join, by = "Region")
colnames(k_members_all) = c("Region", "total_OLS", "female_OLS", "male_OLS")

# flipping labels for total and male 
flip_label = function(k_member_df)
{
  n = nrow(k_member_df)
  out = rep(0, n)
  for(i in 1:n)
  {
    out[i] =ifelse(k_member_df[i,2] == 1, 2, 1)
  }
  
  out_df = data.frame(cbind(k_member_df[,1],out))
  colnames(out_df) = colnames(k_member_df)
  return(out_df)
}


k_members_total_flip = flip_label(k_members_total) 
k_members_male_flip = flip_label(k_members_male) 

k_members_all_flip = list(k_members_total_flip, k_members_female, k_members_male_flip) %>% reduce(left_join, by = "Region")
colnames(k_members_all_flip) = c("Region", "total_OLS", "female_OLS", "male_OLS")


# Temporal TL with long-run variance

# female

state_long_run_prefecture_female_df = data.frame(a_linear = female_TL_coef[1,], b_linear = female_TL_coef[2,], c_linear = female_TL_coef[3,], d_linear = female_TL_coef[4,], a_hampel = female_TL_hampel_coef[1,], b_hampel = female_TL_hampel_coef[2,], c_hampel = female_TL_hampel_coef[3,], d_hampel = female_TL_hampel_coef[4,], a_bisquare = female_TL_bisquare_coef[1,], b_bisquare = female_TL_bisquare_coef[2,], c_bisquare = female_TL_bisquare_coef[3,], d_bisquare = female_TL_bisquare_coef[4,], row.names = state[-1])

state_long_run_prefecture_female_df = data.frame( a_bisquare = female_TL_bisquare_coef[1,], b_bisquare = female_TL_bisquare_coef[2,], c_bisquare = female_TL_bisquare_coef[3,], d_bisquare = female_TL_bisquare_coef[4,], row.names = state[-1])


# Select the number of clusters by K-means
savepdf("wss_female_long_run", width = 12, height = 10, toplines = 0.8)
fviz_nbclust_plot(state_long_run_prefecture_female_df, kmeans, method = "wss")
dev.off()

savepdf("silhouette_female_long_run", width = 12, height = 10, toplines = 0.8)
fviz_nbclust(state_long_run_prefecture_female_df, kmeans, method = "silhouette")
dev.off()

kmeans_female_long_run = kmeans(state_long_run_prefecture_female_df, centers = 2, nstart = 25)
kmeans_female_plot_long_run = fviz_cluster(kmeans_female_long_run, data = state_long_run_prefecture_female_df, main = "", ylab = FALSE) + theme(legend.position="none") + theme(axis.title.x = element_text(size=20))
plot(kmeans_female_plot_long_run)

savepdf("Fig_7b", width = 18, height = 15, toplines =1)
plot(kmeans_female_plot_long_run)
dev.off()

k_members_female_long_run = data.frame(Region = attributes(kmeans_female_long_run$cluster)$names, Cluster_female = as.vector(kmeans_female_long_run$cluster))


# male

state_long_run_prefecture_male_df = data.frame(a_linear = male_TL_coef[1,], b_linear = male_TL_coef[2,], c_linear = male_TL_coef[3,], d_linear = male_TL_coef[4,], a_hampel = male_TL_hampel_coef[1,], b_hampel = male_TL_hampel_coef[2,], c_hampel = male_TL_hampel_coef[3,], d_hampel = male_TL_hampel_coef[4,], a_bisquare = male_TL_bisquare_coef[1,], b_bisquare = male_TL_bisquare_coef[2,], c_bisquare = male_TL_bisquare_coef[3,], d_bisquare = male_TL_bisquare_coef[4,], row.names = state[-1])

state_long_run_prefecture_male_df = data.frame(a_bisquare = male_TL_bisquare_coef[1,], b_bisquare = male_TL_bisquare_coef[2,], c_bisquare = male_TL_bisquare_coef[3,], d_bisquare = male_TL_bisquare_coef[4,], row.names = state[-1])


# Select the number of clusters by K-means
savepdf("wss_male_long_run", width = 12, height = 10, toplines = 0.8)
fviz_nbclust_plot(state_long_run_prefecture_male_df, kmeans, method = "wss")
dev.off()

savepdf("silhouette_male_long_run", width = 12, height = 10, toplines = 0.8)
fviz_nbclust(state_long_run_prefecture_male_df, kmeans, method = "silhouette")
dev.off()

kmeans_male_long_run = kmeans(state_long_run_prefecture_male_df, centers = 2, nstart = 25)
kmeans_male_plot_long_run = fviz_cluster(kmeans_male_long_run, data = state_long_run_prefecture_male_df, main = "", ylab = FALSE)  + theme(legend.title = element_text(size=20), legend.text = element_text(size=15)) + theme(axis.title.x = element_text(size = 20))
plot(kmeans_male_plot_long_run)

savepdf("Fig_7c", width = 18, height = 15, toplines = 0.8)
plot(kmeans_male_plot_long_run)
dev.off()

k_members_male_long_run = data.frame(Region = attributes(kmeans_male_long_run$cluster)$names, Cluster_male = as.vector(kmeans_male_long_run$cluster))

# total

state_long_run_prefecture_total_df = data.frame(slope = total_TL_slope_prefecture, slope_hampel = total_TL_hampel_slope_prefecture, slope_bisquare = total_TL_bisquare_slope_prefecture, row.names = state[-1])

state_long_run_prefecture_total_df = data.frame(a_bisquare = total_TL_bisquare_coef[1,], b_bisquare = total_TL_bisquare_coef[2,], c_bisquare = total_TL_bisquare_coef[3,], d_bisquare = total_TL_bisquare_coef[4,], row.names = state[-1])

# Select the number of clusters by K-means
savepdf("wss_total_long_run", width = 12, height = 10, toplines = 0.8)
fviz_nbclust_plot(state_long_run_prefecture_total_df, kmeans, method = "wss")
dev.off()

savepdf("silhouette_total_long_run", width = 24, height = 20, toplines = 0.8)
fviz_nbclust(state_long_run_prefecture_total_df, kmeans, method = "silhouette")
dev.off()

kmeans_total_long_run = kmeans(state_long_run_prefecture_total_df, centers = 2, nstart = 25)
kmeans_total_plot_long_run = fviz_cluster(kmeans_total_long_run, data = state_long_run_prefecture_total_df, main = "") + theme(axis.title = element_text(size = 20)) + theme(legend.position="none")
plot(kmeans_total_plot_long_run)

savepdf("Fig_7a", width = 18, height = 15, toplines = 0.8)
plot(kmeans_total_plot_long_run)
dev.off()

k_members_total_long_run = data.frame(Region = attributes(kmeans_total_long_run$cluster)$names, Cluster_total = as.vector(kmeans_total_long_run$cluster))

k_members_all_long_run = list(k_members_total_long_run, k_members_female_long_run, k_members_male_long_run) %>% reduce(left_join, by = "Region")


# comparison

## conventional
sum(k_members_all[,"Cluster_female"] != k_members_all[,"Cluster_male"])

## long-run
sum(k_members_all_long_run[,"Cluster_female"] != k_members_all_long_run[,"Cluster_male"])
sum(k_members_all[,"Cluster_female"] != k_members_all_long_run[,"Cluster_female"])
sum(k_members_all[,"Cluster_male"] != k_members_all_long_run[,"Cluster_male"])
sum(k_members_all[,"Cluster_total"] != k_members_all_long_run[,"Cluster_total"])


################################
# Rangking of Prefecture by GDP
################################

GDP_prefecture <- read.csv("X:/Dropbox/Working projects/TL_JMD/TL project/Japan_prefecture.csv")
library(tidyverse)

GDP_prefecture[GDP_prefecture$Region == "Gumma", "Region"] = "Gunma"
# Set variables
all_measure = unique(GDP_prefecture$Measure)

# 2014 ranking
GDP_2014 = GDP_prefecture[,c("Year", "Region", "Value", "Measure")] %>% filter(Year == 2014, Region != "Japan", Measure == all_measure[1]) %>% arrange(Year, -Value, Region)
GDP_2014 = mutate(GDP_2014, Ranking = 1:47) %>% select(Region, Ranking, Value)

GDP_2014_final = merge(GDP_2014, k_members_all, by = "Region") %>% arrange(Ranking, Cluster, Region, Value)

# 2016 ranking
GDP_2016 = GDP_prefecture[,c("Year", "Region", "Value", "Measure")] %>% filter(Year == 2016, Region != "Japan", Measure == all_measure[1]) %>% arrange(Year, -Value, Region)
GDP_2016 = mutate(GDP_2016) %>% select(Region, Value)

GDP_2016_final = merge(GDP_2016, k_members_all_long_run, by = "Region") %>% arrange(-Value, Region, Cluster_total, Cluster_female, Cluster_male)

GDP_2016_ranking = cbind(GDP_2016, 1:47)
colnames(GDP_2016_ranking)[2:3] = c("GDP" , "GDP Ranking")

# 2016 per capita ranking
GDP_2016_per = GDP_prefecture[,c("Year", "Region", "Value", "Measure")] %>% filter(Year == 2016, Region != "Japan", Measure == all_measure[5]) %>% arrange(Year, -Value, Region)
GDP_2016_per = mutate(GDP_2016_per) %>% select(Region, Value)

GDP_2016_per_final = merge(GDP_2016_per, k_members_all_long_run, by = "Region") %>% arrange(-Value, Region, Cluster_total, Cluster_female, Cluster_male)

GDP_2016_per_ranking = cbind(GDP_2016_per, 1:47)
colnames(GDP_2016_per_ranking)[2:3] = c("GDPPP", "GDPPP Ranking")

# GDPPP table output
GDPPP_table_13 = merge(prefecture_index, GDP_2016_per_ranking, by = "Region")
# GDP_table_15 = merge(GDP_table_13, GDP_2016_per_ranking, by = "Region")
GDPPP_table_18 = merge(GDPPP_table_13, k_members_all_long_run, by = "Region")
GDPPP_table_1_12 = merge(GDPPP_table_18, k_members_all_flip, by = "Region") %>% arrange(-GDPPP, Region, Index, Cluster_total, Cluster_female, Cluster_male, total_OLS, female_OLS, male_OLS)

# prefecture index
prefecture_index = data.frame(Region = state[-1], Index = 1:47)

# prefecture with estimated slopes
prefecture_slope = data.frame(Region = state[-1], total_long_run_TL_bisquare_slope_prefecture, female_long_run_TL_bisquare_slope_prefecture, male_long_run_TL_bisquare_slope_prefecture)
colnames(prefecture_slope)[2:4] = c("Total_slope","Female_slope", "Male_slope")

# GDP table output
GDP_table_13 = merge(prefecture_index, GDP_2016_ranking, by = "Region")
# GDP_table_15 = merge(GDP_table_13, GDP_2016_per_ranking, by = "Region")
GDP_table_18 = merge(GDP_table_13, k_members_all_long_run, by = "Region")
GDP_table_1_12 = merge(GDP_table_18, k_members_all_flip, by = "Region") %>% arrange(-GDP, Region, Index, Cluster_total, Cluster_female, Cluster_male, total_OLS, female_OLS, male_OLS)


stargazer::stargazer(GDP_table_1_12, digits = 3, summary = FALSE, rownames = FALSE)

sum(GDP_table_1_12$Cluster_female == GDP_table_1_12$Cluster_male)

# combined table
GDP_table_15_all = merge(GDP_table_13, GDP_2016_per_ranking, by = "Region")
GDP_table_18_all = merge(GDP_table_15_all, k_members_all_long_run, by = "Region")
GDP_table_1_12_all = merge(GDP_table_18_all, k_members_all_flip, by = "Region") %>% arrange(-GDP, Region, Index, Cluster_total, Cluster_female, Cluster_male, total_OLS, female_OLS, male_OLS)

stargazer::stargazer(GDP_table_1_12_all, digits = 3, summary = FALSE, rownames = FALSE)
#############
# Odds ratio
#############

# total: GDPPP
odds_ratio_GDPPP_total = matrix(0, nrow = 2, ncol = 2)
odds_ratio_GDPPP_total[1,1] = sum(GDPPP_table_1_12$Cluster_total[1:22] == 1)
odds_ratio_GDPPP_total[1,2] = sum(GDPPP_table_1_12$Cluster_total[23:47] == 1)
odds_ratio_GDPPP_total[2,1] = sum(GDPPP_table_1_12$Cluster_total[1:22] == 2)
odds_ratio_GDPPP_total[2,2] = sum(GDPPP_table_1_12$Cluster_total[23:47] == 2) 

od_GDPPP_total = odds_ratio_GDPPP_total[1,1]*odds_ratio_GDPPP_total[2,2]/(odds_ratio_GDPPP_total[1,2]*odds_ratio_GDPPP_total[2,1])

odds_ratio_GDPPP_total_OLS = matrix(0, nrow = 2, ncol = 2)
odds_ratio_GDPPP_total_OLS[1,1] = sum(GDPPP_table_1_12$total_OLS[1:22] == 1)
odds_ratio_GDPPP_total_OLS[1,2] = sum(GDPPP_table_1_12$total_OLS[23:47] == 1)
odds_ratio_GDPPP_total_OLS[2,1] = sum(GDPPP_table_1_12$total_OLS[1:22] == 2)
odds_ratio_GDPPP_total_OLS[2,2] = sum(GDPPP_table_1_12$total_OLS[23:47] == 2) 

od_GDPPP_total_OLS = odds_ratio_GDPPP_total_OLS[1,1]*odds_ratio_GDPPP_total_OLS[2,2]/(odds_ratio_GDPPP_total_OLS[1,2]*odds_ratio_GDPPP_total_OLS[2,1])

# female; GDPPP
odds_ratio_GDPPP_female = matrix(0, nrow = 2, ncol = 2)
odds_ratio_GDPPP_female[1,1] = sum(GDPPP_table_1_12$Cluster_female[1:17] == 1)
odds_ratio_GDPPP_female[1,2] = sum(GDPPP_table_1_12$Cluster_female[18:47] == 1)
odds_ratio_GDPPP_female[2,1] = sum(GDPPP_table_1_12$Cluster_female[1:17] == 2)
odds_ratio_GDPPP_female[2,2] = sum(GDPPP_table_1_12$Cluster_female[18:47] == 2) 

od_GDPPP_female = odds_ratio_GDPPP_female[1,1]*odds_ratio_GDPPP_female[2,2]/(odds_ratio_GDPPP_female[1,2]*odds_ratio_GDPPP_female[2,1])

odds_ratio_GDPPP_female_OLS = matrix(0, nrow = 2, ncol = 2)
odds_ratio_GDPPP_female_OLS[1,1] = sum(GDPPP_table_1_12$female_OLS[1:17] == 1)
odds_ratio_GDPPP_female_OLS[1,2] = sum(GDPPP_table_1_12$female_OLS[18:47] == 1)
odds_ratio_GDPPP_female_OLS[2,1] = sum(GDPPP_table_1_12$female_OLS[1:17] == 2)
odds_ratio_GDPPP_female_OLS[2,2] = sum(GDPPP_table_1_12$female_OLS[18:47] == 2) 

od_GDPPP_female_OLS = odds_ratio_GDPPP_female_OLS[1,1]*odds_ratio_GDPPP_female_OLS[2,2]/(odds_ratio_GDPPP_female_OLS[1,2]*odds_ratio_GDPPP_female_OLS[2,1])

# male; GDPPP
odds_ratio_GDPPP_male = matrix(0, nrow = 2, ncol = 2)
odds_ratio_GDPPP_male[1,1] = sum(GDPPP_table_1_12$Cluster_male[1:22] == 1)
odds_ratio_GDPPP_male[1,2] = sum(GDPPP_table_1_12$Cluster_male[23:47] == 1)
odds_ratio_GDPPP_male[2,1] = sum(GDPPP_table_1_12$Cluster_male[1:22] == 2)
odds_ratio_GDPPP_male[2,2] = sum(GDPPP_table_1_12$Cluster_male[23:47] == 2) 

od_GDPPP_male = odds_ratio_GDPPP_male[1,1]*odds_ratio_GDPPP_male[2,2]/(odds_ratio_GDPPP_male[1,2]*odds_ratio_GDPPP_male[2,1])

odds_ratio_GDPPP_male_OLS = matrix(0, nrow = 2, ncol = 2)
odds_ratio_GDPPP_male_OLS[1,1] = sum(GDPPP_table_1_12$male_OLS[1:22] == 1)
odds_ratio_GDPPP_male_OLS[1,2] = sum(GDPPP_table_1_12$male_OLS[23:47] == 1)
odds_ratio_GDPPP_male_OLS[2,1] = sum(GDPPP_table_1_12$male_OLS[1:22] == 2)
odds_ratio_GDPPP_male_OLS[2,2] = sum(GDPPP_table_1_12$male_OLS[23:47] == 2) 

od_GDPPP_male_OLS = odds_ratio_GDPPP_male_OLS[1,1]*odds_ratio_GDPPP_male_OLS[2,2]/(odds_ratio_GDPPP_male_OLS[1,2]*odds_ratio_GDPPP_male_OLS[2,1])

# total; GDP
odds_ratio_GDP_total = matrix(0, nrow = 2, ncol = 2)
odds_ratio_GDP_total[1,1] = sum(GDP_table_1_12$Cluster_total[1:22] == 1)
odds_ratio_GDP_total[1,2] = sum(GDP_table_1_12$Cluster_total[23:47] == 1)
odds_ratio_GDP_total[2,1] = sum(GDP_table_1_12$Cluster_total[1:22] == 2)
odds_ratio_GDP_total[2,2] = sum(GDP_table_1_12$Cluster_total[23:47] == 2) 

od_GDP_total = odds_ratio_GDP_total[1,1]*odds_ratio_GDP_total[2,2]/(odds_ratio_GDP_total[1,2]*odds_ratio_GDP_total[2,1])

odds_ratio_GDP_total_OLS = matrix(0, nrow = 2, ncol = 2)
odds_ratio_GDP_total_OLS[1,1] = sum(GDP_table_1_12$total_OLS[1:22] == 1)
odds_ratio_GDP_total_OLS[1,2] = sum(GDP_table_1_12$total_OLS[23:47] == 1)
odds_ratio_GDP_total_OLS[2,1] = sum(GDP_table_1_12$total_OLS[1:22] == 2)
odds_ratio_GDP_total_OLS[2,2] = sum(GDP_table_1_12$total_OLS[23:47] == 2) 

od_GDP_total_OLS = odds_ratio_GDP_total_OLS[1,1]*odds_ratio_GDP_total_OLS[2,2]/(odds_ratio_GDP_total_OLS[1,2]*odds_ratio_GDP_total_OLS[2,1])
9
# female; GDP
odds_ratio_GDP_female = matrix(0, nrow = 2, ncol = 2)
odds_ratio_GDP_female[1,1] = sum(GDP_table_1_12$Cluster_female[1:17] == 1)
odds_ratio_GDP_female[1,2] = sum(GDP_table_1_12$Cluster_female[18:47] == 1)
odds_ratio_GDP_female[2,1] = sum(GDP_table_1_12$Cluster_female[1:17] == 2)
odds_ratio_GDP_female[2,2] = sum(GDP_table_1_12$Cluster_female[18:47] == 2) 

od_GDP_female = odds_ratio_GDP_female[1,1]*odds_ratio_GDP_female[2,2]/(odds_ratio_GDP_female[1,2]*odds_ratio_GDP_female[2,1])

odds_ratio_GDP_female_OLS = matrix(0, nrow = 2, ncol = 2)
odds_ratio_GDP_female_OLS[1,1] = sum(GDP_table_1_12$female_OLS[1:17] == 1)
odds_ratio_GDP_female_OLS[1,2] = sum(GDP_table_1_12$female_OLS[18:47] == 1)
odds_ratio_GDP_female_OLS[2,1] = sum(GDP_table_1_12$female_OLS[1:17] == 2)
odds_ratio_GDP_female_OLS[2,2] = sum(GDP_table_1_12$female_OLS[18:47] == 2) 

od_GDP_female_OLS = odds_ratio_GDP_female_OLS[1,1]*odds_ratio_GDP_female_OLS[2,2]/(odds_ratio_GDP_female_OLS[1,2]*odds_ratio_GDP_female_OLS[2,1])

# male; GDP
odds_ratio_GDP_male = matrix(0, nrow = 2, ncol = 2)
odds_ratio_GDP_male[1,1] = sum(GDP_table_1_12$Cluster_male[1:22] == 1)
odds_ratio_GDP_male[1,2] = sum(GDP_table_1_12$Cluster_male[23:47] == 1)
odds_ratio_GDP_male[2,1] = sum(GDP_table_1_12$Cluster_male[1:22] == 2)
odds_ratio_GDP_male[2,2] = sum(GDP_table_1_12$Cluster_male[23:47] == 2) 

od_GDP_male = odds_ratio_GDP_male[1,1]*odds_ratio_GDP_male[2,2]/(odds_ratio_GDP_male[1,2]*odds_ratio_GDP_male[2,1])

odds_ratio_GDP_male_OLS = matrix(0, nrow = 2, ncol = 2)
odds_ratio_GDP_male_OLS[1,1] = sum(GDP_table_1_12$male_OLS[1:22] == 1)
odds_ratio_GDP_male_OLS[1,2] = sum(GDP_table_1_12$male_OLS[23:47] == 1)
odds_ratio_GDP_male_OLS[2,1] = sum(GDP_table_1_12$male_OLS[1:22] == 2)
odds_ratio_GDP_male_OLS[2,2] = sum(GDP_table_1_12$male_OLS[23:47] == 2) 

od_GDP_male_OLS = odds_ratio_GDP_male_OLS[1,1]*odds_ratio_GDP_male_OLS[2,2]/(odds_ratio_GDP_male_OLS[1,2]*odds_ratio_GDP_male_OLS[2,1])

rownames(odds_ratio_GDPPP_total) = rownames(odds_ratio_GDP_total) = 
  rownames(odds_ratio_GDPPP_female) = rownames(odds_ratio_GDP_female) = 
  rownames(odds_ratio_GDPPP_male) = rownames(odds_ratio_GDP_male) =
rownames(odds_ratio_GDPPP_total_OLS) = rownames(odds_ratio_GDP_total_OLS) = 
  rownames(odds_ratio_GDPPP_female_OLS) = rownames(odds_ratio_GDP_female_OLS) = 
  rownames(odds_ratio_GDPPP_male_OLS) = rownames(odds_ratio_GDP_male_OLS) = c(1,2)


colnames(odds_ratio_GDPPP_total) = colnames(odds_ratio_GDP_total) = 
  colnames(odds_ratio_GDPPP_female) = colnames(odds_ratio_GDP_female) = 
  colnames(odds_ratio_GDPPP_male) = colnames(odds_ratio_GDP_male) = 
  colnames(odds_ratio_GDPPP_total_OLS) = colnames(odds_ratio_GDP_total_OLS) = 
  colnames(odds_ratio_GDPPP_female_OLS) = colnames(odds_ratio_GDP_female_OLS) = 
  colnames(odds_ratio_GDPPP_male_OLS) = colnames(odds_ratio_GDP_male_OLS) = c("High", "Low")

## make a table
odds_ratio_table = rbind(cbind(odds_ratio_GDP_total, round(od_GDP_total, 2), odds_ratio_GDPPP_total, round(od_GDPPP_total, 2)),
cbind(odds_ratio_GDP_female, round(od_GDP_female, 2), odds_ratio_GDPPP_female, round(od_GDPPP_female, 2)),
cbind(odds_ratio_GDP_male, round(od_GDP_male, 2), odds_ratio_GDPPP_male, round(od_GDPPP_male, 2)))

stargazer(odds_ratio_table, digits = 2)

odds_ratio_table_OLS = rbind(cbind(odds_ratio_GDP_total_OLS, round(od_GDP_total_OLS, 2), odds_ratio_GDPPP_total_OLS, round(od_GDPPP_total_OLS, 2)), cbind(odds_ratio_GDP_female_OLS, round(od_GDP_female_OLS, 2), odds_ratio_GDPPP_female_OLS, round(od_GDPPP_female_OLS, 2)), cbind(odds_ratio_GDP_male_OLS, round(od_GDP_male_OLS, 2), odds_ratio_GDPPP_male_OLS, round(od_GDPPP_male_OLS, 2)))

stargazer(odds_ratio_table_OLS, digits = 2)


###########################
# Order 3 TL scatter plots
###########################

for(ij in 1:47)
{
  # female
  age_select = setdiff(age_all, as.numeric(rownames(female_TL_outliers[[ij]])))
  y_female = log(temporal_female_var[age_all %in% age_select,ij], base = 10)
  x_female = log(temporal_female_mean[age_all %in% age_select,ij], base = 10)
  
  OLS_model_female = lm(y_female ~ x_female + I(x_female^2) + I(x_female^3))
  newdata_female = data.frame(x_female = seq(from = min(x_female)-0.1, to = max(x_female)+0.2, by = 0.01))
  pred_female = predict(OLS_model_female, newdata_female, interval = "predict")
  
  savepdf(paste("prediction", ij, "female", sep = "_"), width = 20, height = 20, toplines = 0.8)
  par(mar = c(5,7,5,3))
  plot(x = log10(temporal_female_mean[,ij]), y = log10(temporal_female_var[,ij]), xlim = c(min(log10(temporal_female_mean[,ij]))-0.2, max(log10(temporal_female_mean[,ij]))+0.2), xlab = expression(paste(log[10], "(Mean)")), ylab = expression(paste(log[10], "(Variance)")), cex.lab = 3, col = rainbow(101, end = 0.9))
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
  y_male = log(temporal_male_var[age_all %in% age_select,ij], base = 10)
  x_male = log(temporal_male_mean[age_all %in% age_select,ij], base = 10)
  
  OLS_model_male = lm(y_male ~ x_male + I(x_male^2) + I(x_male^3))
  newdata_male = data.frame(x_male = seq(from = min(x_male, na.rm = T)-0.1, to = max(x_male, na.rm = T)+0.2, by = 0.01))
  pred_male = predict(OLS_model_male, newdata_male, interval = "predict")
  
  savepdf(paste("prediction", ij, "male", sep = "_"), width = 20, height = 20, toplines = 0.8)
  par(mar = c(5,7,5,3))
  plot(x = log10(temporal_male_mean[,ij]), y = log10(temporal_male_var[,ij]), xlim = c(min(log10(temporal_male_mean[,ij]), na.rm = T)-0.2, max(log10(temporal_male_mean[,ij]), na.rm = T)+0.2), xlab = expression(paste(log[10], "(Mean)")), ylab = expression(paste(log[10], "(Variance)")), cex.lab = 3, col = rainbow(101, end = 0.9))
  if(nrow(male_TL_outliers[[ij]]) > 0)
  {
    points(male_TL_outliers[[ij]][,1], male_TL_outliers[[ij]][,2], pch = 21, bg = 3:(nrow(male_TL_outliers[[ij]])+2), col = "black", cex = 2)
    legend("topleft", col = 3:(nrow(male_TL_outliers[[ij]])+2), pch = 16, legend = rownames(male_TL_outliers[[ij]]), cex = 2)
  }
  title(main = state[ij+1], line = 1, cex.main = 3, font.main= 2)
  lines(x = newdata_male$x, y = pred_male[,1], col = 2, lwd = 2)
  polygon(c(rev(newdata_male$x), newdata_male$x), c(rev(pred_male[,3]), pred_male[,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
  dev.off()
  
  # total
  age_select = setdiff(age_all, as.numeric(rownames(total_TL_outliers[[ij]])))
  y_total = log(temporal_total_var[age_all %in% age_select,ij], base = 10)
  x_total = log(temporal_total_mean[age_all %in% age_select,ij], base = 10)
  
  OLS_model_total = lm(y_total ~ x_total + I(x_total^2) + I(x_total^3))
  newdata_total = data.frame(x_total = seq(from = min(x_total)-0.1, to = max(x_total)+0.2, by = 0.01))
  pred_total = predict(OLS_model_total, newdata_total, interval = "predict")
  
  savepdf(paste("prediction", ij, "total", sep = "_"), width = 20, height = 20, toplines = 0.8)
  par(mar = c(5,7,5,3))
  plot(x = log10(temporal_total_mean[,ij]), y = log10(temporal_total_var[,ij]), xlim = c(min(log10(temporal_total_mean[,ij]))-0.2, max(log10(temporal_total_mean[,ij]))+0.2), xlab = expression(paste(log[10], "(Mean)")), ylab = expression(paste(log[10], "(Variance)")), cex.lab = 3, col = rainbow(101, end = 0.9))
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


#############################################
# Plots of temporal coefficients against GDP
#############################################

library(tidyverse)

GDP_NtoS = GDP_2016 %>% arrange(factor(Region, levels = state[-1]))
GDPPP_NtoS = GDP_2016_per %>% arrange(factor(Region, levels = state[-1]))

## total
savepdf("total_temporal_GDP_intercept", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(log10(GDP_NtoS$Value), total_TL_coef[1,], xlab = "", ylab = expression(log[10](hat(a[3]))), main = "", ylim = c(-4.1, -2.3), cex.lab = 1.5)
points(log10(GDP_NtoS$Value), (total_TL_hampel_coef[1,]), col = 2, pch = 2)
points(log10(GDP_NtoS$Value), (total_TL_bisquare_coef[1,]), col = 4, pch = 4)
legend("topright", c("OLS", "Robust (Hampel)", "Robust (Bisquare)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 1)
dev.off()

savepdf("total_temporal_GDP_order1", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(log10(GDP_NtoS$Value), total_TL_coef[2,], xlab = "", ylab = expression(hat(b)[3]),  main = "", cex.lab = 1.5, ylim = c(-2.3, 2.1))
points(log10(GDP_NtoS$Value), total_TL_hampel_coef[2,], col = 2, pch = 2)
points(log10(GDP_NtoS$Value), total_TL_bisquare_coef[2,], col = 4, pch = 4)
dev.off()

savepdf("total_temporal_GDP_order2", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(log10(GDP_NtoS$Value), total_TL_coef[3,], xlab = "", ylab = expression(hat(c)[3]), main = "", cex.lab = 1.5, ylim = c(-2.5, -0.1))
points(log10(GDP_NtoS$Value), total_TL_hampel_coef[3,], col = 2, pch = 2)
points(log10(GDP_NtoS$Value), total_TL_bisquare_coef[3,], col = 4, pch = 4)
dev.off()

savepdf("total_temporal_GDP_order3", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(log10(GDP_NtoS$Value), total_TL_coef[4,], xlab = expression(paste(log[10], "(Total GDP)")), 
     ylab = expression(hat(d)[3]), ylim = c(-0.4, -0.07), xlim = c(6.2, 8.2), main = "", cex.lab = 1.5)
points(log10(GDP_NtoS$Value), total_TL_hampel_coef[4,], col = 2, pch = 2)
points(log10(GDP_NtoS$Value), total_TL_bisquare_coef[4,], col = 4, pch = 4)
text_ind = c(which.max(total_TL_coef[4,]), which.min(total_TL_coef[4,]), which.max(log10(GDP_NtoS$Value)))
text(log10(GDP_NtoS$Value)[text_ind], total_TL_hampel_coef[4,text_ind], labels = state[-1][text_ind], cex = 0.8, pos = 4)
dev.off()

## female
savepdf("female_temporal_GDP_intercept", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(log10(GDP_NtoS$Value), female_TL_coef[1,], xlab = "", ylab = "", main = "", ylim = c(-4.3, -2.9), cex.lab = 1.5)
points(log10(GDP_NtoS$Value), (female_TL_hampel_coef[1,]), col = 2, pch = 2)
points(log10(GDP_NtoS$Value), (female_TL_bisquare_coef[1,]), col = 4, pch = 4)
dev.off()

savepdf("female_temporal_GDP_order1", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(log10(GDP_NtoS$Value), female_TL_coef[2,], xlab = "", ylab = "",  main = "", cex.lab = 1.5, ylim = c(-2.7, 0.1))
points(log10(GDP_NtoS$Value), female_TL_hampel_coef[2,], col = 2, pch = 2)
points(log10(GDP_NtoS$Value), female_TL_bisquare_coef[2,], col = 4, pch = 4)
dev.off()

savepdf("female_temporal_GDP_order2", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(log10(GDP_NtoS$Value), female_TL_coef[3,], xlab = "", ylab = "", main = "", ylim = c(-2.6, -1), cex.lab = 1.5)
points(log10(GDP_NtoS$Value), female_TL_hampel_coef[3,], col = 2, pch = 2)
points(log10(GDP_NtoS$Value), female_TL_bisquare_coef[3,], col = 4, pch = 4)
dev.off()

savepdf("female_temporal_GDP_order3", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(log10(GDP_NtoS$Value), female_TL_coef[4,], xlab = expression(paste(log[10], "(Total GDP)")),  ylab = "",  ylim = c(-0.41,-0.17), main = "", cex.lab = 1.5, xlim = c(6.2,8.3))
points(log10(GDP_NtoS$Value), female_TL_hampel_coef[4,], col = 2, pch = 2)
points(log10(GDP_NtoS$Value), female_TL_bisquare_coef[4,], col = 4, pch = 4)
text_ind = c(which.max(female_TL_coef[4,]), which.min(female_TL_coef[4,]), which.max(log10(GDP_NtoS$Value)))
text(log10(GDP_NtoS$Value)[text_ind], female_TL_coef[4,text_ind], labels = state[-1][text_ind], cex = 0.8, pos = 4)
dev.off()

## male
savepdf("male_temporal_GDP_intercept", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(log10(GDP_NtoS$Value), male_TL_coef[1,], xlab = "", ylab = "", main = "",  cex.lab = 1.5, ylim = c(-4.5, -2.1))
points(log10(GDP_NtoS$Value), (male_TL_hampel_coef[1,]), col = 2, pch = 2)
points(log10(GDP_NtoS$Value), (male_TL_bisquare_coef[1,]), col = 4, pch = 4)
dev.off()

savepdf("male_temporal_GDP_order1", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(log10(GDP_NtoS$Value), male_TL_coef[2,], xlab = "", ylab = "",  main = "", cex.lab = 1.5, ylim = c(-2.5,2.9))
points(log10(GDP_NtoS$Value), male_TL_hampel_coef[2,], col = 2, pch = 2)
points(log10(GDP_NtoS$Value), male_TL_bisquare_coef[2,], col = 4, pch = 4)
dev.off()

savepdf("male_temporal_GDP_order2", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(log10(GDP_NtoS$Value), male_TL_coef[3,], xlab = "",  ylab = "", main = "", cex.lab = 1.5, ylim = c(-2.3,0.4))
points(log10(GDP_NtoS$Value), male_TL_hampel_coef[3,], col = 2, pch = 2)
points(log10(GDP_NtoS$Value), male_TL_bisquare_coef[3,], col = 4, pch = 4)
dev.off()

savepdf("male_temporal_GDP_order3", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(log10(GDP_NtoS$Value), male_TL_coef[4,], xlab = expression(paste(log[10], "(Total GDP)")), ylab = "", main = "", cex.lab = 1.5, ylim = c(-0.34,0.02), xlim = c(6.2, 8.3))
points(log10(GDP_NtoS$Value), male_TL_hampel_coef[4,], col = 2, pch = 2)
points(log10(GDP_NtoS$Value), male_TL_bisquare_coef[4,], col = 4, pch = 4)
text_ind = c(which.max(male_TL_coef[4,]), which.min(male_TL_coef[4,]), which.max(log10(GDP_NtoS$Value)), which.min(male_TL_bisquare_coef[4,]))
text(log10(GDP_NtoS$Value)[text_ind], male_TL_coef[4,text_ind], labels = state[-1][text_ind], cex = 0.8, pos = 4)
dev.off()


#################################
# Plots of Dimension1 against d3
#################################

## total
savepdf("Fig_8a", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(x = kmeans_total_plot_long_run$plot_env$data$x, y = total_TL_coef[1,], xlab = "", ylab = expression(log[10](hat(a)[3])), cex.lab = 1.5, ylim = c(-4.2,-2.3))
points(x = kmeans_total_plot_long_run$plot_env$data$x, y = total_TL_hampel_coef[1,], col = 2, pch = 2)
points(x = kmeans_total_plot_long_run$plot_env$data$x, y = total_TL_bisquare_coef[1,], col = 4, pch = 4)
legend("topleft", c("OLS", "Robust (Hampel)", "Robust (Bisquare)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 1)
dev.off()

savepdf("Fig_8d", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(x = kmeans_total_plot_long_run$plot_env$data$x, y = total_TL_coef[2,], xlab = "", ylab = expression(hat(b)[3]), cex.lab = 1.5,  ylim = c(-2.2, 2.2))
points(x = kmeans_total_plot_long_run$plot_env$data$x, y = total_TL_hampel_coef[2,], col = 2, pch = 2)
points(x = kmeans_total_plot_long_run$plot_env$data$x, y = total_TL_bisquare_coef[2,], col = 4, pch = 4)
dev.off()

savepdf("Fig_8g", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(x = kmeans_total_plot_long_run$plot_env$data$x, y = total_TL_coef[3,], xlab = "", ylab = expression(hat(c)[3]), cex.lab = 1.5, ylim = c(-2.55, -0.01))
points(x = kmeans_total_plot_long_run$plot_env$data$x, y = total_TL_hampel_coef[3,], col = 2, pch = 2)
points(x = kmeans_total_plot_long_run$plot_env$data$x, y = total_TL_bisquare_coef[3,], col = 4, pch = 4)
dev.off()

savepdf("Fig_8j", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(x = kmeans_total_plot_long_run$plot_env$data$x, y = total_TL_coef[4,], xlab = "Dimension 1", ylab = expression(hat(d)[3]), cex.lab = 1.5, ylim = c(-0.405, -0.05), xlim = c(-5.7,6.5))
points(x = kmeans_total_plot_long_run$plot_env$data$x, y = total_TL_hampel_coef[4,], col = 2, pch = 2)
points(x = kmeans_total_plot_long_run$plot_env$data$x, y = total_TL_bisquare_coef[4,], col = 4, pch = 4)
text_ind = c(which.max(total_TL_coef[4,]), which.min(total_TL_coef[4,]))
text(kmeans_total_plot_long_run$plot_env$data$x[text_ind], total_TL_coef[4,text_ind], labels = state[-1][text_ind], cex = 0.8, pos = 3)
dev.off()

## female
savepdf("Fig_8b", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(x = kmeans_female_plot_long_run$plot_env$data$x, y = female_TL_coef[1,], xlab = "", ylab = "", cex.lab = 1.5, ylim = c(-4.3, -2.9), xlim = c(-5.6, 3.1))
points(x = kmeans_female_plot_long_run$plot_env$data$x, y = female_TL_hampel_coef[1,], col = 2, pch = 2)
points(x = kmeans_female_plot_long_run$plot_env$data$x, y = female_TL_bisquare_coef[1,], col = 4, pch = 4)
dev.off()

savepdf("Fig_8e", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(x = kmeans_female_plot_long_run$plot_env$data$x, y = female_TL_coef[2,], xlab = "", ylab = "", cex.lab = 1.5,  ylim = c(-2.7, 0.2))
points(x = kmeans_female_plot_long_run$plot_env$data$x, y = female_TL_hampel_coef[2,], col = 2, pch = 2)
points(x = kmeans_female_plot_long_run$plot_env$data$x, y = female_TL_bisquare_coef[2,], col = 4, pch = 4)
dev.off()

savepdf("Fig_8h", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(x = kmeans_female_plot_long_run$plot_env$data$x, y = female_TL_coef[3,], xlab = "", ylab = "", cex.lab = 1.5, ylim = c(-2.55, -1))
points(x = kmeans_female_plot_long_run$plot_env$data$x, y = female_TL_hampel_coef[3,], col = 2, pch = 2)
points(x = kmeans_female_plot_long_run$plot_env$data$x, y = female_TL_bisquare_coef[3,], col = 4, pch = 4)
dev.off()

savepdf("Fig_8k", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(x = kmeans_female_plot_long_run$plot_env$data$x, y = female_TL_coef[4,], xlab = "Dimension 1", ylab = "", cex.lab = 1.5, ylim = c(-0.405, -0.17), xlim = c(-5.8,3.5))
points(x = kmeans_female_plot_long_run$plot_env$data$x, y = female_TL_hampel_coef[4,], col = 2, pch = 2)
points(x = kmeans_female_plot_long_run$plot_env$data$x, y = female_TL_bisquare_coef[4,], col = 4, pch = 4)
text_ind = c(which.max(female_TL_bisquare_coef[4,]),which.min(female_TL_bisquare_coef[4,]))
text(kmeans_female_plot_long_run$plot_env$data$x[text_ind], female_TL_coef[4,text_ind], labels = state[-1][text_ind], cex = 0.8, pos = 3)
dev.off()

## male
savepdf("Fig_8c", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(x = kmeans_male_plot_long_run$plot_env$data$x, y = male_TL_coef[1,], xlab = "", ylab = "", cex.lab = 1.5, ylim = c(-4.5,-2))
points(x = kmeans_male_plot_long_run$plot_env$data$x, y = male_TL_hampel_coef[1,], col = 2, pch = 2)
points(x = kmeans_male_plot_long_run$plot_env$data$x, y = male_TL_bisquare_coef[1,], col = 4, pch = 4)
dev.off()

savepdf("Fig_8f", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(x = kmeans_male_plot_long_run$plot_env$data$x, y = male_TL_coef[2,], xlab = "", ylab = "", cex.lab = 1.5,  ylim = c(-2.7, 3))
points(x = kmeans_male_plot_long_run$plot_env$data$x, y = male_TL_hampel_coef[2,], col = 2, pch = 2)
points(x = kmeans_male_plot_long_run$plot_env$data$x, y = male_TL_bisquare_coef[2,], col = 4, pch = 4)
dev.off()

savepdf("Fig_8i", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(x = kmeans_male_plot_long_run$plot_env$data$x, y = male_TL_coef[3,], xlab = "", ylab = "", cex.lab = 1.5, ylim = c(-2.3, 0.4))
points(x = kmeans_male_plot_long_run$plot_env$data$x, y = male_TL_hampel_coef[3,], col = 2, pch = 2)
points(x = kmeans_male_plot_long_run$plot_env$data$x, y = male_TL_bisquare_coef[3,], col = 4, pch = 4)
dev.off()

savepdf("Fig_8l", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(x = kmeans_male_plot_long_run$plot_env$data$x, y = male_TL_coef[4,], xlab = "Dimension 1", ylab = "", cex.lab = 1.5, ylim = c(-0.35, 0.03), xlim = c(-5.5, 5.2))
points(x = kmeans_male_plot_long_run$plot_env$data$x, y = male_TL_hampel_coef[4,], col = 2, pch = 2)
points(x = kmeans_male_plot_long_run$plot_env$data$x, y = male_TL_bisquare_coef[4,], col = 4, pch = 4)
text_ind = c(which.max(male_TL_bisquare_coef[4,]), sort(male_TL_bisquare_coef[4,], decreasing = F, index.return = T)$ix[1:2])
text(kmeans_male_plot_long_run$plot_env$data$x[text_ind], male_TL_coef[4,text_ind], labels = state[-1][text_ind], cex = 0.8, pos = 3)
dev.off()


#####

urban_strip = c("Ibaraki", "Saitama", "Chiba", "Tokyo", "Kanagawa", "Shizuoka", "Aichi", "Gifu", "Mie", "Osaka", "Hyogo", "Wakayama", "Okayama", "Hiroshima", "Yamaguchi", "Fukuoka", "Oita")

k_members_female_long_run[k_members_female_long_run$Cluster_female==1,"Region"] %in% GDP_table_1_12$Region[1:17]

urban_strip %in% k_members_female_long_run[k_members_female_long_run$Cluster_female==1,"Region"]

urban_strip %in% k_members_male_long_run[k_members_male_long_run$Cluster_male==1,"Region"]

urban_strip %in% k_members_total_long_run[k_members_total_long_run$Cluster_total==1,"Region"]

urban_strip %in% c(k_members_total_long_run[k_members_total_long_run$Cluster_total==1,"Region"], k_members_male_long_run[k_members_male_long_run$Cluster_male==1,"Region"], k_members_female_long_run[k_members_female_long_run$Cluster_female==1,"Region"])
