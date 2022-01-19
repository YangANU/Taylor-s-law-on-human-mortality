#############
# Spatial TL
#############

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

# identify outliers

spatial_female_TL_mahalanobis = spatial_male_TL_mahalanobis = spatial_total_TL_mahalanobis = matrix(NA, nrow = 101, ncol = 44)
rownames(spatial_female_TL_mahalanobis) = rownames(spatial_male_TL_mahalanobis) = rownames(spatial_total_TL_mahalanobis) = 0:100
colnames(spatial_female_TL_mahalanobis) = colnames(spatial_male_TL_mahalanobis) = colnames(spatial_total_TL_mahalanobis) = 1975:2018

for(ij in 1:44)
{
  female_select = cbind(log(spatial_dist_female_var[,ij], base = 10), log(spatial_female_mean[,ij], base = 10))
  female_TL_mahalanobis[,ij] = mahalanobis(x = female_select, center = colMeans(female_select), cov = cov(female_select))
  
  male_select = cbind(log(spatial_dist_male_var[,ij], base = 10), log(spatial_male_mean[,ij], base = 10))
  male_TL_mahalanobis[!is.na(spatial_male_mean[,ij]),ij] = mahalanobis(x = na.omit(male_select), center = colMeans(na.omit(male_select)), cov = cov(na.omit(male_select)))
  
  total_select = cbind(log(spatial_dist_total_var[,ij], base = 10), log(spatial_total_mean[,ij], base = 10))
  total_TL_mahalanobis[,ij] = mahalanobis(x = total_select, center = colMeans(total_select), cov = cov(total_select))
}

spatial_female_TL_outliers = spatial_male_TL_outliers = spatial_total_TL_outliers = 
  spatial_female_TL_mahalanobis_outliers = spatial_male_TL_mahalanobis_outliers = spatial_total_TL_mahalanobis_outliers = 
  spatial_female_TL_rstudent_outliers = spatial_male_TL_rstudent_outliers = spatial_total_TL_rstudent_outliers = list()


for(ij in 1:44)
{
  female_select = data.frame(log_var = log10(spatial_female_mean[,ij]), log_mean = log10(spatial_dist_female_var[,ij]))
  spatial_female_TL_mahalanobis_outliers[[ij]] = female_select[spatial_female_TL_mahalanobis[,ij] > qchisq(p = 0.95, df = 2), , drop = FALSE]
  spatial_female_TL_rstudent_outliers[[ij]] = female_select[abs(rstudent(lm(log10(spatial_dist_female_var[,ij]) ~ log10(spatial_female_mean[,ij])))) > 2, , drop = FALSE]
  outlier_age = as.numeric(intersect(row.names(spatial_female_TL_mahalanobis_outliers[[ij]]), row.names(spatial_female_TL_rstudent_outliers[[ij]])))
  spatial_female_TL_outliers[[ij]] = female_select[outlier_age+1,]
  
  male_select = data.frame(log_var = log10(spatial_male_mean[,ij]), log_mean = log10(spatial_dist_male_var[,ij]))
  spatial_male_TL_mahalanobis_outliers[[ij]] = male_select[spatial_male_TL_mahalanobis[,ij] > qchisq(p = 0.95, df = 2), , drop = FALSE]
  spatial_male_TL_rstudent_outliers[[ij]] = male_select[abs(rstudent(lm(log10(spatial_dist_male_var[,ij]) ~ log10(spatial_male_mean[,ij])))) > 2, , drop = FALSE]
  outlier_age = as.numeric(intersect(row.names(spatial_male_TL_mahalanobis_outliers[[ij]]), row.names(spatial_male_TL_rstudent_outliers[[ij]])))
  spatial_male_TL_outliers[[ij]] = male_select[outlier_age+1,]
  
  total_select = data.frame(log_var = log10(spatial_total_mean[,ij]), log_mean = log10(spatial_dist_total_var[,ij]))
  spatial_total_TL_mahalanobis_outliers[[ij]] = total_select[spatial_total_TL_mahalanobis[,ij] > qchisq(p = 0.95, df = 2), , drop = FALSE]
  spatial_total_TL_rstudent_outliers[[ij]] = total_select[abs(rstudent(lm(log10(spatial_dist_total_var[,ij]) ~ log10(spatial_total_mean[,ij])))) > 2, , drop = FALSE]
  outlier_age = as.numeric(intersect(row.names(spatial_total_TL_mahalanobis_outliers[[ij]]), row.names(spatial_total_TL_rstudent_outliers[[ij]])))
  spatial_total_TL_outliers[[ij]] = total_select[outlier_age+1,]
}

names(spatial_female_TL_outliers) = names(spatial_male_TL_outliers) = names(spatial_total_TL_outliers) = 
  names(spatial_female_TL_mahalanobis_outliers) = names(spatial_male_TL_mahalanobis_outliers) = names(spatial_total_TL_mahalanobis_outliers) =
  names(spatial_female_TL_rstudent_outliers) = names(spatial_male_TL_rstudent_outliers) = names(spatial_total_TL_rstudent_outliers) = 1975:2018

sum(unlist(lapply(spatial_female_TL_mahalanobis_outliers, nrow)))
sum(unlist(lapply(spatial_female_TL_outliers, nrow)))

unlist(lapply(spatial_female_TL_outliers, nrow))
unlist(lapply(spatial_male_TL_outliers, nrow))
unlist(lapply(spatial_total_TL_outliers, nrow))

########################
# 43*3 spatial TL plots
########################

for(ij in 1:44)
{
  # female
  age_select = setdiff(age_all, as.numeric(rownames(spatial_female_TL_outliers[[ij]])))
  y = log(spatial_dist_female_var[age_all %in% age_select,ij], base = 10)
  x = log(spatial_female_mean[age_all %in% age_select,ij], base = 10)
  
  OLS_model = lm(y ~ x + I(x^2) + I(x^3))
  newdata = data.frame(x = seq(from = min(x)-0.1, to = max(x)+0.2, by = 0.01))
  pred_conf = predict(OLS_model, newdata, interval = "confidence")
  pred_pred = predict(OLS_model, newdata, interval = "predict")
  
  savepdf(paste("spatial_prediction_female", ij, sep = "_"), width = 20, height = 20, toplines = 0.8)
  par(mar = c(5,7,5,3))
  plot(x = log10(spatial_female_mean[,ij]), y = log10(spatial_dist_female_var[,ij]), xlim = c(min(log10(spatial_female_mean[,ij]))-0.2, max(log10(spatial_female_mean[,ij]))+0.2), ylim = c(min(pred_pred[,2])-0.2, max(pred_pred[,3])+0.2), xlab = expression(paste(log[10], "(Mean)")), ylab = expression(paste(log[10], "(Variance)")), cex.lab = 2, cex.axis = 2, col = rainbow(101, end = 0.9))
  if(nrow(spatial_female_TL_outliers[[ij]]) > 0)
  {
    points(spatial_female_TL_outliers[[ij]][,1], spatial_female_TL_outliers[[ij]][,2], pch = 21, bg = 3:(nrow(spatial_female_TL_outliers[[ij]])+2), col = "black", cex = 2)
    legend("topleft", col = 3:(nrow(spatial_female_TL_outliers[[ij]])+2), pch = 16, legend = rownames(spatial_female_TL_outliers[[ij]]), cex = 2)
  }
  title(main = (1975:2018)[ij], line = 1, cex.main = 3, font.main= 2)
  lines(x = newdata$x, y = pred_pred[,1], col = 2, lwd = 2)
  polygon(c(rev(newdata$x), newdata$x), c(rev(pred_pred[,3]), pred_pred[,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
  dev.off()
  
  # male
  age_select = setdiff(age_all, as.numeric(rownames(spatial_male_TL_outliers[[ij]])))
  y = log(spatial_dist_male_var[age_all %in% age_select,ij], base = 10)
  x = log(spatial_male_mean[age_all %in% age_select,ij], base = 10)
  
  OLS_model = lm(y ~ x + I(x^2) + I(x^3))
  newdata = data.frame(x = seq(from = min(x, na.rm = TRUE)-0.1, to = max(x, na.rm = TRUE)+0.2, by = 0.01))
  pred_conf = predict(OLS_model, newdata, interval = "confidence")
  pred_pred = predict(OLS_model, newdata, interval = "predict")
  
  savepdf(paste("spatial_prediction_male", ij, sep = "_"), width = 20, height = 20, toplines = 0.8)
  par(mar = c(5,7,5,3))
  plot(x = log10(spatial_male_mean[,ij]), y = log10(spatial_dist_male_var[,ij]), xlim = c(min(log10(spatial_male_mean[,ij]), na.rm = TRUE)-0.2, max(log10(spatial_male_mean[,ij]), na.rm = TRUE)+0.2), ylim = c(min(pred_pred[,2])-0.2, max(pred_pred[,3])+0.2), xlab = expression(paste(log[10], "(Mean)")), ylab = expression(paste(log[10], "(Variance)")), cex.lab = 2, cex.axis = 2, col = rainbow(101, end = 0.9))
  if(nrow(spatial_male_TL_outliers[[ij]]) > 0)
  {
    points(spatial_male_TL_outliers[[ij]][,1], spatial_male_TL_outliers[[ij]][,2], pch = 21, bg = 3:(nrow(spatial_male_TL_outliers[[ij]])+2), col = "black", cex = 2)
    legend("topleft", col = 3:(nrow(spatial_male_TL_outliers[[ij]])+2), pch = 16, legend = rownames(spatial_male_TL_outliers[[ij]]), cex = 2)
  }
  title(main = (1975:2018)[ij], line = 1, cex.main = 3, font.main= 2)
  lines(x = newdata$x, y = pred_pred[,1], col = 2, lwd = 2)
  polygon(c(rev(newdata$x), newdata$x), c(rev(pred_pred[,3]), pred_pred[,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
  dev.off()
  
  # total
  age_select = setdiff(age_all, as.numeric(rownames(spatial_total_TL_outliers[[ij]])))
  y = log(spatial_dist_total_var[age_all %in% age_select,ij], base = 10)
  x = log(spatial_total_mean[age_all %in% age_select,ij], base = 10)
  
  OLS_model = lm(y ~ x + I(x^2) + I(x^3))
  newdata = data.frame(x = seq(from = min(x)-0.1, to = max(x)+0.2, by = 0.01))
  pred_conf = predict(OLS_model, newdata, interval = "confidence")
  pred_pred = predict(OLS_model, newdata, interval = "predict")
  
  savepdf(paste("spatial_prediction_total", ij, sep = "_"), width = 20, height = 20, toplines = 0.8)
  par(mar = c(5,7,5,3))
  plot(x = log10(spatial_total_mean[,ij]), y = log10(spatial_dist_total_var[,ij]), xlim = c(min(log10(spatial_total_mean[,ij]))-0.2, max(log10(spatial_total_mean[,ij]))+0.2), ylim = c(min(pred_pred[,2])-0.2, max(pred_pred[,3])+0.2), xlab = expression(paste(log[10], "(Mean)")), ylab = expression(paste(log[10], "(Variance)")), cex.lab = 2, cex.axis = 2, col = rainbow(101, end = 0.9))
  if(nrow(spatial_total_TL_outliers[[ij]]) > 0)
  {
    points(spatial_total_TL_outliers[[ij]][,1], spatial_total_TL_outliers[[ij]][,2], pch = 21, bg = 3:(nrow(spatial_total_TL_outliers[[ij]])+2), col = "black", cex = 2)
    legend("topleft", col = 3:(nrow(spatial_total_TL_outliers[[ij]])+2), pch = 16, legend = rownames(spatial_total_TL_outliers[[ij]]), cex = 2)
  }
  title(main = (1975:2018)[ij], line = 1, cex.main = 3, font.main= 2)
  lines(x = newdata$x, y = pred_pred[,1], col = 2, lwd = 2)
  polygon(c(rev(newdata$x), newdata$x), c(rev(pred_pred[,3]), pred_pred[,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
  dev.off()
}

#########################################
# Taylor's law slope parameter estimates
#########################################
library(MASS)
library(tidyverse)

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

spatial_female_classic = spatial_female_classic_hampel = spatial_female_classic_bisquare = matrix(NA, nrow = 2, ncol = 44)
spatial_female_classic_p = spatial_female_classic_hampel_p = spatial_female_classic_bisquare_p = matrix(NA, nrow = 2, ncol = 44)
spatial_female_classic_R_squared_prefecture = spatial_female_classic_hampel_R_squared_prefecture = spatial_female_classic_bisquare_R_squared_prefecture = vector("numeric", 44)

for(ij in 1:44)
{
  y = log(spatial_female_var[,ij], base = 10)
  x = log(spatial_female_mean[,ij], base = 10)
  
  
  OLS_model = lm(y ~ x)
  spatial_female_classic[,ij] = OLS_model$coefficients
  spatial_female_classic_p[,ij] = summary(OLS_model)$coef[,4]
  spatial_female_classic_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  
  hampel_model = rlm(y ~ x, psi = psi.hampel, maxit = 200)
  spatial_female_classic_hampel[,ij] = hampel_model$coefficients
  spatial_female_classic_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  spatial_female_classic_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x, psi = psi.bisquare, maxit = 200)
  spatial_female_classic_bisquare[,ij] = bisquare_model$coefficients
  spatial_female_classic_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  spatial_female_classic_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
}

# 2: benchmark of quadratic OLS TL

spatial_female_quadratic = spatial_female_quadratic_hampel = spatial_female_quadratic_bisquare = matrix(NA, nrow = 3, ncol = 44)
spatial_female_quadratic_p = spatial_female_quadratic_hampel_p = spatial_female_quadratic_bisquare_p = matrix(NA, nrow = 3, ncol = 44)
spatial_female_quadratic_R_squared_prefecture = spatial_female_quadratic_hampel_R_squared_prefecture = spatial_female_quadratic_bisquare_R_squared_prefecture = vector("numeric", 44)

for(ij in 1:44)
{
  y = log(spatial_female_var[,ij], base = 10)
  x = log(spatial_female_mean[,ij], base = 10)
  
  OLS_model = lm(y ~ x + I(x^2))
  spatial_female_quadratic[,ij] = OLS_model$coefficients
  spatial_female_quadratic_p[,ij] = summary(OLS_model)$coef[,4]
  spatial_female_quadratic_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  
  hampel_model = rlm(y ~ x + I(x^2), psi = psi.hampel, maxit = 200)
  spatial_female_quadratic_hampel[,ij] = hampel_model$coefficients
  spatial_female_quadratic_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  spatial_female_quadratic_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x + I(x^2), psi = psi.bisquare, maxit = 200)
  spatial_female_quadratic_bisquare[,ij] = bisquare_model$coefficients
  spatial_female_quadratic_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  spatial_female_quadratic_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
}

# 3: cubic TL

spatial_female_TL_coef = spatial_female_TL_hampel_coef = spatial_female_TL_bisquare_coef = matrix(NA, nrow = 4, ncol = 44)
spatial_female_TL_p = spatial_female_TL_hampel_p = spatial_female_TL_bisquare_p = matrix(NA, nrow = 4, ncol = 44)
spatial_female_TL_R_squared_prefecture = spatial_female_TL_hampel_R_squared_prefecture = spatial_female_TL_bisquare_R_squared_prefecture = vector("numeric", 44)

for(ij in 1:44)
{
  y = log(spatial_female_var[,ij], base = 10)
  x = log(spatial_female_mean[,ij], base = 10)
  
  OLS_model = lm(y ~ x + I(x^2) + I(x^3))
  spatial_female_TL_coef[,ij] = OLS_model$coefficients
  spatial_female_TL_p[,ij] = summary(OLS_model)$coef[,4]
  spatial_female_TL_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  
  hampel_model = rlm(y ~ x + I(x^2) + I(x^3), psi = psi.hampel_95, maxit = 200)
  spatial_female_TL_hampel_coef[,ij] = hampel_model$coefficients
  spatial_female_TL_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  spatial_female_TL_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x + I(x^2) + I(x^3), psi = psi.bisquare, maxit = 200)
  spatial_female_TL_bisquare_coef[,ij] = bisquare_model$coefficients
  spatial_female_TL_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  spatial_female_TL_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
}

## BIC
spatial_female_classic_BIC = spatial_female_quadratic_BIC = spatial_female_cubic_BIC = rep(0, 44)
for(ij in 1:44)
{
  y = log(spatial_female_var[,ij], base = 10)
  x = log(spatial_female_mean[,ij], base = 10)
  
  OLS_model = lm(y ~ x)
  OLS_quadratic_model = lm(y ~ x + I(x^2))
  OLS_cubic_model = lm(y ~ x + I(x^2) + I(x^3))
  
  spatial_female_classic_BIC[ij] = BIC(OLS_model)
  spatial_female_quadratic_BIC[ij] = BIC(OLS_quadratic_model)
  spatial_female_cubic_BIC[ij] = BIC(OLS_cubic_model)
}

# male

# 1: benchmark of the classic OLS TL

spatial_male_classic = spatial_male_classic_hampel = spatial_male_classic_bisquare = matrix(NA, nrow = 2, ncol = 44)
spatial_male_classic_p = spatial_male_classic_hampel_p = spatial_male_classic_bisquare_p = matrix(NA, nrow = 2, ncol = 44)
spatial_male_classic_R_squared_prefecture = spatial_male_classic_hampel_R_squared_prefecture = spatial_male_classic_bisquare_R_squared_prefecture = vector("numeric", 44)

for(ij in 1:44)
{
  y = log(spatial_male_var[,ij], base = 10)
  x = log(spatial_male_mean[,ij], base = 10)
  
  
  OLS_model = lm(y ~ x)
  spatial_male_classic[,ij] = OLS_model$coefficients
  spatial_male_classic_p[,ij] = summary(OLS_model)$coef[,4]
  spatial_male_classic_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  
  hampel_model = rlm(y ~ x, psi = psi.hampel, maxit = 200)
  spatial_male_classic_hampel[,ij] = hampel_model$coefficients
  spatial_male_classic_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  spatial_male_classic_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x, psi = psi.bisquare, maxit = 200)
  spatial_male_classic_bisquare[,ij] = bisquare_model$coefficients
  spatial_male_classic_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  spatial_male_classic_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
}

# 2: benchmark of quadratic OLS TL

spatial_male_quadratic = spatial_male_quadratic_hampel = spatial_male_quadratic_bisquare = matrix(NA, nrow = 3, ncol = 44)
spatial_male_quadratic_p = spatial_male_quadratic_hampel_p = spatial_male_quadratic_bisquare_p = matrix(NA, nrow = 3, ncol = 44)
spatial_male_quadratic_R_squared_prefecture = spatial_male_quadratic_hampel_R_squared_prefecture = spatial_male_quadratic_bisquare_R_squared_prefecture = vector("numeric", 44)

for(ij in 1:44)
{
  y = log(spatial_male_var[,ij], base = 10)
  x = log(spatial_male_mean[,ij], base = 10)
  
  OLS_model = lm(y ~ x + I(x^2))
  spatial_male_quadratic[,ij] = OLS_model$coefficients
  spatial_male_quadratic_p[,ij] = summary(OLS_model)$coef[,4]
  spatial_male_quadratic_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  
  hampel_model = rlm(y ~ x + I(x^2), psi = psi.hampel, maxit = 200)
  spatial_male_quadratic_hampel[,ij] = hampel_model$coefficients
  spatial_male_quadratic_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  spatial_male_quadratic_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x + I(x^2), psi = psi.bisquare, maxit = 200)
  spatial_male_quadratic_bisquare[,ij] = bisquare_model$coefficients
  spatial_male_quadratic_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  spatial_male_quadratic_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
}

# 3: cubic TL

spatial_male_TL_coef = spatial_male_TL_hampel_coef = spatial_male_TL_bisquare_coef = matrix(NA, nrow = 4, ncol = 44)
spatial_male_TL_p = spatial_male_TL_hampel_p = spatial_male_TL_bisquare_p = matrix(NA, nrow = 4, ncol = 44)
spatial_male_TL_R_squared_prefecture = spatial_male_TL_hampel_R_squared_prefecture = spatial_male_TL_bisquare_R_squared_prefecture = vector("numeric", 44)

for(ij in 1:44)
{
  y = log(spatial_male_var[,ij], base = 10)
  x = log(spatial_male_mean[,ij], base = 10)
  
  OLS_model = lm(y ~ x + I(x^2) + I(x^3))
  spatial_male_TL_coef[,ij] = OLS_model$coefficients
  spatial_male_TL_p[,ij] = summary(OLS_model)$coef[,4]
  spatial_male_TL_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  
  hampel_model = rlm(y ~ x + I(x^2) + I(x^3), psi = psi.hampel_95, maxit = 10000)
  spatial_male_TL_hampel_coef[,ij] = hampel_model$coefficients
  spatial_male_TL_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  spatial_male_TL_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x + I(x^2) + I(x^3), psi = psi.bisquare, maxit = 10000)
  spatial_male_TL_bisquare_coef[,ij] = bisquare_model$coefficients
  spatial_male_TL_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  spatial_male_TL_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
}

## BIC
spatial_male_classic_BIC = spatial_male_quadratic_BIC = spatial_male_cubic_BIC = rep(0, 44)
for(ij in 1:44)
{
  y = log(spatial_male_var[,ij], base = 10)
  x = log(spatial_male_mean[,ij], base = 10)
  
  OLS_model = lm(y ~ x)
  OLS_quadratic_model = lm(y ~ x + I(x^2))
  OLS_cubic_model = lm(y ~ x + I(x^2) + I(x^3))
  
  spatial_male_classic_BIC[ij] = BIC(OLS_model)
  spatial_male_quadratic_BIC[ij] = BIC(OLS_quadratic_model)
  spatial_male_cubic_BIC[ij] = BIC(OLS_cubic_model)
}

# total

# 1: benchmark of the classic OLS TL

spatial_total_classic = spatial_total_classic_hampel = spatial_total_classic_bisquare = matrix(NA, nrow = 2, ncol = 44)
spatial_total_classic_p = spatial_total_classic_hampel_p = spatial_total_classic_bisquare_p = matrix(NA, nrow = 2, ncol = 44)
spatial_total_classic_R_squared_prefecture = spatial_total_classic_hampel_R_squared_prefecture = spatial_total_classic_bisquare_R_squared_prefecture = vector("numeric", 44)

for(ij in 1:44)
{
  y = log(spatial_total_var[,ij], base = 10)
  x = log(spatial_total_mean[,ij], base = 10)
  
  
  OLS_model = lm(y ~ x)
  spatial_total_classic[,ij] = OLS_model$coefficients
  spatial_total_classic_p[,ij] = summary(OLS_model)$coef[,4]
  spatial_total_classic_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  
  hampel_model = rlm(y ~ x, psi = psi.hampel, maxit = 200)
  spatial_total_classic_hampel[,ij] = hampel_model$coefficients
  spatial_total_classic_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  spatial_total_classic_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x, psi = psi.bisquare, maxit = 200)
  spatial_total_classic_bisquare[,ij] = bisquare_model$coefficients
  spatial_total_classic_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  spatial_total_classic_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
}

# 2: benchmark of quadratic OLS TL

spatial_total_quadratic = spatial_total_quadratic_hampel = spatial_total_quadratic_bisquare = matrix(NA, nrow = 3, ncol = 44)
spatial_total_quadratic_p = spatial_total_quadratic_hampel_p = spatial_total_quadratic_bisquare_p = matrix(NA, nrow = 3, ncol = 44)
spatial_total_quadratic_R_squared_prefecture = spatial_total_quadratic_hampel_R_squared_prefecture = spatial_total_quadratic_bisquare_R_squared_prefecture = vector("numeric", 44)

for(ij in 1:44)
{
  y = log(spatial_total_var[,ij], base = 10)
  x = log(spatial_total_mean[,ij], base = 10)
  
  OLS_model = lm(y ~ x + I(x^2))
  spatial_total_quadratic[,ij] = OLS_model$coefficients
  spatial_total_quadratic_p[,ij] = summary(OLS_model)$coef[,4]
  spatial_total_quadratic_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  
  hampel_model = rlm(y ~ x + I(x^2), psi = psi.hampel, maxit = 200)
  spatial_total_quadratic_hampel[,ij] = hampel_model$coefficients
  spatial_total_quadratic_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  spatial_total_quadratic_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x + I(x^2), psi = psi.bisquare, maxit = 200)
  spatial_total_quadratic_bisquare[,ij] = bisquare_model$coefficients
  spatial_total_quadratic_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  spatial_total_quadratic_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
}

# 3: cubic TL

spatial_total_TL_coef = spatial_total_TL_hampel_coef = spatial_total_TL_bisquare_coef = matrix(NA, nrow = 4, ncol = 44)
spatial_total_TL_p = spatial_total_TL_hampel_p = spatial_total_TL_bisquare_p = matrix(NA, nrow = 4, ncol = 44)
spatial_total_TL_R_squared_prefecture = spatial_total_TL_hampel_R_squared_prefecture = spatial_total_TL_bisquare_R_squared_prefecture = vector("numeric", 44)

for(ij in 1:44)
{
  y = log(spatial_total_var[,ij], base = 10)
  x = log(spatial_total_mean[,ij], base = 10)
  
  OLS_model = lm(y ~ x + I(x^2) + I(x^3))
  spatial_total_TL_coef[,ij] = OLS_model$coefficients
  spatial_total_TL_p[,ij] = summary(OLS_model)$coef[,4]
  spatial_total_TL_R_squared_prefecture[ij] = summary(OLS_model)$adj.r.squared
  
  hampel_model = rlm(y ~ x + I(x^2) + I(x^3), psi = psi.hampel_95, maxit = 500)
  spatial_total_TL_hampel_coef[,ij] = hampel_model$coefficients
  spatial_total_TL_hampel_p[,ij] = find_p_value(t_values = summary(hampel_model)$coef[,3], df = summary(hampel_model)$df[2])
  spatial_total_TL_hampel_R_squared_prefecture[ij] = 1 - (sum((hampel_model$residuals)^2)/sum((hampel_model$model[,1] - mean(hampel_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
  
  bisquare_model = rlm(y ~ x + I(x^2) + I(x^3), psi = psi.bisquare, maxit = 500)
  spatial_total_TL_bisquare_coef[,ij] = bisquare_model$coefficients
  spatial_total_TL_bisquare_p[,ij] = find_p_value(t_values = summary(bisquare_model)$coef[,3], df = summary(bisquare_model)$df[2])
  spatial_total_TL_bisquare_R_squared_prefecture[ij] = 1 - (sum((bisquare_model$residuals)^2)/sum((bisquare_model$model[,1] - mean(bisquare_model$model[,1]))^2))*(length(y)-1)/(length(y)-4)
}

## BIC
spatial_total_classic_BIC = spatial_total_quadratic_BIC = spatial_total_cubic_BIC = rep(0, 44)
for(ij in 1:44)
{
  y = log(spatial_total_var[,ij], base = 10)
  x = log(spatial_total_mean[,ij], base = 10)
  
  OLS_model = lm(y ~ x)
  OLS_quadratic_model = lm(y ~ x + I(x^2))
  OLS_cubic_model = lm(y ~ x + I(x^2) + I(x^3))
  
  spatial_total_classic_BIC[ij] = BIC(OLS_model)
  spatial_total_quadratic_BIC[ij] = BIC(OLS_quadratic_model)
  spatial_total_cubic_BIC[ij] = BIC(OLS_cubic_model)
}


###############################
# Plots of spatial coeffcients
###############################

time_select = 1975:2018

# female

savepdf("Fig_3b", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(time_select, spatial_female_TL_coef[1,], xlab = "", ylab = "")
points(time_select, spatial_female_TL_hampel_coef[1,], col = 2, pch = 2)
points(time_select, spatial_female_TL_bisquare_coef[1,], col = 4, pch = 4)
dev.off()

savepdf("Fig_3e", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(time_select, spatial_female_TL_coef[2,], xlab = "", ylab = "")
points(time_select, spatial_female_TL_hampel_coef[2,], col = 2, pch = 2)
points(time_select, spatial_female_TL_bisquare_coef[2,], col = 4, pch = 4)
dev.off()

savepdf("Fig_3h", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(time_select, spatial_female_TL_coef[3,], xlab = "", ylab = "")
points(time_select, spatial_female_TL_hampel_coef[3,], col = 2, pch = 2)
points(time_select, spatial_female_TL_bisquare_coef[3,], col = 4, pch = 4)
dev.off()

savepdf("Fig_3k", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(time_select, spatial_female_TL_coef[4,], xlab = "Year", ylab = "", cex.lab = 1.5)
points(time_select, spatial_female_TL_hampel_coef[4,], col = 2, pch = 2)
points(time_select, spatial_female_TL_bisquare_coef[4,], col = 4, pch = 4)
dev.off()

# male

savepdf("Fig_3c", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(time_select, (spatial_male_TL_coef[1,]), xlab = "", ylab = "", ylim = c(-1.7,0.8))
points(time_select, (spatial_male_TL_hampel_coef[1,]), col = 2, pch = 2)
points(time_select, (spatial_male_TL_bisquare_coef[1,]), col = 4, pch = 4)
dev.off()

savepdf("Fig_3f", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(time_select, spatial_male_TL_coef[2,], xlab = "", ylab = "", ylim = c(3, 6.5))
points(time_select, spatial_male_TL_hampel_coef[2,], col = 2, pch = 2)
points(time_select, spatial_male_TL_bisquare_coef[2,], col = 4, pch = 4)
dev.off()

savepdf("Fig_3i", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(time_select, spatial_male_TL_coef[3,], xlab = "", ylab = "", ylim = c(0.6, 2.3))
points(time_select, spatial_male_TL_hampel_coef[3,], col = 2, pch = 2)
points(time_select, spatial_male_TL_bisquare_coef[3,], col = 4, pch = 4)
dev.off()

savepdf("Fig_3l", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(time_select, spatial_male_TL_coef[4,], xlab = "Year", ylab = "", ylim = c(0.06, 0.32), cex.lab = 1.5)
points(time_select, spatial_male_TL_hampel_coef[4,], col = 2, pch = 2)
points(time_select, spatial_male_TL_bisquare_coef[4,], col = 4, pch = 4)
dev.off()

# total

# expression(paste(log[10], "(Spatial Mean) estimates"))

savepdf("Fig_3a", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(time_select, (spatial_total_TL_coef[1,]), xlab = "", ylab = expression(log[10](hat(a[3]))), main = "", cex.lab = 1.5)
points(time_select, (spatial_total_TL_hampel_coef[1,]), col = 2, pch = 2)
points(time_select, (spatial_total_TL_bisquare_coef[1,]), col = 4, pch = 4)
legend("topright", c("OLS", "Robust (Hampel)", "Robust (Bisquare)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 1)
dev.off()

savepdf("Fig_3d", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(time_select, spatial_total_TL_coef[2,], xlab = "", ylab = expression(hat(b)[3]), main = "", ylim = c(2.2, 5.5), cex.lab = 1.5)
points(time_select, spatial_total_TL_hampel_coef[2,], col = 2, pch = 2)
points(time_select, spatial_total_TL_bisquare_coef[2,], col = 4, pch = 4)
# legend("topright", c("OLS", "Robust (Hampel)", "Robust (Bisquare)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 1)
dev.off()

savepdf("Fig_3g", width = 12, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
plot(time_select, spatial_total_TL_coef[3,], xlab = "", ylab = expression(hat(c)[3]), main = "", cex.lab = 1.5)
points(time_select, spatial_total_TL_hampel_coef[3,], col = 2, pch = 2)
points(time_select, spatial_total_TL_bisquare_coef[3,], col = 4, pch = 4)
dev.off()

savepdf("Fig_3j", width = 12, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
plot(time_select, spatial_total_TL_coef[4,], xlab = "Year", ylab = "",  main = "", cex.lab = 1.5)
title(ylab = expression(hat(d)[3]), cex.lab = 1.5)
points(time_select, spatial_total_TL_hampel_coef[4,], col = 2, pch = 2)
points(time_select, spatial_total_TL_bisquare_coef[4,], col = 4, pch = 4)
dev.off()

############################
# Spatial mean and variance
############################

savepdf("female_spatial_variance", width = 14, height = 10, toplines = 0.8)
par(mar = c(3,3,2,2))
matplot(log(spatial_female_var, base = 10), lty = 1, type = "l", col = "grey", xlab = "", ylab = "", ylim = c(-8.5,0))
lines(log(spatial_female_var[,1], base = 10), col = 2, lwd = 2)
lines(log(spatial_female_var[,37], base = 10), col = 3, lwd = 2)
lines(log(spatial_female_var[,44], base = 10), col = 4, lwd = 2)
dev.off()

savepdf("female_spatial_mean", width = 14, height = 10, toplines = 0.8)
par(mar = c(4,3,2,2))
matplot(log(spatial_female_mean, base = 10), lty = 1, type = "l", col = "grey", xlab = "Age", ylab = "", ylim = c(-4.5,0), cex.lab = 2)
lines(log(spatial_female_mean[,1], base = 10), col = 2, lwd = 2)
lines(log(spatial_female_mean[,37], base = 10), col = 3, lwd = 2)
lines(log(spatial_female_mean[,44], base = 10), col = 4, lwd = 2)
dev.off()

savepdf("male_spatial_variance", width = 14, height = 10, toplines = 0.8)
par(mar = c(3,3,2,2))
matplot(log(spatial_male_var, base = 10), lty = 1, type = "l", col = "grey", xlab = "", ylab = "", ylim = c(-8.5,0))
lines(log(spatial_male_var[,1], base = 10), col = 2, lwd = 2)
lines(log(spatial_male_var[,37], base = 10), col = 3, lwd = 2)
lines(log(spatial_male_var[,44], base = 10), col = 4, lwd = 2)
dev.off()

savepdf("male_spatial_mean", width = 14, height = 10, toplines = 0.8)
par(mar = c(4,3,2,2))
matplot(log(spatial_male_mean, base = 10), lty = 1, type = "l", col = "grey", xlab = "Age", ylab = "", ylim = c(-4.5,0), cex.lab = 2)
lines(log(spatial_male_mean[,1], base = 10), col = 2, lwd = 2)
lines(log(spatial_male_mean[,37], base = 10), col = 3, lwd = 2)
lines(log(spatial_male_mean[,44], base = 10), col = 4, lwd = 2)
dev.off()

savepdf("total_spatial_variance", width = 14, height = 10, toplines = 0.8)
par(mar = c(3,4.5,2,2))
matplot(log(spatial_total_var, base = 10), lty = 1, type = "l", col = "grey", xlab = "", ylab = expression(paste(log[10], "(Spatial Variance)")), ylim = c(-8.5,0), cex.lab = 2)
lines(log(spatial_total_var[,1], base = 10), col = 2, lwd = 2)
lines(log(spatial_total_var[,37], base = 10), col = 3, lwd = 2)
lines(log(spatial_total_var[,44], base = 10), col = 4, lwd = 2)
legend("topleft", c("1985","2011","2018"), lty = c(1,1,1), col = c(2,3,4), lwd = c(2,2,2))
dev.off()

savepdf("total_spatial_mean", width = 14, height = 10, toplines = 0.8)
par(mar = c(4,4.5,2,2))
matplot(log(spatial_total_mean, base = 10), lty = 1, type = "l", col = "grey", xlab = "Age", ylab = expression(paste(log[10], "(Spatial Mean)")), ylim = c(-4.5,0), cex.lab = 2)
lines(log(spatial_total_mean[,1], base = 10), col = 2, lwd = 2)
lines(log(spatial_total_mean[,37], base = 10), col = 3, lwd = 2)
lines(log(spatial_total_mean[,44], base = 10), col = 4, lwd = 2)
dev.off()

par(mfrow = c(1,3))
plot(log(spatial_female_mean[,1], base = 10), log(spatial_female_var[,1], base = 10))
points(log(spatial_male_mean[,1], base = 10), log(spatial_male_var[,1], base = 10), col = 2)
plot(log(spatial_female_mean[,37], base = 10), log(spatial_female_var[,37], base = 10))
points(log(spatial_male_mean[,37], base = 10), log(spatial_male_var[,37], base = 10), col = 2)
plot(log(spatial_female_mean[,44], base = 10), log(spatial_female_var[,44], base = 10))
points(log(spatial_male_mean[,44], base = 10), log(spatial_male_var[,44], base = 10), col = 2)