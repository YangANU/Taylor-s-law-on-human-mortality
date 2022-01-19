#########################
# Mortality improvement
########################

# convert non-stationary mortality series to stationary mortality improvement series

state_ratio = paste(state, "ratio", sep = "_")
state_smooth_ratio = paste(state_smooth, "ratio", sep = "_")

for(ij in 1:length(state_ratio))
{
  temp_female_ratio = temp_male_ratio = temp_total_ratio = matrix(NA, 101, 42)
  temp_female_smooth_ratio = temp_male_smooth_ratio = temp_total_smooth_ratio = matrix(NA, 101, 42)
  
  for(ik in 1:42)
  {
    temp_female_ratio[,ik] = (get(state[ij])$rate$female[,ik] - get(state[ij])$rate$female[,ik+1])/get(state[ij])$rate$female[,ik]
      
      #1 - get(state[ij])$rate$female[,ik+1]/get(state[ij])$rate$female[,ik]
      
      #2*((get(state[ij])$rate$female[,ik-1] - get(state[ij])$rate$female[,ik])/(get(state[ij])$rate$female[,ik-1] + get(state[ij])$rate$female[,ik]))
    temp_male_ratio[,ik] =  1 - get(state[ij])$rate$male[,ik+1]/get(state[ij])$rate$male[,ik]
      #2*((get(state[ij])$rate$male[,ik-1] - get(state[ij])$rate$male[,ik])/(get(state[ij])$rate$male[,ik-1] + get(state[ij])$rate$male[,ik]))
    temp_total_ratio[,ik] =  1 - get(state[ij])$rate$total[,ik+1]/get(state[ij])$rate$total[,ik]
      #2*((get(state[ij])$rate$total[,ik-1] - get(state[ij])$rate$total[,ik])/(get(state[ij])$rate$total[,ik-1] + get(state[ij])$rate$total[,ik]))
  
    temp_female_smooth_ratio[,ik] = 1 - get(state_smooth[ij])$rate$female[,ik+1]/get(state_smooth[ij])$rate$female[,ik]
      #2*((get(state_smooth[ij])$rate$female[,ik-1] - get(state_smooth[ij])$rate$female[,ik])/(get(state_smooth[ij])$rate$female[,ik-1] + get(state_smooth[ij])$rate$female[,ik]))
    temp_male_smooth_ratio[,ik] = 1 - get(state_smooth[ij])$rate$male[,ik+1]/get(state_smooth[ij])$rate$male[,ik]
      #2*((get(state_smooth[ij])$rate$male[,ik-1] - get(state_smooth[ij])$rate$male[,ik])/(get(state_smooth[ij])$rate$male[,ik-1] + get(state_smooth[ij])$rate$male[,ik]))
    temp_total_smooth_ratio[,ik] = 1 - get(state_smooth[ij])$rate$total[,ik+1]/get(state_smooth[ij])$rate$total[,ik]
      #2*((get(state_smooth[ij])$rate$total[,ik-1] - get(state_smooth[ij])$rate$total[,ik])/(get(state_smooth[ij])$rate$total[,ik-1] + get(state_smooth[ij])$rate$total[,ik]))
  }
  
  nT = length(get(state[ij])$year)
  
  # store mortality improvement series; smooth
  obj_smooth = list(type = "mortality", label = state[ij], lambda = 0)
  obj_smooth$year = get(state_smooth[ij])$year[-nT]
  obj_smooth$age = get(state_smooth[ij])$age
  obj_smooth$pop = get(state_smooth[ij])$pop
  obj_smooth$rate = list()
  obj_smooth$rate[[1]] = temp_female_smooth_ratio
  obj_smooth$rate[[2]] = temp_male_smooth_ratio
  obj_smooth$rate[[3]] = temp_total_smooth_ratio
  for(i in 1:3)
  {
    dimnames(obj_smooth$rate[[i]]) = list(obj_smooth$age, obj_smooth$year)
  }

  names(obj_smooth$pop) = names(obj_smooth$rate) = names(get(state_smooth[ij])$pop)
  assign(state_smooth_ratio[ij], structure(obj_smooth, class = "demogdata"))
  
  # store mortality improvement series; updated raw
  obj = list(type = "mortality", label = state[ij], lambda = 0)
  obj$year = get(state[ij])$year[-nT]
  obj$age = get(state[ij])$age
  obj$pop = get(state[ij])$pop
  obj$rate = list()
  obj$rate[[1]] = temp_female_ratio
  obj$rate[[2]] = temp_male_ratio
  obj$rate[[3]] = temp_total_ratio
  
  # for(i in 1:3)
  # {
  #   dimnames(obj$rate[[i]]) = list(obj$age, obj$year)
  # 
  #   obj$rate[[i]][is.na(obj$rate[[i]])] = obj_smooth$rate[[i]][is.na(obj$rate[[i]])]
  # }
  # 
  names(obj$pop) = names(obj$rate) = names(get(state[ij])$pop)
  assign(state_ratio[ij], structure(obj, class = "demogdata"))
}

#############
# Taylor law
#############

# across prefectures

year_ratio = 1976:2017
n_year_ratio = length(year_ratio)

total_data_ratio = male_data_ratio = female_data_ratio = total_data_smooth_ratio = male_data_smooth_ratio = female_data_smooth_ratio = array(NA, dim = c(n_age, n_year_ratio, n_prefecture+1))
for(ik in 1:(n_prefecture + 1))
{
  female_data_ratio[,,ik] = get(state_ratio[ik])$rate$female
  male_data_ratio[,,ik]   = get(state_ratio[ik])$rate$male
  total_data_ratio[,,ik]  = get(state_ratio[ik])$rate$total
  
  female_data_smooth_ratio[,,ik] = get(state_smooth_ratio[ik])$rate$female
  male_data_smooth_ratio[,,ik]   = get(state_smooth_ratio[ik])$rate$male
  total_data_smooth_ratio[,,ik]  = get(state_smooth_ratio[ik])$rate$total
}

temporal_female_ratio_mean = apply(female_data_ratio[,,2:48], c(1, 3), mean, na.rm = T)
temporal_male_ratio_mean = apply(male_data_ratio[,,2:48], c(1, 3), mean, na.rm = T)
temporal_total_ratio_mean = apply(total_data_ratio[,,2:48], c(1, 3), mean, na.rm = T)

temporal_long_run_female_ratio_var = apply(female_data_ratio[,,2:48], c(1, 3), lrvar, type = "Andrews", na.rm = T)
temporal_long_run_male_ratio_var = apply(male_data_ratio[,,2:48], c(1, 3), lrvar, type = "Andrews")
temporal_long_run_total_ratio_var = apply(total_data_ratio[,,2:48], c(1, 3), lrvar, type = "Andrews")

temporal_long_run_female_ratio_var_2 = matrix(NA, ncol = 101, nrow = 47)
for(ij in 2:48)
{
  temp = long_run_covariance_estimation((female_data_ratio[,,ij]))
  temporal_long_run_female_ratio_var_2[,ij-1] = diag(temp$BT_FT_fix_C0)
}


test_mean = apply(female_data_smooth_ratio[,,1], 1, mean, na.rm = T)
test_var = apply(female_data_smooth_ratio[,,1], 1, var, na.rm = T)
which(test_mean < 0)

plot(log10(test_mean), log10(test_var))
test_model = lm(log10(test_var) ~ log10(test_mean))
summary(test_model)

#########################################
# Taylor's law slope parameter estimates
#########################################

library(MASS)

TL_estimate = function(y, x, method = c("OLS", "Hampel", "Bisquare"))
{
  n = ncol(x)
  result = R_squared = adj_R_squared = rep(0, n)
  for(i in 1:n)
  {
    neg_ind = which(x[,i] < 0)
    if(length(neg_ind) > 0)
    {
      y_no_neg = y[-neg_ind,i]
      x_no_neg = x[-neg_ind,i]
    } else {
      y_no_neg = y[,i]
      x_no_neg = x[,i]
    }
    
    
    if(length(y_no_neg) != length(x_no_neg))
    {
      warning("X and Y have different lengths")
    }
    
    
    if(method == "OLS")
    {
      model = lm(log(y_no_neg, base = 10) ~ log(x_no_neg, base = 10))
      result[i] = coef(model)[2]
      R_squared[i] = summary(model)$r.squared
      adj_R_squared[i] = summary(model)$adj.r.squared
    }
    
    if(method == "Hampel")
    {
      model = rlm(log(y_no_neg, base = 10) ~ log(x_no_neg, base = 10), psi = psi.hampel, maxit = 200)
      result[i] = coef(model)[2]
      R_squared[i] = 1 - sum((model$residuals)^2)/sum((model$model[,1] - mean(model$model[,1]))^2)
      
      
      adj_R_squared[i] = summary(model)$adj.r.squared
    }
    
    if(method == "Bisquare")
    {
      result[i] = rlm(log(y_no_neg, base = 10) ~ log(x_no_neg, base = 10), psi = psi.bisquare, maxit = 200)$coefficients[2]
    }
  }
  return(list(coef = result, R_squared = R_squared, adj_R_squared = adj_R_squared))
}


# female

female_TL_long_run_ratio_prefecture = TL_estimate(y = temporal_long_run_female_ratio_var, x = temporal_female_ratio_mean, method = "OLS")$coef

female_TL_long_run_ratio_prefecture_R_squared = TL_estimate(y = temporal_long_run_female_ratio_var, x = temporal_female_ratio_mean, method = "OLS")$R_squared



female_TL_hampel_long_run_ratio_prefecture = TL_estimate(y = temporal_long_run_female_ratio_var, x = temporal_female_ratio_mean, method = "Hampel")
female_TL_bisquare_long_run_ratio_prefecture = TL_estimate(y = temporal_long_run_female_ratio_var, x = temporal_female_ratio_mean, method = "Bisquare")

savepdf("female_long_run_ratio_temporal", width = 12, height = 10, toplines = 0.8)
plot(1:47, female_TL_long_run_ratio_prefecture, xlab = "Prefecture ordered geographically by North to South", 
     ylab = "Taylor's Law slope parameter estimates", ylim = c(-1.1, 2))
     #,main = expression(paste(log[10], "(temporal ratio long-run variance) versus ", log[10], "(temporal ratio mean)")))
points(1:47, female_TL_hampel_long_run_ratio_prefecture, col = 2, pch = 2)
points(1:47, female_TL_bisquare_long_run_ratio_prefecture, col = 4, pch = 4)
legend("topright", c("LM", "RLM (Hampel weight)", "RLM (Bisquare weight)"), col = c(1, 2, 4), pch = c(1, 2, 4), cex = 1)
dev.off()


# male

male_TL_long_run_ratio_prefecture = TL_estimate(y = temporal_long_run_male_ratio_var, x = temporal_male_ratio_mean, method = "OLS")
male_TL_hampel_long_run_ratio_prefecture = TL_estimate(y = temporal_long_run_male_ratio_var, x = temporal_male_ratio_mean, method = "Hampel")
male_TL_bisquare_long_run_ratio_prefecture = TL_estimate(y = temporal_long_run_male_ratio_var, x = temporal_male_ratio_mean, method = "Bisquare")

savepdf("male_long_run_ratio_temporal", width = 12, height = 10, toplines = 0.8)
plot(1:47, male_TL_long_run_ratio_prefecture, xlab = "Prefecture ordered geographically by North to South", 
     ylab = "Taylor's Law slope parameter estimates", ylim = c(-1.1, 2))
points(1:47, male_TL_hampel_long_run_ratio_prefecture, col = 2, pch = 2)
points(1:47, male_TL_bisquare_long_run_ratio_prefecture, col = 4, pch = 4)
dev.off()

# total

total_TL_long_run_ratio_prefecture = TL_estimate(y = temporal_long_run_total_ratio_var, x = temporal_total_ratio_mean, method = "OLS")
total_TL_hampel_long_run_ratio_prefecture = TL_estimate(y = temporal_long_run_total_ratio_var, x = temporal_total_ratio_mean, method = "Hampel")
total_TL_bisquare_long_run_ratio_prefecture = TL_estimate(y = temporal_long_run_total_ratio_var, x = temporal_total_ratio_mean, method = "Bisquare")

savepdf("total_long_run_ratio_temporal", width = 12, height = 10, toplines = 0.8)
plot(1:47, total_TL_long_run_ratio_prefecture, xlab = "Prefecture ordered geographically by North to South", 
     ylab = "Taylor's Law slope parameter estimates", ylim = c(-1.1, 2))
points(1:47, total_TL_hampel_long_run_ratio_prefecture, col = 2, pch = 2)
points(1:47, total_TL_bisquare_long_run_ratio_prefecture, col = 4, pch = 4)
dev.off()







