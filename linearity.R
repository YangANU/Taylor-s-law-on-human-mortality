#####################
# Check of linearity
#####################
library(qpcR)
library(asbio)
library(MASS)

# Tokyo (13)

# temporal
tokyo_temporal_OLS_female = tokyo_temporal_OLS_male = matrix(NA, nrow = 10, ncol = 5)
colnames(tokyo_temporal_OLS_female) = colnames(tokyo_temporal_OLS_male) = c("AICc", "BIC", "Adj_R_sqaured", "Pred_R_squared", "Num_significant")
for(i in 1:10)
{
  OLS_female = lm(log(temporal_female_var[,14], base = 10) ~ poly(log(temporal_female_mean[,14], base = 10), i))
  tokyo_temporal_OLS_female[i, "AICc"] = AICc(OLS_female)
  tokyo_temporal_OLS_female[i, "BIC"] = BIC(OLS_female)
  tokyo_temporal_OLS_female[i, "Adj_R_sqaured"] = summary(OLS_female)$adj.r.squared
  tokyo_temporal_OLS_female[i, "Pred_R_squared"] = press(OLS_female, as.R2 = TRUE)
  tokyo_temporal_OLS_female[i, "Num_significant"] = sum(summary(OLS_female)$coef[-1,4] < 0.05)
  
  OLS_male = lm(log(temporal_male_var[,14], base = 10) ~ poly(log(temporal_male_mean[,14], base = 10), i))
  tokyo_temporal_OLS_male[i, "AICc"] = AICc(OLS_male)
  tokyo_temporal_OLS_male[i, "BIC"] = BIC(OLS_male)
  tokyo_temporal_OLS_male[i, "Adj_R_sqaured"] = summary(OLS_male)$adj.r.squared
  tokyo_temporal_OLS_male[i, "Pred_R_squared"] = press(OLS_male, as.R2 = TRUE)
  tokyo_temporal_OLS_male[i, "Num_significant"] = sum(summary(OLS_male)$coef[-1,4] < 0.05)
}

stargazer(cbind(1:10, tokyo_temporal_OLS_female, tokyo_temporal_OLS_male))

tokyo_test = cbind(log(temporal_male_var[,14], base = 10), log(temporal_male_mean[,14], base = 10))
rownames(tokyo_test) = 0:100
tokyo_test_mean = colMeans(tokyo_test)
tokyo_test_cov = cov(tokyo_test)
tokyo_mahalanobis = mahalanobis(x = tokyo_test, center = tokyo_test_mean, cov = tokyo_test_cov)

toyp_test_outliers = tokyo_test[tokyo_mahalanobis > qchisq(p = 0.95, df = 2),]

model = lm(log(temporal_male_var[, 14], base = 10) ~ poly(log(temporal_male_mean[,14], base = 10), 4))
summary(model)

model = lm(log(temporal_male_var[-as.numeric(rownames(toyp_test_outliers))-1, 14], base = 10) ~ poly(log(temporal_male_mean[-as.numeric(rownames(toyp_test_outliers))-1,14], base = 10), 4))
summary(model)

plot(model, which = 1)
plot(model, which = 2)
plot(model, which = 4)


model = lm(log(temporal_female_var[, 14], base = 10) ~ poly(log(temporal_female_mean[,14], base = 10), 1))

plot(x = model$fitted.values, y = rstandard(model), ylab = "Standardized residuals", xlab = "Fitted values", main = "Standardized Residauls vs Fitted")
abline(h = 0, lty = 2)
plot(model, which = 2)

cooksd <- cooks.distance(model)
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])
head(tokyo_test[influential,])

tokyo_temporal_hampel_female = tokyo_temporal_hampel_male = matrix(NA, nrow = 10, ncol = 5)
colnames(tokyo_temporal_hampel_female) = colnames(tokyo_temporal_hampel_male) = c("AICc", "BIC", "Adj_R_sqaured", "Pred_R_squared", "Num_significant")
for(i in 1:10)
{
  hampel_female = rlm(log(temporal_female_var[,14], base = 10) ~ poly(log(temporal_female_mean[,14], base = 10), i), psi = psi.hampel, maxit = 200)
  tokyo_temporal_hampel_female[i, "AICc"] = AICc(hampel_female)
  tokyo_temporal_hampel_female[i, "BIC"] = BIC(hampel_female)
  tokyo_temporal_hampel_female[i, "Adj_R_sqaured"] = 1 - sum((hampel_female$residuals)^2)/sum((hampel_female$model[,1] - mean(hampel_female$model[,1]))^2)
  tokyo_temporal_hampel_female[i, "Pred_R_squared"] = press(hampel_female, as.R2 = TRUE)
  tokyo_temporal_hampel_female[i, "Num_significant"] = sum(summary(hampel_female)$coef[-1,3] < qt(0.025, summary(hampel_female)$df[2])) + sum(summary(hampel_female)$coef[-1,3] > qt(0.975, summary(hampel_female)$df[2]))
  
  hampel_male = rlm(log(temporal_male_var[,14], base = 10) ~ poly(log(temporal_male_mean[,14], base = 10), i), psi = psi.hampel, maxit = 200)
  tokyo_temporal_hampel_male[i, "AICc"] = AICc(hampel_male)
  tokyo_temporal_hampel_male[i, "BIC"] = BIC(hampel_male)
  tokyo_temporal_hampel_male[i, "Adj_R_sqaured"] = 1 - sum((hampel_male$residuals)^2)/sum((hampel_male$model[,1] - mean(hampel_male$model[,1]))^2)
  tokyo_temporal_hampel_male[i, "Pred_R_squared"] = press(hampel_male, as.R2 = TRUE)
  tokyo_temporal_hampel_male[i, "Num_significant"] = sum(summary(hampel_male)$coef[-1,3] < qt(0.025, summary(hampel_male)$df[2])) + sum(summary(hampel_male)$coef[-1,3] > qt(0.975, summary(hampel_male)$df[2]))
}

tokyo_temporal_bisquare_female = tokyo_temporal_bisquare_male = matrix(NA, nrow = 10, ncol = 5)
colnames(tokyo_temporal_bisquare_female) = colnames(tokyo_temporal_bisquare_male) = c("AICc", "BIC", "Adj_R_sqaured", "Pred_R_squared", "Num_significant")
for(i in 1:10)
{
  bisquare_female = rlm(log(temporal_female_var[,14], base = 10) ~ poly(log(temporal_female_mean[,14], base = 10), i), psi = psi.bisquare, maxit = 200)
  tokyo_temporal_bisquare_female[i, "AICc"] = AICc(bisquare_female)
  tokyo_temporal_bisquare_female[i, "BIC"] = BIC(bisquare_female)
  tokyo_temporal_bisquare_female[i, "Adj_R_sqaured"] = 1 - sum((bisquare_female$residuals)^2)/sum((bisquare_female$model[,1] - mean(bisquare_female$model[,1]))^2)
  tokyo_temporal_bisquare_female[i, "Pred_R_squared"] = press(bisquare_female, as.R2 = TRUE)
  tokyo_temporal_bisquare_female[i, "Num_significant"] = sum(summary(bisquare_female)$coef[-1,3] < qt(0.025, summary(bisquare_female)$df[2])) + sum(summary(bisquare_female)$coef[-1,3] > qt(0.975, summary(bisquare_female)$df[2]))
  
  bisquare_male = rlm(log(temporal_male_var[,14], base = 10) ~ poly(log(temporal_male_mean[,14], base = 10), i), psi = psi.bisquare, maxit = 200)
  tokyo_temporal_bisquare_male[i, "AICc"] = AICc(bisquare_male)
  tokyo_temporal_bisquare_male[i, "BIC"] = BIC(bisquare_male)
  tokyo_temporal_bisquare_male[i, "Adj_R_sqaured"] = 1 - sum((bisquare_male$residuals)^2)/sum((bisquare_male$model[,1] - mean(bisquare_male$model[,1]))^2)
  tokyo_temporal_bisquare_male[i, "Pred_R_squared"] = press(bisquare_male, as.R2 = TRUE)
  tokyo_temporal_bisquare_male[i, "Num_significant"] = sum(summary(bisquare_male)$coef[-1,3] < qt(0.025, summary(bisquare_male)$df[2])) + sum(summary(bisquare_male)$coef[-1,3] > qt(0.975, summary(bisquare_male)$df[2]))
}

savepdf("Resid_tokyo_OLS_female_p1", width = 24, height = 10, toplines = 0.8)
par(mfrow = c(1,2))
model = lm(log(temporal_female_var[,14], base = 10) ~ poly(log(temporal_female_mean[,14], base = 10), 1))
plot(x = model$fitted.values, y = rstandard(model), ylab = "Standardized residuals", xlab = "Fitted values", main = "Standardized Residauls vs Fitted")
abline(h = 0, lty = 2)
plot(model, which = 2)
dev.off()

savepdf("Resid_tokyo_OLS_female_p2", width = 24, height = 10, toplines = 0.8)
par(mfrow = c(1,2))
model = lm(log(temporal_female_var[,14], base = 10) ~ poly(log(temporal_female_mean[,14], base = 10), 2))
plot(x = model$fitted.values, y = rstandard(model), ylab = "Standardized residuals", xlab = "Fitted values", main = "Standardized Residauls vs Fitted")
abline(h = 0, lty = 2)
plot(model, which = 2)
dev.off()

savepdf("Resid_tokyo_OLS_female_p7", width = 24, height = 10, toplines = 0.8)
par(mfrow = c(1,2))
model = lm(log(temporal_female_var[,14], base = 10) ~ poly(log(temporal_female_mean[,14], base = 10), 7))
plot(x = model$fitted.values, y = rstandard(model), ylab = "Standardized residuals", xlab = "Fitted values", main = "Standardized Residauls vs Fitted")
abline(h = 0, lty = 2)
plot(model, which = 2)
dev.off()


savepdf("Resid_tokyo_hampel_male_p1", width = 12, height = 10, toplines = 0.8)
par(mfrow = c(1,2))
model = rlm(log(temporal_male_var[,14], base = 10) ~ poly(log(temporal_male_mean[,14], base = 10), 1), psi = psi.hampel, maxit = 200)
plot(x = model$fitted.values, y = model$residuals/summary(model)$sigma/sqrt(1-hatvalues(model)), ylab = "Standardized residuals", xlab = "Fitted values", main = "Standardized Residauls vs Fitted")
abline(h = 0, lty = 2)
plot(model, which = 2)
dev.off()

savepdf("Resid_tokyo_hampel_male_p2", width = 12, height = 10, toplines = 0.8)
par(mfrow = c(1,2))
model = rlm(log(temporal_male_var[,14], base = 10) ~ poly(log(temporal_male_mean[,14], base = 10), 2), psi = psi.hampel, maxit = 200)
plot(x = model$fitted.values, y = model$residuals/summary(model)$sigma/sqrt(1-hatvalues(model)), ylab = "Standardized residuals", xlab = "Fitted values", main = "Standardized Residauls vs Fitted")
abline(h = 0, lty = 2)
plot(model, which = 2)
dev.off()

savepdf("Resid_tokyo_hampel_male_p7", width = 12, height = 10, toplines = 0.8)
par(mfrow = c(1,2))
model = rlm(log(temporal_male_var[,14], base = 10) ~ poly(log(temporal_male_mean[,14], base = 10), 7), psi = psi.hampel, maxit = 200)
plot(x = model$fitted.values, y = model$residuals/summary(model)$sigma/sqrt(1-hatvalues(model)), ylab = "Standardized residuals", xlab = "Fitted values", main = "Standardized Residauls vs Fitted")
abline(h = 0, lty = 2)
plot(model, which = 2)
dev.off()


savepdf("scatter_tokyo_OLS_female_p1", width = 24, height = 10, toplines = 0.8)
model = lm(log(temporal_female_var[,14], base = 10) ~ poly(log(temporal_female_mean[,14], base = 10), 1))
plot(log10(temporal_female_mean[,14]), log10(temporal_female_var[,14]), ylab = "Variance (log10)", xlab = "Mean (log10)", main = "Order = 1")
lines(sort(log10(temporal_female_mean[,14])), model$fitted.values[as.numeric(names(sort(log10(temporal_female_mean[,14]))))+1], lwd = 2, col = 2)
dev.off()

savepdf("scatter_tokyo_OLS_female_p2", width = 24, height = 10, toplines = 0.8)
model = lm(log(temporal_female_var[,14], base = 10) ~ poly(log(temporal_female_mean[,14], base = 10), 2))
plot(log10(temporal_female_mean[,14]), log10(temporal_female_var[,14]), ylab = "Variance (log10)", xlab = "Mean (log10)", main = "Order = 2")
lines(sort(log10(temporal_female_mean[,14])), model$fitted.values[as.numeric(names(sort(log10(temporal_female_mean[,14]))))+1], lwd = 2, col = 2)
dev.off()

savepdf("scatter_tokyo_OLS_female_p7", width = 24, height = 10, toplines = 0.8)
model = lm(log(temporal_female_var[,14], base = 10) ~ poly(log(temporal_female_mean[,14], base = 10), 7))
plot(log10(temporal_female_mean[,14]), log10(temporal_female_var[,14]), ylab = "Variance (log10)", xlab = "Mean (log10)", main = "Order = 7")
lines(sort(log10(temporal_female_mean[,14])), model$fitted.values[as.numeric(names(sort(log10(temporal_female_mean[,14]))))+1], lwd = 2, col = 2)
dev.off()

savepdf("scatter_tokyo_OLS_male", width = 12, height = 10, toplines = 0.8)
par(mfrow=c(1,3))
model = lm(log(temporal_male_var[,14], base = 10) ~ poly(log(temporal_male_mean[,14], base = 10), 1))
plot(log10(temporal_male_mean[,14]), log10(temporal_male_var[,14]), ylab = "Variance (log10)", xlab = "Mean (log10)", main = "Order = 1")
lines(sort(log10(temporal_male_mean[,14])), model$fitted.values[as.numeric(names(sort(log10(temporal_male_mean[,14]))))+1], lwd = 2, col = 2)

model = lm(log(temporal_male_var[,14], base = 10) ~ poly(log(temporal_male_mean[,14], base = 10), 2))
plot(log10(temporal_male_mean[,14]), log10(temporal_male_var[,14]), ylab = "Variance (log10)", xlab = "Mean (log10)", main = "Order = 2")
lines(sort(log10(temporal_male_mean[,14])), model$fitted.values[as.numeric(names(sort(log10(temporal_male_mean[,14]))))+1], lwd = 2, col = 2)

model = lm(log(temporal_male_var[,14], base = 10) ~ poly(log(temporal_male_mean[,14], base = 10), 7))
plot(log10(temporal_male_mean[,14]), log10(temporal_male_var[,14]), ylab = "Variance (log10)", xlab = "Mean (log10)", main = "Order = 7")
lines(sort(log10(temporal_male_mean[,14])), model$fitted.values[as.numeric(names(sort(log10(temporal_male_mean[,14]))))+1], lwd = 2, col = 2)
dev.off()

# spatial
tokyo_spatial_OLS_female = tokyo_spatial_OLS_male = matrix(NA, nrow = 10, ncol = 5)
colnames(tokyo_spatial_OLS_female) = colnames(tokyo_spatial_OLS_male) = c("AICc", "BIC", "Adj_R_sqaured", "Pred_R_squared", "Num_significant")
for(i in 1:10)
{
  OLS_female = lm(log(spatial_female_var[,14], base = 10) ~ poly(log(spatial_female_mean[,14], base = 10), i))
  tokyo_spatial_OLS_female[i, "AICc"] = AICc(OLS_female)
  tokyo_spatial_OLS_female[i, "BIC"] = BIC(OLS_female)
  tokyo_spatial_OLS_female[i, "Adj_R_sqaured"] = summary(OLS_female)$adj.r.squared
  tokyo_spatial_OLS_female[i, "Pred_R_squared"] = press(OLS_female, as.R2 = TRUE)
  tokyo_spatial_OLS_female[i, "Num_significant"] = sum(summary(OLS_female)$coef[-1,4] < 0.05)
  
  OLS_male = lm(log(spatial_male_var[,14], base = 10) ~ poly(log(spatial_male_mean[,14], base = 10), i))
  tokyo_spatial_OLS_male[i, "AICc"] = AICc(OLS_male)
  tokyo_spatial_OLS_male[i, "BIC"] = BIC(OLS_male)
  tokyo_spatial_OLS_male[i, "Adj_R_sqaured"] = summary(OLS_male)$adj.r.squared
  tokyo_spatial_OLS_male[i, "Pred_R_squared"] = press(OLS_male, as.R2 = TRUE)
  tokyo_spatial_OLS_male[i, "Num_significant"] = sum(summary(OLS_male)$coef[-1,4] < 0.05)
}

tokyo_spatial_hampel_female = tokyo_spatial_hampel_male = matrix(NA, nrow = 10, ncol = 5)
colnames(tokyo_spatial_hampel_female) = colnames(tokyo_spatial_hampel_male) = c("AICc", "BIC", "Adj_R_sqaured", "Pred_R_squared", "Num_significant")
for(i in 1:10)
{
  hampel_female = rlm(log(spatial_female_var[,14], base = 10) ~ poly(log(spatial_female_mean[,14], base = 10), i), psi = psi.hampel, maxit = 200)
  tokyo_spatial_hampel_female[i, "AICc"] = AICc(hampel_female)
  tokyo_spatial_hampel_female[i, "BIC"] = BIC(hampel_female)
  tokyo_spatial_hampel_female[i, "Adj_R_sqaured"] = 1 - sum((hampel_female$residuals)^2)/sum((hampel_female$model[,1] - mean(hampel_female$model[,1]))^2)
  tokyo_spatial_hampel_female[i, "Pred_R_squared"] = press(hampel_female, as.R2 = TRUE)
  tokyo_spatial_hampel_female[i, "Num_significant"] = sum(summary(hampel_female)$coef[-1,3] < qt(0.025, summary(hampel_female)$df[2])) + sum(summary(hampel_female)$coef[-1,3] > qt(0.975, summary(hampel_female)$df[2]))
  
  hampel_male = rlm(log(spatial_male_var[,14], base = 10) ~ poly(log(spatial_male_mean[,14], base = 10), i), psi = psi.hampel, maxit = 200)
  tokyo_spatial_hampel_male[i, "AICc"] = AICc(hampel_male)
  tokyo_spatial_hampel_male[i, "BIC"] = BIC(hampel_male)
  tokyo_spatial_hampel_male[i, "Adj_R_sqaured"] = 1 - sum((hampel_male$residuals)^2)/sum((hampel_male$model[,1] - mean(hampel_male$model[,1]))^2)
  tokyo_spatial_hampel_male[i, "Pred_R_squared"] = press(hampel_male, as.R2 = TRUE)
  tokyo_spatial_hampel_male[i, "Num_significant"] = sum(summary(hampel_male)$coef[-1,3] < qt(0.025, summary(hampel_male)$df[2])) + sum(summary(hampel_male)$coef[-1,3] > qt(0.975, summary(hampel_male)$df[2]))
}

tokyo_spatial_bisquare_female = tokyo_spatial_bisquare_male = matrix(NA, nrow = 10, ncol = 5)
colnames(tokyo_spatial_bisquare_female) = colnames(tokyo_spatial_bisquare_male) = c("AICc", "BIC", "Adj_R_sqaured", "Pred_R_squared", "Num_significant")
for(i in 1:10)
{
  bisquare_female = rlm(log(spatial_female_var[,14], base = 10) ~ poly(log(spatial_female_mean[,14], base = 10), i), psi = psi.bisquare, maxit = 200)
  tokyo_spatial_bisquare_female[i, "AICc"] = AICc(bisquare_female)
  tokyo_spatial_bisquare_female[i, "BIC"] = BIC(bisquare_female)
  tokyo_spatial_bisquare_female[i, "Adj_R_sqaured"] = 1 - sum((bisquare_female$residuals)^2)/sum((bisquare_female$model[,1] - mean(bisquare_female$model[,1]))^2)
  tokyo_spatial_bisquare_female[i, "Pred_R_squared"] = press(bisquare_female, as.R2 = TRUE)
  tokyo_spatial_bisquare_female[i, "Num_significant"] = sum(summary(bisquare_female)$coef[-1,3] < qt(0.025, summary(bisquare_female)$df[2])) + sum(summary(bisquare_female)$coef[-1,3] > qt(0.975, summary(bisquare_female)$df[2]))
  
  bisquare_male = rlm(log(spatial_male_var[,14], base = 10) ~ poly(log(spatial_male_mean[,14], base = 10), i), psi = psi.bisquare, maxit = 200)
  tokyo_spatial_bisquare_male[i, "AICc"] = AICc(bisquare_male)
  tokyo_spatial_bisquare_male[i, "BIC"] = BIC(bisquare_male)
  tokyo_spatial_bisquare_male[i, "Adj_R_sqaured"] = 1 - sum((bisquare_male$residuals)^2)/sum((bisquare_male$model[,1] - mean(bisquare_male$model[,1]))^2)
  tokyo_spatial_bisquare_male[i, "Pred_R_squared"] = press(bisquare_male, as.R2 = TRUE)
  tokyo_spatial_bisquare_male[i, "Num_significant"] = sum(summary(bisquare_male)$coef[-1,3] < qt(0.025, summary(bisquare_male)$df[2])) + sum(summary(bisquare_male)$coef[-1,3] > qt(0.975, summary(bisquare_male)$df[2]))
}

savepdf("Resid_tokyo_OLS_female_p1_spatial", width = 12, height = 10, toplines = 0.8)
par(mfrow = c(1,2))
model = lm(log(spatial_female_var[,14], base = 10) ~ poly(log(spatial_female_mean[,14], base = 10), 1))
plot(x = model$fitted.values, y = rstandard(model), ylab = "Standardized residuals", xlab = "Fitted values", main = "Standardized Residauls vs Fitted")
abline(h = 0, lty = 2)
plot(model, which = 2)
dev.off()

savepdf("Resid_tokyo_OLS_female_p2_spatial", width = 12, height = 10, toplines = 0.8)
par(mfrow = c(1,2))
model = lm(log(spatial_female_var[,14], base = 10) ~ poly(log(spatial_female_mean[,14], base = 10), 2))
plot(x = model$fitted.values, y = rstandard(model), ylab = "Standardized residuals", xlab = "Fitted values", main = "Standardized Residauls vs Fitted")
abline(h = 0, lty = 2)
plot(model, which = 2)
dev.off()

savepdf("Resid_tokyo_OLS_female_p7_spatial", width = 12, height = 10, toplines = 0.8)
par(mfrow = c(1,2))
model = lm(log(spatial_female_var[,14], base = 10) ~ poly(log(spatial_female_mean[,14], base = 10), 7))
plot(x = model$fitted.values, y = rstandard(model), ylab = "Standardized residuals", xlab = "Fitted values", main = "Standardized Residauls vs Fitted")
abline(h = 0, lty = 2)
plot(model, which = 2)
dev.off()


savepdf("Resid_tokyo_hampel_male_p1_spatial", width = 12, height = 10, toplines = 0.8)
par(mfrow = c(1,2))
model = rlm(log(spatial_male_var[,14], base = 10) ~ poly(log(spatial_male_mean[,14], base = 10), 1), psi = psi.hampel, maxit = 200)
plot(x = model$fitted.values, y = model$residuals/summary(model)$sigma/sqrt(1-hatvalues(model)), ylab = "Standardized residuals", xlab = "Fitted values", main = "Standardized Residauls vs Fitted")
abline(h = 0, lty = 2)
plot(model, which = 2)
dev.off()

savepdf("Resid_tokyo_hampel_male_p2_spatial", width = 12, height = 10, toplines = 0.8)
par(mfrow = c(1,2))
model = rlm(log(spatial_male_var[,14], base = 10) ~ poly(log(spatial_male_mean[,14], base = 10), 2), psi = psi.hampel, maxit = 200)
plot(x = model$fitted.values, y = model$residuals/summary(model)$sigma/sqrt(1-hatvalues(model)), ylab = "Standardized residuals", xlab = "Fitted values", main = "Standardized Residauls vs Fitted")
abline(h = 0, lty = 2)
plot(model, which = 2)
dev.off()

savepdf("Resid_tokyo_hampel_male_p7_spatial", width = 12, height = 10, toplines = 0.8)
par(mfrow = c(1,2))
model = rlm(log(spatial_male_var[,14], base = 10) ~ poly(log(spatial_male_mean[,14], base = 10), 7), psi = psi.hampel, maxit = 200)
plot(x = model$fitted.values, y = model$residuals/summary(model)$sigma/sqrt(1-hatvalues(model)), ylab = "Standardized residuals", xlab = "Fitted values", main = "Standardized Residauls vs Fitted")
abline(h = 0, lty = 2)
plot(model, which = 2)
dev.off()


savepdf("scatter_tokyo_OLS_female", width = 12, height = 10, toplines = 0.8)
par(mfrow=c(1,3))
model = lm(log(spatial_female_var[,14], base = 10) ~ poly(log(spatial_female_mean[,14], base = 10), 1))
plot(log10(spatial_female_mean[,14]), log10(spatial_female_var[,14]), ylab = "Variance (log10)", xlab = "Mean (log10)", main = "Order = 1")
lines(sort(log10(spatial_female_mean[,14])), model$fitted.values[as.numeric(names(sort(log10(spatial_female_mean[,14]))))+1], lwd = 2, col = 2)

model = lm(log(spatial_female_var[,14], base = 10) ~ poly(log(spatial_female_mean[,14], base = 10), 2))
plot(log10(spatial_female_mean[,14]), log10(spatial_female_var[,14]), ylab = "Variance (log10)", xlab = "Mean (log10)", main = "Order = 2")
lines(sort(log10(spatial_female_mean[,14])), model$fitted.values[as.numeric(names(sort(log10(spatial_female_mean[,14]))))+1], lwd = 2, col = 2)

model = lm(log(spatial_female_var[,14], base = 10) ~ poly(log(spatial_female_mean[,14], base = 10), 7))
plot(log10(spatial_female_mean[,14]), log10(spatial_female_var[,14]), ylab = "Variance (log10)", xlab = "Mean (log10)", main = "Order = 7")
lines(sort(log10(spatial_female_mean[,14])), model$fitted.values[as.numeric(names(sort(log10(spatial_female_mean[,14]))))+1], lwd = 2, col = 2)
dev.off()

savepdf("scatter_tokyo_OLS_male", width = 12, height = 10, toplines = 0.8)
par(mfrow=c(1,3))
model = lm(log(spatial_male_var[,14], base = 10) ~ poly(log(spatial_male_mean[,14], base = 10), 1))
plot(log10(spatial_male_mean[,14]), log10(spatial_male_var[,14]), ylab = "Variance (log10)", xlab = "Mean (log10)", main = "Order = 1")
lines(sort(log10(spatial_male_mean[,14])), model$fitted.values[as.numeric(names(sort(log10(spatial_male_mean[,14]))))+1], lwd = 2, col = 2)

model = lm(log(spatial_male_var[,14], base = 10) ~ poly(log(spatial_male_mean[,14], base = 10), 2))
plot(log10(spatial_male_mean[,14]), log10(spatial_male_var[,14]), ylab = "Variance (log10)", xlab = "Mean (log10)", main = "Order = 2")
lines(sort(log10(spatial_male_mean[,14])), model$fitted.values[as.numeric(names(sort(log10(spatial_male_mean[,14]))))+1], lwd = 2, col = 2)

model = lm(log(spatial_male_var[,14], base = 10) ~ poly(log(spatial_male_mean[,14], base = 10), 7))
plot(log10(spatial_male_mean[,14]), log10(spatial_male_var[,14]), ylab = "Variance (log10)", xlab = "Mean (log10)", main = "Order = 7")
lines(sort(log10(spatial_male_mean[,14])), model$fitted.values[as.numeric(names(sort(log10(spatial_male_mean[,14]))))+1], lwd = 2, col = 2)
dev.off()

#########################
# 47*3 temporal TL plots
#########################

# female
for(ij in 1:47)
{
  age_select = setdiff(age_all, as.numeric(rownames(female_TL_outliers[[ij]])))
  y = log(temporal_female_var[age_all %in% age_select,ij], base = 10)
  x = log(temporal_female_mean[age_all %in% age_select,ij], base = 10)
  
  OLS_model = lm(y ~ x + I(x^2) + I(x^3))
  newdata = data.frame(x = seq(from = min(x)-0.1, to = max(x)+0.2, by = 0.01))
  pred_conf = predict(OLS_model, newdata, interval = "confidence")
  pred_pred = predict(OLS_model, newdata, interval = "predict")

  # savepdf(paste("fitted", ij, "female", sep = "_"), width = 20, height = 20, toplines = 0.8)
  # par(mar = c(5,5,5,3))
  # plot(x = log10(temporal_female_mean[,ij]), y = log10(temporal_female_var[,ij]), xlim = c(min(log10(temporal_female_mean[,ij]))-0.2, max(log10(temporal_female_mean[,ij]))+0.2), xlab = expression(paste(log[10], "(Mean)")), ylab = expression(paste(log[10], "(Variance)")), cex.lab = 2, cex.axis = 2, col = rainbow(101, end = 0.9))
  # if(nrow(female_TL_outliers[[ij]]) > 0)
  # {
  #   points(female_TL_outliers[[ij]][,1], female_TL_outliers[[ij]][,2], pch = 21, bg = 3:(nrow(female_TL_outliers[[ij]])+2), col = "black", cex = 1.5)
  #   legend("topleft", col = 3:(nrow(female_TL_outliers[[ij]])+2), pch = 16, legend = rownames(female_TL_outliers[[ij]]), cex = 2)
  # }
  # title(main = state[ij+1], line = 1, cex.main = 2, font.main= 2)
  # lines(x = newdata$x, y = pred_conf[,1], col = 2, lwd = 2)
  # dev.off()
  # 
  
  # savepdf(paste("confidence", ij, "female", sep = "_"), width = 20, height = 20, toplines = 0.8)
  # par(mar = c(5,5,5,3))
  # plot(x = log10(temporal_female_mean[,ij]), y = log10(temporal_female_var[,ij]), xlim = c(min(log10(temporal_female_mean[,ij]))-0.2, max(log10(temporal_female_mean[,ij]))+0.2), xlab = expression(paste(log[10], "(Mean)")), ylab = expression(paste(log[10], "(Variance)")))
  # if(nrow(female_TL_outliers[[ij]]) > 0)
  # {
  #   points(x = female_TL_outliers[[ij]][,1], y = female_TL_outliers[[ij]][,2], pch = 21, bg = 3:(nrow(female_TL_outliers[[ij]])+2), col = "black")
  #   legend("topleft", col = 3:(nrow(female_TL_outliers[[ij]])+2), pch = 16, legend = rownames(female_TL_outliers[[ij]]))
  # }
  # title(main = state[ij+1], line = 1, cex.main = 2, font.main= 2)
  # lines(x = newdata$x, y = pred_conf[,1], col = 2, lwd = 2)
  # polygon(c(rev(newdata$x), newdata$x), c(rev(pred_conf[,3]), pred_conf[,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
  # dev.off()
  
  savepdf(paste("prediction", ij, "female", sep = "_"), width = 20, height = 20, toplines = 0.8)
  par(mar = c(5,7,5,3))
  plot(x = log10(temporal_female_mean[,ij]), y = log10(temporal_female_var[,ij]), xlim = c(min(log10(temporal_female_mean[,ij]))-0.2, max(log10(temporal_female_mean[,ij]))+0.2), xlab = expression(paste(log[10], "(Mean)")), ylab = expression(paste(log[10], "(Variance)")), cex.lab = 2, cex.axis = 2, col = rainbow(101, end = 0.9))
  if(nrow(female_TL_outliers[[ij]]) > 0)
  {
    points(female_TL_outliers[[ij]][,1], female_TL_outliers[[ij]][,2], pch = 21, bg = 3:(nrow(female_TL_outliers[[ij]])+2), col = "black", cex = 2)
    legend("topleft", col = 3:(nrow(female_TL_outliers[[ij]])+2), pch = 16, legend = rownames(female_TL_outliers[[ij]]), cex = 2)
  }
  title(main = state[ij+1], line = 1, cex.main = 3, font.main= 2)
  lines(x = newdata$x, y = pred_pred[,1], col = 2, lwd = 2)
  polygon(c(rev(newdata$x), newdata$x), c(rev(pred_pred[,3]), pred_pred[,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
  dev.off()
}

# male
for(ij in 1:47)
{
  age_select = setdiff(age_all, as.numeric(rownames(male_TL_outliers[[ij]])))
  y = log(temporal_male_var[age_all %in% age_select,ij], base = 10)
  x = log(temporal_male_mean[age_all %in% age_select,ij], base = 10)
  
  OLS_model = lm(y ~ x + I(x^2) + I(x^3))
  newdata = data.frame(x = seq(from = min(x, na.rm = TRUE)-0.1, to = max(x, na.rm = TRUE)+0.2, by = 0.01))
  pred_conf = predict(OLS_model, newdata, interval = "confidence")
  pred_pred = predict(OLS_model, newdata, interval = "predict")
  # 
  # savepdf(paste("fitted", ij, "male", sep = "_"), width = 20, height = 20, toplines = 0.8)
  # par(mar = c(5,5,5,3))
  # plot(x = log10(temporal_male_mean[,ij]), y = log10(temporal_male_var[,ij]), xlim = c(min(log10(temporal_male_mean[,ij]), na.rm = TRUE)-0.2, max(log10(temporal_male_mean[,ij]), na.rm = TRUE)+0.2), xlab = expression(paste(log[10], "(Mean)")), ylab = expression(paste(log[10], "(Variance)")))
  # if(nrow(male_TL_outliers[[ij]]) > 0)
  # {
  #   points(male_TL_outliers[[ij]][,1], male_TL_outliers[[ij]][,2], pch = 21, bg = 3:(nrow(male_TL_outliers[[ij]])+2), col = "black")
  #   legend("topleft", col = 3:(nrow(male_TL_outliers[[ij]])+2), pch = 16, legend = rownames(male_TL_outliers[[ij]]))
  # }
  # title(main = state[ij+1], line = 1, cex.main = 2, font.main= 2)
  # lines(x = newdata$x, y = pred_conf[,1], col = 2, lwd = 2)
  # dev.off()
  # 
  # 
  # savepdf(paste("confidence", ij, "male", sep = "_"), width = 20, height = 20, toplines = 0.8)
  # par(mar = c(5,5,5,3))
  # plot(x = log10(temporal_male_mean[,ij]), y = log10(temporal_male_var[,ij]), xlim = c(min(log10(temporal_male_mean[,ij]), na.rm = TRUE)-0.2, max(log10(temporal_male_mean[,ij]), na.rm = TRUE)+0.2), xlab = expression(paste(log[10], "(Mean)")), ylab = expression(paste(log[10], "(Variance)")))
  # if(nrow(male_TL_outliers[[ij]]) > 0)
  # {
  #   points(x = male_TL_outliers[[ij]][,1], y = male_TL_outliers[[ij]][,2], pch = 21, bg = 3:(nrow(male_TL_outliers[[ij]])+2), col = "black")
  #   legend("topleft", col = 3:(nrow(male_TL_outliers[[ij]])+2), pch = 16, legend = rownames(male_TL_outliers[[ij]]))
  # }
  # title(main = state[ij+1], line = 1, cex.main = 2, font.main= 2)
  # lines(x = newdata$x, y = pred_conf[,1], col = 2, lwd = 2)
  # polygon(c(rev(newdata$x), newdata$x), c(rev(pred_conf[,3]), pred_conf[,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
  # dev.off()
   
  savepdf(paste("prediction", ij, "male", sep = "_"), width = 20, height = 20, toplines = 0.8)
  par(mar = c(5,7,5,3))
  plot(x = log10(temporal_male_mean[,ij]), y = log10(temporal_male_var[,ij]), xlim = c(min(log10(temporal_male_mean[,ij]), na.rm = T)-0.2, max(log10(temporal_male_mean[,ij]), na.rm = T)+0.2), xlab = expression(paste(log[10], "(Mean)")), ylab = expression(paste(log[10], "(Variance)")), cex.lab = 2, cex.axis = 2, col = rainbow(101, end = 0.9))
  if(nrow(male_TL_outliers[[ij]]) > 0)
  {
    points(male_TL_outliers[[ij]][,1], male_TL_outliers[[ij]][,2], pch = 21, bg = 3:(nrow(male_TL_outliers[[ij]])+2), col = "black", cex = 2)
    legend("topleft", col = 3:(nrow(male_TL_outliers[[ij]])+2), pch = 16, legend = rownames(male_TL_outliers[[ij]]), cex = 2)
  }
  title(main = state[ij+1], line = 1, cex.main = 3, font.main= 2)
  lines(x = newdata$x, y = pred_pred[,1], col = 2, lwd = 2)
  polygon(c(rev(newdata$x), newdata$x), c(rev(pred_pred[,3]), pred_pred[,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
  dev.off()
}

# total
for(ij in 1:47)
{
  age_select = setdiff(age_all, as.numeric(rownames(total_TL_outliers[[ij]])))
  y = log(temporal_total_var[age_all %in% age_select,ij], base = 10)
  x = log(temporal_total_mean[age_all %in% age_select,ij], base = 10)
  
  OLS_model = lm(y ~ x + I(x^2) + I(x^3))
  newdata = data.frame(x = seq(from = min(x)-0.1, to = max(x)+0.2, by = 0.01))
  pred_conf = predict(OLS_model, newdata, interval = "confidence")
  pred_pred = predict(OLS_model, newdata, interval = "predict")
  
  # savepdf(paste("fitted", ij, "total", sep = "_"), width = 20, height = 20, toplines = 0.8)
  # par(mar = c(5,5,5,3))
  # plot(x = log10(temporal_total_mean[,ij]), y = log10(temporal_total_var[,ij]), xlim = c(min(log10(temporal_total_mean[,ij]))-0.2, max(log10(temporal_total_mean[,ij]))+0.2), xlab = expression(paste(log[10], "(Mean)")), ylab = expression(paste(log[10], "(Variance)")))
  # if(nrow(total_TL_outliers[[ij]]) > 0)
  # {
  #   points(total_TL_outliers[[ij]][,1], total_TL_outliers[[ij]][,2], pch = 21, bg = 3:(nrow(total_TL_outliers[[ij]])+2), col = "black")
  #   legend("topleft", col = 3:(nrow(total_TL_outliers[[ij]])+2), pch = 16, legend = rownames(total_TL_outliers[[ij]]))
  # }
  # title(main = state[ij+1], line = 1, cex.main = 2, font.main= 2)
  # lines(x = newdata$x, y = pred_conf[,1], col = 2, lwd = 2)
  # dev.off()
  # 
  # 
  # savepdf(paste("confidence", ij, "total", sep = "_"), width = 20, height = 20, toplines = 0.8)
  # par(mar = c(5,5,5,3))
  # plot(x = log10(temporal_total_mean[,ij]), y = log10(temporal_total_var[,ij]), xlim = c(min(log10(temporal_total_mean[,ij]))-0.2, max(log10(temporal_total_mean[,ij]))+0.2), xlab = expression(paste(log[10], "(Mean)")), ylab = expression(paste(log[10], "(Variance)")))
  # if(nrow(total_TL_outliers[[ij]]) > 0)
  # {
  #   points(x = total_TL_outliers[[ij]][,1], y = total_TL_outliers[[ij]][,2], pch = 21, bg = 3:(nrow(total_TL_outliers[[ij]])+2), col = "black")
  #   legend("topleft", col = 3:(nrow(total_TL_outliers[[ij]])+2), pch = 16, legend = rownames(total_TL_outliers[[ij]]))
  # }
  # title(main = state[ij+1], line = 1, cex.main = 2, font.main= 2)
  # lines(x = newdata$x, y = pred_conf[,1], col = 2, lwd = 2)
  # polygon(c(rev(newdata$x), newdata$x), c(rev(pred_conf[,3]), pred_conf[,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
  # dev.off()
  
  savepdf(paste("prediction", ij, "total", sep = "_"), width = 20, height = 20, toplines = 0.8)
  par(mar = c(5,7,5,3))
  plot(x = log10(temporal_total_mean[,ij]), y = log10(temporal_total_var[,ij]), xlim = c(min(log10(temporal_total_mean[,ij]))-0.2, max(log10(temporal_total_mean[,ij]))+0.2), xlab = expression(paste(log[10], "(Mean)")), ylab = expression(paste(log[10], "(Variance)")), cex.lab = 2, cex.axis = 2, col = rainbow(101, end = 0.9))
  if(nrow(total_TL_outliers[[ij]]) > 0)
  {
    points(total_TL_outliers[[ij]][,1], total_TL_outliers[[ij]][,2], pch = 21, bg = 3:(nrow(total_TL_outliers[[ij]])+2), col = "black", cex = 2)
    legend("topleft", col = 3:(nrow(total_TL_outliers[[ij]])+2), pch = 16, legend = rownames(total_TL_outliers[[ij]]), cex = 2)
  }
  title(main = state[ij+1], line = 1, cex.main = 3, font.main= 2)
  lines(x = newdata$x, y = pred_pred[,1], col = 2, lwd = 2)
  polygon(c(rev(newdata$x), newdata$x), c(rev(pred_pred[,3]), pred_pred[,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
  dev.off()
}





















