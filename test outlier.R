ij = 8

age_select = setdiff(age_all, as.numeric(rownames(female_TL_outliers[[ij]])))
y = log(temporal_female_var[age_all %in% age_select,ij], base = 10)
x = log(temporal_female_mean[age_all %in% age_select,ij], base = 10)
test_model_1 = lm(y ~ x + I(x^2) + I(x^3))


y = log(temporal_female_var[,ij], base = 10)
x = log(temporal_female_mean[,ij], base = 10)
test_model_2 = lm(y ~ x + I(x^2) + I(x^3))


y = log(temporal_female_var[-1,ij], base = 10)
x = log(temporal_female_mean[-1,ij], base = 10)
test_model_3 = lm(y ~ x + I(x^2) + I(x^3))

y = log(temporal_female_var[,ij], base = 10)
x = log(temporal_female_mean[,ij], base = 10)
dummy = c(1,rep(0,100))

test_model_4 = lm(y ~ x + I(x^2) + I(x^3) + dummy)
summary(test_model_4)

cbind(test_model_1$coefficients, test_model_2$coefficients, test_model_3$coefficients)

summary(test_model_1)
summary(test_model_2)
summary(test_model_3)

plot(x = log10(temporal_female_mean[,ij]), y = log10(temporal_female_var[,ij]), xlim = c(min(log10(temporal_female_mean[,ij]))-0.2, max(log10(temporal_female_mean[,ij]))+0.2), xlab = expression(paste(log[10], "(Mean)")), ylab = expression(paste(log[10], "(Temporal Variance)")))


lines(x = sort(x), y = (predict(test_model_1))[sort(x,index.return=T)$ix], col = 1, lwd = 1)

lines(x = sort(x), y = (predict(test_model_2))[sort(x,index.return=T)$ix], col = 2, lwd = 1)

lines(x = sort(x), y = (predict(test_model_3))[sort(x,index.return=T)$ix], col = 5, lwd = 1)


abline(h = mean(y))
abline(v = mean(x))







ij = 4

female_TL_outliers[[ij]]

y = log(temporal_female_var[,ij], base = 10)
x = log(temporal_female_mean[,ij], base = 10)

OLS_model = lm(y ~ x + I(x^2) + I(x^3))
summary(OLS_model)

rstudent(OLS_model)[as.numeric(rownames(female_TL_outliers[[ij]]))+1]


lca_model = lca(Japan, series = "female", restype = "rates")


bds.test(lca_model$residuals$y[,"2016"])

test = gls(y ~ x + I(x^2) + I(x^3),  correlation = corARMA(form=~1, p = 2, q = 1))
plot(test, which = 1)
summary(test)

plot(test$residuals)
bds.test(test$residuals)
