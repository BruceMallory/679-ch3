#Problem 3.11
set.seed(1)
x <- rnorm(100)
y <- 2*x + rnorm(100)

par(mfrow=c(1,2))

plot(x, y, xlim=c(-4,4), ylim=c(-4,4))
abline(0, 2, lwd=7, col="grey")
abline(lm(y~x+0), col="red")

plot(y, x, xlim=c(-4,4), ylim=c(-4,4))
abline(0, .5, lwd=7, col="grey")
abline(lm(x~y+0), col="red")


set.seed(1)
x = rnorm(100)
y = 2*x
lm.fit = lm(y~x+0)
lm.fit2 = lm(x~y+0)
summary(lm.fit)
summary(lm.fit2)

set.seed(1)
x <- rnorm(100)
y <- -sample(x, 100)
sum(x^2)
sum(y^2)
lm.fit <- lm(y~x+0)
lm.fit2 <- lm(x~y+0)
summary(lm.fit)
summary(lm.fit2)
# Problem 3.11 walks you through idea that the t-statistic is the same y~x 
# versus x~y.  Cool.  BUT the regression slope coefficient for y=2x+epsilon is
# close to 2, while the slope coefficient for the inverse, x=0.5y - 0.5 epsilon
# is NOT 0.5.  It's closer to 0.4.  AND when I make epsilon smaller 
# ( e.g. y <- 2x + 0.25*random(100) ), the slope coefficient gets closer to 0.5.
# AND finally, I notice that for y~x the epsilon's are vertical, while for x~y
# the epsilon's are horizontal.  And I notice that the epsilon term for the
# inverse function is causing the estimated slope to always be 
# less than 0.5 (never greater).


#Problem 3.13
#(a)
x <- rnorm(100)
#(b)
eps <- rnorm(100, 0, 0.25)
#(c)
Y <- -1 + 0.5 * x + eps
# Y is also 100 long, and beta_0 = -1 and beta_1 = 0.5
#(d)
plot(x, Y)
# From the scatterplot - strongly and linearly corelated with a positive 
# slope of about 1/2.
#(e)
fit.e <- lm(Y ~ x)
summary(fit.e)
# Both coefficients are significant and the F-statistic of the model is 
# really high.  And both beta_hats are quite close to the underlying betas.
#(f)
abline(fit.e)
abline(-1, 0.5, col="red", lty=2)
legend("bottomright", c("sample regression model", "population regression model"),
      lty=1:2, col=c("black","red"))
#(g)
fit.g <- lm(Y~poly(x, 2))
summary(fit.g)
# The coefficient of the squared term in not significant, and the R^2 for the 
# poly model is no different (0.8067 vs. 0.8064) for the linear vs. quadratic model.
#(h)
eps <- rnorm(100, 0, 0.1)
Y <- -1 + 0.5 * x + eps
fit.h <- lm(Y ~ x)
plot(x, Y)
summary(fit.h)
abline(fit.h)
abline(-1, 0.5, col="red", lty=2)
legend("bottomright", c("sample regression model", "population regression model"),
       lty=1:2, col=c("black","red"))
#(i)
eps <- rnorm(100, 0, 0.5)
Y <- -1 + 0.5 * x + eps
fit.i <- lm(Y ~ x)
plot(x, Y)
summary(fit.i)
abline(fit.i)
abline(-1, 0.5, col="red", lty=2)
legend("bottomright", c("sample regression model", "population regression model"),
       lty=1:2, col=c("black","red"))
#(j)
#original
confint(fit.e)
#less noisy
confint(fit.h)
#more noisy
confint(fit.i)
# The confidence intervals get wider as we move from the less-noisy data, to the
# original data, to the more-noisy data.

#Problem 3.14
#(a)
set.seed(1)
x1 <- runif(100)
x2 <- 0.5*x1 + rnorm(100)/10
y <- 2 + 2*x1 + 0.3*x2 + rnorm(100)
# beta_0 = 2, beta_1 = 2, beta_2 = 0.3

#(b)
cor(x1, x2)
plot(x1, x2)
# The correlation (0.83) is positive and strong.

#(c)
fit.c <- lm(y~x1 + x2)
summary(fit.c)
confint(fit.c)
# The model has a low R-squared (0.2088), but a significant F-statistic.
# beta_hat_0 = 2.13, beta_hat_1 = 1.44, beta_hat_2 = 1.01
# beta_hat_1's confidence interval does NOT contain zero so we can reject H_null
# but beta_hat_2's C.I. does contain zero so we can not reject H_null.

#(d)
fit.d <- lm(y~x1)
summary(fit.d)
# Using just the x1 predictor, we also get a significant beta_hat_1 with a p-value
# of 0.000003.  The F-statistic is also twice the size of the F-statistic for the  
# two predictor model.

#(e)
fit.e <- lm(y~x2)
summary(fit.e)
# Yes, using x2 we also get a significant beta_hat (p-value = 0.00001).

#(f)
# On their own, the two predictors show significant effects.  BUT when we put them
# together in a model the second predictor is no longer significant.  That's because
# the two predictors are correlated and the effect of the second predictor is being
# "sucked up" by the first predictor when they are both put in the model.

#(g)
x1.new <- c(x1, 0.1)
x2.new <- c(x2, 0.8)
y.new <- 2 + 2*x1.new + 0.3*x2.new + rnorm(100)
par(mfrow=c(1,3))
plot(x1.new, y.new)
points(x1.new[101],y.new[101], col="red")
plot(x2.new, y.new)
points(x2.new[101],y.new[101], col="red")
plot(x1.new, x2.new)
points(x1.new[101],x2.new[101], col="red")

fit.both <- lm(y.new ~ x1.new + x2.new)
summary(fit.both)
summary(fit.c)
fit.x1 <- lm(y.new ~ x1.new)
summary(fit.x1)
summary(fit.d)
fit.x2 <- lm(y.new ~ x2.new)
summary(fit.x2)
summary(fit.e)

# When you look at the plots, you can see that the new point is an outlier in the x2
# x2 data, but not in the x1 data.  The addition of the new point changes the models
# that use x2 (the both model and the just x2 model).  In both of these cases it is 
# an influential point (as well as being an outlier)