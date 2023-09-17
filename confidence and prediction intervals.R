# data
x <- rnorm(100)
y <- (2*x) + rnorm(100)

# model
mod <- lm(y~x)
summary.lm(mod)

# plotting
plot(x, y, pch=20)
abline(mod, col="blue", lwd=2)

# test dataset
testdata <- rnorm(100)
testpred <- predict(mod, data.frame(testdata))

# conf int
pp <- predict(mod, data.frame(testdata), interval = "confidence", level = .95)
points(x=x, y=pp[, 2], col="red", lwd=2, lty=2, type="l") # lower 2.5
points(x=x, y=pp[, 3], col="red", lwd=2, lty=2, type="l") # upper 97.5

# pred int
pp <- predict(mod, data.frame(testdata), interval = "predict", level = .95)
points(x=x, y=pp[, 2], col="green", lwd=2, lty=2, type="l") # lower 2.5
points(x=x, y=pp[, 3], col="green", lwd=2, lty=2, type="l") # upper 97.5
