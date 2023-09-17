
# indicator variable coding and its effects on the regression parameters
y <- c(36730, 40650, 46820, 50149, 59679, 67360,51535, 62289, 72486,
       75022,93379,105979)
x1 <- c(5,7,9,10,14,17,5,7,9,10,14,17)
mp <- c(rep(0, 6), rep(1, 6))
fp <- c(rep(1, 6), rep(0, 6))

# male priority coded male as 1, female 0
xmat <- cbind(rep(1, 12), x1, mp)
beta_mp <- solve(t(xmat) %*% xmat) %*% (t(xmat) %*% y)

# female priority coded female as 1, male as 0
xmat <- cbind(rep(1, 12), x1, fp)
beta_fp <- solve(t(xmat) %*% xmat) %*% (t(xmat) %*% y)
