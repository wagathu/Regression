x<-c(2.10, 2.40, 2.50, 3.20, 3.60, 3.80, 4.10, 4.20, 4.50, 5.00)
y<-c(2.18, 2.06, 2.54, 2.61, 3.67, 3.25, 4.02, 3.71, 4.38, 4.45)

plot(x,y,
     pch=20,
     xlab="x values", ylab="y values",
     main="LINEAR REGRESSION",
     xlim=c(2,7),
     ylim=c(2,7))

# b<-sxy/sxx
# a<-y - bx

sxx<-sum(x**2)-(sum(x))**2/length(x)
sxy<-sum(x*y)-(sum(x)*sum(y))/length(x)

b<-sxy/sxx
a<-y - b*x
abline(mean(a), b, col="blue", lty=2, lwd=2) # do mean of alphas 

# show that line passess thru meanx and meany
points(mean(x),mean(y), pch=16, col="red")
legend(6, 3, col="red", "mean(x,y)", pch=20)

lmod<-lm(y~x)
abline(lmod, col="red", lwd=3)

summary(lmod)
