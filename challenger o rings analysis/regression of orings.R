### simple llinear regression

# path
setwd("C:/Users/Admin/Desktop/MISCELLANEOUS R/ml projects/regression/challenger o rings analysis")

# data
orings<-read.csv("orings.csv", as.is=T)
View(orings)

summary(orings)
cor(orings) # correlation
cor(orings, method="pearson") ## pearson's rank
cor(orings, method="spearman") ## spearman's rank

# visualisation
plot(orings$Temperature, orings$Total, xlab="Temperature", 
     ylab="Launch Distress", pch=20, main="TEMPERATURE VERSUS ORING FAILURES", 
     ylim=c(0, 10), xlim=c(0, 100), sub=paste("Th correlation coeficient is:",
                                              cor(orings$Temperature, orings$Total)), 
     font.sub=3, col.sub="red")
grid(10)

# the regression model
reg<-lm(orings$Total ~ orings$Temperature)

# plotting the model
abline(reg, col="blue", lwd=2)


#### multiple linear regression
m_reg <- function(y, x)
{
  x <- as.matrix(x) # converting input to a matrix
  x <- cbind(Intercept = 1, x) # setting beta_naught to 1
  solve(t(x) %*% x) %*% t(x) %*% y
  # solve - inverse || t() - transpose
}

m_reg(y=orings$Total, x=orings$Temperature)

