linearmodel<-function(y,x, include.plots=c(T, F))
{
  # stopifnot(is.null(df$b)==T && is.null(df$a)==T)
  x<-as.numeric(x)
  y<-as.numeric(y)
  # x is an explanatory variable
  # y is an independent variable
  
  sxx<-sum(x**2)-(sum(x))**2/length(x)
  sxy<-sum(x*y)-(sum(x)*sum(y))/length(x)
  syy<-sum(y**2)-(sum(y))**2/length(y)
  
  bb<-sxy/sxx
  aa<-mean(y)-bb*mean(x)
  
  # residual sum of squares
  ssres<-syy-(sxy**2/sxx)
  
  # total sum of squares
  sstot=syy
  
  # regression sum of squares
  ssreg<-sstot-ssres
  
  p0<-message("Sum of Squares statistics: ")
  divider0<-message("=========================================")
  p1<-message("Total sum of squares: ", sstot)
  p2<-message("Residual sum of squares: ", round(ssres, 4))
  p3<-message("Regression sum of squares: ",round(ssreg, 4))
  divider<-message("=========================================")
  p4<-message("Coefficient(intercept): ", round(aa, 4), sep=" ", collapse="")
  p5<-message("Coefficient x: ", round(bb, 4), sep=" ", collapse="")
  
  pred<-((bb*x)+aa) # pedicted values
  res<-pred-y
  
  
  # print("Summary of residuals: ")
  # summary(res)
  # sres<-paste(print("Summary of residuals: ", summary(res)))
  
  if(include.plots==T)
  {
    # plot area setting
    par(mfrow=c(1,4)) 
    # the data plot
    plot(x,y, ylab="Independent variable",xlab="Explanatory variable",
         main="Data plot")
    # normal qq plot
    qqnorm(res)
    qqline(res, col="blue")
    # residual plot
    plot(res, main="Residual Plot", ylab="Residuals", ylim=c(-1.5,1.5))
    abline(h=0, lwd=2, col="red")
    # residual vs explanatory variable plot
    plot(x,res, ylab="Residuals", xlab="Explanatory Variable(x)", 
         main="Residual vs Explanatory variable plot")
  }
  else{}

  return(c(p0,divider0,p1,p2,p3,divider,p4,p5))
}
  
# a small example to illustrate its working
carmileage<-c(2.10, 2.40, 2.50, 3.20, 3.60, 3.80, 4.10, 4.20, 4.50, 5.00)
claimamount<-c(2.18, 2.06, 2.54, 2.61, 3.67, 3.25, 4.02, 3.71, 4.38, 4.45)

linearmodel(claimamount,carmileage, include.plots = T)

# comparing its working with lm
lm(claimamount~carmileage)
