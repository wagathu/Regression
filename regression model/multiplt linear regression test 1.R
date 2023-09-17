linearmodel<-function(y, x_vec, include.plots=c(T, F))
{
  # stopifnot(is.null(df$b)==T && is.null(df$a)==T)
  #x<-as.numeric(x)
  y<-as.numeric(y)
  # x are explanatory variables
  # y is an independent variable
  
  sxx <- vector()
  sxy <- vector()
  for (i in 1:ncol(x_vec))
  {
    sxx[i] <- (sum(x_vec[,i]**2)-(sum(x_vec[,i]))**2)/length(x_vec[,i])
    sxy[i]<-(sum(x_vec[,i]*y)-(sum(x_vec[,i])*sum(y)))/length(x_vec[,i])
  }
  syy<-(sum(y**2)-(sum(y))**2)/length(y)
  
  message("sxx is: ")
  print(sxx)
  message("sxy is: ")
  print(sxy)
  message("syy is: ")
  print(syy)
  
  
  bb<-sxy/sxx
  aa <- list()
  for (i in 1:ncol(x_vec))
  {
    aa[[i]] <- mean(y)-bb*mean(x_vec[,i])
  }
  message("bb then aa: ")
  print(bb)
  print(aa)
  # aa<-mean(y)-bb*mean(x)
  
  # residual sum of squares
  ssres<-syy-(sxy**2/sxx)
  message("ssres is: ")
  print(ssres)
  
  # total sum of squares
  sstot=syy
  message("sstot is: ")
  print(sstot)
  
  # regression sum of squares
  ssreg<-sstot-ssres
  message("ssreg is: ")
  print(ssreg)
  
  
  
  # print("Summary of residuals: ")
  # summary(res)
  # sres<-paste(print("Summary of residuals: ", summary(res)))
}

carmileage<-c(2.10, 2.40, 2.50, 3.20, 3.60, 3.80, 4.10, 4.20, 4.50, 5.00)
set.seed(27); mpg <- runif(10, min=32.37, max=54.7)
claimamount<-c(2.18, 2.06, 2.54, 2.61, 3.67, 3.25, 4.02, 3.71, 4.38, 4.45)

xdata <- data.frame(cbind(carmileage, mpg))

linearmodel(y = claimamount, x_vec = xdata, include.plots = F)
