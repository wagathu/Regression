
mlr <- function(response, predictor, df)
{
  require(dplyr)
  
  xmat <- dplyr::select(df, all_of(predictor)) %>%
    as.matrix()
  
  # adding a column of 1's for b0
  xmat <- cbind(rep(1, nrow(xmat)), xmat)
  
  #print(xmat)
  #message(class(xmat))
  
  yvec <- dplyr::select(df, all_of(response)) %>%
    unlist() %>%
    as.numeric()
  
  #print(yvec)
  #message(class(yvec))
  
  # design matrix
  bvec <- solve(t(xmat) %*% xmat) %*% (t(xmat) %*% yvec)
  
  return(bvec)
}

polyreg <- function(y, x, order)
{
  require(dplyr)
  
  xmat <- matrix(NA, nrow=length(x), ncol = order)
  for (i in 1:order)
  {
    xmat[,i] <- x**i
  }
  xmat <- cbind(rep(1, length(x)), # for b0
                xmat)
  #print(xmat)
  # design matrix
  bvec <- solve(t(xmat) %*% xmat) %*% (t(xmat) %*% y)
  
  return(bvec)
}

polyreg_wr <- function(y, x, order, lambda)
{
  require(dplyr)
  
  xmat <- matrix(NA, nrow=length(x), ncol = order)
  for (i in 1:order)
  {
    xmat[,i] <- x**i
  }
  xmat <- cbind(rep(1, length(x)), # for b0
                xmat)
  #print(xmat)
  # design matrix
  bvec <- solve(lambda * diag(order+1) + t(xmat) %*% xmat) %*% t(xmat) %*% y
  return(bvec)
}

plot_poly <- function(model, xval)
{
  yhat <- vector()
  s <- 0
  xval <- seq(min(xval),max(xval),len=50)
  for (i in 1:length(model))
  {
    s <- s + (model[i] * (xval**(i-1)))
  }
  points(xval, s, type="l", col="blue", lwd=2)
  
  return(s)
}



# random data
x <- seq(0,1, length = 10)
set.seed(1)
y <- sin(2*pi*x) + rnorm(n = 10, mean = 0, sd = .1)
plot(x, y, type="p", pch=20)

pmd <- polyreg(y=y, x=x, order=9) %>% as.vector()
yhat <- plot_poly(pmd, x)
points(x, yhat, col="blue", type="l", lwd=2)
points(x, sin(2*pi*x), col="green", lwd=2, type="l")
# order 3: yhat <- pmd[1] + pmd[2]*x + pmd[3]*(x**2) + pmd[4]*(x**3)

pmd <- polyreg_wr(y=y, x=x, order=9, lambda = exp(-18)) %>% as.vector()
yhat <- plot_poly(pmd, x)

yy <- plot_poly(pmd, x)

# illustrating the polynomial case with/without regularizer

# data
x <- c(1,2,3,5,7)
y <- c(3,5,1,12,10)

par(mfrow=c(1,2))
plot(x, y, type="p", pch=20, ylim=c(1,20))

# without regularizer
pmd <- polyreg(y=y, x=x, order=4) %>% as.vector()
yhat <- plot_poly(pmd, x)

plot(x, y, type="p", pch=20, ylim=c(1,20))

# with regularizer
pmd <- polyreg_wr(y=y,x=x, order = 1, lambda = -4069.21) 
yhat <- plot_poly(pmd, x)


x <- 1:100
y <- (2*x)+6+rnorm(mean=0, sd=13, n=100)
plot(x, y, pch=20)

# with regularizer
pmd <- polyreg_wr(y=y,x=x, order = 9, lambda = exp(-18)) 
yh <- pred_poly(pmd, x)
mse(yh, y) # 0.002756901
yhat <- plot_poly(pmd, x)



sx <- .1111111
sy <- .73554456
e <- vector()
l <- c(.05)
p <- vector()

for (i in 1:1000)
{
  pmd <- polyreg_wr(y=y,x=x, order = 9, lambda = (l[i])) 
  p <- pred_poly(pmd, x)
  e[i] <- mse(y, p)
  l[1+i] <- l[i] - (1/2)*(sum((pmd[-1,])**2))
  
  
}

