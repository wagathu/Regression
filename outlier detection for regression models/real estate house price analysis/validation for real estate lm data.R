# k fold cross validation
hlm_kfold <- lm_kfold(formula = `Y house price of unit area`~
                   `X2 house age`+`X3 distance to the nearest MRT station`+
                   `X4 number of convenience stores`, 
                 df=newdd_train, predictors = 3,folds=78,times=50)

# using the k fold model to predict
kfold_coef <- as.matrix(hlm_kfold, ncol=1)

kfold_pred <- (as.matrix(newdd_test[, -4]) %*% kfold_coef[-1,]) + kfold_coef[1,]

# testing mse
mse(newdd_test[,4], kfold_pred)



# trying to loop between various times in k fold --------------------------

span <- 1:100
rec_mse <- vector()
for (i in 1:length(span))
{
  # k fold cross validation
  hlm_kfold <- lm_kfold(formula = `Y house price of unit area`~
                          `X2 house age`+`X3 distance to the nearest MRT station`+
                          `X4 number of convenience stores`, 
                        df=newdd_train, predictors = 3, folds=78,times=span[i])
  
  # using the k fold model to predict
  kfold_coef <- as.matrix(hlm_kfold, ncol=1)
  
  kfold_pred <- (as.matrix(newdd_test[, -4]) %*% kfold_coef[-1,]) + kfold_coef[1,]
  
  # testing mse
  rec_mse[i] <- mse(newdd_test[,4], kfold_pred)
  
}

# leave one out cross validation ------------------------------------------

hlm_loocv <- lm_loocv(formula = `Y house price of unit area`~
                        `X2 house age`+`X3 distance to the nearest MRT station`+
                        `X4 number of convenience stores`, 
                      df=newdd_train, predictors = 3)

loocv_coef <- as.matrix(hlm_loocv, ncol=1)

loocv_pred <- (as.matrix(newdd_test[, -4]) %*% loocv_coef[-1,]) + loocv_coef[1,]

# testing mses
mse(newdd_test[,4], loocv_pred)
lm_pred <- predict.lm(hlm2_levout2, newdd_test, type = "response")

# plotting ----------------------------------------------------------------

plot(rec_mse, type="b", pch=20, axes=F, main="MSE OF K FOLD BATCHES",
     xlab="Times taken by batches in k fold", ylab="Mean square error",
     sub="The red line is the actual MSE|| The purple line is the LOOCV MSE", col.sub="blue")
axis(1, at=1:100, labels=1:100, las=2)
axis(2)
abline(h=c(
  mse(newdd_test[,4], loocv_pred),
  mse(newdd_test[,4], lm_pred)
), 
       col=c("red", "purple"))
