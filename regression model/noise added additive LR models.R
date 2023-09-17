
# additive lm models with noise addition

additive_lm <- function(formulae, train, test, kval)
{
  # hold test results
  temp <- vector()
  
  for (i in 1:nrow(test))
  {
    print(paste("Training instances at: ", nrow(train)))
    
    # the model
    regmod <- lm(formula = formulae, data=train)
    
    message(paste("Model fitting test instance: ",i))
    temp[i] <- predict.lm(regmod, test[i,], type = "response")
    
    # adding noise to predicted test instance
    nn_lm <- combine_knnlm(train = train[, -6],
                           test = test[i, ],
                           train_labs = train[, 6],
                           k=kval) %>%
      unlist()
    
    print("check nn_lm")
    message(nn_lm)
    
    res_knnlm <- mean((regmod$residuals)[nn_lm], na.rm=T)
    w_noise <- temp[i] + res_knnlm
    print(w_noise)
    
    print("check end noise adds")
    
    temp[i] <- w_noise
    rm(w_noise)
    
    # continuation
    temp_testrow <- c(test[i, ], temp[i])
    names(temp_testrow) <- names(train)
    
    train <- rbind(train, temp_testrow)
    rm(temp_testrow)
  }
  return(temp)
}

# trying it on real estate dataset
addnoise <- additive_lm(formulae = `Y house price of unit area`~
                          `X2 house age`+`X3 distance to the nearest MRT station`+
                          `X4 number of convenience stores`+`X5 latitude`, 
                        train = newdd_train, 
                        test = newdd_test[100:1, -6], kval = 7)

mse(addnoise, newdd_test[100:1, 6])


# using many values of k
p <- vector()
for (i in 1:20)
{
  print(paste("Using neighbours as: ", i))
  # trying it on real estate dataset
  addnoise <- additive_lm(formulae = `Y house price of unit area`~
                            `X2 house age`+`X3 distance to the nearest MRT station`+
                            `X4 number of convenience stores`+`X5 latitude`, 
                          train = newdd_train, 
                          test = newdd_test[100:1, -6], kval = i)
  
  p[i] <- mse(addnoise, newdd_test[100:1, 6])
}

plot(p, type="o", col="maroon", pch=20)


# plot this after line 235 of the main file please.
# thanks