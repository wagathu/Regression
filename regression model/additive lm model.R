additive_lm <- function(formulae, train, test)
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
    
    temp_testrow <- c(test[i,], temp[i])
    names(temp_testrow) <- names(train)
    
    train <- rbind(train, temp_testrow)
    rm(temp_testrow)
  }
  return(temp)
}

nt <- newdd_test[,-4]
addtrainlm <- additive_lm(formulae = `Y house price of unit area`~
              `X2 house age`+`X3 distance to the nearest MRT station`+
              `X4 number of convenience stores`,
            train = newdd_train,
            test = nt)
