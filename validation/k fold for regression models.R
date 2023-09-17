lm_kfold <- function(formula, df, predictors, folds, times)
{
  # formulae is lm formulae
  # df is the data frame
  # folds are number of instances to omit in validation
  # times-the number of times to validate
  
  # outliers removal function from models
  outliers_fn <- function(model, d_f)
  {
    require(magrittr)
    model_table <- data.frame(cbind(model$fitted.values,
                                    rstudent(model))) %>%
      filter(
        X2 > 3 | X2 < (-3)
      )
    outliers <- rownames(model_table) %>% as.integer()
    
    remout_model <- update(model, data = d_f[-outliers, ])
    return(remout_model)
  }
  
  # prediction vectors
  temp_pred <- vector()
  full_pred <- vector()
  
  # intercept vectors
  main_coef <- rep(0, predictors+1)
  avg_coef <- vector()
  
  for (i in 1:times)
  {
    # preparing data
    rand <- sample(1:nrow(df), folds)
    train <- df[-rand,]
    test <- df[rand,]
    #print(paste("nrow of train:",nrow(train)," nrow of test",nrow(test)))
    # model
    message(paste("Running Batch Number: ", i))
    temp_mod <- lm(formula, data=train)
    out_model <- outliers_fn(model = temp_mod, d_f = train)
    
    temp_pred <- predict(out_model, data=test)
    
    full_pred <- c(full_pred, temp_pred)
    
    # recording the intercepts
    main_coef <- main_coef+coefficients(out_model)
    avg_coef <- main_coef/i
    
    # clearing vectors
    rm(rand)
    rm(temp_pred)
    rm(train)
    rm(test)
  }
  
  print("Coefficients:")
  print(avg_coef)
  
  print(paste("length of the full prediction is: ",length(full_pred)))

  #return(full_pred)
  return(avg_coef)
}


