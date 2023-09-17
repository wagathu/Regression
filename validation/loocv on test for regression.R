## Leave one out cross validation

lm_loocv_test <- function(formula, df_train, df_test,
                          return_type=c("coef", "pred"),
                          outliers.rm = c(TRUE, FALSE))
{
  # formulae is lm formulae
  # df is the dataframe
  # there was once a parameter called predictors
  # when revising previous analyses please omit it: 4/2/2021
  
  require(dplyr) # for pipes, and case_when
  
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
  
  full_pred <- rep(0, nrow(df_test))
  temp_pred <- vector()
  df_pred <- data.frame()
  df_pred[1:nrow(df_test), 1] <- 1:nrow(df_test)
  
  acc_vec <- vector()
  
  # initially i set it:   main_coef <- rep(0, predictors+1)
  predictors <- lm(formula = formula, data = df_train) %>%
    coefficients() %>%
    length()
  
  main_coef <- rep(0, predictors)
  avg_coef <- vector()
  
  for (i in 1:nrow(df_train))
  {
    message(paste("Instance omitted: ", i))
    temp_mod <- lm(formula, data=df_train[-i,])
    out_model <- outliers_fn(temp_mod, d_f=df_train[-i, ])
    
    temp_pred <- case_when(
      outliers.rm == T ~ predict.lm(out_model, df_test, type = "response"),
      TRUE ~ predict.lm(temp_mod, df_test, type = "response")
    )
    #temp_pred <- predict.lm(out_model, df_test, type = "response")
    
    df_pred[1:nrow(df_test), (i+1)] <- temp_pred
    
    #full_pred <- (full_pred + temp_pred)/i
    
    
    # fetching the coefficients
    main_coef <- case_when(
      outliers.rm == T ~ main_coef+coefficients(out_model),
      TRUE ~ main_coef+coefficients(temp_mod)
    )
    # main_coef <- main_coef+coefficients(out_model)
    avg_coef <- main_coef/i
    

    rm(temp_pred)
    rm(temp_mod)
    rm(out_model)
  }
  # obtaining names of coefficients
  names(avg_coef) <- lm(formula = formula, data = df_train) %>%
    coefficients() %>% names()
  
  message("The chosen average estimates are: ")
  print(avg_coef)
  
  # obtaining averaged predictions
  df_pred <- df_pred[, -1]
  full_pred <- rowMeans(df_pred, na.rm=T)
  rm(df_pred)
  
  # you'll always choose which one you'd want returned
  # between predictions and the intercepts obtained
  # return(temp_pred)
  #return(full_pred)
  if (return_type == "coef")
  {
    return(avg_coef)
  }
  else
  {
    return(full_pred)
  }
}
