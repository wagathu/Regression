## Leave one out cross validation

lm_loocv <- function(formula, df, predictors)
{
  # formulae is lm formulae
  # df is the dataframe
  
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
  
  train_pred <- vector()
  acc_vec <- vector()
  temp_pred <- vector()
  main_coef <- rep(0, predictors+1)
  avg_coef <- vector()
  
  for (i in 1:nrow(df))
  {
    #print(paste("Instance: ", i))
    temp_mod <- lm(formula, data=df[-i,])
    
    out_model <- outliers_fn(temp_mod, d_f=df[-i, ])
    
    temp_pred[i] <- predict(out_model, df[i,])
    
    # fetching the coefficients
    main_coef <- main_coef+coefficients(out_model)
    avg_coef <- main_coef/i
    
    # The summary
    # summ <- summary.lm(temp_mod)
    # acc_vec[i] <- summ$adj.r.squared
    #rm(temp_mod)
  }
  message("The chosen average estimates are: ")
  print(avg_coef)
  
  # you'll always choose which one you'd want returned
  # between predictions and the intercepts obtained
  return(temp_pred)
  # return(avg_coef)
}

#n <- c("medv", "sexp", "loocv", "kfold")
#f = as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
