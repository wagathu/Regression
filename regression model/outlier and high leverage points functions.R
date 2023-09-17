
# outlier removal function ------------------------------------------------

outliers_fn <- function(model, d_f, deviations=c(NULL))
{
  deviations <- ifelse(is.null(deviations) == T,
                       yes = 3,
                       no = deviations)
  require(magrittr)
  message(paste("Using deviation :", deviations))
  
  # we normally consider outliers being 3 stdevs away from mean
  model_table <- data.frame(cbind(model$fitted.values,
                                  rstudent(model))) %>%
    filter(
      X2 > deviations | X2 < (-deviations)
    )
  outliers <- rownames(model_table) %>% as.integer()
  
  remout_model <- update(model, data = d_f[-outliers, ])
  return(remout_model)
}


# high leverage points removal --------------------------------------------

high_lev <- function(model, d_f, max_levpoint)
{
  leverage <- hatvalues(model) 
  l <- leverage[order(leverage, decreasing = T)]
  
  # plotting high leverage points and outliers
  plot(leverage, rstudent(model),
       main="Outliers/High leverage", 
       xlab="Leverage", ylab="Studentized residuals")
  
  lev_out <- data.frame(cbind(rstudent(model), leverage))
  names(lev_out) <- c("studentizedresiuals", "leverage")
  
  lev_out2 <- filter(lev_out, leverage > max_levpoint)
  omit <- rownames(lev_out2) %>% as.integer()
  points(lev_out2$leverage, lev_out2$studentizedresiuals, 
         pch=20, col="red")
  
  # improved model
  rem_highlev <- update(model, data = d_f[-omit, ])
  
  return(rem_highlev)
}
