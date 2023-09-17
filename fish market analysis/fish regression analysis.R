## Regression analysis on fish market data with the dependent variable as
## weight of the fish

# directory
setwd("C:/Users/stanley/Desktop/MISCELLANEOUS R/ml projects/regression/fish market analysis")


# libraries ---------------------------------------------------------------

require(pacman)
p_load(dplyr, stringr, ggplot2, ModelMetrics, gridExtra)


# data and preparation ----------------------------------------------------

fishdd <- read.csv("Fish.csv", as.is=T)

# summary and structure
View(fishdd)
str(fishdd)
names(fishdd) <- c("species", "weight", "length1", "length2", "length3", "height", "width")
summary(fishdd)

# wrangling
fishdd$species <- factor(fishdd$species)

# cheking NA values
sum(is.na(fishdd))

# dividing data into trainig and testing sets
set.seed(33)
rand <- sample(159, 159)
fishdd <- fishdd[rand, ]
rownames(fishdd) <- 1:159

# train <- sample(1:159, 129)
f_train <- fishdd[1:129, ]
f_test <- fishdd[130:159, ]

# cheking class balance
length(unique(f_train$species))
length(unique(f_test$species))


# visualization -----------------------------------------------------------

# fish species
ggplot(data=f_train)+
  geom_bar(aes(x=species))+
  labs(title="Distribution of various species of fish",
       x="Fish species", y="Count")
# weight
ggplot(data=f_train)+
  geom_histogram(aes(x=weight), bins=30, col="maroon")+
  labs(title="Distribution of the weight of fish",
       x="Weight of fish", 
       y="count")

ggplot(data=f_train)+
  geom_boxplot(aes(x=species, y=weight, fill=species), show.legend = F)+
  labs(title="Distribution of the weight of fish among species",
       x="Species of fish", 
       y="Weight of fish")

# lengths
l1 <- ggplot(data=f_train)+
  geom_histogram(aes(x=length1), bins=30, col="red")+
  labs(title="LENGTH(1)",
       x="Length 1")
l2 <- ggplot(data=f_train)+
  geom_histogram(aes(x=length2), bins=30, col="red")+
  labs(title="LENGTH(2)",
       x="Length 2")
l3 <- ggplot(data=f_train)+
  geom_histogram(aes(x=length3), bins=30, col="red")+
  labs(title="LENGTH(3)",
       x="Length 3")

gridExtra::grid.arrange(l1, l2, l3, nrow=1)

# lengths and species
b1 <- ggplot(data=f_train)+
  geom_boxplot(aes(x=species, y=length1, fill=species), show.legend = F)+
  labs(title="LENGTH(1)",
       x="Species")
b2 <- ggplot(data=f_train)+
  geom_boxplot(aes(x=species, y=length2, fill=species), show.legend = F)+
  labs(title="LENGTH(2)",
       x="Species")
b3 <- ggplot(data=f_train)+
  geom_boxplot(aes(x=species, y=length3, fill=species), show.legend = F)+
  labs(title="LENGTH(3)",
       x="Species")
grid.arrange(b1, b2, b3, nrow=1)

# lengths and weight
p1 <- ggplot(data=f_train)+
  geom_point(aes(x=length1, y=weight, col=species), show.legend = F)+
  labs(title="LENGTH(1)",
       x="Length 1")
p2 <- ggplot(data=f_train)+
  geom_point(aes(x=length2, y=weight, col=species), show.legend = F)+
  labs(title="LENGTH(2)",
       x="Length 2")
p3 <- ggplot(data=f_train)+
  geom_point(aes(x=length3, y=weight, col=species))+
  labs(title="LENGTH(3)",
       x="Length 3")
grid.arrange(p1, p2, p3, nrow=1)
# not informative for colour blinds

# height
ggplot(data=f_train)+
  geom_histogram(aes(x=height), bins=30, col="red")+
  labs(title="Distribution of height", 
       x="Height of fish")

# height and species
ggplot(data=f_train)+
  geom_boxplot(aes(x=species, y=height, fill=species), show.legend = F)+
  labs(title="Distribution of height", 
       x="Species",
       y="Height of fish")

# height and weight
ggplot(data=f_train)+
  geom_point(aes(x=height, y=weight))+
  labs(title="Effect of Height on Weight of fish",
       x="Height of fish", y="Weight of fish")
# looks like a quadratic curve
# one can apply square root to get linear rship

# width
ggplot(data=f_train)+
  geom_histogram(aes(x=width), bins=30, col="red")+
  labs(title="Distribution of width", 
       x="width of fish")

# width and species
ggplot(data=f_train)+
  geom_boxplot(aes(x=species, y=width, fill=species), show.legend = F)+
  labs(title="Distribution of width", 
       x="Species",
       y="width of fish")

# width and weight
ggplot(data=f_train)+
  geom_point(aes(x=width, y=weight))+
  labs(title="Effect of width on Weight of fish",
       x="width of fish", y="Weight of fish")
# looks like a quadratic curve
# one can apply square root to get linear rship

# testing the square root transformation
par(mfrow=c(1,2))
plot(f_train$width, f_train$weight, pch=20,
     xlab="Width", ylab="Weight",main="Width::weight")
plot(f_train$width, sqrt(f_train$weight), pch=20,
     xlab="Width", ylab="sqrt::Weight",main="Width::sqrt(weight)")

### end of visual analysis


# start of modellind data -------------------------------------------------


# simple linear model
mod1 <- lm(weight~.,
           data=f_train)
summary(mod1)

mod2 <- lm(weight~length1 + length2 + length3 + height + width,
           data=f_train)
summary(mod2)

mod3 <- lm(sqrt(weight) ~ species + length1 + length2 + length3 + height + width,
           data=f_train)
summary(mod3)

par(mfrow=c(2,2))
plot(mod3)

# using the trained lm mod3 to predit test data
mod3_pred <- predict(mod3, f_test)

# taking back the square root we enforced on data
mod3_pred <- mod3_pred**2

# cheking MSE
mse(mod3_pred, f_test$weight)

# improving mode3 by removing outliers and high leverage points

# outliers removal function from models
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
mod3_out <- outliers_fn(model = mod3, d_f = f_train)
summary(mod3_out)

# using the outlier removed model for prediction
m3out_pred <- predict(mod3_out, f_test)

# we square in order to revert sqrt-ed weight in model
m3out_pred <- m3out_pred**2

# MSE
mse(m3out_pred, f_test$weight)

# removing high leverage points
# the high leverage function
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

mod3_lev <- high_lev(model=mod3, d_f = f_train, max_levpoint = 0.2)
summary(mod3_lev)

# using the model without the high leverage points to predict test
m3lev_pred <- predict(mod3_lev, f_test)

# squaring it ...
m3lev_pred <- m3lev_pred**2

# mse
mse(m3lev_pred, f_test$weight)

# now removing the outliers as well as high leverage points from the model
# consider writing a function for this, however, i tried and results not pleasant
# the model obtained wasnt different from mod3 itself

# using validation on train dataset to predict test dataset
# i will not use kfold since the trainig set is so small

# loocv
loocv_pred <- lm_loocv_test(formula = sqrt(weight) ~ species + length1 + 
                              length2 + length3 + height + width,
                            df_train = f_train,
                            df_test = f_test,
                            return_type = "pred", 
                            outliers.rm = T)

loocv_pred <- loocv_pred**2
mse(loocv_pred, f_test$weight)

# using the knn regression function

# the split function will help split data into subsets based on species
reg_knn <- split_knn(train = f_train[, -2],
                     test = f_test[, -2],
                     train_labs = f_train[, 2],
                     k_value = 5,
                     cat = 1)
reg_knn

# testing MSE
mse(f_test$weight, reg_knn[, 2])
# performing poorly on smaller datasets

# using the add noise function for linear models
lm_nn <- combine_knnlm(train = f_train[, 3:7],
                       test = f_test[, 3:7],
                       train_labs = f_train$weight,
                       k = 10)
lm_nn

pred_nnlm <- vector()
for (i in 1:length(lm_nn))
{
  pred_nnlm[i] <- mod3_pred[i] + sum(mod3$residuals[lm_nn[[i]]])/5
}

mse(pred_nnlm, f_test$weight)

# doing the combine knn lm model using split by classes

# knn for classification type
split_lm_knn <- function(lm_model, train, test, train_labs, k_value, cat)
{
  # libraries
  require(dplyr)
  
  # cleansing the labels
  # train_labs <- factor(train_labs)
  
  # retaining row names
  train <- cbind(rownames(train), train)
  test <- cbind(rownames(test), test)
  cat <- (cat+1)
  
  # print("check1")
  
  # creating split holders
  c <- unique(train[, cat])
  split_train <- list()
  split_test <- list()
  split_labs <- list()
  
  # print("check2")
  
  # splitting and removing the columns with splitter categories
  
  # use dplyr::select to avoid conflict with other functions named select
  c_name <- names(train[cat])
  for (i in 1:length(c))
  {
    split_train[[i]] <- filter(train, train[, cat] == c[i]) %>%
      dplyr::select(!all_of(c_name))
    
    split_test[[i]] <- filter(test, test[, cat] == c[i]) %>%
      dplyr::select(!all_of(c_name))
    
    split_labs[[i]] <- train_labs[as.integer(split_train[[i]][, 1])]
  }
  # return(split_train)
  
  # print("check3")
  
  # performing knn
  # this is the part we now insert the knn function
  # (be it classification or regression)
  
  require(class)
  
  pred_labs <- list()
  for (i in 1:length(split_test))
  {
    # reducing k to number of rows: when k >= nrows(segment)
    temp_k <- k_value
    temp_k <- ifelse(k_value >= nrow(split_train[[i]]), 
                     yes = nrow(split_train[[i]]),
                     no = k_value)
    message(paste("Using inner k as: ", temp_k))
    
    # the continuous knn regression model
    pred_labs[[i]] <- combine_knnlm(train = split_train[[i]][, -1],
                              test = split_test[[i]][, -1],
                              train_labs = split_labs[[i]],
                              k = temp_k)
    rm(temp_k)

  }
  return(pred_labs)
  
  # using the lm model to predict the test dataset
  lm_pred <- predict(lm_model, test)
  
  print("Check 4")
  # obtaining the random noise
  res_knnlm <- vector() # residuals of knn lm
  for (i in 1:length(pred_labs))
  {
    print("Mid check 4")
    res_knnlm[i] <- mean((lm_model$residuals)[pred_labs[[i]]], na.rm=T)
  }
  
  print("check 5")
  
  # adding the random noise
  final_pred <- vector()
  final_pred <- lm_pred + res_knnlm
  
  final_labs <- final_pred
  
  # re arranging rownames of test
  # i had to write a function which behaves like
  # stringi::stri_join_list but for dataframes.
  str_join_df <- function(df)
  {
    # df is a list of dataframes
    l <- length(df)
    final <- data.frame()
    
    for (i in 1:length(df))
    {
      final <- rbind(final, df[[i]])
    }
    return(final)
  }
  
  test_df <- str_join_df(split_test)
  
  # the rownames for the test dataset and labels
  r_names_test <- test_df[, 1] %>%
    as.integer()
  
  final_pred <- cbind(r_names_test, final_labs)
  names(final_pred) <- c("rownames", "pred_labs")
  
  # ordering in order to have proper rownames
  final_pred <- final_pred[order(final_pred[, 1]), ]
  
  
  return(final_pred) 
}

split_knnlm <- split_lm_knn(lm_model = mod3,
                            train = f_train[, -2],
                            test = f_test[, -2],
                            train_labs = f_train[, 2],
                            k_value = 5,
                            cat = 1)

