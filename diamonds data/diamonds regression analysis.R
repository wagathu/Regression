
# using linear regression on the diamonds dataset
# to test various MSE reducing effects

# dir
setwd("C:/Users/stanley/Desktop/MISCELLANEOUS R/ml projects/regression/diamonds data")

# lib
library(pacman)
p_load(ggplot2, dplyr, stringr, stringi, ModelMetrics)

# data
dd <- ggplot2::diamonds
dd <- read.csv("C:\\Users\\owner\\Desktop\\stanley\\regression\\diamonds data\\dddd.csv")
attach(dd)
View(dd)

# shape
dim(dd)

# splitting into train and test
dd_train <- dd[1:43152, -1]
dd_test <- dd[43153:53940, -1]

# str and summary
summary(dd_train)
str(dd_train)

## viz

# carat
ggplot(data=dd_train)+
  geom_histogram(aes(x=carat), bins = 120, col="maroon")+
  labs(x="Weight of the diamonds(carat)",
       title="Distribution of the weight of diamonds")

# cut
ggplot(data=dd_train)+
  geom_bar(aes(x=cut))+
  labs(title="Distribution of Quality of cut", 
       x="Quality of Cut")

# color
ggplot(data=dd_train)+
  geom_bar(aes(x=color))+
  labs(title="Distribution of Diamond color", 
       x="Diamond color")

# clarity
ggplot(data=dd_train)+
  geom_bar(aes(x=clarity))+
  labs(title="Distribution of Clarity", 
       x="Clarity of Diamond")

# depth
ggplot(data=dd_train)+
  geom_histogram(aes(x=depth), bins=60, col="maroon")+
  labs(title="Depth of diamonds", 
       x="Total depth percentage")

# table
ggplot(data=dd_train)+
  geom_histogram(aes(x=table), bins=60, col="maroon")+
  labs(title="Table in diamonds", 
       x="Table width")

# rship to price
# carat and price
ggplot(data=dd_train)+
  geom_point(aes(x=carat, y=price))+
  labs(title="Carat and price distribution", 
       x="Weight of the diamond",
       y="Price of the diamond")

# carat price and cut
ggplot(data=dd_train)+
  geom_point(aes(x=carat, y=price))+
  labs(title="Carat and price distribution", 
       x="Weight of the diamond",
       y="Price of the diamond")+
  facet_grid(.~cut)

ggplot(data=dd_train)+
  geom_boxplot(aes(x=cut, y=price, col=cut), 
               show.legend = F)+
  labs(title="Diamond Cut and price distribution", 
       x="Cut of the diamond",
       y="Price of the diamond")

# carat price and color
ggplot(data=dd_train)+
  geom_point(aes(x=carat, y=price))+
  labs(title="Carat and price distribution", 
       x="Weight of the diamond",
       y="Price of the diamond")+
  facet_grid(.~color)

ggplot(data=dd_train)+
  geom_boxplot(aes(x=color, y=price, col=color), 
               show.legend = F)+
  labs(title="Carat and price distribution", 
       x="Colour of the diamons",
       y="Price of the diamond")

# price, color, cut
ggplot(data=dd_train)+
  geom_boxplot(aes(x=color, y=price, fill=cut), 
               show.legend = T)+
  labs(title="Colour, cut and price distribution", 
       x="Color of the diamons",
       y="Price of the diamond")

# price and depth
ggplot(data=dd_train)+
  geom_point(aes(x=depth, y=price))+
  labs(title="Depth on the price",
       x="Depth")

# cleansing data
sum(is.na(dd_train))

# fitting model
# the full model
mod <- lm(price~.,
          data=dd_train)
summary(mod)

# second model
mod1 <- lm(price~
             carat+cut+color+clarity+depth+table+x, 
           data=dd_train)
summary(mod1)

par(mfrow=c(2,2))
plot(mod1)

pp <- predict(mod, dd_test)
pp1 <- predict(mod1, dd_test)
mse(pp1, dd_test$price)

# corss validation on train dataset
lpred <- lm_loocv(formula = price~carat+cut+color+clarity+depth+table+x,
                  df = dd_train, 
                  predictors = 22)

# splitting data
# using leave one out
loocv <- lm_loocv_test(formula = price~carat+cut+color+clarity+depth+table+x,
                       df_train = dd_train,
                       df_test = dd_test,
                       predictors = 22,
                       return_type = "pred")


# Adding noise using the knn algo -----------------------------------------

# knn for classification type
multisplit_knn <- function(train, test, train_labs, k_value, cat)
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
  #c_name <- names(train[cat])
  
  
  # splitting train and test
  #categorical vars: cut, color, clarity
  nms <- names(train[cat])
  tbl_unique <- unique(train[cat])
  for (i in 1:nrow(tbl_unique))
  {
    split_train[[i]] <- filter(train, 
                               .data[[nms[[1]]]] == tbl_unique[i, 1],
                               .data[[nms[[2]]]] == tbl_unique[i, 2],
                               .data[[nms[[3]]]] == tbl_unique[i, 3]) %>%
      dplyr::select(!all_of(nms))
    
    split_test[[i]] <- filter(test, 
                              .data[[nms[[1]]]] == tbl_unique[i, 1],
                              .data[[nms[[2]]]] == tbl_unique[i, 2],
                              .data[[nms[[3]]]] == tbl_unique[i, 3]) %>%
      dplyr::select(!all_of(nms))
    
    split_labs[[i]] <- train_labs[as.integer(split_train[[i]][, 1])]
  }
  
  #return(split_train)
  # print("check3")
  
  # performing knn
  # this is the part we now insert the knn function
  # (be it classification or regression)
  test_u <- unique(dd_test[(cat-1)])
  require(class)
  
  pred_labs <- list()
  for (i in 1:length(split_test))
  {
    # reducing k to number of rows
    temp_k <- k_value
    temp_k <- ifelse(k_value >= nrow(split_test[[i]]), 
                      yes = nrow(split_test[[i]]),
                      no = k_value)
    
    message(paste("Batch: ", i, "Using inner k as: ", temp_k))
    print("Category: ")
    message(test_u[i, ])
    
    # the continuous knn regression model
    pred_labs[[i]] <- cknn_of(train = split_train[[i]][, -1],
                              test = split_test[[i]][, -1],
                              train_labs = split_labs[[i]],
                              k=temp_k)
  }
  
  final_labs <- unlist(pred_labs)
  
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

knn_regression <- multisplit_knn(train = dd_train[,-7], 
                                 test = dd_test[, -7], 
                                 train_labs = dd_train[,7],
                                 k_value = 1,
                                 cat = 2:4)
# the function stops since not all the classes are distributed equally
# among the test and train batches e.g Fair-G-VVSI and others

knn_regression
