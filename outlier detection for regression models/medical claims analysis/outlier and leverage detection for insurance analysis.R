
# packages/libraries ------------------------------------------------------


library(pacman)
p_load(ggplot2, gganimate, plotly, dplyr, e1071, ModelMetrics)


# directory
setwd("C:/Users/stanley/Desktop/MISCELLANEOUS R/ml projects/regression/medical claims analysis")

# reading data
ins<-read.csv("insurance.csv", stringsAsFactors = T)
View(ins)

# data summary
summary(ins)
str(ins)

# cheking NA
sum(is.na(ins))

# age act
agecat<-function(age)
{
  if (age<=27)
  {return("Young")}
  else if(age>=28&&age<=39)
  {return("Prime")}
  else if(age>=40&&age<=51)
  {return("Middle")}
  else{return("Old")}
}
category<-character()
for (i in 1:nrow(ins))
{
  category[i]<-agecat(ins[i,1])
}
ins$category<-category

# age dist
# 19 - late twenties - young adults
# middle starts at 45 , then old
# seniors 65 and above


# adding non linear rships
ins$age2 <- ins$age^2

# transformation
# numeric to binary
ins$bmi_above30 <- ifelse(ins$bmi >= 30, 1, 0)
# here we are investigating whether only obese pple affect their own fees

# bmi above 30 and smoker at the same time
for (i in 1:nrow(ins))
{
  if (ins[i,3]>=30 &&
      ins[i,5]=="yes")
  {
    ins[i,11] <- 1
  }
  else
  {
    ins[i,11] <- 0
  }
}

colnames(ins) <- c("age", "sex", "bmi", "children", "smoker", "region", "charges", 
                   "category", "age2", "bmi_above30", "bmi30smokeryes")

## spliting the data into train and test
str(ins)

set.seed(23)
rand <- sample(1:nrow(ins), 1000, replace=F)
ins_train <- ins[rand,]
ins_test <- ins[-rand,]

# fitting a model on train data
ins_eval_model <- lm(charges ~ category + age2 + children + bmi + sex +
                       bmi_above30*smoker+ region, data = ins_train)

summary(ins_eval_model)

# assessing model fit and improving model performance
par(mfrow=c(2,2))
plot(ins_eval_model)

# outlier detection and removal
par(mfrow=c(1,2))

plot(ins_eval_model$fitted.values, ins_eval_model$residuals,
     main="Fitted values/Residuals", xlab="Fitted values",
     ylab="Residuals")

plot(ins_eval_model$fitted.values, rstudent(ins_eval_model),
     main="Fitted values/studentized residuals",
     xlab="Fitted values", ylab="Studentized residuals")
abline(h=c(3,-3), col="red")

ins_modtable <- data.frame(cbind(ins_eval_model$fitted.values, 
                                 rstudent(ins_eval_model))) %>%
  filter(# filtering studentized residuals greater than 3/-3
    X2 > 3 | X2 < (-3)
  )

names(ins_modtable) <- c("fittedvalues", "stud_res")

points(ins_modtable$fittedvalues, ins_modtable$stud_res, col="red",
       pch=20)
outliers <- rownames(ins_modtable) %>% as.integer()

ins_eval_remout <- lm(charges ~ category + age2 + children + bmi + sex +
                        bmi_above30*smoker+ region, 
                      data = ins_train[-outliers,])
# raises adjusted r squared a little

# an outliers removal function, returns a model ith removed outliers
outliers_fn <- function(model, df)
{
  require(magrittr)
  model_table <- data.frame(cbind(model$fitted.values,
                                  rstudent(model))) %>%
    filter(
      X2 > 3 | X2 < (-3)
    )
  outliers <- rownames(model_table) %>% as.integer()
  
  remout_model <- update(model, data = df[-outliers, ])
  return(remout_model)
}

# high leverage points ----------------------------------------------------

leverage <- hatvalues(ins_eval_model)

plot(leverage, rstudent(ins_eval_model),
     main="Leverage/Studentized residuals",
     xlab="Leverage", ylab="Studentized residuals")

# cheking points with high leverage
df_lev <- data.frame(cbind(leverage, rstudent(ins_eval_model))) %>%
  filter(
    leverage > .025
  )

points(df_lev$leverage, df_lev$V2, col="blue", pch=20)

lev_omit <- rownames(df_lev) %>% as.integer()

ins_eval_levout <- lm(charges ~ category + age2 + children + bmi + sex +
                        bmi_above30*smoker+ region, 
                      data = ins_train[-lev_omit,])

# cheking points with high leverage as well as outliers
df_levout <- data.frame(cbind(leverage, rstudent(ins_eval_model))) %>%
  filter(# V2 is the studentized residuals
    leverage > .020 & V2 > 3 |
      leverage > .020 & V2 < (-3)
  )

points(df_levout$leverage, df_levout$V2, col="red", pch=20)

omit_levout <- rownames(df_levout) %>% as.integer()

ins_eval_levout2 <- lm(charges ~ category + age2 + children + bmi + sex +
                         bmi_above30*smoker+ region, 
                       data = ins_train[-omit_levout,])
