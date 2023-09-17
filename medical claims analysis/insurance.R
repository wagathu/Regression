## insurance claims


# packages ----------------------------------------------------------------

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



pie(table(ins$category),
    main="AGE DISTRIBUTION AMONG POLICY HOLDERS",
    col=c("gray60", "gray90","gray70","black"))


# sex dist and age
ggplot(data=ins, aes(x=age))+
  geom_bar(width=1, aes(col="black"), show.legend = F)+
  facet_grid(.~sex)+labs(title="DISTRIBUTION OF AGE IN THE GENDER")

# bmi
ggplot(data=ins, aes(x=bmi))+
  geom_histogram(bins = 30, aes(col="red"), show.legend = F)+
  geom_freqpoly(bins=30, col="blue", lty=2)

# cheking skewness and kurtosis

skewness(ins$bmi)
kurtosis(ins$bmi)
qqnorm(ins$bmi, main="Normal QQ plot for the BMI")
qqline(ins$bmi, col="red", lty="dotted", lwd=2)

# age and BMI
ba <- ggplot(data=ins, aes(x=age, y=bmi))+
  geom_point(size=2)+
  geom_smooth(method="lm", se=F, col="red")+
  labs(title="THE RED LINE IS A REGRESSION LINE")


ba+facet_wrap(sex~.)+geom_smooth(method="lm", se=F, col="red")

# children
ggplot(data=ins, aes(x=children))+geom_bar()+
  labs(title="DISTRIBUTION OF NUMBER OF CHILDREN IN FAMILIES")

## smoker and bmi
ggplot(data=ins, aes(x=smoker))+geom_bar(width=0.2)+
  labs(title="SMOKER STATUS")

ggplot(data=ins, aes(x=bmi))+geom_histogram(bins=30, aes(col="red"), show.legend = F)+
  facet_grid(.~smoker)+ # ideal BMI: 18.5 - 24.9
  geom_vline(xintercept=c(18.5, 24.9), col="blue", lwd=1)+
  labs(title="BODY MASS INDEX AND SMOKER STATUS", x="Body Mass Index", 
       caption  = "The parallel blue lines indicate normal BMI which ranges from 18.5 to 24.9")

# region and sex
ggplot(data=ins, aes(x=region))+
  geom_bar(width=0.3, aes(fill=sex), position="dodge")+
  labs(title="REGION WITH SEX DISTRIBUTION")

# region and smoker status
pie(table(ins$region),
    main="REGIONS DISTRIBUTION",
    labels=c("North east", "North west", "South East","South West"),
    col=c("red", "blue","green","black"))

ggplot(data=ins, aes(x=region))+
  geom_bar(width=0.3, aes(fill=smoker), position="dodge")+
  labs(title="REGION WITH SMOKER STATUS DISTRIBUTION")


# region and children covered
ggplot(data=ins, aes(x=region))+
  geom_bar(width=0.3, aes(fill=children), position="dodge")+
  labs(title="REGION WITH SMOKER STATUS DISTRIBUTION")

pie(table(ins$smoker),
    main="SMOKER STATUS AMONG INSURED",
    labels=c("Smoker: No", "Smoker: Yes"),
    col=c("red", "blue"))

# total medical expense planned
ggplot(data=ins, aes(x=charges))+
  geom_histogram(bins=60, aes(col="red"),show.legend = F)+
  labs(title = "PREMIUM CHARGES FOR HEALTH INSURANCE")

# inspecting distribution of medical charges premium
skewness(ins$charges)
qqnorm(ins$charges)
qqline(ins$charges,col="red", lwd=2)
# not normally distrbuted

# charged expenses vs smoker status
ggplot(data=ins, aes(x=charges))+
  geom_histogram(bins=60, aes(col="red"),show.legend = F)+
  facet_grid(.~smoker)+
  labs(title = "SMOKER STATUS AND PREMIUM CHARGES")
  
# non smoker charges which are alarmingly high
ggplot(data=ins, aes(x=charges))+
  geom_histogram(bins=60, aes(col="red"),show.legend = F)+
  facet_grid(smoker~children)+
  geom_vline(xintercept=30000)+
  labs(title="SMOKER STATUS, CHILDREN COVERED vs PREMIUM CHARGES")
  # observing rare none smoker charges
# Majority of none smokers regardless of their children are below the 30,000 line


# none smoker charges in area
ggplot(data=ins, aes(x=charges))+
  geom_histogram(bins=60, aes(col="red"),show.legend = F)+
  facet_grid(smoker~region)+
  geom_vline(xintercept=30000)+
  labs(title = "SMOKER STATUS, AREA, vs PREMIUM CHARGES",
       subtitle = "The 30,000 line is the overall mean premium charge")

# observing rare none smoker charges

# none smoker charges in sex
ggplot(data=ins, aes(x=charges))+
  geom_histogram(bins=60, aes(col="red"),show.legend = F)+
  facet_grid(smoker~sex)+
  geom_vline(xintercept=30000)+
  labs(title = "SMOKER STATUS, GENDER vs PREMIUM CHARGES")
# observing rare none smoker charges

# none smoker and age of primary beneficiary or premium holder
ggplot(data=ins, aes(x=age, y=charges))+geom_point()+
  labs(y="Medical expenses", x="Age of primary beneficiary", 
       title="DISTRIBUTION OF PREMIUM CHARGES OVER AGE",
       subtitle = paste("The correlation coefficent of charges and age is: ", cor(ins$age, ins$charges)))

# observing relationship between charges and BMI
ggplot(data=ins, aes(x=bmi, y=charges))+geom_point()+
  labs(y="Medical expenses", x="BMI of primary beneficiary", 
       title="DISTRIBUTION OF PREMIUM CHARGES OVER BMI",
       subtitle = paste("The correlation coefficent of charges and BMI is: ", cor(ins$bmi, ins$charges)))



#######################################################
# END OF ANALYSIS
table(ins$sex)
table(ins$children)
table(ins$smoker)
table(ins$region)

table(ins$sex, ins$region)
table(ins$sex, ins$smoker)
pie(table(ins$sex, ins$smoker), # pie chART for gender and smoker status
    labels=c("Female-Smoker: No", "Male-Smoker: No",
             "Female-Smoker: Yes", "Male-Smoker: Yes"))

## a correlation matrix of the data
cor(ins[c("age", "bmi", "children", "charges")])

# creating a scatter plot matrix
pairs(ins[c("age", "bmi", "children", "charges")])

## plotting notable observations
p <- ggplot(data=ins, aes(x=bmi, y=charges))+
  geom_point(size=1)+
  geom_jitter()+
  geom_smooth(method="lm", se=F)+
  labs(x="Body mass index", y="Medical charges",
       title="BODY MASS INDEX AND PREMIUM CHARGES" ,
       subtitle = paste("The correlation between bmi and charges is: ", cor(ins$bmi, ins$charges)))

p
## faceting the plot further
p+facet_wrap(smoker~.)

p+facet_grid(smoker~children)

p+facet_grid(smoker~age_category)

p+facet_grid(age_category~children)

# splitting data to test and train
dim(ins)


ins_model <- lm(charges ~ category+bmi+children+smoker,
                data = ins)

# fitting the linear model using  lm() ------------------------------------

# cheking fit
qqnorm(ins_model$residuals, main="Normal QQ plot for Insurance data")
qqline(ins_model$residuals, col="red", lwd=2,
       sub="The model is not a good fit to the data", col.sub="red")



# IMPROVING MODEL PERFORMANCE ---------------------------------------------

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
rand <- sample(1:1338, 1000, replace=F)
ins_train <- ins[rand,]
ins_test <- ins[-rand,]

# fitting a model on train data
ins_eval_model <- lm(charges ~ category + age2 + children + bmi + sex +
                   bmi_above30*smoker+ region, data = ins_train)

summary(ins_eval_model)
# bmi_above30*smoker is interaction of whether obese smokers affect their fee

qqnorm(ins_eval_model$residuals, main="Normal QQ plot for Insurance data")
qqline(ins_eval_model$residuals, col="red", lwd=2,
       sub="The model is not a good fit to the data", col.sub="red")


## using the trained model to predict charges
library(ModelMetrics)
library(magrittr)
pred_test <- predict.lm(ins_eval_model, ins_test, type="response") %>%
  as.numeric()

mse(pred_test, ins_test[,7])

## plotting the paths
require(plotly)

# a dataframe of holding the actaul and predicted charges of the test dataset
dd <- data.frame()
dd[1:nrow(ins_test),1] <- 1:nrow(ins_test)
dd$actual <- ins_test$charges
dd$pred <- pred_test

p <- ggplot(data=dd, aes(x=V1, y=actual))+
  geom_line(color="black")+
  geom_line(aes(x=V1, y=pred), color="red")

ggplotly(p)


# validation of train data ------------------------------------------------

# leave one out
loocv <- lm_loocv(formula = charges ~ category + age2 + children + bmi + sex +
        bmi_above30*smoker+ region,
      df=ins_train)
d <- data.frame(cbind(1:nrow(ins_train), ins_train$charges, train_validation))
names(d) <- c("index", "actual", "predicted")

# k fold
kkfold <- lm_kfold(formula = charges ~ category + age2 + children + bmi + sex +
                    bmi_above30*smoker+ region,
                  df=ins_train, folds=200, times=1)

  
View(ins_train)


# mse function ------------------------------------------------------------

mse(d[,2], d[,3])

p <- ggplot(data=d[5:11,], aes(x=index, y=actual))+
  geom_line(color="black")+
  geom_line(aes(x=index, y=predicted), color="red")

ggplotly(p)
p + transition_reveal(index)


# dummy data initialization -----------------------------------------------


dummy_ins <- data.frame()

# age
dummy_ins[1:1000,1] <- sample(18:64, 1000, replace = T)

# sex
dummy_ins[,2] <- sample(c("male","female"), 1000, replace=T, prob=c(0.5,0.5))

# bmi
dummy_ins[,3] <- rnorm(1000, mean=mean(ins$bmi), sd=sqrt(var(ins$bmi)))

# children
dummy_ins[,4] <- sample(5, 1000, replace=T)

# smoker
dummy_ins[,5] <- sample(c("yes", "no"), 1000, replace=T, prob=c(0.4,0.6))

# region
dummy_ins[,6] <- sample(c("northeast","northwest","southeast","southwest"),
                        1000, replace=T, prob=c(0.25,0.23,0.26,0.26))

# category
for (i in 1:nrow(dummy_ins))
{
  dummy_ins[i,7]<-agecat(dummy_ins[i,1])
}

# miscellaneous
dummy_ins[,8] <- dummy_ins[,1]**2

# bmi above 30
dummy_ins[,9] <- ifelse(dummy_ins[,3]>=30, 1, 0)

# bmi above 30 and smoker: yes
for (i in 1:nrow(dummy_ins))
{
  if (dummy_ins[i,3]>=30 &&
      dummy_ins[i,5]=="yes")
  {
    dummy_ins[i,10] <- 1
  }
  else
  {
    dummy_ins[i,10] <- 0
  }
}

colnames(dummy_ins) <- c("age", "sex", "bmi", "children", "smoker", "region", 
                         "category", "age2", "bmi_above30", "bmi30smokeryes")
View(dummy_ins)

# actual train model
ins_eval_model <- lm(charges ~ category + age2 + children + bmi + sex +
                       bmi_above30*smoker+ region, data = ins_train)

## predicitng dummy data charges
dummy_ins$charges <- predict.lm(ins_eval_model, dummy_ins)

## storing the dummy data and loading it back
write.csv(dummy_ins, "dummydata.csv", row.names = F)
rm(dummy_ins)

## reading the data back
dummy_ins <- read.csv("dummydata.csv")

## predicting back the ins_train dataset
dummy_mod <- lm(charges ~ category + age2 + children + bmi + sex +
                  bmi_above30*smoker+ region, data = dummy_ins)

## plotting an interactive chart for the actual train and the dummy train
pd <- data.frame()
pd[1:nrow(ins_train),1] <- 1:nrow(ins_train)
pd[,2] <- ins_train$charges
pd[,3] <- pred_train
colnames(pd) <- c("index", "actual", "predicted")

ggplot(data=pd[1:20,])+geom_line(aes(x=index, y=actual), col="black", lwd=1.5)+
  geom_line(aes(x=index, y=predicted), col="red", lwd=1.5)+
  transition_reveal(index)


## now predicting the insurance test data
test_data <- ins_test[,-7]
View(test_data)

pred_traintest <- predict(ins_eval_model, test_data) %>%
  as.numeric()

pred_full <- predict(dummy_mod, test_data) %>%
  as.numeric()





mse_own(actual=ins_train$charges, predicted = pred_train)
mse(ins_train$charges, pred_train)
## the MSE is a bit low

## mean square error
# traintest
mse(pred_traintest, ins_test[, 7])
mse(pred_dummytest, ins_test[,7])

# period
pd <- 1:50

d_f <- as.data.frame(cbind(pd, ins_test[1:50,7], pred_traintest[1:50], pred_full[1:50]))
colnames(d_f) <- c("pd", "actual", "pred_traintest", "pred_dummytest")
View(d_f)

require(gganimate)
# plotting the paths
dummy <- ggplot(data=d_f)+geom_line(aes(x=pd, y=actual), col="black")+
  geom_line(aes(x=pd, y=pred_traintest), col="red")
  #geom_line(aes(x=pd, y=pred_dummytest), col="blue")
dummy + transition_reveal(pd)
ggplotly(dummy)

actual_train <- ggplot(data=d_f)+geom_line(aes(x=pd, y=actual), col="black")+
  geom_line(aes(x=pd, y=pred_traintest), col="red")+
  #geom_line(aes(x=pd, y=pred_dummytest), col="blue")+
  labs(title = "Black  -Actual, Red - Predicted", x="Prices")+
  transition_reveal(pd)
actual_train

library(gridExtra)
grid.arrange(dummy, actual_train, ncol=2, nrow=1)
## cannot work


# end of dummy data -------------------------------------------------------

# Removing outliers and high leverage points


# outlier removal ---------------------------------------------------------

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

remout_insmodel <- outliers_fn(ins_eval_model, ins_train)

remout_pred <- predict(remout_insmodel, ins_test)

mse(remout_pred, ins_test$charges)
# doesnt improve performance of model



# high leverage points removal --------------------------------------------

leverage <- hatvalues(ins_eval_model) 
l <- leverage[order(leverage, decreasing = T)]

# plotting high leverage points and outliers
plot(leverage, rstudent(ins_eval_model),
     main="Outliers/High leverage", 
     xlab="Leverage", ylab="Studentized residuals")

lev_out <- data.frame(cbind(rstudent(ins_eval_model), leverage))
names(lev_out) <- c("studentizedresiuals", "leverage")

lev_out2 <- filter(lev_out, leverage > 0.025)
omit <- rownames(lev_out2) %>% as.integer()
points(lev_out2$leverage, lev_out2$studentizedresiuals, 
       pch=20, col="red")

# improved model

remlev_insmodel <- lm(charges ~ category + age2 + children + bmi + sex +
                        bmi_above30*smoker+ region, 
                      data = ins_train[-omit,])

remlev_pred <- predict(remlev_insmodel, ins_test)
mse(remlev_pred, ins_test$charges)

# improves model


# both outliers and high leverage points ----------------------------------

# removing both outliers and high leverage points
lev_out3 <- filter(lev_out, 
                   leverage > 0.025 & studentizedresiuals > 3 |
                     leverage > 0.025 & studentizedresiuals < (-3))

omit2 <- rownames(lev_out3) %>% as.integer()
points(lev_out3$leverage, lev_out3$studentizedresiuals, 
       pch=20, col="red")

remoutlev_insmodel <- lm(charges ~ category + age2 + children + bmi + sex +
                        bmi_above30*smoker+ region, 
                      data = ins_train[-omit2,])

levout_pred <- predict(remoutlev_insmodel, ins_test)
mse(levout_pred, ins_test$charges)


# using leave one out to do test predictions ------------------------------


loocv_pred <- lm_loocv_test(formula = charges ~ category + age2 + children + bmi + sex +
                              bmi_above30*smoker+ region,
                            df_train = ins_train,
                            df_test = ins_test,
                            predictors = 14,
                            return_type = "pred",
                            outliers.rm = T)
loocv_pred

mse(loocv_pred, ins_test$charges)
# greatly improves the model when outliers are removed during loocv test

##
