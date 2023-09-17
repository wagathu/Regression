#diretory
setwd("C:/Users/stanley/Desktop/MISCELLANEOUS R/ml projects/regression/real estate regression analysis")

# libraries
require(pacman)
p_load(readxl, dplyr, ggplot2, magrittr, 
       plotly, gganimate, ModelMetrics, stringr, 
       raster, sp)

# data
dd <- read_excel("Real estate valuation full data.xlsx", sheet=1) %>%
  as.data.frame()
View(dd)

# analysis
corrmat <- cor(cbind(dd[,2], dd[,3], dd[,4]), dd[,7])
colnames(corrmat) <- colnames(dd[7])
rownames(corrmat) <- colnames(dd[c(2,3,4)])

# summary and structure
dim(dd)
summary(dd)
str(dd)

unique(dd$`X4 number of convenience stores`)

# plotting the relationships
ggplot(data=dd)+
  geom_point(aes(x=`X2 house age`, y=`Y house price of unit area`))+
  labs(title="HOUSE AGE AND THEIR PRICES")

ggplot(data=dd)+
  geom_point(aes(x=`X3 distance to the nearest MRT station`,
                 y=`Y house price of unit area`))+
  labs(title="MRT STATIONS AND HOUSE PRICES")


#dd$`X4 number of convenience stores` <- factor(dd$`X4 number of convenience stores`)
ggplot(data=dd)+
  geom_boxplot(aes(x=factor(`X4 number of convenience stores`), 
                   y=`Y house price of unit area`, 
                   fill=`X4 number of convenience stores`), show.legend = F)+
  labs(title="CONVENIENCE STORES AND HOUSE PRICES", 
       x="Number of Convenience stores", y="House prices")

# investigating hig altitude areas using lat, lng, distance to MRT station
ggplot(data=dd)+
  geom_point(aes(x=`X5 latitude`, y=`X6 longitude`, 
                 col=`X3 distance to the nearest MRT station`))+
  labs(title="LATITUDE/LONGITUDE AND DISTANCE TO MRT STATION", 
       subtitle = "Areas which are lightly shaded must be mountaneus regions", 
       x="Latitude", y="Longitude")

# generating altitude data ------------------------------------------------

altitude_data <- dplyr::select(dd, `X6 longitude`, `X5 latitude`)
altitude_data <- data.frame(cbind(dd$`X6 longitude`, dd$`X5 latitude`))
head(altitude_data)
names(altitude_data) <- c("lon", "lat")

# using raster
x <- getData('alt', country = "TW")
plot(x)
full_alt <- cbind(altitude_data, alt = extract(x, altitude_data))
# write.csv(full_alt, "altitudes.csv")

alt_dd <- cbind(dd, full_alt$alt)
names(alt_dd) <- c(names(dd), "altitude")

# analyzing relationship of altitude and distance to nearest MRT station
ggplot(data=alt_dd)+
  geom_point(aes(x=altitude, y=`X3 distance to the nearest MRT station`))+
  labs(title="ALTITUDE AND DISTANCE TO NEAREST MRT STATION",
       x="The altitude", y="Distance to nearest station")

# relationship of altitude and house ages
ggplot(data=alt_dd)+
  geom_point(aes(x=altitude, y=`X2 house age`))+
  labs(title="ALTITUDE AND HOUSE AGES ANALYSIS", 
       x="Altitude", y="The house age")

# analysing altitude and house prices
ggplot(data=alt_dd)+
  geom_point(aes(x=altitude, y=`Y house price of unit area`))+
  #geom_smooth(aes(x=altitude, y=`Y house price of unit area`), 
  #           method="loess", se=F)+
  labs(title="ANALYSING ALTITUDE AND HOUSE PRICES", 
       x="Altitude", y="House prices per unit area")

# start of model analysis -------------------------------------------------

newdd <- dplyr::select(dd, 
                `X2 house age`,
                `X3 distance to the nearest MRT station`,
                `X4 number of convenience stores`,
                `X5 latitude`,`X6 longitude`,
                `Y house price of unit area`)

# splitting data into test and train
set.seed(2824)
train <- sample(1:nrow(newdd), 314)
newdd_train <- newdd[train, ]
newdd_test <- newdd[-train, ]

# just choosing sequentially
newdd_train <- newdd[1:314, ]
newdd_test <- newdd[315:414, ]

# fitting linear regression model

# THE FULL MODEL
hlm1 <- lm(`Y house price of unit area`~., data=newdd_train)
summary(hlm1)

hlm2 <- lm(`Y house price of unit area`~
             `X2 house age`+`X3 distance to the nearest MRT station`+
             `X4 number of convenience stores`+`X5 latitude`, data=newdd_train)
summary(hlm2)

# predicting test data
lm_pred <- predict.lm(hlm2, newdd_test, type = "response")

# obtaining MSE
mse(lm_pred, newdd_test$`Y house price of unit area`)

# combining knn and regression --------------------------------------------

# the knn is used to add random noise to the regression output
nn_lm <- combine_knnlm(train=newdd_train[,-6],
                       test=newdd_test[,-6],
                       train_labs = newdd_train[,6],
                       k=7)

# actually trying to choose noise based on the sign of the residuals
wrt_Signholder <- list()
vote_holder <- vector()
# calling the vote function
source("C:/Users/stanley/Desktop/MISCELLANEOUS R/random/vote.R")

for (i in 1:length(nn_lm))
{
  wrt_Signholder[[i]] <- ifelse(
    (hlm2$residuals)[nn_lm[[i]]] < 0, "Negative", "Positive"
  )
  vote_holder[i] <- vote(wrt_Signholder[[i]])
}

# actual splitting wrt to negative
rnn_lm <- list()
for (i in 1:length(nn_lm))
{
  temp <- hlm2$residuals[nn_lm[[i]]]
  rnn_lm[[i]] <- ifelse(vote_holder[i] == "Negative", 
                        yes = mean(temp[temp < 0], na.rm = T),
                        no = mean(temp[temp > 0], na.rm = T))
}
# adding the noise wrt to sign to the predicted output
wrt_signfinalpred <- lm_pred + unlist(rnn_lm)

final_pred <- vector()
res_knnlm <- vector()
for (i in 1:length(nn_lm))
{
  res_knnlm[i] <- mean((hlm2$residuals)[nn_lm[[i]]], na.rm=T)
}
final_pred <- lm_pred + res_knnlm

# testing if it works
mse(newdd_test$`Y house price of unit area`, lm_pred)
mse(newdd_test$`Y house price of unit area`, final_pred)

# fetching noise directly from rnorm dist
v <- var(hlm2$residuals)
k <- 7
noise <- rnorm(n = nrow(newdd_test), mean = 0, sd = sqrt(v/5))

noise_pred <- lm_pred + noise

# testing MSE
mse(noise_pred, newdd_test$`Y house price of unit area`)

# obtaining loops on mse of noise from rnorm(0, sigma/k) for a range of k
for (k in 15:18)
{
  v_mse <- vector()
  for (i in 1:100)
  {
    noise <- rnorm(n = nrow(newdd_test), mean = 0, sd = sqrt(v/(k**2)))
    noise_pred <- lm_pred + noise
    # testing MSE
    v_mse[i] <- mse(noise_pred, newdd_test$`Y house price of unit area`)
    
  }
  plot(v_mse, pch=20, col="maroon", type="o", 
       main = paste("K value: ", k),
       xlab="Rounds", ylab="Mean square error", 
       ylim=c(48, 100))
  abline(h = mse(newdd_test$`Y house price of unit area`, lm_pred), col=c("red", "blue"))
  
}



# capturing MSEs from a range of k values
var_mse <- vector()

for (kval in 1:20)
{
  message(paste("Training using k: ", kval))
  nn_lm <- combine_knnlm(train=newdd_train[,-6], test=newdd_test[,-6], 
                         train_labs = newdd_train[,6], k=kval)
  
  final_pred <- vector()
  res_knnlm <- vector()
  for (i in 1:length(nn_lm))
  {
    res_knnlm[i] <- mean((hlm2$residuals)[nn_lm[[i]]], na.rm=T)
  }
  final_pred <- lm_pred + res_knnlm
  
  var_mse[kval] <- mse(newdd_test$`Y house price of unit area`, 
                       final_pred)
}
plot(var_mse, type="o",
     col="maroon", pch=20, xlab="K value", ylab="Mean square error",
     ylim=c(40, 65),
     main="MEAN SQUARE ERROR OF MODELS", sub = "The blue line is the baseline MSE")
abline(h=mse(newdd_test$`Y house price of unit area`, 
           lm_pred), col="blue")
text(x = 10,y = mse(newdd_test$`Y house price of unit area`, 
                    lm_pred), labels = "The baseline MSE", col = "blue", 
     pos = 3)
axis(1, at=1:20)

# Improving predictions of linear regression models by adding noise.
# In this trial, after fitting the regression model on the test dataset,
# i used k nearest neighbour(kNN) algorithm to find set N - {the nearest neighbours
# of each of the test instances(rows)}, we then obtain the mean of the residuals of set N
# {the nearest neighbours} and then add that mean obtained to the predicted
# test output(y bar), 

# results: wonderful

# fetching model from outlier and leverage file ---------------------------

levout_pred <- predict.lm(hlm2_levout2, newdd_test)

# adding a bit of noise
for (i in 1:length(nn_lm))
{
  res_knnlm[i] <- mean((hlm2_levout2$residuals)[nn_lm[[i]]], na.rm=T)
}

final_levout <- levout_pred + res_knnlm
mse(newdd_test$`Y house price of unit area`, final_levout)

# fenerating random noise on the predicted data
# meam 0, common variance sigma squared
holder <- vector()
for (i in 1:100)
{
  temp <- lm_pred + sample(hlm2$residuals, length(lm_pred))
  holder[i] <- mse(newdd_test$`Y house price of unit area`, temp)
}
plot(holder, type="b", pch=20)
abline(h=mse(newdd_test$`Y house price of unit area`,lm_pred), col="red")

# using knn regression algorithm
knn_pred <- cknn_of(train=newdd_train[,-6], test=newdd_test[,-6], 
                    train_labs = newdd_train[,6], k=5)

# using add train knn algorithm
addtrain_pred <- add_trainKnn(train=newdd_train[,-6], test=newdd_test[,-6],
                              train_labs = newdd_train[,6],k=10)

# using support vector machines
require(kernlab)
svmmod <- ksvm(`Y house price of unit area`~
                `X2 house age`+`X3 distance to the nearest MRT station`+
                `X4 number of convenience stores`, 
              data=newdd_train,
              kernel="laplacedot",
              C=24,
              sigma=10)

svmpred <- predict(svmmod, newdd_test[,-6])
mse(newdd_test[,6], svmpred)

# testing the mean square error in all the predicts
mse(newdd_test[,6], lm_pred)
mse(newdd_test[,6], knn_pred)
mse(newdd_test[,6], addtrain_pred)
mse(newdd_test[,6], svmpred)


# testing k values from 1 to 20
knn_pred <- data.frame()
knn_pred[1:nrow(newdd_test), 1] <- 1:nrow(newdd_test)

for (k in 1:30)
{
  message(paste("Training with k: ", k))
  knn_pred[, (k+1)] <- cknn_of(train=newdd_train[,-6], test=newdd_test[,-6], 
                               train_labs = newdd_train[,6], k=k)
}


acc_vec <- vector()
acc_vec[1] <- mse(newdd_test[,6], lm_pred)
acc_vec[2] <- mse(newdd_test[,6], svmpred)
acc_vec[3] <- mse(newdd_test[,6], addtrain_pred)


for (i in 2:ncol(knn_pred))
{
  acc_vec[i+2] <- mse(newdd_test[,6], knn_pred[,i])
}
names(acc_vec) <- c("lm_pred", "svm", "add_train", str_c("knn:", 1:30))

plot(acc_vec, type="o", pch=20, 
     main="MEAN SQUARE ERRORS FOR MODELS", xlab="", 
     ylab="Mean square error", axes=F)
axis(1, at=1:33, labels=names(acc_vec), las=2)
axis(2);grid(col="lightblue")

# trying add train model on the data to improve MSE

# testing k values from 1 to 20
addtrainknn_pred <- data.frame()
addtrainknn_pred[1:nrow(newdd_test), 1] <- 1:nrow(newdd_test)

for (k in 1:30)
{
  message(paste("Training with k: ", k))
  addtrainknn_pred[, (k+1)] <- add_trainKnn(train=newdd_train[,-4], test=newdd_test[,-4], 
                               train_labs = newdd_train[,4], k=k)
}


acc_vec <- vector()
acc_vec[1] <- mse(newdd_test$`Y house price of unit area`, lm_pred)
acc_vec[2] <- mse(newdd_test$`Y house price of unit area`, lm_pred)
acc_vec[3] <- mse(newdd_test$`Y house price of unit area`, lm_pred)



for (i in 2:ncol(addtrainknn_pred))
{
  acc_vec[i+2] <- mse(newdd_test[,6], addtrainknn_pred[,i])
}
names(acc_vec) <- c(rep("lm_pred", 3), str_c("knn:", 1:30))

plot(acc_vec, type="o", pch=20, col="red", 
     main="MSE FOR ADDTRAIN KNN MODEL", axes=F,
     xlab="", ylab="Mean square error")
axis(1, at=1:33, labels=names(acc_vec), las=2)
axis(2);grid(col="lightblue")


# doesnt improve the model


# cross validation LOOCV/Kfold --------------------------------------------


# leave one out
loocvtable <- loocv_cknn(train_dataset = newdd_train[,-6], 
                             train_lab = newdd_train[,6],
                             k_range = 1:20)

plot(loocvtable, type="b", axes=F, pch=20, xlab="", 
     main="MEAN SQUARE ERRORS FOR THE MODELS", ylab="Mean square error")
axis(1, at=1:20, labels=names(loocvtable), las=2)
axis(2);grid(col="lightblue")



# using split data function for combine knn lm ----------------------------




tr <- newdd_train[, 1:5]
te <- newdd_test[, 1:5]
trl <- newdd_train[, 6]

ww <- split_knn(train=tr, 
                test = te, 
                train_labs = trl, 
                k_value = 5, 
                cat=3)

# cheking the mse
mse(newdd_test$`Y house price of unit area`, ww[, 2])

for (i in 1:10)
{
  message(paste("Using k as: ", i))
  
  ww <- split_knn(train=tr, 
                  test = te, 
                  train_labs = trl, 
                  k_value = i, 
                  cat=3)
  
  print(mse(newdd_test$`Y house price of unit area`, ww[, 2]))
}

