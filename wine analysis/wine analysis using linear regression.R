## directory
setwd("C:/Users/Admin/Desktop/MISCELLANEOUS R/ml projects/regression/wine analysis")

# data
wine<-read.csv("wine quality.csv", as.is=T)
View(wine)
str(wine)
dim(wine)

summary(wine)

# the correlation matrix
cor(wine)
pairs(wine) # dont run its so heavy

# plotting the histograms, to see how data is spread
par(mfrow=c(3,4))
for (i in 1:12)
{
  hist(wine[1:nrow(wine), i], 
       main=colnames(wine[i]), 
       xlab=colnames(wine[i]), 
       col=colors()[i])
}

# the linear model
wine_model<-lm(quality~., data=wine)

summary(wine_model)

norm<-function(x)
{
  return((x-mean(x))/sd(x))
}

## trying to see if normalizing helps linear regression
nwine<-wine
nwine<-as.data.frame(scale(nwine[1:11]))
nwine<-cbind(nwine, wine[12])
nwine_model<-lm(quality~., data=nwine)
