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
# x <- getData('alt', country = "TW")
plot(x)
# full_alt <- cbind(altitude_data, alt = extract(x, altitude_data))
write.csv(full_alt, "altitudes.csv")

alt_dd <- cbind(dd, full_alt$alt)
names(alt_dd) <- c(names(dd), "altitude")

# analyzing relationship of altitude and distance to nearest MRT station
ggplot(data=alt_dd)+
  geom_point(aes(x=altitude, y=`X3 distance to the nearest MRT station`))+
  labs(title="ALTITUDE AND DISTANCE TO NEAREST MASS RAPID TRANSIT STATION",
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
                `Y house price of unit area`)

# splitting data into test and train
set.seed(2824)
train <- sample(1:nrow(newdd), 314)
newdd_train <- newdd[train, ]
newdd_test <- newdd[-train, ]


# fitting linear regression model -----------------------------------------

hlm2 <- lm(`Y house price of unit area`~
             `X2 house age`+`X3 distance to the nearest MRT station`+
             `X4 number of convenience stores`+`X5 latitude`,
           data=newdd_train)


# outlier detection and action --------------------------------------------


# plotting the model
par(mfrow=c(2,2))
plot(hlm2)

# checking outliers
#plotting residua;s vs fitted and studentized residuals
par(mfrow=c(1,2))
plot(hlm2$fitted.values, hlm2$residuals, 
     main="Fitted/Residuals", xlab="Fitted values", ylab="Residuals")
plot(hlm2$fitted.values, rstudent(hlm2), 
     main="Fitted/Studentized residuals", xlab="Fitted values",
     ylab="Studentized residuals");abline(h=c(-3,3), col="red", lwd=2)

resids <- data.frame(cbind(hlm2$fitted.values), rstudent(hlm2))
names(resids) <- c("hlm2.fittedvalues", "hlm2.studentizedresiduals")


hlm2_outliers <- filter(resids,
                        hlm2.studentizedresiduals > 3 | hlm2.studentizedresiduals < (-3))

points(hlm2_outliers$hlm2.fittedvalues, hlm2_outliers$hlm2.studentizedresiduals, 
       col="red", pch=20)
text(x=hlm2_outliers$hlm2.fittedvalues, y=hlm2_outliers$hlm2.studentizedresiduals,
     labels=rownames(hlm2_outliers), pos=2, cex=0.7)

hlm2_remout <- lm(`Y house price of unit area`~
                    `X2 house age`+`X3 distance to the nearest MRT station`+
                    `X4 number of convenience stores`, data=newdd_train[-c(149,127,271),])
pp1 <- predict(hlm2_remout, newdd_test)
mse(newdd_test$`Y house price of unit area`, pp1)
# very little change in the adjusted r squared.

# high-leverage points
leverage <- hatvalues(hlm2) 
l <- leverage[order(leverage, decreasing = T)]

# plotting high leverage points and outliers
plot(leverage, rstudent(hlm2),
     main="Outliers/High leverage", 
     xlab="Leverage", ylab="Studentized residuals")

lev_out <- data.frame(cbind(rstudent(hlm2), leverage))
names(lev_out) <- c("studentizedresiuals", "leverage")

lev_out2 <- filter(lev_out, leverage > 0.04)
omit <- rownames(lev_out2) %>% as.integer()
points(lev_out2$leverage, lev_out2$studentizedresiuals, 
       pch=20, col="red")

# improved model
hlm2_levout <- lm(`Y house price of unit area`~
             `X2 house age`+`X3 distance to the nearest MRT station`+
             `X4 number of convenience stores`, data=newdd[-omit,])

pp2 <- predict(hlm2_levout, newdd_test)
mse(newdd_test$`Y house price of unit area`, pp2)

# removing both outliers and high leverage points
lev_out3 <- filter(lev_out, 
                   leverage > ((3+1)/nrow(newdd_train)) & studentizedresiuals > 3 |
                     leverage > ((3+1)/nrow(newdd_train)) & studentizedresiuals < (-3))

omit2 <- rownames(lev_out3) %>% as.integer()
points(lev_out3$leverage, lev_out3$studentizedresiuals, 
       pch=20, col="red")

hlm2_levout2 <- lm(`Y house price of unit area`~
                    `X2 house age`+`X3 distance to the nearest MRT station`+
                    `X4 number of convenience stores`, data=newdd[-omit2,])

pp3 <- predict(hlm2_levout2, newdd_test)
mse(newdd_test$`Y house price of unit area`, pp3)
# greatly improves model

# analysis of variance
anova(object=hlm2, hlm2_levout2)
