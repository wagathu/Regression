# directory
setwd("C:/Users/stanley/Desktop/MISCELLANEOUS R/ml projects/regression/car details analysis")

# libraries
require(pacman)
p_load(dplyr, ggplot2, ModelMetrics, magrittr, stringr, stringi,
       grid, gridExtra)

# data
cardd <- read.csv("CAR DETAILS FROM CAR DEKHO.csv", as.is=T)
head(cardd)

# summary and structure
summary(cardd)
dim(cardd)
str(cardd)

# variables
attach(cardd)
unique(year)
unique(fuel)
unique(seller_type)
unique(transmission)
unique(owner)

# splitting into test and train
set.seed(2823)
rand <- sample(11:nrow(cardd), round(nrow(cardd)*0.769))

cardd_train <- cardd[rand, ]
cardd_test <- cardd[-rand, ]

# analysis
names(cardd_train)
View(cardd_train)

# selling price by year
year <- as.integer(year)
p1 <- ggplot(data=cardd_train, aes(x=year, y=selling_price))+
  geom_boxplot(aes(group=year))+
  labs(title="PRICES OF CARS THROUGH YEARS", x="Year", y="Selling price")
p2 <- ggplot(data=cardd_train, aes(x=year, y=selling_price))+
  geom_point()+
  labs(title="PRICES OF CARS THROUGH YEARS", x="Year", y="Selling price")

grid.arrange(p1, p2, nrow=1)

# kilometres driven
ggplot(data=cardd_train, aes(x=km_driven, y=selling_price))+
  geom_point()+
  labs(title="KILOMETRES DRIVEN ON SELLING PRICE", 
       x="Kilometres driven", y="Selling price")

# fuel and selling price
ggplot(data=cardd_train, aes(x=fuel))+
  geom_bar(width = .6, aes(fill=fuel), show.legend = F)+
  labs(title="FUEL TYPE COUNT", x="Fuel type")

ggplot(data=cardd_train, aes(x=fuel, y=selling_price))+
  geom_boxplot(aes(fill=fuel), show.legend = F)+
  labs(title="FUEL TYPE AND SELLING PRICE", x="Fuel type", 
       y="Selling price")

# fuel and kilometres driven by selling price
ggplot(data=cardd_train, aes(x=km_driven, y=selling_price))+
  geom_point(aes(col=fuel))+
  labs(title="KILOMETRES DRIVEN ON SELLING PRICE", 
       x="Kilometres driven", y="Selling price")

ggplot(data=cardd_train, aes(x=km_driven, y=selling_price))+
  geom_point()+
  facet_wrap(.~fuel)+
  labs(title="KILOMETRES DRIVEN ON SELLING PRICE", 
       x="Kilometres driven", y="Selling price")

# seller type
ggplot(data=cardd_train, aes(x=seller_type))+
  geom_bar(width = .6, aes(fill=seller_type), show.legend = F)+
  labs(title="SELLER TYPE COUNT", x="Seller type")

# seller type and fuel
ggplot(data=cardd_train, aes(x=seller_type))+
  geom_bar(width = .6, aes(fill=fuel), position = "dodge")+
  labs(title="SELLER TYPE COUNT", x="Seller type")

p1 <- ggplot(data=cardd_train, aes(x=seller_type, y=selling_price))+
  geom_boxplot(aes(fill=seller_type), show.legend = F)+
  labs(title="SELLER TYPE AND SELLING PRICES", 
       x="Seller type", y="Selling price")

p2 <- ggplot(data=cardd_train, aes(x=seller_type, y=selling_price))+
  geom_boxplot(aes(fill=fuel), position = "dodge", show.legend = T)+
  labs(title="SELLER TYPE, FUEL TYPE AND SELLING PRICES", 
       x="Seller type", y="Selling price")

grid.arrange(p1, p2, nrow=1)

# transmission and seller details
p1 <- ggplot(data=cardd_train, aes(x=transmission))+
  geom_bar(width=.3)+labs(title="TRANSMISSION TYPE")

p2 <- ggplot(data=cardd_train, aes(x=transmission, y=selling_price))+
  geom_boxplot(aes(fill=transmission), show.legend = F)+
  labs(title="TRANSMISSION TYPE AND PRICES")

p3 <- ggplot(data=cardd_train, aes(x=transmission, y=selling_price))+
  geom_boxplot(aes(fill=fuel), position="dodge")+
  labs(title="TRANSMISSION TYPE AND FUEL TYPE")

p4 <- ggplot(data=cardd_train, aes(x=transmission, y=selling_price))+
  geom_boxplot(aes(fill=seller_type), position="dodge")+
  labs(title="TRANSMISSION TYPE AND SELLER TYPE")

grid.arrange(p1, p2, p3, p4, nrow=2)

# car owners
p1 <- ggplot(data=cardd_train, aes(x=owner))+
  geom_bar(width=.3)+labs(title="OWNER TYPE")

p2 <- ggplot(data=cardd_train, aes(x=owner, y=selling_price))+
  geom_boxplot(aes(fill=owner), show.legend = F)+
  labs(title="OWNER TYPE AND PRICES")

p3 <- ggplot(data=cardd_train, aes(x=owner, y=selling_price))+
  geom_boxplot(aes(fill=fuel), position="dodge")+
  labs(title="OWNER TYPE AND FUEL TYPE")

p4 <- ggplot(data=cardd_train, aes(x=owner, y=selling_price))+
  geom_boxplot(aes(fill=seller_type), position="dodge")+
  labs(title="OWNER TYPE AND SELLER TYPE")

grid.arrange(p1, p2, p3, p4, nrow=2)


# linear regression models ------------------------------------------------

cardd_train <- select(cardd_train, -name)
carlm <- lm(selling_price~., data=cardd_train)
summary(carlm)
