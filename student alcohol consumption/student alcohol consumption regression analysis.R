# analysing student alcohol consumption data

# directory
setwd("C:\\Users\\Admin\\Desktop\\MISCELLANEOUS R\\ml projects\\regression\\student alcohol consumption")

# data
sa<-read.csv("data.csv")
View(sa)

# summary and structure
dim(sa)
summary(sa)
str(sa)

# fitting a linear model
sa_model<-lm(G3~., data=sa)
summary(sa_model)
