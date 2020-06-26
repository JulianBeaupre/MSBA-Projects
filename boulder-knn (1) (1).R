# regression tree for boulder dataset 
# Dan Zhang, July 2014 Revised September 2014

# Please change the working directory to the one on your computer
rm(list = ls())
setwd("D:/Dropbox/courses/DataAnalytics/data/housing")
load("boulder-cleaned.RData")
str(boulder.clean)
boulder1 <- boulder.clean[, sapply(boulder.clean, is.numeric)]
boulder1 <- na.omit(boulder1)

# cross validation
library(caret)
set.seed(300)
train <- sample(nrow(boulder1), nrow(boulder1) * 0.6)
knn.fit <- knnreg(LIST.PRICE ~ ., data=boulder1, subset = train, k = 5)
mean((boulder1$LIST.PRICE - predict(knn.fit, boulder1))[-train]^2)

# effect of scaling
boulder2 <- as.data.frame(scale(boulder1))
boulder2$LIST.PRICE <- boulder1$LIST.PRICE
knn.fit <- knnreg(LIST.PRICE ~ ., data=boulder2, subset = train, k = 5)
mean((boulder2$LIST.PRICE - predict(knn.fit, boulder2))[-train]^2)

# choosing best k; elbow method
vk <- seq(1, 20)
MSE <- vk
for (i in 1:length(vk)) {
  knn.fit <- knnreg(LIST.PRICE ~ ., data=boulder2, subset = train, k = vk[i])
  MSE[i] <- mean((boulder2$LIST.PRICE - predict(knn.fit, boulder2))[-train]^2)
}
plot(vk, MSE, xlab = "k", ylab = "MSE", col = "blue")


