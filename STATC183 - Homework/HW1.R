x <- c(15, 12, 5, 9)
y <- c(36, 15, 7, 21)

w <- c("W", "X", "Y", "Z")

data <- data.frame(x,y)

y1 <- seq(min(y), max(y))

x1 <- 0.06*(y1-24)^2 + 8

plot(x,y, xlab='Standard Deviation', ylab='Return', type='n') ; lines(x1, y1, col='red') ; text(x,y,labels=w)

y1

?text

hw1 <- read.csv("/Users/user/Desktop/Yonsei/Junior/3-2/Statistical Models in Finance/stockData.csv", sep=',', header=T)

head(hw1)

r_hw1 <- (hw1[-1, 3:ncol(hw1)]-hw1[-nrow(hw1),3:ncol(hw1)])/hw1[-nrow(hw1),3:ncol(hw1)]

r_hw1

means_hw1 <- colMeans(r_hw1[-1])

means_hw1

covmat_hw1 <- cov(r_hw1[-1])

covmat_hw1

corrmat_hw1 <- cor(r_hw1[-1])

corrmat_hw1

variances_hw1 <- diag(covmat_hw1)

variances_hw1

stdev_hw1 <- diag(covmat_hw1)^(0.5)

stdev_hw1

plot(stdev_hw1, means_hw1)

equal_means <- mean(means_hw1)

equal_means

mean(stdev_hw1)

mean(covmat_hw1)

equal_stdev <- (1/(length(r_hw1)-1)*mean(stdev_hw1)^2 + (length(r_hw1)-1)/length(r_hw1)*mean(covmat_hw1))^(1/2)

equal_stdev

plot(stdev_hw1, means_hw1)
points(equal_stdev, equal_means, col='red')

A_hw1 <- sum(covmat_hw1^(-1) * means_hw1)

A_hw1

B_hw1 <- sum(covmat_hw1^(-1) * means_hw1 * means_hw1)

B_hw1

C_hw1 <- sum(covmat_hw1^(-1))

C_hw1

D_hw1 <- B_hw1 * C_hw1 - A_hw1 * A_hw1

D_hw1

plot(stdev_hw1, means_hw1)
points(equal_stdev, equal_means, col='red')

y1_hw1 <- seq(-1, 1, 0.001)

y1_hw1

x1_hw1 <- ((C_hw1 * y1_hw1^2 - 2 * A_hw1 * y1_hw1 + B_hw1) / D_hw1)^(1/2)

plot(stdev_hw1, means_hw1)
points(equal_stdev, equal_means, col='red')
lines(x1_hw1, y1_hw1, col='red')
