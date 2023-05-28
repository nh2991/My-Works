a <- read.table("http://www.stat.ucla.edu/~nchristo/statistics_c183_c283/statc183c283_5stocks.txt", header=T)

head(a)
tail(a)

install.packages("plyr")
library(plyr)

head(arrange(a, a$date))
tail(arrange(a, a$date))

head(a)

a <- arrange(a, a$date)

a_hw3 <- (a[-1, 2:ncol(a)]-a[-nrow(a), 2:ncol(a)])/a[-nrow(a), 2:ncol(a)]

means_hw3 <- colMeans(a_hw3)
covmat_hw3 <- cov(a_hw3)
corrmat_hw3 <- cor(a_hw3)
variances_hw3 <- diag(covmat_hw3)
stdev_hw3 <- diag(covmat_hw3)^(0.5)

means_hw3
covmat_hw3
corrmat_hw3
variances_hw3
stdev_hw3

A_hw3 <- sum(covmat_hw3^(-1) * means_hw3)
B_hw3 <- sum(covmat_hw3^(-1) * means_hw3 * means_hw3)
C_hw3 <- sum(covmat_hw3^(-1))
D_hw3 <- B_hw3 * C_hw3 - A_hw3 * A_hw3

x_hw3 <- seq(-0.2, 0.2, 0.001)
sigma_squared_hw3 <- (C_hw3 * x_hw3 * x_hw3 - 2 * A_hw3 * x_hw3 + B_hw3) / D_hw3

plot(sigma_squared_hw3, x_hw3, type='l')

a_hw3[c(1,5)]

two_means_hw3 <- colMeans(a_hw3[c(1,5)])
two_covmat_hw3 <- cov(a_hw3[c(1,5)])
two_corrmat_hw3 <- cor(a_hw3[c(1,5)])
two_variances_hw3 <- diag(two_covmat_hw3)
two_stdev_hw3 <- diag(two_covmat_hw3)^(0.5)
two_means <- mean(two_means_hw3)
two_stdev <- (1/(2-1)*mean(two_stdev_hw3)^2 + (2-1)/2*mean(two_covmat_hw3))^(1/2)

plot(sigma_squared_hw3, x_hw3, type='l')
points(two_stdev, two_means, col='red')

a2 <- read.table("http://www.stat.ucla.edu/~nchristo/datac183c283/statc183c283_abc.txt", header=T)

head(a2)
tail(a2)

head(a2[1:3, 1])

a2_hw3 <- (a2[-1, 1:ncol(a2)]-a2[-nrow(a2), 1:ncol(a2)])/a2[-nrow(a2), 1:ncol(a2)]

head(a2_hw3)

three_means_hw3 <- colMeans(a2_hw3)
three_covmat_hw3 <- cov(a2_hw3)
three_corrmat_hw3 <- cor(a2_hw3)
three_variances_hw3 <- diag(two_covmat_hw3)
three_stdev_hw3 <- diag(two_covmat_hw3)^(0.5)
three_means <- mean(two_means_hw3)
three_stdev <- (1/(3-1)*mean(two_stdev_hw3)^2 + (3-1)/2*mean(two_covmat_hw3))^(1/2)

plot(sigma_squared_hw3, x_hw3, type='l')
points(two_stdev, two_means, col='red')
points(three_stdev, three_means, col='blue')

### f. ###

sigma_hw3 <- seq(0, 1, 0.001)
r_f <- 0.001

plot(sigma_squared_hw3, x_hw3, type='l')
lines(sigma_hw3, r_f + sigma_hw3 * (C_hw3 * r_f^2 -2 * A_hw3 * r_f + B_hw3)^(1/2))
points(three_stdev, three_means, col='blue')
points(0.6*three_stdev + 0.4 * r_f, 0.6 * three_means + 0.4 * r_f, col='green')
points(0.6 *two_stdev + 0.4 * r_f, 0.6 * two_means + 0.4 * r_f, col='darkgreen')

### h. ###

covmat_hw3^(-1)
means_hw3
r_f_vec <- 0.001 * c(1,1,1,1,1)

r_f_vec

((mean(means_hw3) - r_f) * covmat_hw3^(-1) %*% as.matrix(means_hw3 - r_f_vec))  / as.numeric(t(as.matrix(means_hw3 - r_f_vec)) %*% covmat_hw3^(-1) %*% as.matrix(means_hw3 - r_f_vec))

### i. ###

sigma_hw3 <- seq(0, 1, 0.001)
r_f <- 0.001
r_f2 <- 0.002

y_hw3 <- r_f + sigma_hw3 * (C_hw3 * r_f^2 -2 * A_hw3 * r_f + B_hw3)^(1/2)
y1_hw3 <- r_f2 + sigma_hw3 * (C_hw3 * r_f2^2 - 2 * A_hw3 * r_f2 + B_hw3)^(1/2)
y2_hw3 <- (1/2) * y_hw3 + (1/2) * y1_hw3

plot(sigma_squared_hw3, x_hw3, type='l')
lines(sigma_hw3, r_f + sigma_hw3 * (C_hw3 * r_f^2 -2 * A_hw3 * r_f + B_hw3)^(1/2), col='blue')
lines(sigma_hw3, r_f2 + sigma_hw3 * (C_hw3 * r_f2^2 - 2 * A_hw3 * r_f2 + B_hw3)^(1/2), col='red')
lines(sigma_hw3, y2_hw3, col='green')

cov(y_hw3, y1_hw3)

a2_hw3
