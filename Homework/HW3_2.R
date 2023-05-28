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

ones_hw3 <- c(1,1,1,1,1)

means_hw3 <- colMeans(a_hw3)
covmat_hw3 <- cov(a_hw3)

A_hw3 <- t(ones_hw3) %*% solve(covmat_hw3) %*% means_hw3

B_hw3 <- t(means_hw3) %*% solve(covmat_hw3) %*% means_hw3

C_hw3 <- t(ones_hw3) %*% solve(covmat_hw3) %*% ones_hw3

D_hw3 <- C_hw3 * B_hw3 - A_hw3 * A_hw3

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

### e. ###

a2 <- read.table("http://www.stat.ucla.edu/~nchristo/datac183c283/statc183c283_abc.txt", header=T)

mean_exxon_hw3 <- mean(a2$a)
mean_mcdonalds_hw3 <- mean(a2$b)
mean_boeing_hw3 <- mean(a2$c)

var_exxon_hw3 <- var(a2$a)
var_mcdonalds_hw3 <- var(a2$b)
var_boeing_hw3 <- var(a2$c)

cov12_hw3 <- cov(a2$a, a2$b)
cov13_hw3 <- cov(a2$a, a2$c)
cov23_hw3 <- cov(a2$b, a2$c)

rp_bar_hw3 <- a2$a * mean_exxon_hw3 + a2$b * mean_mcdonalds_hw3 + a2$c * mean_boeing_hw3

sigma_p_hw3 <- ((a2$a)^2*var_exxon_hw3 +(a2$b)^2* var_mcdonalds_hw3+(a2$c)^2*var_boeing_hw3
            +2*a2$a*a2$b*cov12_hw3+2*a2$a*a2$c*cov13_hw3+2*a2$b*a2$c*cov23_hw3)^.5

plot(sigma_p_hw3, rp_bar_hw3, col='green')

r_f1_hw3 <- 0.001

lambda1_hw3 <- (C_hw3 * r_f1_hw3 - A_hw3) / D_hw3
lambda2_hw3 <- (B_hw3 - A_hw3 * r_f1_hw3) / D_hw3

Xb_hw3 <- solve(covmat_hw3) %*% (lambda1_hw3 * means_hw3 + lambda2_hw3 * ones_hw3)

R1bar_hw3 <- t(Xb_hw3) %*% means_hw3
var1_hw3 <- t(Xb_hw3) %*% covmat_hw3 %*% Xb_hw3

sd1_hw3 <- sqrt(var1_hw3)

points(sd1_hw3, R1bar_hw3, pch=19, col='red')

minvar_hw3 <- 1 / C_hw3
minE_hw3 <- A_hw3 / C_hw3
  
sdeff_hw3 <- seq((minvar_hw3)^0.5, 1, by = 0.0001)

y1_hw3 <- (A_hw3 + sqrt(D_hw3*(C_hw3*sdeff_hw3^2 - 1)))*(1/C_hw3) 
y2_hw3 <- (A_hw3 - sqrt(D_hw3*(C_hw3*sdeff_hw3^2 - 1)))*(1/C_hw3) 

plot(sdeff_hw3, y1_hw3, type = "n", ylim=c(-0.1,0.1), xlab="Portfolio standard deviation", ylab="Expected return", xaxt="no", yaxt="no")

axis(1, at=seq(0, 2, 0.02))
axis(2, at=seq(-0.5,0.5, 0.02))

points(sdeff_hw3, y1_hw3, lwd=5,type = "l")
points(sdeff_hw3, y2_hw3, lwd=5,type = "l")  

### g unhappy..... ###

### h. ###

covmat_hw3^(-1)
means_hw3
r_f_vec <- 0.001 * c(1,1,1,1,1)

r_f_vec

((mean(means_hw3) - r_f) * covmat_hw3^(-1) %*% as.matrix(means_hw3 - r_f_vec))  
/ as.numeric(t(as.matrix(means_hw3 - r_f_vec)) %*% covmat_hw3^(-1) %*% as.matrix(means_hw3 - r_f_vec))

### x represent the weight(or percentage) of each stocks. ###

### i. ###

r_f2_hw3 <- 0.002

lambda1_f2_hw3 <- (C_hw3 * r_f2_hw3 - A_hw3) / D_hw3
lambda2_f2_hw3 <- (B_hw3 - A_hw3 * r_f2_hw3) / D_hw3

Xb_f2_hw3 <- solve(covmat_hw3) %*% (lambda1_f2_hw3 * means_hw3 + lambda2_f2_hw3 * ones_hw3)

R2bar_hw3 <- t(Xb_f2_hw3) %*% means_hw3
var2_hw3 <- t(Xb_f2_hw3) %*% covmat_hw3 %*% Xb_f2_hw3

sd2_hw3 <- sqrt(var2_hw3)

a <- seq(-30, 30, 0.001)
b <- 1-a

sigma_ab_hw3 <- t(Xb_hw3) %*% covmat_hw3 %*% Xb_f2_hw3

R_pbar_hw3 <- a*R1bar_hw3 + b*R2bar_hw3

var_p_hw3 <-  a^2*var1_hw3 + b^2*var2_hw3 + 2*a*b* sigma_ab_hw3

sd_p_hw3 <- var_p_hw3^.5

plot(sd_p_hw3, R_pbar_hw3, type = "n",xlim=c(0,0.3), ylim=c(-0.03,0.03), xlab="Portfolio standard deviation", ylab="Expected return", xaxt="no", yaxt="no")

axis(1, at=seq(0, 0.3, 0.02))
axis(2, at=seq(-0.05,0.10, 0.02))

plot(sigma_p_hw3, rp_bar_hw3, col='green')
lines(sd_p_hw3, R_pbar_hw3, col="blue", type="l", lwd=8)

### covariance -> sigma_ab_hw3 ###

### 3. 4. unhappy.... ###



