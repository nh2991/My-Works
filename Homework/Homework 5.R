z1 <- 10
z2 <- 4.444
z5 <- 10
z6 <- 4.4

x1 <- z1 / (sum(z1,z2,z3,z4))
x2 <- z2 / (sum(z1,z2,z3,z4))
x5 <- z5 / (sum(z1,z2,z5,z6))
x6 <- z6 / (sum(z1,z2,z5,z6))

x <- c(x1,x2,x5,x6)
R <- c(0.15, 0.2, 0.1, 0.14)

E <- t(x) %*% R

Return <- c(0.15, 0.2, 0.18, 0.12, 0.1, 0.14, 0.16, E * 0.8 + 0.2 * 0.05)
Sd <- c(0.1, 0.15, 0.2, 0.1, 0.05, 0.1, 0.2, (0.2294846)^(1/2))

plot(Sd, Return)
points(Sd[8], Return[8], col='red')

0.8 * E + 0.2 * 0.05

col1 <- c(0.1^2, 0.5, 0.5, 0.5)
col2 <- c(0.5, 0.15^2, 0.5, 0.5)
col3 <- c(0.5, 0.5, 0.05^2, 0.5)
col4 <- c(0.5, 0.5, 0.5, 0.1^2)

sigma <- cbind(col1, col2, col3, col4)
sigma

var <- t(x) %*% sigma %*% x
var

0.8^2 * var

2 / 1.2

35.5 / 6.5125

0.02 * (10 - 5.451056)
0.02 * (10 - 4.52)

E
var

######

hw <- read.csv("/Users/user/Desktop/Yonsei/Junior/3-2/Statistical Models in Finance/stockData.csv",sep=',', header=T)

r_hw5 <- (hw[-1, 3:ncol(hw)]-hw[-nrow(hw),3:ncol(hw)])/hw[-nrow(hw),3:ncol(hw)]

covmat_hw5 <- var(r_hw5)
beta_hw5 <- covmat_hw5[1,-1] / covmat_hw5[1,1]

rrr_hw5 <- r_hw5[,-c(1,which(beta_hw5<0)+1)]

beta_new_hw5 <- rep(0,ncol(rrr_hw5))
alpha_hw5 <- rep(0,ncol(rrr_hw5))
mse_hw5 <- rep(0,ncol(rrr_hw5))
Ribar_hw5 <- rep(0,ncol(rrr_hw5))
Ratio_hw5 <- rep(0,ncol(rrr_hw5))
stock_hw5 <- rep(0,ncol(rrr_hw5))

rf_hw5 <- 0.001

for(i in 1:ncol(rrr_hw5)) {
  q_hw5 <- lm(data=rrr_hw5, formula=rrr_hw5[,i]~r_hw5[,1])
  beta_new_hw5[i] <- q_hw5$coefficients[2]
  alpha_hw5[i] <- q_hw5$coefficients[1]
  mse_hw5[i] <- summary(q_hw5)$sigma^2
  Ribar_hw5[i] <- q_hw5$coefficients[1] + q_hw5$coefficients[2] * mean(r_hw5[,1])
  Ratio_hw5[i] <- (Ribar_hw5[i] - rf_hw5) / beta_new_hw5[i]
  stock_hw5[i] <- i
}

xx_hw5 <- (cbind(stock_hw5, alpha_hw5, beta_new_hw5, Ribar_hw5, mse_hw5, Ratio_hw5))

head(xx_hw5)

A_hw5 <- xx_hw5[order(-xx_hw5[,6]),]

col1_hw5 <- rep(0,nrow(A_hw5))
col2_hw5 <- rep(0,nrow(A_hw5))
col3_hw5 <- rep(0,nrow(A_hw5))
col4_hw5 <- rep(0,nrow(A_hw5))
col5_hw5 <- rep(0,nrow(A_hw5)) 

col1_hw5 <- (A_hw5[,4]-rf_hw5)*A_hw5[,3]/A_hw5[,5]
col3_hw5 <- A_hw5[,3]^2 / A_hw5[,5]
for(i in 1:nrow(A_hw5)) {
  col2_hw5[i] <- sum(col1_hw5[1:i])
  col4_hw5[i] <- sum(col3_hw5[1:i])
}

head(cbind(A_hw5, col1_hw5, col2_hw5, col3_hw5, col4_hw5))

for(i in 1:nrow(A_hw5)) {
  col5_hw5[i] <- var(r_hw5[,1])*col2_hw5[i]/(1+var(r_hw5[,1])*col4_hw5[i])
}

z_short_hw5 <- (A_hw5[,3]/A_hw5[,5])*(A_hw5[,6]-col5_hw5[nrow(A_hw5)])
x_short_hw5 <- z_short_hw5/sum(z_short_hw5)


A_hw5
col1_hw5

covmat_hw5

x_short_hw5

length(colnames(r_hw5))
length(col1_hw5)

covmat_market_hw5 <- var(r_hw5[-1])

covmat_market_hw5

var_market_hw5 <- t(as.matrix(x_short_hw5)) %*% covmat_market_hw5 %*% as.matrix(x_short_hw5)

C_hw5 <- (A_hw5[,4]-rf_hw5)*A_hw5[,3]*as.numeric(var_market_hw5)/A_hw5[,5]

C_hw5

####

1 / (0.5 * 0.03) * (8 - 3.8)
1 / (0.5 * 0.02) * (7 - 3.8)
1 / (0.5 * 0.15) * (0.2 - 3.8)

### Exercise 7 ###

(0.001) / 0.94
0.94^2 / 0.0033 * (0.001 / 0.94)

0.0018 * (0.2848485 / (1 + 0.0018 * 267.7576)) 

(0.011 - 0.005) / 0.61
0.61^2 / 0.0038 * ((0.011 - 0.005) / 0.61) + 0.2848485

97.92105 + 267.7576

0.0018 * (1.248006 / (1 + 0.0018 * 365.6787))

(0.015 - 0.005) / 1.12

1.12^2 / 0.0046 * ((0.015 - 0.005) / 1.12)

1.248006 + 2.434783
365.6787 + 272.6957

0.0018 * (3.682789 / (1 + 0.0018 * 638.3744))

(1 / 0.94) * 267.7576 * (0.00106383 - 0.0003457783)

(1/0.61) * 97.92105 * (0.009836066 - 0.0003457783)

(1/1.12) * 272.6957 * (0.008928571 - 0.0003457783)
