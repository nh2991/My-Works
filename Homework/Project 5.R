### a. ###

hw <- read.csv("/Users/user/Desktop/Yonsei/Junior/3-2/Statistical Models in Finance/stockData.csv",sep=',', header=T)

r_hw5 <- (hw[-1, 3:ncol(hw)]-hw[-nrow(hw),3:ncol(hw)])/hw[-nrow(hw),3:ncol(hw)]

head(r_hw5)

covmat_hw5 <- var(r_hw5)
beta_hw5 <- covmat_hw5[1,-1] / covmat_hw5[1,1]

### Beta < 0 would be eliminated.

rrr_hw5 <- r_hw5[,-c(1,which(beta_hw5<0)+1)]

beta_new_hw5 <- rep(0,ncol(rrr_hw5))
alpha_hw5 <- rep(0,ncol(rrr_hw5))
mse_hw5 <- rep(0,ncol(rrr_hw5))
Ribar_hw5 <- rep(0,ncol(rrr_hw5))
Ratio_hw5 <- rep(0,ncol(rrr_hw5))
stock_hw5 <- rep(0,ncol(rrr_hw5))

### Setting R_f = 0.001.

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

xx_hw5

### Order with C_i, descending.

A_hw5 <- xx_hw5[order(-xx_hw5[,6]),]

A_hw5

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

cbind(A_hw5, col1_hw5, col2_hw5, col3_hw5, col4_hw5)

for(i in 1:nrow(A_hw5)) {
  col5_hw5[i] <- var(r_hw5[,1])*col2_hw5[i]/(1+var(r_hw5[,1])*col4_hw5[i])
}

### Short Sales are Allowed

z_short_hw5 <- (A_hw5[,3]/A_hw5[,5])*(A_hw5[,6]-col5_hw5[nrow(A_hw5)])
x_short_hw5 <- z_short_hw5/sum(z_short_hw5)

Weights_with_short_hw5 <- cbind(A_hw5, col1_hw5, col2_hw5, col3_hw5, col4_hw5, col5_hw5, z_short_hw5, x_short_hw5)

Weights_with_short_hw5

### Thus, the last column is the percentage of each stock.

### Short Sales are NOT Allowed

table1_hw5 <- cbind(A_hw5, col1_hw5, col2_hw5, col3_hw5, col4_hw5, col5_hw5)
table2_hw5 <- table1_hw5[1:which(col5_hw5==max(col5_hw5)),]

z_no_short_hw5 <- (table2_hw5[,3]/table2_hw5[,5]) * (table2_hw5[,6] - max(col5_hw5))
  
x_no_short_hw5 <- z_no_short_hw5 / sum(z_no_short_hw5)
  
Weights_no_short_hw5 <- cbind(table2_hw5, z_no_short_hw5, x_no_short_hw5)

Weights_no_short_hw5

### b. ###

blume1_hw5 <- read.csv("/Users/user/Desktop/Yonsei/Junior/3-2/Statistical Models in Finance/stockData_hw5_1_blume.csv", sep=',', header=T)
blume2_hw5 <- read.csv("/Users/user/Desktop/Yonsei/Junior/3-2/Statistical Models in Finance/stockData_hw5_2_blume.csv", sep=',', header=T)

vasicek_hw5 <- read.csv("/Users/user/Desktop/Yonsei/Junior/3-2/Statistical Models in Finance/stockData_hw5_vasicek_withoutBABA.csv", sep=',', header=T)

blume1_adj_hw5 <- (blume1_hw5[-1, 3:ncol(blume1_hw5)]-blume1_hw5[-nrow(blume1_hw5),3:ncol(blume1_hw5)])/blume1_hw5[-nrow(blume1_hw5),3:ncol(blume1_hw5)]
blume2_adj_hw5 <- (blume2_hw5[-1, 3:ncol(blume2_hw5)]-blume2_hw5[-nrow(blume2_hw5),3:ncol(blume2_hw5)])/blume2_hw5[-nrow(blume2_hw5),3:ncol(blume2_hw5)]

vasicek_adj_hw5 <- (vasicek_hw5[-1, 3:ncol(vasicek_hw5)]-vasicek_hw5[-nrow(vasicek_hw5),3:ncol(vasicek_hw5)])/vasicek_hw5[-nrow(vasicek_hw5),3:ncol(vasicek_hw5)]

covmat_vasicek_hw5 <- cov(vasicek_adj_hw5)

beta_vasicek_hw5 <- covmat_vasicek_hw5[1,-1] / covmat_vasicek_hw5[1,1]

var_beta_vasicek_hw5 <- rep(0,29)

for (i in 1:29) {
  q_vasicek_hw5 <- lm(data=vasicek_adj_hw5, formula=vasicek_adj_hw5[,i+1]~vasicek_adj_hw5[,1])
  var_beta_vasicek_hw5[i] <- vcov(q_vasicek_hw5)[2,2]
}

beta_adj_vasicek_hw5 <- var_beta_vasicek_hw5*mean(beta_vasicek_hw5)/(var(beta_vasicek_hw5)+var_beta_vasicek_hw5) 
                        + var(beta_vasicek_hw5)*beta_vasicek_hw5/(var(beta_vasicek_hw5)+var_beta_vasicek_hw5)

beta_adj_vasicek_hw5

### c. ###

PRESS_vasicek_hw5 <- sum((beta_adj_vasicek_hw5-beta_vasicek_hw5)^2) / 29

PRESS_vasicek_hw5
