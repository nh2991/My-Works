hw <- read.csv("/Users/user/Desktop/Yonsei/Junior/3-2/Statistical Models in Finance/stockData.csv",sep=',', header=T)

r_hw4 <- (hw[-1, 3:ncol(hw)]-hw[-nrow(hw),3:ncol(hw)])/hw[-nrow(hw),3:ncol(hw)]

head(r_hw4)

means_hw4 <- colMeans(r_hw4)

means_hw4

covmat_hw4 <- cov(r_hw4)

covmat_hw4

variances_hw4 <- diag(covmat_hw4)

variances_hw4

E_hw4 <- 0.01

means_hw4 - 0.01

z_hw4 <- covmat_hw4^(-1) %*% (means_hw4 - E_hw4)

z_hw4

x_hw4 <- z_hw4 / sum(z_hw4)

x_hw4

sum(x_hw4)

lm_hw4 <- lm(r_hw4$AAPL~r_hw4$X.GSPC)

summary(lm_hw4)

mean(lm_hw4$residuals^2)

################################

a1 <- read.csv("/Users/user/Desktop/Yonsei/Junior/3-2/Statistical Models in Finance/stockData_hw4_1.csv", sep=",", header=TRUE)
a2 <- read.csv("/Users/user/Desktop/Yonsei/Junior/3-2/Statistical Models in Finance/stockData_hw4_2.csv", sep=",", header=TRUE)
a3 <- read.csv("/Users/user/Desktop/Yonsei/Junior/3-2/Statistical Models in Finance/stockData_hw4_3.csv", sep=",", header=TRUE)

r1 <- (a1[-1,3:ncol(a1)]-a1[-nrow(a1),3:ncol(a1)])/a1[-nrow(a1),3:ncol(a1)]
r2 <- (a2[-1,3:ncol(a2)]-a2[-nrow(a2),3:ncol(a2)])/a2[-nrow(a2),3:ncol(a2)]
r3 <- (a3[-1,3:ncol(a3)]-a3[-nrow(a3),3:ncol(a3)])/a3[-nrow(a3),3:ncol(a3)]

covmat1 <- var(r1)
covmat2 <- var(r2)
covmat3 <- var(r3)

beta1 <- covmat1[1,-1] / covmat1[1,1]
beta2 <- covmat2[1,-1] / covmat2[1,1]
beta3 <- covmat3[1,-1] / covmat3[1,1]

PRESS1 <- sum((beta2-beta3)^2) / 30

U1 <- ( mean(beta3) - mean(beta2) )^2

q1 <- lm(beta3 ~ beta2)
Sp12 <- (29/30)*var(beta2)
U2 <- (1-q1$coef[2])^2*Sp12

Sa2 <- (29/30)*var(beta3)
rap12 <- ( cor(beta2,beta3) )^2
U3 <- (1-rap12)*Sa2

U1+U2+U3 ; PRESS1

#######

beta_hw4 <- 4.299189 / (51.70104 - 2 * (32.44349 / 30) * 32.44349 + 30 * (32.44349 / 30)^2)

(32.26206 / 30 - 32.44349 / 30)^2 + (1 - beta_hw4) * (0.4558062 / 48) + (1 - ((beta_hw4)^2 * 0.4558062) / 0.3235921)

0.3^2 * 1.08 + 0.5^2 * 0.8 + 0.2^2 * 1.22

(0.01 + 1.08 * 500000 * 0.3) + (0.04 + 0.8 * 500000 * 0.5) + (0.08 + 1.22 * 500000 * 0.2)

(0.012 - 0.005) / 0.61

(0.118 - 0.1) / 1.08 ; (0.12 - 0.1) / 0.8 ; (0.202 - 0.1) / 1.22

(0.118 - 0.1) * 1.08 / 0.003

(0.12 - 0.1) * 0.8 / 0.006

(0.202 - 0.1) * 1.22 / 0.001

0.61^2 / 0.0038

1.22^2 / 0.001

1.08^2 / 0.003

0.8^2 / 0.006

(0.002 * 124.44) / (1 + 0.002 * 1488.4)

(0.002 * 130.92) / (1 + 0.002 * 1877.2)

(0.002 * 133.587) / (1 + 0.002 * 1983.867)

z3_hw4 <- 1 / 1.22 * 1488.4 * (0.0836 - 0.06258298)

z1_hw4 <- 1 / 1.08 * 1877.2 * (0.0167 - 0.0550732)

z2_hw4 <- 1 / 0.8 * 1983.867 * (0.025 - 0.05378187)

x1_hw4 <- z1_hw4 / (z1_hw4 + z2_hw4 + z3_hw4)

x2_hw4 <- z2_hw4 / (z1_hw4 + z2_hw4 + z3_hw4)

x3_hw4 <- z3_hw4 / (z1_hw4 + z2_hw4 + z3_hw4)

sum(x1_hw4, x2_hw4, x3_hw4)
x1_hw4 ; x2_hw4 ; x3_hw4

(0.08 + 1.22 * x3_hw4 * 300000) + (0.01 + 1.08 * x1_hw4) + (0.04 + 0.8 * x2_hw4) + (0.08 + 1.22 * 0.2 * 500000) + (0.01 + 1.08 * 0.3 * 500000) + (0.04 + 0.8 * 0.5 * 500000)

0.3^2 * 1.08 * 0.002 + 0.5^2 * 0.8 * 0.002 + 0.2^2 * 1.22 * 0.002 + x1_hw4^2 * 1.08 * 0.002 + x2_hw4^2 * 0.8 * 0.002 + x3_hw4^2 * 1.22 * 0.002

0.346 * 0.002

0.6 * ((0.01 + 1.08 * 500000 * 0.3) + (0.04 + 0.8 * 500000 * 0.5) + (0.08 + 1.22 * 500000 * 0.2)) + 0.4 * 300000 * 1.002

0.6^2 * (0.3^2 * 1.08 * 0.002 + 0.5^2 * 0.8 * 0.002 + 0.2^2 * 1.22 * 0.002)
1.08 * 0.002

(1/20)^2 * 20 * (0.9^2 * 0.2 + 0.5) + (1/20)^2 * 20 * 19 * (0.9^2 * 0.2)

0.2744995 * 0.5225564 / 1.154281

1.042003 - 0.1242691 * 1.068617

0.9092069 + 0.1242691 * 1.042003

0.79^2
