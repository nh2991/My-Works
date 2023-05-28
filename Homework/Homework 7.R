105 * exp(-0.05)

51 * exp(-0.005) - 46

(50)*(1.005)

### Question 1.

x7_hw7 <- seq(30, 50)
y7_hw7 <- rep(-5, length(x7_hw7))

x8_hw7 <- seq(50, 70)
y8_hw7 <- -55 + x8_hw7

plot(c(x7_hw7, x8_hw7), c(y7_hw7, y8_hw7), type='l', xlim=c(45, 60), ylim=c(-10, 5), xlab='Stock Price at Period 1', ylab='Profit', main='Buy 1 put + Buy 1 Stock (Example)')
abline(v=50, lty='dashed')
abline(h=-5, lty='dashed')
points(50,-5, pch=16)
text(52,-6, '(50, -5)')

### Question 3.

x_hw7 <- seq(30, 50)
y_hw7 <- rep(4,21)

x2_hw7 <- seq(50, 70)
y2_hw7 <- 54 - x2_hw7

plot(c(x_hw7,x2_hw7), c(y_hw7,y2_hw7), type='l', xlim=c(40, 60), ylim=c(-6, 10), xlab='Stock Price at Period 1', ylab='Profit', main='Sell 1 call')
abline(v=54, lty='dashed')
abline(h=0, lty='dashed')
points(54, 0, pch=16)
text(56, 1, '(54, 0)')

### Question 4.

x3_hw7 <- seq(20, 40)
y3_hw7 <- 37 - x3_hw7

x4_hw7 <- seq(40, 60)
y4_hw7 <- rep(-3, length(x4_hw7))

plot(c(x3_hw7, x4_hw7), c(y3_hw7, y4_hw7), type='l', xlim=c(30, 50), ylim=c(-6,6), xlab='Stock Price at Period 1', ylab='Profit', main='Buy 1 put')
abline(v=37, lty='dashed')
abline(h=0, lty='dashed')
points(37,0,pch=16)
text(39,1, '(37,0)')

### Question 5.

x6_hw7 <- seq(50, 70)
y6_hw7 <- rep(-12, length(x6_hw7))

x5_hw7 <- seq(30, 50)
y5_hw7 <- 88 - 2 * x5_hw7

plot(c(x5_hw7, x6_hw7), c(y5_hw7, y6_hw7), type='l', xlim=c(45, 60), ylim=c(-20, 7), xlab='Stock Price at Period 1', ylab='Profit', main='Buy 2 puts')
abline(v=50, lty='dashed')
abline(h=-12, lty='dashed')
points(50,-12,pch=16)
text(52,-10, '(50, -12)')

x7_hw7 <- seq(30, 50)
y7_hw7 <- rep(-5, length(x7_hw7))

x8_hw7 <- seq(50, 70)
y8_hw7 <- -55 + x8_hw7

plot(c(x7_hw7, x8_hw7), c(y7_hw7, y8_hw7), type='l', xlim=c(45, 60), ylim=c(-10, 5), xlab='Stock Price at Period 1', ylab='Profit', main='Buy 1 call')
abline(v=50, lty='dashed')
abline(h=-5, lty='dashed')
points(50,-5, pch=16)
text(52,-6, '(50, -5)')

plot(c(x7_hw7, x8_hw7), c(y7_hw7, y8_hw7), type='l', xlim=c(40, 70), ylim=c(-20, 20), xlab='Stock Price at Period 1', ylab='Profit', main='Buy (1 call + 2 puts)')
lines(c(x5_hw7, x6_hw7), c(y5_hw7, y6_hw7))
lines(c(x5_hw7, x6_hw7), c(y5_hw7 + y7_hw7, y6_hw7 + y8_hw7), col='red')
points(50, -5, pch=16)
points(50, -12, pch=16)
points(50, -17, pch=16, col='red')
abline(h=0, lty='dashed')
points(67, 0, pch=16, col='red')
text(54, -18, '(50, -17)', col='red')
text(64, 2, '(67, 0)', col='red')

### Question 6.

x9_hw7 <- seq(20, 40)
y9_hw7 <- rep(2, length(x9_hw7))

x10_hw7 <- seq(40, 45)
y10_hw7 <- x10_hw7 - 38

x11_hw7 <- seq(45, 60)
y11_hw7 <- 52 - x11_hw7

x12_hw7 <- seq(20, 45)
y12_hw7 <- rep(10, length(x12_hw7))

x13_hw7 <- seq(45, 60)
y13_hw7 <- 100 - 2 * x13_hw7

x14_hw7 <- seq(20, 40)
y14_hw7 <- rep(-8, length(x14_hw7))

x15_hw7 <- seq(40, 60)
y15_hw7 <- x15_hw7 - 48

plot(c(x9_hw7, x10_hw7, x11_hw7), c(y9_hw7, y10_hw7, y11_hw7), type='l', col='red', xlim=c(35, 57), ylim=c(-10,10),
     xlab='Stock Price at Period 1', ylab='Profit', main='Buy 1 call + Sell 2 calls')
lines(x12_hw7, y12_hw7)
lines(x13_hw7, y13_hw7)
lines(x14_hw7, y14_hw7)
lines(x15_hw7, y15_hw7)
abline(h=0, lty='dashed')
points(40, 2, pch=16, col='red')
points(45, 7, pch=16, col='red')
points(52, 0, pch=16, col='red')
text(38, 3, '(40, 2)', col='red')
text(43, 8, '(45, 7)', col='red')
text(54, 1, '(52, 0)', col='red')

### Question 7.

## a. buy put, buy stock

plot(c(x7_hw7, x8_hw7), c(y7_hw7, y8_hw7), type='l', xlim=c(45, 60), ylim=c(-10, 5), xlab='Stock Price at Period 1', ylab='Profit', main='Long put + Long Stock')
abline(v=55, lty='dashed')
abline(h=0, lty='dashed')
points(55,0, pch=16)
text(53,1, '(S + p, 0)')

## b. sell put, sell stock

plot(c(x_hw7,x2_hw7), c(y_hw7,y2_hw7), type='l', xlim=c(40, 60), ylim=c(-6, 10), xlab='Stock Price at Period 1', ylab='Profit', main='Short put + Short stock')
abline(v=54, lty='dashed')
abline(h=0, lty='dashed')
points(54, 0, pch=16)
text(57, 1, '(p + S, 0)')

## c. buy call, sell stock

plot(c(x_hw7,x2_hw7), c(y_hw7 - 8,y2_hw7 - 8), type='l', xlim=c(40, 60), ylim=c(-6, 2), xlab='Stock Price at Period 1', ylab='Profit', main='Long call + Short stock')
abline(v=50, lty='dashed')
abline(h=0, lty='dashed')
points(50, -4, pch=16)
text(54, -3.5, '(E, - c + S - E)')

## d. sell call, buy stock

plot(c(x7_hw7, x8_hw7), c(y7_hw7, y8_hw7), type='l', xlim=c(45, 60), ylim=c(-10, 5), xlab='Stock Price at Period 1', ylab='Profit', main='Short call + Long Stock')
abline(v=55, lty='dashed')
abline(h=0, lty='dashed')
points(55,0, pch=16)
text(53,1, '(c + S, 0)')

#### Question 8.

x16_hw7 <- seq(-10, 50)
y16_hw7 <- rep(-5, length(x16_hw7))

x17_hw7 <- seq(50, 60)
y17_hw7 <-  -5 - 50 + x17_hw7

x18_hw7 <- seq(60, 90)
y18_hw7 <- rep(5, length(x18_hw7))

plot(c(x16_hw7, x17_hw7, x18_hw7), c(y16_hw7, y17_hw7, y18_hw7), type='l', xlim=c(40, 70), ylim=c(-8, 8), 
     xlab='Stock Price at Period 1', ylab= 'Profit', main='Bull call spread')
abline(v=50, lty='dashed')
abline(v=60, lty='dashed')
points(50,-5, pch=16)
points(60, 5, pch=16)
text(46, -4, 'C2 - C1')
text(65, 4, 'C2 - C1 + 10')

plot(c(x16_hw7, x17_hw7, x18_hw7), c(y16_hw7 + 10, (-1) * y17_hw7, y18_hw7 - 10), type='l', xlim=c(40, 70), ylim=c(-8,8),
     xlab='Stock Price at Period 1', ylab= 'Profit', main='Bear put spread')
abline(v=50, lty='dashed')
abline(v=60, lty='dashed')
points(50, 5, pch=16)
points(60, -5, pch=16)
text(45, 4, '10 + P2 - P1')
text(64, -4, 'P2 - P1')

plot(c(x16_hw7, x17_hw7, x18_hw7), c(y16_hw7 + y16_hw7 + 10, y17_hw7 + (-1) * y17_hw7, y18_hw7 + y18_hw7 - 10), type='l', xlim=c(40, 70), ylim=c(-8,8),
     xlab='Stock Price at Period 1', ylab= 'Profit', main='Bull call Spread \n + Bear put spread', col='red')
lines(c(x16_hw7, x17_hw7, x18_hw7), c(y16_hw7, y17_hw7, y18_hw7), type='l')
lines(c(x16_hw7, x17_hw7, x18_hw7), c(y16_hw7 + 10, (-1) * y17_hw7, y18_hw7 - 10), type='l')
text(64, 4, 'Bull call')
text(64, -4, 'Bear put')

#### Question 9.

#### Question 10.

plot(c(x16_hw7, x17_hw7, x18_hw7), c(y16_hw7 + 10, (-1) * y17_hw7, y18_hw7 - 10), type='l', xlim=c(40, 70), ylim=c(-8,8),
     xlab='Stock Price at Period 1', ylab= 'Profit', main='Sell one call + Buy one call')
abline(v=50, lty='dashed')
abline(v=60, lty='dashed')
points(50, 5, pch=16)
points(60, -5, pch=16)
text(46, 4, 'C1 - C2')
text(65, -4, 'C1 - C2 - 10')

?plot
