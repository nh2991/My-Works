

hw6 <- read.csv("/Users/user/Desktop/Yonsei/Junior/3-2/Introduction to Data Analysis and Regression/Cleveland Data.csv")
hw6$exand[hw6$exand=='exercise indicued angina'] <- 1
hw6$exand[hw6$exand=='no exercise indicued angina'] <- 0
hw6$exand <- as.integer(hw6$exand)
head(hw6)

sort(hw6$maxheartrate)

?sort

length(hw6$exand[hw6$maxheartrate == 108])
sum(hw6$exand[hw6$maxheartrate == 108])

maxheartrate <- c() ; exand <- c() ; m <- c() 

for(i in min(hw6$maxheartrate):max(hw6$maxheartrate)) {
  maxheartrate[i-70] = i
  exand[i-70] = sum(hw6$exand[hw6$maxheartrate == i])
  m[i-70] = length(hw6$exand[hw6$maxheartrate == i])
}

maxheartrate

exand

m

hw6 <- data.frame(maxheartrate, exand, m, 'm-y'=m-exand, 'theta'=exand / m, '1-theta' = 1-exand/m)

hw6

plot(hw6$maxheartrate, hw6$theta)

####

hw6$maxheartrate[is.nan(hw6$theta)]
hw6$maxheartrate[is.nan(hw6$theta)==FALSE]

hw6_improve <- data.frame(hw6$maxheartrate[is.nan(hw6$theta)==F], hw6$exand[is.nan(hw6$theta)==F], hw6$m[is.nan(hw6$theta)==F],
                          hw6$m.y[is.nan(hw6$theta)==F], hw6$theta[is.nan(hw6$theta)==F], hw6$X1.theta[is.nan(hw6$theta)==F])

hw6_improve

colnames(hw6_improve) <- c('maxheartrate', 'exand', 'm', 'm.y', 'theta', '1-theta')

hw6_improve

plot(hw6_improve$maxheartrate, hw6_improve$theta)

###

m1 <- glm(cbind(hw6$exand, hw6$m.y)~maxheartrate, family=binomial)
summary(m1)


x <- seq(60, 220, 0.1)
y <- 1/(1+exp(-1*(m1$coefficients[1] + m1$coefficients[2] * x)))
plot(hw6$maxheartrate, hw6$theta)
lines(x,y)

#######


baseball <- read.table("/Users/user/Desktop/Yonsei/Junior/3-2/Introduction to Data Analysis and Regression/playoffs.txt", header=T)

head(baseball)

sort <- sort(baseball$Population, decreasing=T)

sort <- sort[-c(1,3,5)]

sort

Population <- c() ; PlayoffAppearances <- c() ; m <- c()

for(i in 1:length(sort)) {
  Population[i] = sort[i]
  PlayoffAppearances[i] = sum(baseball$PlayoffAppearances[baseball$Population == sort[i]])
  m[i] = sum(baseball$n[baseball$Population == sort[i]])
}

Population

PlayoffAppearances

m

hw6_2 <- data.frame(Population, PlayoffAppearances, m, 'm-y'=m-PlayoffAppearances, 
                    'theta'= PlayoffAppearances / m, '1-theta' = 1- PlayoffAppearances/m)

hw6_2

plot(hw6_2$Population, hw6_2$theta)
