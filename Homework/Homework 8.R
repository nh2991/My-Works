1.05^(3/12) - 1

(exp(0.01227223) - 0.95) / (1.06 - 0.95)

(0.5668 * 2 + 0.5083 * 0) * exp(-0.01227223)

(0.5668 * 5.18 + 0.4332 * 0) * exp(-0.01227223)

0.4332 * 3.5 * exp(-0.01227223)

exp(-0.01227223)

(0.5668 * 1 + 0.4332 * 5.875) * exp(-0.01227223)

1.12 + 51 * exp(-0.01227223)

1/1.2

S_0 <- 50 ; E <- 60 ; u <- 1.2 ; d <- 1/u ; r <- 0.1

k <- log(E/(d^10 * S_0)) / log(u/d)
k

p <- (exp(r) - d) / (u-d)
p ; 1-p

c10 <- rep(0, 11)

for (i in 0:10) {
  print(choose(10, 10-i) * u^(10-i) * d^i * p^(10-i) * (1-p)^i)
  c10[i+1] <- choose(10, 10-i) * u^(10-i) * d^i * p^(10-i) * (1-p)^i
}

c10


for (i in 0:10) {
  print(S_0 * u^(10-i) * d^i)
}
intrinsic <- rep(0, 11)
for (i in 0:10) {
  print(S_0 * u^(10-i) * d^i - 60)
  intrinsic[i+1] <- S_0 * u^(10-i) * d^i - 60
}
intrinsic
hw8 <- rep(0, 4)
for (i in 0:4) {
  hw8[i+1] <- choose(10, 10-i) * intrinsic[i+1] * p^(10-i) * (1-p)^i
}
hw8
sum(hw8) * exp(-1)

####

pdot <- (p * u) / (1 + r)
pdot

50 * pbinom(5, 10, pdot, lower.tail=F) - 60 * exp(-1) * pbinom(5, 10, p, lower.tail=F)

