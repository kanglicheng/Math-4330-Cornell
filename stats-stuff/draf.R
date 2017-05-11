T1 <- c(8.9,1.6,7.9,9.3,3.6,7.2,11.3,8.3)
T2 <- c(6.8,3.9,9.8,2.4,8.8,4.1,6.3,7)

D1 <- c(1.3,-0.4,1.7,-0.5,1,2.6,2.3,3.9)
D2 <- c(-1,-0.7,-1.8,0.8,-0.6,2.3,-1.7,-0.2)

meanT1 <- mean(T1)
meanT2 <- mean(T2)

## equality of residual effect test
## H0: rho1=rho2
diff <- meanT1-meanT2

sT <- sqrt((sd(T1)^2*(length(T1)-1)+sd(T2)^2*(length(T2)-1))/(length(T1)+length(T2)-2))

t.stat <- diff/sT/sqrt(2/8)

qt(1-0.025, df=16-2)

## equality of trt effect
diff <- mean(D1)-mean(D2)

sD <- sqrt((sd(D1)^2+sd(D2)^2)/2)

t.stat <- diff/sD/sqrt(2/8)
t.stat

## 95% CI of trt effect

diff/2 + c(qt(0.025,df=14),qt(0.975,df=14)) * sD/2 * sqrt(2/8)

qnorm(0.0125)
qnorm(2*(1-pnorm(2.24/sqrt(0.7))))
              