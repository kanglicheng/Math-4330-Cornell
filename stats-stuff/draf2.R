test <- c(93,78,108,154,131,71)
ref <- c(86,72,102,138,120,81)

logt <- log(test)
logr <- log(ref)
mean(logt)
sd(logt)
mean(logr)
sd(logr)
sP <- sqrt((sd(logt)^2+sd(logr)^2)/2)
sP

t.stat <- (mean(logt)-mean(logr))/sP/sqrt(1/6+1/6)
t.stat

qt(0.95,df=12-2)

exp((mean(logt)-mean(logr)) + c(-1,1) * qt(0.95,df=10) * sP * sqrt(1/3))

T1 <- c(1.2,-0.7,-0.6,1.7,0.7,2.7)
T2 <- c(2.2,-1.3,0.6,-1.1,-1.4,-1.2,-2.2,-2.0)

D1 <- c(-0.8,0.7,-1.0,-0.5,-0.1,0.3)
D2 <- c(0.4,-3.3,-0.6,-0.5,0.6,-4.6,-1.6,-3.8)

diffT <- mean(T1)-mean(T2)

sT <- sqrt((sd(T1)^2*5+sd(T2)^2*7)/12)

t.stat <- diffT/sT/sqrt(1/6+1/8)
t.stat

qt(0.975,df=12)

## fail to reject

diffD <- 0.5*(mean(D1)-mean(D2))

sD <- 0.5*sqrt((sd(D1)^2*5+sd(D2)^2*7)/12)

t.stat <- diffD/sD/sqrt(1/6+1/8)
t.stat


