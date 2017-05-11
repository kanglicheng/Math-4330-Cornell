pla <- c(55,47,48)
trtA <- c(55,64,64)
trtB <- c(50,44,41)

mean <- mean(c(pla,trtA,trtB))

## df=3-1=2
SSB <- (mean(pla)-mean)^2 * 3 +(mean(trtA)-mean)^2*3 + (mean(trtB)-mean)^2*3
MSB <- SSB/2
## df=9-3
SSW <- sum((pla-mean(pla))^2)+sum((trtA-mean(trtA))^2)+sum((trtB-mean(trtB))^2)
MSW <- SSW/6

F.stat <- MSB/MSW
F.stat

qf(0.975, df1=2,df2=6)
