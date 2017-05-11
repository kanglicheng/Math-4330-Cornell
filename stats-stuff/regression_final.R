
library(car)
library(dplyr)
library(HH)
library("leaps")
library(boot)
library(ggplot2)
library(nnet)
library(glmnet)


data = read.csv("lawsuit.csv")

#plot(Sal94)
#hist(Sal94)



#data_lawsuit$avesal <- (data_lawsuit$Sal94+data_lawsuit$Sal95)/2
par(mfrow=c(1,2))
data$Gender = ifelse(data$Gender==0, "Female", "Male") 

p=ggplot


hist(data$Sal94)
plot(data$Sal94)
#after log more nomal distribution in salary


# categorical variable
data_lawsuit$Dept <- as.factor(data_lawsuit$Dept)
data_lawsuit$Gender <- as.factor(data_lawsuit$Gender)
data_lawsuit$Clin <- as.factor(data_lawsuit$Clin)
data_lawsuit$Cert <- as.factor(data_lawsuit$Cert)
data_lawsuit$Rank <- as.factor(data_lawsuit$Rank)


#new dataset with the variables 
data_new<- subset(data_lawsuit, select=c(2:8,12)) 
data_new$Rank = ifelse(data_new$Rank=="1", "Assistant", ifelse(data_new$Rank=="2", "Associate", "Full Professor"))


#group into F and M
data_f <- filter(data_lawsuit, Gender=="Female")
data_m <- filter(data_lawsuit, Gender=="Male")
summary(data_f)
summary(data_m)
sd(data_f$Prate)
sd(data_m$Prate)
sd(data_f$Exper)
sd(data_m$Exper)
sd(data_m$Sal94)
sd(data_f$Sal94)
sd(data_m$Sal95)
sd(data_f$Sal95)

summary(data_lawsuit)
sd(data_new$Prate)
sd(data_new$Exper)
sd(data_lawsuit$Sal94)
sd(data_lawsuit$Sal95)


#t test for two groups
t.test(data_f$Prate, data_m$Prate)
t.test(data_f$Exper, data_m$Exper)

# chi-sq for categorical variable and gender
chisq.test(data_new$Dept,data_new$Gender) #dependent
chisq.test(data_new$Clin, data_new$Gender)  ##indeoendent
chisq.test(data_new$Cert, data_new$Gender)  ##independent
chisq.test(data_new$Rank, data_new$Gender) #dependent



#descriptive 
plot(data_lawsuit$avesal)
hist(data_lawsuit$avesal) #so log transformation
pairs(data_lawsuit[2:10])
plot(data_new$Prate)
plot(data_new$Exper)

scatter = ggplot(data=data_new, aes(x=Gender,y=lnavesal))
scatter + geom_boxplot(aes(color=Gender, shape=Gender,fill=Gender),col="dark grey")+
  xlab("Gender")+ylab("ln(salary of 94 and 95)")+
  theme_bw()+theme(panel.grid=element_blank())


sc = ggplot(data=data_new, aes(x=Exper,y=lnavesal))
sc + geom_point(aes(color=Gender, shape=Gender))+xlab("Exper")+ylab("ln(salary of 94 and 95)")+
  ggtitle("Scatterplot with smoothers")+geom_smooth(aes(color=Gender),method="lm")+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))+theme(panel.grid=element_blank())

sc1 = ggplot(data=data_new, aes(x=Rank,y=lnavesal))
sc1 + geom_boxplot(aes(fill=Gender, shape=Gender), col="dark grey")+xlab("Rank")+ylab("ln(salary of 94 and 95)")+
  ggtitle("Boxplot for Rank and Salary")+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))+theme(panel.grid=element_blank())

sc3 = ggplot(data=data_new, aes(x=Rank,..count..))
sc3 + geom_bar(aes(fill=Gender, shape=Gender),position="dodge", col="dark grey")+xlab("Rank")+ylab("Number of People")+
  ggtitle("Number of People for ")+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))+theme(panel.grid=element_blank())


scatter1 =ggplot(data=data_new, aes(x=Gender, y=Exper))
scatter1 + geom_boxplot(aes(color=Gender, shape=Gender))+xlab("Gender")+ylab("Publication Rate")
#outlier

scatter1 =ggplot(data=data_new, aes(x=Gender, y=Rank))
scatter1 + geom_histogram(aes(color=Gender, shape=Gender,fill=Gender),col="grey")+xlab("Gender")+ylab("Rank")


#vol = ggplot(data = data_new, aes(x=Exper))
#vol+ stat_density(aes(ymax=..density.., ymin=-..density, fill=Gender),color="black", geom="ribbon", position="identity")



#model
full1 <- lm(lnavesal~., data=data_new)
summary(full1)
#prate p-value > 0.05, delete

step(full, direction='backward')
plot(full)  #82, 122, 184
#prate is include, AIC chosen
vif(full)
influence.measures(full)

attach(data_new)
#check the interaction term
f1 <- lm(lnavesal~.+Gender*Dept, data=data_new)
anova(full1, f1) #dept is not interacted with gender 
f2 <- lm(lnavesal~.+Gender*Clin, data=data_new)
summary(f2) #Clinics is not interacted with gender
f3 <- lm(lnavesal~.+Gender*Cert, data=data_new)
anova(full1, f3) #Cert is not interact 
f4 <- lm(lnavesal~.+Gender*Prate, data=data_new)
summary(f4) #Prate is not  interact
f5 <- lm(lnavesal~.+Gender*Exper, data=data_new)
anova(full1,f5) # Exper is interacted with gender＃＃＃＃

f6 <-lm(lnavesal~.+Gender*Rank, data=data_new)
anova(full1,f6) # Rank is interacted with gender
summary(f6)  #final model-f6

f7 <- lm(lnavesal~Gender+Dept+Clin+Cert+Rank+Exper+Gender*Rank+Gender*Exper,data=data_new)
anova(f7)  # Only exper is interacted with gender  
 

summary(lm(lnavesal~.-(ID+Sal94+Sal95+avesal+Prate), data=data_lawsuit)) #delete Prate
cor(Prate, lnavesal) #-0.768, highly correlated, not delete


#diagnosis
par(mfrow=c(2,2))
plot(f5) #184, 56, 122 are the outliers.

#get rid of 184th observation
#data_no184 <- data_new[-184,]
#model_no184 <- lm(lnavesal~.+Gender*Rank, data=data_no184)
#summary(model_no184)

model_no184_Prate <- lm(lnavesal~.-Prate+Gender*Rank, data=data_no184)
summary(model_no184_Prate)

#model_inter_no184 <- lm(lnavesal~.+Prate*Rank+Gender*Exper,data=data_no184)
#summary(model_inter_no184) no interaction between prate&rank

fit.model <- model_no184_Prate  #final model no 184 record and no Prate
plot(fit.model) 
#vif(fit.model)

#leaps(x = data_no184[,1:7],y = data_no184[,8], method="Cp")


#full2 <- lm(avesal~.-(ID+Sal94+Sal95+lnavesal), data=data_lawsuit)
#summary(full2)
par(mfrow=c(2,2))
plot(f6)
#violate the linear model assumption, so logy is needed
boxCox(full2)  #lamada=0 -> log(y) is better


#Multinominal logisitic regression
#l1 <- multinom(Rank~Gender+Dept+Cert+Clin+Exper, data=data_new)
#summary(l1)

#use rank as the outcome
data_new1 <- data_lawsuit[,1:8]
x<-model.matrix(~., data=data_new1[,-8])
data_new1$Rank <- ifelse(data_new1$Rank)
y<-data_new1[,8]
#rmodel<- glm(Rank~Dept+Gender+Clin+Cert+Prate+Exper, data=data_new, family="binomial")-wrong


mixor(Rank~Dept+Gender+Clin+Cert+Prate+Exper, data_new1, link="logit")
r1 <- mixor(y~x, data_new1, id=ID, link="logit", indep.re=TRUE)
summary(r1)

library(MASS)
r2 <-polr(factor(Rank)~.-lnavesal, data_new, Hess=FALSE)
summary(r2)
vif

# with rank
full2 <- lm(lnavesal~.-Prate, data=data_new)
summary(full2) #Gender is not significant associated
anova(full2)  #Rank is associated with salary
vif(full2)
