
##Kang-Li Cheng p8111 final project code

library(car)
library(dplyr)
library(HH)
library("leaps")
library(boot)
library(ggplot2)
library(nnet)
library(glmnet)
require(gridExtra)
library(caTools)

data = read.csv("lawsuit.csv")
drops = c("ID")
data$lnavgSal = log((data$Sal95+data$Sal94)/2)
data[, !(names(data) %in% drops)]
data$Sal94 <- log(data$Sal94)
#plot(Sal94)
#hist(Sal94)



#data_lawsuit$avesal <- (data_lawsuit$Sal94+data_lawsuit$Sal95)/2
#data organization and processing

data$Gender = ifelse(data$Gender==0, "Female", "Male") 
data$Dept= factor(data$Dept, labels=c("Biochem", "Physiology", "Genetics", "Pediatrics", "Medicine", "Surgery"))
data$Rank=factor(data$Rank, labels=c("Assistant", "Associate", "Full"))

par(mfrow=c(1,2))
p=ggplot(data, aes(x=Rank, y=Sal94), fill=Gender)+geom_boxplot(aes(fill=Gender))+scale_x_discrete(name = "Rank") + scale_y_continuous(name="1994 Salary")+ggtitle("Boxplot Salary by Rank")+scale_fill_manual(values=c("#FF3366", "#3399FF"))


p2=ggplot(data, aes(x=Dept, y=Sal94), fill=Gender)+geom_boxplot(aes(fill=Gender))+scale_x_discrete(name = "Department") + scale_y_continuous(name="1994 Salary")+ggtitle("Boxplot Salary by Department")+scale_fill_manual(values=c("#FF3366", "#3399FF"))
#grid.arrange(p, p2, ncol=2)


# categorical variable

data$Clin<- as.factor(data$Clin)
data$Cert <- as.factor(data$Cert)
data$Rank <- as.factor(data$Rank)
data$Dept = as.factor(data$Dept)


#t test for two groups
t.test(data_f$Prate, data_m$Prate)
t.test(data_f$Exper, data_m$Exper)



#model
full1 <- lm(lnavgSal~ Dept+Gender+Clin+Prate+Exper+Rank+Cert, data=data)
summary(full1)

fit2=lm(lnavgSal~Dept+Gender+Clin+Exper+Rank+Cert, data=data)
#prate p-value > 0.05, delete

#stepwise not used in the end, want to preserve gender in model
#step(full1, direction='backward')
#plot(full)  #82, 122, 184

vif(full)
influence.measures(full)


#check for interaction terms
f1 <- lm(lnavgSal~Dept + Gender+Prate+Exper+Rank+Cert+Gender*Dept, data=data)
anova(full1, f1) #dept no interaction with gender

final <-lm(lnavgSal~ Dept+Gender+Exper+Rank+Cert+Gender*Rank, data=data)
anova(full1,final) # Rank and gender interaction is significant
summary(final)  #Chosen as final model


#predict the salaries
set.seed(101)
require(caTools)
sample=sample.split(data,SplitRatio=0.25)
train=data[sample, 1:11]
test=data[!sample, 1:11]

#train$Gender= ifelse(train$Gender==0,"Female", "Male")

m1=lm(lnavgSal~Dept+Gender+Exper+Rank+Cert+Gender*Rank, data=train)


#test$Gender = ifelse(test$Gender==0, "Female", "Male") 

#train$Gender = ifelse(train$Gender==0, "Female", "Male")
femaleTest=test[which(test$Gender=="Female"),]
maleTest=test[which(test$Gender=="Male"),]

salPredictF= predict(m1, newdata=femaleTest)
salPredictM=predict(m1, newdata=maleTest)

