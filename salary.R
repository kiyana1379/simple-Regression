salary=read.csv("C:/Users/NoteBook/Desktop/Alzahra/Salary_dataset.CSV")
attach(salary)
plot(Salary~YearsExperience,col="purple")
lm.S=lm(Salary~YearsExperience)
lm.S
abline(lm.S,lty="dotted",col="black")
fitted(lm.S)
residuals(lm.S)
model.o=lm(Salary~0+YearsExperience)
model.o
plot(Salary~0+YearsExperience,col="brown")
abline(model.o)
par(mfrow=c(1,2))
##########Confidence interval for beta1hat
#first solution
confint(lm.S)[2,]
#second solution
n=length(YearsExperience)
n
beta1hat=lm.S$coefficients[2]
beta1hat
MSE=sum(residuals(lm.S)^2)/(n-2)
MSE
SSX=(n-1)*var(YearsExperience)
SSX
CI=c(beta1hat-qt(0.975,(n-2))*sqrt(MSE/SSX),beta1hat+qt(0.975,(n-2)))
CI
names(CI)=c("lowers","uppers");CI
summary(lm.S)
summary(lm.S)$coefficients
summary(lm.S)$coefficients[2,4]
#We want to calculate pvalue without summary
#And we also want to do statistical hypothesis
t01=beta1hat/(sqrt(MSE/SSX))
t01
pvalue1=2*(1-pt(abs(t01),n-2))
pvalue1
#confident interval for beta0hat
confint(lm.S)[1,]
n=length(YearsExperience)
beta0hat=lm.S$coefficients[1]
beta0hat
MSE=sum(residuals(lm.S)^2)/(n-2)
MSE
SSX=(n-1)*var(YearsExperience)
SSX
xbar=mean(YearsExperience)
c(beta0hat-qt(0.975,(n-2))*sqrt(MSE*((1/n)+xbar^2/SSX)),beta0hat+qt(0.975,(n-2))*sqrt(MSE*((1/n)+xbar^2/SSX)))
#statistical hypothetic for trying beta0 to if its 0 or no
t00=(beta0hat)/(sqrt(MSE*((1/n)+xbar^2/SSX)))
t00
pvalue2=2*(1-pt(t00,n-2))
pvalue2
#regression which elusive from provenance(mabda)
betahat=coef(model.o)
e=residuals(model.o)
MSE1=t(e)%*%e/(n-1)
SSX=sum(YearsExperience^2)
SSX
c(betahat-qt(0.975,n-1)*sqrt(MSE/SSX),betahat+qt(0.975,n-1)*sqrt(MSE/SSX))


hist(trees$Height,probability=TRUE,labels=TRUE)
qqnorm(trees$Height)
qqline(trees$Height)
boxplot(trees$Volume)




install.packages("datarium")
library(datarium)
marketing
head(marketing, 4)
