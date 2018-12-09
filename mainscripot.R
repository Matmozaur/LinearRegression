install.packages("faraway")
library(faraway)
A<-read.csv("newhamp.csv",header = TRUE)
F<-A$X
B<-subset(A,select = -X)
row.names(B)<-F
attach(B)


plot(B)
reg=lm(log(pObama)~.,B)
summary(reg)
qqnorm(reg$res)
qqline(reg$res)
boxplot(dem,main="dem")
boxplot(povrate,main="povrate")
boxplot(pci,main="pci")
boxplot(Dean,main="Dean")
boxplot(Kerry,main="Kerry")
boxplot(white,main="white")
boxplot(absentee,main="absentee")
boxplot(population,main="population")
boxplot(pObama,main="pObama")

B<-B[-21,]
reg=lm(pObama~.,B)
summary(reg)
attach(B)
boxplot(dem,main="dem")
boxplot(log(dem),main="dem")
boxplot(povrate,main="povrate")
boxplot(pci,main="pci")
boxplot(log(pci),main="pci")
boxplot(Dean,main="Dean")
boxplot(Kerry,main="Kerry")
boxplot(white,main="white")
boxplot((white),main="white")
boxplot(absentee,main="absentee")
boxplot(log(absentee),main="absentee")
boxplot(population,main="population")
boxplot(log(population),main="population")
boxplot(pObama,main="pObama")

reg2=lm(pObama~+log(dem)+povrate+log(pci)+Dean+Kerry+white+log(absentee)+log(population),B)
summary(reg2)
reg=lm(pObama~.,B)
summary(reg)
qqnorm(reg2$res)
qqline(reg2$res)
shapiro.test(reg2$res)

C<-as.data.frame(cbind("log(dem)"=log(dem),povrate,"log(pci)"=log(pci),Dean,Kerry,white,"log(absentee)"=log(absentee),"log(population)"=log(population),pObama))
C
attach(C)
reg<-lm(pObama~.,C)
summary(reg)
cor(C)
plot(reg$res~.,C)
reg3=lm(pObama~.+povrate*logpopulation,C)
summary(reg3)
step(reg,direction="backward")

reg=lm(formula = pObama ~ povrate + `log(pci)` + Dean + Kerry + white + 
     `log(absentee)`, data = C)
summary(reg)
qqnorm(reg$res)
qqline(reg$res)
shapiro.test(reg$res)
plot(reg$res~povrate,C)
plot(reg$res~log(pci),C)
plot(reg$res~Dean,C)
plot(reg$res~Kerry,C)
plot(reg$res~white,C)
plot(reg$res~`log(absentee)`,C)
plot(reg$res~Kerry,C)
plot(reg$res~log(dem),C)
plot(reg$res)


install.packages("leaps")
library(leaps)
x<-model.frame(reg2)[,-1]
y<-model.frame(reg2)[,1]
leaps(x,y,method="Cp")->cp
Cpplot(cp)
sort(hatvalues(reg))

D<-C[-85,]
attach(D)
reg5<-lm(pObama~povrate + `log(pci)` + Dean + Kerry + white + 
           `log(absentee)`,data=D)
summary(reg5)
?rstudent
rstudent(reg)
plot(rstudent(reg))
summary(reg)
C

summary(reg)
K[1][2]<-plot(reg$res~I(Kerry*Dean))
K<-as.list(nrow(8),ncol(8))
for(i in 1:8){
  for(j in 1:8){
    paste("plot",i,j,".png",sep="")
    png(filename=paste("plot",i,j,".png",sep=""))
    plot(reg$res~I(C[[i]]*C[[j]]),xlab=paste("I(",i,"*",j,")",sep=""))
    dev.off()
  }
}


data("newhamp")
write.csv(newhamp,file="nHtest.csv")
TestD<-read.csv("nHtest.csv")
TestD
attach(TestD)
TestDnew<-as.data.frame(cbind(povrate,"log(pci)"=log(pci),Dean,Kerry,white,"log(absentee)"=log(absentee),pObama))
TestDnew
pred1<-predict(reg,newdata = TestDnew)
rmse <- sqrt(sum((pred1 - TestDnew$pObama)^2)/length(TestDnew$pObama))
rmse
par(mfrow=c(1,1))
plot(pred1,TestDnew$pObama)
abline(a=0,b=1)
