#Codigo para problema 2
#Codigo para problema 2
mis_dades<-iris
mis_dades
dim(mis_dades)
names(mis_dades)

mean(mis_dades$Petal.Length)
sd(mis_dades$Petal.Length)
hist(mis_dades$Petal.Length)

x<-mis_dades$Petal.Length
y<-mis_dades$Sepal.Length
plot(x,y)

m<-sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2);m
b<-mean(y)-m*mean(x)

m*1.5+b

mod<-lm(y~x);mod
summary(mod)

ypredict<- predict(mod,data.frame(x=x))

plot(x,y,col='red',pch=16)
lines(x,ypredict,col='black')

#coeficiente de determinacion
Rsq<-sum((ypredict-mean(y))^2)/sum((y-mean(y))^2);Rsq


#Problema 1 preguntas
sqrt(0.1923)
m2<-0.06576
b2<-11.797555
r<-647.1*m2+b2;r

data<-read.table('coches.txt',header=TRUE,sep='\t')
