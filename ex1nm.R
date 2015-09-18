#exer1 new method

if(!require("combinat")){ 
  install.packages("combinat")
  library(combinat)
}

# criando o dataset
clas1=c(5,0,18);clas2=c(4,1,56);clas3=c(3,2,110)
clas4=c(2,3,88);clas5=c(1,4,40);clas6=c(0,5,8)

data = data.frame(clas1,clas2,clas3,clas4,clas5,clas6,c(0,0,sum(clas1[3],clas2[3],clas3[3],clas4[3],clas5[3],clas6[3])))
names(data)=c("clas1","clas2","clas3","clas4","clas5","clas6","total")
row.names(data)=c("m","h","nfami")
attach(data)
data

#frequencias observadas
fob=data[3,1:6]
fob

#frequencias experadas
aux=0:5
prob=dbinom(aux,5,0.5)
prob
fexp=prob*320
fexp


totalh=sum(data[3,1:6]*data[2,1:6]) # total de homens

totalm=sum(data[3,1:6]*data[1,1:6]) # total de mulheres

fh=totalh/(totalh+totalm) # porcentagem de homens 
fh


# teste qui-quadrado

n=320 # numero de familias

Ei=fexp # frequencias esperadas

Oi=fob # frequências Observadas

k=length(Oi) # quantidade de frequências esperadas

Q=sum(((Oi-Ei)^2)/Ei) # Estatística de teste

p_valor=pchisq(Q,k-1,lower=F) # calculando o p-valor
p_valor

a=0.05
qa=qchisq(1-a,k-1) # quantil teorico com alpha=0.05
qa


plot(c(0,20),c(-0.05,0.2), type="n",main="Teste Qui-Quadrado de Pearson", xlab='', ylab="", cex.lab=1.5,axes = F, frame.plot = F)
abline(h=0,lwd=1)
curve(dchisq(x,k-1),lwd=2,from=0,to=20,add=T)
#Descomente para desenhar a area do p-valor
As=seq(qa,30,len=10)
polygon(c(qa,As,20),c(0,dchisq(As,k-1),0),col="gray")
text(4,0.05,expression(1-alpha))
text(10.5,0.01,expression(alpha))
text(qa,-.01,bquote(chi[.(k-1)]^2 == .(round(qa,2))))
Bs=seq(Q,30,len=60)
polygon(c(Q,Bs,20),c(0,dchisq(Bs,k-1),0),col="green")

text(12.5,0.006,expression(p))
text(Q+1,-.01,bquote(p[.obs] == .(round(Q,2))))

# recalculando as frequencias experadas
aux2=0:5
prob2=dbinom(aux,5,fh)
prob2
fexp2=prob2*320
fexp2 # nova frequencia experada

# teste qui-quadrado 2

n=320 # numero de familias

Ei=fexp2 # frequencias esperadas

Oi=fob # frequências Observadas

k=length(Oi) # quantidade de frequências esperadas

Q=sum(((Oi-Ei)^2)/Ei) # Estatística de teste

p_valor=pchisq(Q,k-1,lower=F) # calculando o p-valor
p_valor

a=0.05
qa=qchisq(1-a,k-1) # quantil teorico com alpha=0.05
qa


plot(c(0,20),c(-0.05,0.2), type="n",main="Teste Qui-Quadrado de Pearson", xlab='', ylab="", cex.lab=1.5,axes = F, frame.plot = F)
abline(h=0,lwd=1)
curve(dchisq(x,k-1),lwd=2,from=0,to=20,add=T)
#Descomente para desenhar a area do p-valor
Bs=seq(Q,30,len=60)
polygon(c(Q,Bs,20),c(0,dchisq(Bs,k-1),0),col="green")
text(6.5,0.04,expression(p))
text(Q+1,-.01,bquote(p[.obs] == .(round(Q,2))))
As=seq(qa,30,len=10)
polygon(c(qa,As,20),c(0,dchisq(As,k-1),0),col="gray")
text(2,0.07,expression(1-alpha))
text(11.5,0.01,expression(alpha))
text(qa,-.01,bquote(chi[.(k-1)]^2 == .(round(qa,2))))




