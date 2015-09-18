## Programmer1: Giovani Carrara Rodrigues 
## Programmer2: Vitor Bonini

## Date: sep/12/2015

## Description: Exercise 1 of Non-parametric methods

## Binomial test

a=0.05 # nível de significancia
n=1600 # número total de pessoas (homens + mulheres)
p0=0.5 # # se p=1/2 então a prob de nascer homens e mulheres é igual
B=860 # poderia ser também = 740(quantidade todal de homens)

# como n é grande vamos padronizar e usar a propriedade assintótica do Teorema do limite Central
B1=(B-n*p0)/sqrt(n*p0*(1-p0))

# vamos fazer o teste bilateral

z_a=qnorm(1-a/2)
z_a

p_valor=round(pnorm(B1,lower=F),4)
p_valor

pdf("bino1.pdf")
plot(c(-4,4),c(-0.05,.45), type="n",main="Teste Binomial com aproximação pela curva Normal", xlab='', ylab="", cex.lab=1.5,axes = F, frame.plot = F)
abline(h=0,lwd=1)
curve(dnorm(x),lwd=2,from=-4,to=4,add=T)
As=seq(z_a,4,len=10)
polygon(c(z_a,As,3),c(0,dnorm(As),0),col="gray")
text(0,0.15,expression(1-alpha))
text(2.15,0.02,expression(alpha/2))
text(z_a,-.02,bquote(z[alpha/2] == .(round(z_a,2))))
# meus comandos
Bs=seq(-4,-z_a,len=10)
polygon(c(-3,Bs,-z_a),c(0,dnorm(Bs),0),col="gray")
text(-2.15,0.02,expression(alpha/2))
text(-z_a,-.02,bquote(z[alpha/2] == .(round(-z_a,2))))

#comandos para desenhar o p_valor no gráfico
# à direita
Cs=seq(B1,4,len=10)
polygon(c(B1,Cs,4),c(0,dnorm(Cs),0),col="green")

text(3.2,0.025,expression(p/2))
text((B1+0.2),-.02,bquote(z["Obs"] == .(round(B1,2))))
# à esquerda
Ds=seq(-4,-B1,len=10)
polygon(c(-4,Ds,-B1),c(0,dnorm(Ds),0),col="green")

text(-3.2,0.025,expression(p/2))

text(-(B1+0.2),-.02,bquote(z["Obs"] == .(round(-B1,2))))

dev.off()

# Pelo gráfico vemos que o p-valor é menor do que z_a ou seja, rejeitamos a hipótese H0.

# Vamos agora fazer o teste Qui-Quadrado de Pearson

## Chi-squared test

#Obs: as frequencias esperadas vem de uma distribuição binomial(n=5,p=1/2)

n=320 # numero de familias

Ei=c(0.03125,0.15625,0.3125,0.3125,0.15625,0.03125) # frequencias esperadas

Oi=c(0.05625,0.175,0.34375,0.275,0.125,0.025) # frequências Observadas

k=length(Oi) # quantidade de frequências esperadas

Q=sum(((Oi-Ei)^2)/Ei) # Estatística de teste

p_valor=pchisq(Q,k-1,lower=F)
p_valor

a=0.05
qa=qchisq(1-a,k-1)
qa

pdf("qui1.pdf")

plot(c(0,20),c(-0.05,0.2), type="n",main="Teste Qui-Quadrado de Pearson", xlab='', ylab="", cex.lab=1.5,axes = F, frame.plot = F)
abline(h=0,lwd=1)
curve(dchisq(x,k-1),lwd=2,from=0,to=20,add=T)
# Descomente para desenhar a area do p-valor
#Bs=seq(Q,30,len=60)
#polygon(c(Q,Bs,20),c(0,dchisq(Bs,k-1),0),col="green")
As=seq(qa,30,len=10)
polygon(c(qa,As,20),c(0,dchisq(As,k-1),0),col="gray")
text(4,0.05,expression(1-alpha))
text(10.5,0.01,expression(alpha))
text(qa,-.01,bquote(chi[.(k-1)]^2 == .(round(qa,2))))

dev.off()