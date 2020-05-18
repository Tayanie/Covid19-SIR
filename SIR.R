brasil=read.csv2("Brasil.csv",sep = ",") #até 04/05/20
head(brasil) 
brasil=brasil[36:nrow(brasil),] #Iniciando os dados no dia 26/02 - primeiro caso confirmado
brasil$dia=1:nrow(brasil)
brasil2=brasil[50:69,] #20 últimos dias 4/15/20 a 5/4/20
brasil2

0.17/0.095
N=210147125 #N=210147125 (IBGE - 2019)
beta=0.1  
S=1
I=brasil2[1,3]/N
R=0
eqm=NULL
alfa=seq(0.15,0.7,by=0.02)
n=length(brasil2$casos)-1

for(j in 1:length(alfa)){
for(i in 1:n){
  S[i+1]=S[i]-alfa[j]*S[i]*I[i]
  I[i+1]=I[i]+alfa[j]*S[i]*I[i]-beta*I[i]
  R[i+1]=R[i]+beta*I[i]
}
  eqm[j]=mean((brasil2$casos-N*I)^2)
}

names(eqm)=alfa
names(eqm)[which.min(eqm)] #alfa=0.17 
min(eqm) #EQM de 20942774

alfa=0.17
beta=0.1
S=1
I=brasil2[1,3]/N
R=0
for( i in 1:n){
  S[i+1]=S[i]-alfa*S[i]*I[i]
  I[i+1]=I[i]+alfa*S[i]*I[i]-beta*I[i]
  R[i+1]=R[i]+beta*I[i]
}

with(brasil2, plot(dia,casos/1000, pch = 19, col = "red",xlab = "dias", ylab = "número de casos/1000"))
lines(brasil2$dia[1]:brasil2$dia[n+1], (I*N/1000), col = "red")
brasil2$casos

R0=alfa/beta
#Verificou-se que a reta está bem ajustada, porém, ao final
#da curva subestima-se o nº de casos.
#Com os parâmetros iniciais chegamos a R0=1,7
#Isso significa que cada pessoa infectada contagia 1,7 outras
#Contudo, há indícios de que a taxa aumentou, talvez por relaxamento
#das medidas de isolamento
#Desta forma, iremos diminuir o valor de beta tal que R0 aumente
#visando ajustar melhor a reta

alfa=0.17
beta=seq(0.085,0.099,by=0.001)
S=1
I=brasil2[1,3]/N
R=0
eqm=NULL
for(j in 1:length(beta)){
  for(i in 1:n){
    S[i+1]=S[i]-alfa*S[i]*I[i]
    I[i+1]=I[i]+alfa*S[i]*I[i]-beta[j]*I[i]
    R[i+1]=R[i]+beta[j]*I[i]
  }
  eqm[j]=mean((brasil2$casos-N*I)^2)
}
names(eqm)=beta
names(eqm)[which.min(eqm)] #beta=0.95
min(eqm) #EQM de 2113067

#### Plotar a curva

alfa=0.17
beta=0.095
S=1
I=brasil2[1,3]/N
R=0
for( i in 1:n){
  S[i+1]=S[i]-alfa*S[i]*I[i]
  I[i+1]=I[i]+alfa*S[i]*I[i]-beta*I[i]
  R[i+1]=R[i]+beta*I[i]
}

with(brasil2, plot(dia,casos/1000, pch = 19, col = "red",xlab = "dias", ylab = "número de casos/1000"))
lines(brasil2$dia[1]:brasil2$dia[n+1], (I*N/1000), col = "red")
brasil2$casos

#A curva se ajustou melhor aos dados
#Logo, escolheu-se os parâmetros a=0.17 e beta=0.095
#O que nos dá R0=1.789474

#Previsão curto prazo, 5 dias
alfa=0.17
beta=0.095
S=1
I=brasil2[1,3]/N
R=0
for( i in 1:24){
  S[i+1]=S[i]-alfa*S[i]*I[i]
  I[i+1]=I[i]+alfa*S[i]*I[i]-beta*I[i]
  R[i+1]=R[i]+beta*I[i]
}

#Estimou-se para os próximos 5 dias
#29155.89 138818.68 149201.94 160358.98 172347.05

plot(50:74,N*I/1000, pch = 19, col = "red",xlab = "dias", ylab = "número de casos previstos/1000")

#Previsão para o pico da pandemia
alfa=0.17
beta=0.095
S=1
I=brasil2[1,3]/N
R=0
for( i in 1:200){
  S[i+1]=S[i]-alfa*S[i]*I[i]
  I[i+1]=I[i]+alfa*S[i]*I[i]-beta*I[i]
  R[i+1]=R[i]+beta*I[i]
}

plot(50:250,N*I/1000, pch = 19, col = "red",xlab = "dias", ylab = "número de casos previstos/1000")
a=N*I/1000
d=1:151
names(a)=d
names(a)[which.max(a)] #117dias [22 de junho]   24866.68*1000 pessoas 

###################################################
##Previsão antes da quarentena###
#Estimação para o pico da pandemia

brasil2=brasil[1:15,] #Primeiros 15 dias 26/02/202 a 11/03
brasil2

N=210147125 
beta=0.095
S=1
I=brasil2[1,3]/N
R=0
eqm=NULL
alfa=seq(0.15,0.7,by=0.02)
n=length(brasil2$casos)-1

for(j in 1:length(alfa)){
  for(i in 1:n){
    S[i+1]=S[i]-alfa[j]*S[i]*I[i]
    I[i+1]=I[i]+alfa[j]*S[i]*I[i]-beta*I[i]
    R[i+1]=R[i]+beta*I[i]
  }
  eqm[j]=mean((brasil2$casos-N*I)^2)
}

names(eqm)=alfa
names(eqm)[which.min(eqm)] #alfa=0.39 
min(eqm) #EQM de 3.970148

##
alfa=0.39
beta=0.095
alfa/beta
S=1
I=brasil2[1,3]/N
R=0
for( i in 1:n){
  S[i+1]=S[i]-alfa*S[i]*I[i]
  I[i+1]=I[i]+alfa*S[i]*I[i]-beta*I[i]
  R[i+1]=R[i]+beta*I[i]
}

with(brasil2, plot(dia,casos, pch = 19, col = "red",xlab = "dias", ylab = "número de casos"))
lines(brasil2$dia[1]:brasil2$dia[n+1], (I*N), col = "red")

#Previsão longo prazo antes da quarentena
alfa=0.39
beta=0.095
S=1
I=brasil2[1,3]/N
R=0
for( i in 1:150){
  S[i+1]=S[i]-alfa*S[i]*I[i]
  I[i+1]=I[i]+alfa*S[i]*I[i]-beta*I[i]
  R[i+1]=R[i]+beta*I[i]
}


R0=alfa/beta #4.105


plot(1:151,N*I/1000,pch = 19, col = "red",xlab = "dias", ylab = "número de casos previstos/1000")
a=N*I/1000
d=1:151
names(a)=d
names(a)[which.max(a)] #79 dias [14 de  maio] 90931.58*1000 pessoas

