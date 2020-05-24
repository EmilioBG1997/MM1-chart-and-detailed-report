#Un lavacarros puede atender un auto cada 5 minutos y la tasa media de llegadas es de 9 autos por hora. 
#a) Obtenga las medidas de desempeño de acuerdo con el modelo M/M/1. 
#b) Obtenga la probabilidad de tener 0 clientes en el sistema 
#c) Obtenga la probabilidad de tener más de 3 clientes en el sistema
#d) Obtenga la probabilidad de esperar más de 30 minutos en la cola
#e) Obtenga la probabilidad de esperar más de 30 minutos en el sistema
library("queueing") #Podemos identificar inmediatamente que es un problema de colas
lbda =  9/60 # Razon de llegada (9 autos cada 60 min)
mu = 1/5 #tasa de servicio (1 auto cada 5 min)

#Inciso A)
i <- NewInput.MM1(lambda = lbda, mu = mu, n=0)
x <- QueueingModel(i)
print("Factor de Uso")
print(RO(x)) ## Obtiene el factor de uso
#Inciso B)
print("Probabilidad de tener 0 clientes")
print(Pn(x)) ## probabilidad de tener 0 clientes en el sistema

#Inciso C)
print("probabilidad de que haya mas de 3 clientes en el sis.")
i <- NewInput.MM1(lambda = lbda, mu = mu, n=3)
x <- QueueingModel(i)
pmi3 = Pn(x)[1] #Probabilidad de 0
pmi3 = pmi3+Pn(x)[2] #ac mas probabilidad de 1
pmi3 = pmi3+Pn(x)[3] #ac mas probabilidad de 2
pmi3 = pmi3+Pn(x)[4] #ac mas probabilidad de 3
pm3 = 1-pmi3 #Probabilidad de que sea mayor a 3
print(pm3)

#Inciso D)
ro = RO(x) #calculo de ro
eul = exp(1)^(-(mu)*(1-ro)*30)
pwqmay30 =  ro*eul
print("probabilidad de que el tiempo de espera de cola sea mayor a 30 min")
print(pwqmay30)
print("probabilidad de que el tiempo de espera de sis sea mayor a 30 min")
print(eul) #Inciso E)
