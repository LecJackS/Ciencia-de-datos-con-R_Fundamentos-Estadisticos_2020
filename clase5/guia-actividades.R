#########################
# 1) Una Moneda
#########################
# a) Simulo moneda
cara_ceca <- function(){
  return(sample(x=c(0,1), size=1))
}

#######
# b) unas ~500 veces de las 1000, o sea, ~50%

#######
# c) Simulo
Nrep=1000
muchas_repeticiones <- rep(NA, Nrep)
experimento.1c <- function(Nrep=1000){
  for(i in 1:Nrep){
    muchas_repeticiones[i] <- cara_ceca()
  }
  return(muchas_repeticiones)
}

muchas_repeticiones <- experimento.1c()
# Sumo unos y cuento caras
sum(muchas_repeticiones)

#########################
# 2) Repito varias monedas
#########################
# a)
repito_moneda <- function(n){
  resul <- rep(NA, n)
  for(i in 1:n){
    resul[i] <- cara_ceca()
  }
  return(resul)
}
repito_moneda(50)

#######
# b)


cuantas_caras <- function(n){
  return(sum(repito_moneda(n)))
}
cuantas_caras(50)

#########################
# 3) Sigue tirando
#########################
# a)
perseverancia_cara <- function(){
  tiradas <- 1
  while(cara_ceca() != 1){
    tiradas <- tiradas + 1
  }
  return(tiradas)
}
perseverancia_cara()

#######
# b)

experimento.2b <- function(Nrep=1000){
  muchas_perseverancia_cara <- rep(NA, Nrep)
  
  for(i in 1:Nrep){
    muchas_perseverancia_cara[i] <- perseverancia_cara()
  }
  
  return(muchas_perseverancia_cara)
}
Nrep=1000
data <- experimento.2b(Nrep)
freq <- data/Nrep
table(freq)

