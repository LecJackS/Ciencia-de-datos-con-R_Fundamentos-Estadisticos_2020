
intervalo.mu.asin<-function(x,alfa)
{
  IC<-c(mean(x)-qnorm(1-alfa/2)*sd(x)/sqrt(length(x)),mean(x)+qnorm(1-alfa/2)*sd(x)/sqrt(length(x)))
  return(IC)
}

# Cubrimiento empirico
Nrep<-1000
n<-1000
sigma<-0.1
alfa<-0.05
long<-c()
incluye<-c()
for(i in 1:Nrep)
{ 
  x<-rnorm(n,0,sigma)
  ic<-intervalo.mu.asin(x,alfa)
  long[i]<-ic[2]-ic[1]
  incluye[i]<-(ic[1]<0)*(ic[2]>0)
}


mean(incluye)

### AHORA CON UNIFORMES


#4
intervalo.mu.exacto.normal<-function(x,alfa)
{
  gl<-length(x)-1
  IC<-c(mean(x)-qt(1-alfa/2,gl)*sd(x)/sqrt(length(x)),mean(x)+qt(1-alfa/2,gl)*sd(x)/sqrt(length(x)))
  return(IC)
}


# Cubrimiento empirico
Nrep<-1000
n<-5
sigma<-0.1
alfa<-0.05
long<-c()
incluye<-c()
for(i in 1:Nrep)
{ 
  x<-rnorm(n,0,sigma)
  ic<-intervalo.mu.exacto.normal(x,alfa)
  long[i]<-ic[2]-ic[1]
  incluye[i]<-(ic[1]<0)*(ic[2]>0)
}

mean(incluye)


#Diferencia de medias

dif.mu.interseccion<-function(x,y,alfa)
{
  gl<-length(x)-1
  ICx<-c(mean(x)-qt(1-alfa/2,gl)*sd(x)/sqrt(length(x)),mean(x)+qt(1-alfa/2,gl)*sd(x)/sqrt(length(x)))
  ICy<-c(mean(x)-qt(1-alfa/2,gl)*sd(x)/sqrt(length(x)),mean(x)+qt(1-alfa/2,gl)*sd(x)/sqrt(length(x)))
  se_intersecan<-1
  if(ICx[2]<ICy[1] || ICy[2]<ICx[1])
  { #NO SE INTERSECAN
    se_intersecan <- 0
  }
  return(se_intersecan)
}


dif.mu.intervalo<-function(x,y,alfa)
{
  z<-x-y
  gl<-length(z)-1
  IC<-c(mean(z)-qt(1-alfa/2,gl)*sd(z)/sqrt(length(z)),mean(z)+qt(1-alfa/2,gl)*sd(z)/sqrt(length(z)))
  return((IC[1]<0)*(IC[2]>0))
}

x<-rnorm(n,1,2)
y<-rnorm(n,2,2)
dif.mu.interseccion(x,y,0.05)
dif.mu.intervalo(x,y,0.05)

# Bootstrap para la exponencial

set.seed(27)
lam<-3
n<- 200
Nrep<-1000
Nboot<-1000
alfa<-0.05
x<- rexp(n,3)

# tita = exp(-lam*1)
tita<-exp(-lam)

tita_hat<-mean(x>1)


## INTERVALO BOOTSTRAP NORMAL
tita.boot<- rep(NA,Nboot)
for(i in 1:Nboot) 
{
  ind.boot<-sample(1:n, replace = TRUE)
  tita.boot[i]<-mean(x[ind.boot]>1) 
}

media.tita.boot<-mean(tita.boot)
var.tita.boot<- mean((tita.boot-media.tita.boot)^2)

tita_LI<- tita_hat-qnorm(1-alfa/2)*sqrt(var.tita.boot)
tita_LS<- tita_hat+qnorm(1-alfa/2)*sqrt(var.tita.boot)
c(tita_LI,tita_LS)


# INTERVALO Bootstrap percentil
IC.tita.per_LI<- quantile(tita.boot, probs = alfa/2, na.rm = T)
IC.tita.per_LS<- quantile(tita.boot, probs = 1-alfa/2, na.rm = T)
c(IC.tita.per_LI,IC.tita.per_LS)


#veamos el cubrimiento empirico para Nrep y los diferentes tamaños de n

#Para boot normal


cubrimiento<-c()
for (j in 1: Nrep)
{
  x<- rexp(n,3)
  tita_hat<-mean(x>1)
  tita.boot<- rep(NA,Nboot)
  for(i in 1:Nboot) 
  {
    ind.boot<-sample(1:n, replace = TRUE)
    tita.boot[i]<-mean(x[ind.boot]>1) 
  }
  media.tita.boot<-mean(tita.boot)
  var.tita.boot<- mean((tita.boot-media.tita.boot)^2)
  tita_LI<- tita_hat-qnorm(1-alfa/2)*sqrt(var.tita.boot)
  tita_LS<- tita_hat+qnorm(1-alfa/2)*sqrt(var.tita.boot)
  cubrimiento[j]<-(tita_LI<tita)*(tita_LS>tita)
}

cub_normal<-mean(cubrimiento)


# Para el boot percentil

cubrimiento<-c()
for (j in 1: Nrep)
{
  x<- rexp(n,3)
  tita_hat<-mean(x>1)
  tita.boot<- rep(NA,Nboot)
  for(i in 1:Nboot) 
  {
    ind.boot<-sample(1:n, replace = TRUE)
    tita.boot[i]<-mean(x[ind.boot]>1) 
  }
  IC.tita.per_LI<- quantile(tita.boot, probs = alfa/2, na.rm = T)
  IC.tita.per_LS<- quantile(tita.boot, probs = 1-alfa/2, na.rm = T)
  cubrimiento[j]<-(IC.tita.per_LI<tita)*(IC.tita.per_LS>tita)
}

cub_perc<-mean(cubrimiento)


# boot paramétrico

tita<-exp(-lam)

set.seed(27)

n <- 200
x<- rexp(n,3)

tita_hat<-exp(-1/mean(x))
lam_hat<-1/mean(x)
cubrimiento<-c()
long <- c()
for (j in 1: Nrep)
{
  tita.boot<- rep(NA,Nboot)
  for(i in 1:Nboot) 
  {
    x.boot<-rexp(n,lam_hat)
    tita.boot[i]<-exp(-1/mean(x.boot)) 
  }
  
  media.tita.boot<-mean(tita.boot)
  var.tita.boot<- mean((tita.boot-media.tita.boot)^2)
  
  tita_LI<- tita_hat-qnorm(1-alfa/2)*sqrt(var.tita.boot)
  tita_LS<- tita_hat+qnorm(1-alfa/2)*sqrt(var.tita.boot)
  long[j]<-tita_LS-tita_LI
  cubrimiento[j]<-(tita_LI<tita)*(tita_LS>tita)
}

cub_param<-mean(cubrimiento)
cubrimiento
long
