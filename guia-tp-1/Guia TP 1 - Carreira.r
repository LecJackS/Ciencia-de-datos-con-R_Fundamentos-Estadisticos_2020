# Variables de datos
x <- c(1,2,3)
y <- c(6,5,4)

# Operaciones sobre x e y
x * 2
x * y
x[1] * y[2]
1/x
(1:10) * x[2]
rep(c(1,1,2), times = 2)
seq(from = 0, to = 10, length.out = 5) + 1:10

c(2,3,4)*c(1,1,1,1,1)

A <- 1
B <- 2
C <- 3

# Forma 1
tratamiento <- c(rep(A, 20), rep(B, 18), rep(C, 22))
tratamiento

# Forma 2. Con funciones que NO vimos todavia, pero al ser escalable, lo agrego
coordenadas  <- c(A,B,C)
repeticiones <- c(20,18,22)
# mapply: aplica FUN entre los elementos de los vectores uno a uno, en orden
tratamiento  <- unlist(mapply(FUN=rep, coordenadas, repeticiones))
tratamiento

J <- seq(1, 30, by=2)
sum(J[1], J[8])

sum(1:100)

sum(c(1:100)^2)

#?airquality
head(airquality)

colSums(is.na(airquality))

sum(airquality$Month == 5)

#help(mtcars)
head(mtcars)

as.list(row.names(mtcars[mtcars$gear == 4,]))

as.list(row.names(mtcars[mtcars$disp > 150 & mtcars$mpg > 20,]))

as.list(row.names(mtcars[mtcars$gear == 4 & mtcars$am == 1,]))

mean(mtcars[mtcars$carb == 2, 'mpg'])

data <- read.csv('./arbolado-en-espacios-verdes.csv', colClasses = "character")

nrow(data)
ncol(data)

as.list(colnames(data))

mean(as.numeric(data$altura_tot))

sum(data$espacio_ve == "ARENALES")

arboles_cercanos <- data.frame(data[data$espacio_ve == "HOLANDA",])
nrow(arboles_cercanos)
#head(arboles_cercanos)

unique(arboles_cercanos$nombre_com)

# Se necesita para ejercicios 8 y 9
install.packages("PASWR2")

# Importo librería PASWR2
require(PASWR2)

# Verifico que cargue los datos
head(TITANIC3)

# Males 1st class
total_males    <- nrow(TITANIC3[TITANIC3$sex=="male" & TITANIC3$pclass=="1st" ,])
survived_males <- nrow(TITANIC3[TITANIC3$sex=="male" & TITANIC3$pclass=="1st" & TITANIC3$survived==1 ,])
rate_males     <- survived_males/total_males
# Females 3rd class
total_females    <- nrow(TITANIC3[TITANIC3$sex=="female" & TITANIC3$pclass=="3rd",])
survived_females <- nrow(TITANIC3[TITANIC3$sex=="female" & TITANIC3$pclass=="3rd" & TITANIC3$survived==1 ,])
rate_females     <- survived_females/total_females

rate_males
rate_females

max(TITANIC3[TITANIC3$sex=="female" & TITANIC3$survived==1 , "age"], na.rm=TRUE)

total_cars <- CARS2004$cars * CARS2004$population
cars_per_country <- data.frame(CARS2004$country, total_cars)
colnames(cars_per_country) <- c("country", "cars")
cars_per_country 

tasas_mortalidad <- CARS2004$deaths / total_cars
# Agrego columna de países y renombro
tasa <- data.frame(CARS2004$country, tasas_mortalidad)
colnames(tasa) <- c("country","dead_rate")
tasa

tasa <- tasa[order(tasa$dead_rate),]

# Agrando margen inferior
par(mar=c(7,4,4,2)) 

# Degrade de colores
# NOTA: NO proporcionales a las alturas!
color.function <- colorRampPalette( c("lightblue" , "red") )
color.ramp <- color.function( n=nrow(tasa) )

# Grafico de barras
bb <- barplot(height=tasa$dead_rate, names.arg=tasa$country,
              las=2, cex.names=0.8,
              ylim=c(0, 0.0006), yaxt = "n",
              main="Tasa de Mortalidad por países",
              ylab="Tasa (muertes / cant.automóviles)",
              col=color.ramp)
text(bb, round(tasa$dead_rate, 7)+0.00005, round(tasa$dead_rate, 7), srt=90, cex=0.7)
# Ticks en eje y
axis(side=2, at=seq(0,0.0005,0.0001), las=2)
# Eje x
mtext("País", side=1, line=6)


print("País con tasa mínima:")
tasa[which.min(tasa$dead_rate), c("country", "dead_rate")]
print("País con tasa máxima:")
tasa[which.max(tasa$dead_rate), c("country", "dead_rate")]

total_cars <- CARS2004$cars * CARS2004$population
total_pobl <- CARS2004$population * 1000
plot(total_pobl, total_cars,
     xlab="Cantidad de habitantes", ylab="Total de automóviles",
     col="steelblue", pch=19)
grid()

plot(total_pobl, total_cars,
     xlab="Cantidad de habitantes", ylab="Total de automóviles",
     col="steelblue", pch=19)
lines(c(0, 1.75*max(total_cars)), c(0, max(total_cars)), col="red", lwd=2)
lines(c(0, 2*max(total_cars)), c(0, max(total_cars)), col="orange", lwd=2)
lines(c(0, 2.75*max(total_cars)), c(0, max(total_cars)), col="yellow", lwd=2)
grid()
legend("topleft", title="Autos por persona",
       legend=c("1 cada 1.75","1 cada 2","1 cada 2.75"),
       fill=c("red","orange","yellow"),
       bg="lightgray")

cars_vs_deadrate <- data.frame(total_cars, tasas_mortalidad)
colnames(cars_vs_deadrate) <- c("cars","dead_rate")

plot(cars_vs_deadrate,
     xlab="Total de Automóviles", ylab="Tasa de Mortalidad",
     col="steelblue", pch=19)
grid()
