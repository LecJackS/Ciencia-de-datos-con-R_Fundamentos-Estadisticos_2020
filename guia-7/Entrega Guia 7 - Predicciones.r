library(ggplot2)
# Datos a usar
data <- read.csv('./alturas_n_500.csv')
data.M <- data[data$genero == 'M',]

head(data.M)

predigo_altura_masculino <- function(altura, altura_madre, 
                                     altura_mama_nueva, h){
    min_altura <- altura_mama_nueva - h
    max_altura <- altura_mama_nueva + h
    relevantes <- altura[altura_madre > min_altura & altura_madre < max_altura]
    pred <- mean(relevantes)
    return(pred)
}

# 16. Grilla de ventanas. 
# Uso de extremos max/min altura_madre.
# Notar que el promedio en los extremos será sesgado hacia "el centro"
hs <- c(0.5, 1.0, 5.0, 20.0)
min_altura <- min(data.M$altura_madre)
max_altura <- max(data.M$altura_madre)
grilla <- seq(min_altura, max_altura, by=0.1)

m <- length(grilla)
preds <- data.frame(h05=rep(0,m),
                    h1=rep(0,m),
                    h5=rep(0,m),
                    h20=rep(0,m))
for(i in 1:m){
    altura_mama <- grilla[i]
    for(j in 1:length(hs)){
        h <- hs[j]
        preds[i, j] <- predigo_altura_masculino(data.M$altura, data.M$altura_madre, altura_mama, h)
    }
}

library(ggplot2)
ggplot() + 
    geom_point(data=data.M, aes(x=altura_madre, y=altura, col="Altura"), size=0.2)+
    geom_line(data=preds, aes(x=grilla, y=h05, col="h=0.5")) +
    geom_line(data=preds, aes(x=grilla, y=h1, col="h=1")) +
    geom_line(data=preds, aes(x=grilla, y=h5, col="h=5")) +
    geom_line(data=preds, aes(x=grilla, y=h20, col="Promedio total\ncon h=20")) +
    ggtitle("Predicción para distintas ventanas") +
    labs(x = "Altura de madre", y="Altura de hijo (predicción)")

predigoVentana <- function(X, Y, xNuevo, h){
    min_x <- xNuevo - h
    max_x <- xNuevo + h
    relevantes <- altura[X > min_x & X < max_x]
    pred <- mean(relevantes)
    return(pred)
}


