# El Rmd es mucho más lindo ~

class.nopar <- function(xnew, X, Y, h1, h0){
  # Asume clase y=1 para variedad 1,
  #       clase y=0 para variedad 2
  n <- length(Y)
  # Estimación de priors
  prior1 <- sum(Y==1) / n
  prior0 <- sum(Y==0) / n
  
  # Estimación de densidades
  f1.h1 <- density(X[Y==1], bw=h1, kernel='gaussian', n=1, from=xnew, to=xnew)$y
  f0.h0 <- density(X[Y==0], bw=h0, kernel='gaussian', n=1, from=xnew, to=xnew)$y
  
  g_h0.h1 <- as.numeric(f1.h1 * prior1 >= f0.h0 * prior0)
  
  return(g_h0.h1)
}

class.nopar.12 <- function(xnew, X, Y, h1, h0){
  # Asume clase y=1 para variedad 1,
  #       clase y=2 para variedad 2
  # Convierto 2s en 0s
  Y.10 <- Y - 2 * (Y==2)
  g_h0.h1 <- class.nopar(xnew, X, Y10, h1, h0)
  if(g_h0.h1==0){
    return (2)
  }else{
    return (1)
  }
}