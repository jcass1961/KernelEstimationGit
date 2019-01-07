############################################################################
##### DEFINE ESTIMADOR LOGCUMULANT de orden 2

f2 <- function(alfa,est.2,L)
{est.2 - psigamma(L, deriv = 1) - psigamma(-alfa, deriv = 1)}

f1 <- function(gama,est.1,L,alfa)
{est.1 + log(L) - log(gama) - digamma(L) + digamma(-alfa)}


logcumAlfa = function(muestra, L){
  logcum.est.1 <- mean(log(muestra))
  logcum.est.2 <- mean((log(muestra)-logcum.est.1)^2)
  
  if (f2(-20,logcum.est.2,L)*f2(-10^(-3),logcum.est.2,L)<0)
    alfa.LC <- uniroot(f2, c(-20,-10^(-3)), tol = 0.0001,logcum.est.2,L)$root
  else
    alfa.LC <- 0.1
  
  salida<-c(alfa.LC)
  return(salida)
}

logcumGama = function(muestra, L,gamaini){
  
  logcum.est.1 <- mean(log(muestra))
  
  alfa.LC<-logcumAlfa(muestra, L)
  
  if (f1(1/gamaini,logcum.est.1,L,alfa.LC)*f1(10*gamaini,logcum.est.1,L,alfa.LC)<0)
    gama.LC <- uniroot(f1, c(1/gamaini,10*gamaini), tol = 0.0001,logcum.est.1,L,alfa.LC)$root
  else
    gama.LC <- 0.1
  
  salida<-c(gama.LC)
  return(salida)
}
