############################################################################
#### DEFINE ESTIMADOR NUCLEO GAMA VERSION 1
EstimadorNucleoGama1<-function(x,datos,ancho,Cte)
{
  n<-length(datos)
  EstNuGa<-1/(n*Cte)*sum(sapply(x,function(x) dgamma(datos,shape=x/ancho+1,scale=ancho)))
  return(EstNuGa)
}


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 1
integrand.DT.NG1<-function(x,a,ga,L,datos,ancho,Cte) (GI0.alfagama(x,a,ga,L)-EstimadorNucleoGama1(x,datos,ancho,Cte))^2/(GI0.alfagama(x,a,ga,L)+EstimadorNucleoGama1(x,datos,ancho,Cte))

############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 1
DT.NG1.Cte2Par<-function(a,ga,L,datos,ancho,Cte)
  adaptIntegrate(integrand.DT.NG1,lower = 0, upper = 400,a,ga,L,datos,ancho,Cte)$integral