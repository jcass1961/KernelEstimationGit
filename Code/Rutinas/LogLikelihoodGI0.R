#############################################################
#### DEFINE ESTIMADOR DE MAXIMA VEROSIMILITUD            ####
#### para los parámetro alfa y gamma                     ####
#### de una distribución GI0                             #### 
#### CARGAR LIBRERIA stats4                              ####
#############################################################


LogLike <- function(alfa,ga, n,L,datos) 
{
  -(n*L*log(L)+n*log(gamma(L-alfa))-n*log(gamma(L))-n*log(gamma(-alfa))-n*alfa*log(ga)+
      sum((L-1)*log(datos))-(L-alfa)*sum(log(ga+datos*L)))
}




