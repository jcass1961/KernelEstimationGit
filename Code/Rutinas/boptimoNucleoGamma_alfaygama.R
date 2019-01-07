boptim<-function(alfa,gama0,L,n)
{
  num1<-(-3 + 2*L)*gamma(-(1/2) + L)*gamma(L)*gamma(2+2*L-2*alfa)*gamma(1/2 - alfa)*gamma(-alfa)
  
  num2<- L^(3/2)*(L - alfa)*(-1 + alfa)*(1/gama0)^(5/2)*(3*(-2 + 4*alfa - gama0)*gama0 + 
         2*L*(4 + 4*alfa^2 + 2*gama0 + gama0^2 - 2*alfa*(5 + 2*gama0)))*gamma(-1 + 2*L)*gamma(1 - 2*alfa)*gamma(L - alfa)^2
  
  num3<-2^(4/5)*n^(2/5)*pi^(1/5)
  
  b<- (-num1/num2)^(2/5)*(1/num3)
  return(b)
}


