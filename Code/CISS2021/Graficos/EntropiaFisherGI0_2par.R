EntropiaFischer<-function(d1,d2)
{
  log(d2/d1)+log(beta(d1/2, d2/2))+(1 - d1/2)*digamma(d1/2)+((d1 + d2)/2)*digamma((d1 + d2)/2) - (1 + d2/2)*digamma(d2/2)
}

EntropiaGI0.2P<-function(alfa,gama,L) EntropiaFischer(2*L,-2*alfa)-log(-alfa/(gama))
  

