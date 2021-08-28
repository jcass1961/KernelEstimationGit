#################################
## Defino función de densidad GI0
d.GI0<-function(z,alfap,gamap,L)
{L^L*gamma(L-alfap)/((gamap)^alfap*gamma(L)*gamma(-alfap))*z^( L - 1)/(gamap + L*z)^(L - alfap)}

#################################
## Defino función hipergeométrica
Gauss2F1b <- function(a,b,c,x){
  if(x>=0 & x<1){
    hyperg_2F1(a,b,c,x)
  }else{
    hyperg_2F1(a,c-b,c,1-1/(1-x))/(1-x)^a
  }
}

#################################
## Defino función de distribución GI0
p.GI0<-function(z,alfap,gamap,L) 
  {
  (L^(L - 1)*z^L*gamma(L-alfap)*((L*z)/gamap+ 1)^(L- alfap)*(gamap+L*z)^(alfap-L)*
     Gauss2F1b(L, L-alfap,L+1,-(L*z)/gamap))/(gamap^alfap*gamma(-alfap)*gamma(L))
}
  

######################################
## Defino función de densidad de Fisher de parámetros
## m y n en función de la GIO

d.Fisher<-function(z,m,n) d.GI0(z,-m/2,m/2,n/2)

######################################
## Defino función de distribución de Fisher de parámetros
## m y n en función de la GIO

p.Fisher<-function(z,m,n) p.GI0(z,-m/2,m/2,n/2)

p.Fisher(3,4,5)
