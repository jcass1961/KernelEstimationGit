############################################################################
##### DEFINE ESTIMADOR DE MOMENTO de alfa y gamma, considerando mom 1/2 y 1
alfa.mom0 = function(muestra, L)
{
  mom_sample_1_2 = sum(muestra^(0.5))/length(muestra)
  xraya = mean(muestra)
  #browser()
  
  f <- function(x,mom_sample_1_2,xraya,L){-mom_sample_1_2*gamma(L)/gamma(L+0.5)*(L/xraya*(-x-1))^(0.5) + 
      gamma(-x-0.5)/gamma(-x)}
  if (f(-20,mom_sample_1_2,xraya,L)*f(-1.00000001,mom_sample_1_2,xraya,L)<0)
  {alpha_mom <- uniroot(f, c(-20,-1.00000001), tol = 0.0001,mom_sample_1_2,xraya,L)$root
  }
  else
    alpha_mom<-0

   x0<-c(alpha_mom,xraya)
  return(x0)
  
}

