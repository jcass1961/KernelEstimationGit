############################################################################
##### DEFINE ESTIMADOR DE MOMENTO 1/2
mom_1_2 = function(muestra, L)
{
  mom_sample_1_2 = sum(muestra^(0.5))/length(muestra)
  #browser()
  
  f <- function(x,mom_sample_1_2,L){-mom_sample_1_2*gamma(L)/gamma(L+0.5)*(L/(-x-1))^(0.5) + 
      gamma(-x-0.5)/gamma(-x)}
  if (f(-20,mom_sample_1_2,L)*f(-1.00000001,mom_sample_1_2,L)<0)
  {alpha_mom_1_2 <- uniroot(f, c(-20,-1.00000001), tol = 0.0001,mom_sample_1_2,L)
   alpha_mom_1_2 <-alpha_mom_1_2$root
  }
  else
    alpha_mom_1_2 <-0
  return(alpha_mom_1_2)
  
}
