GammaNewton = function(muestra,alpha0){
  # Sucesion que converge al e.m.v. del parametro alpha de la gamma
  m = mean(muestra)
  mlog = mean(log(muestra))
  alpha1 = alpha0 *(1+(digamma(alpha0)+log(m/alpha0)-mlog)/(1-alpha0*
                                                              trigamma(alpha0)))
}

emvalphaGamma = function(muestra){
  # Calcula el e.m.v. del parametro alpha de una Gamma
  m = mean(muestra)
  v = var(muestra)
  alpha0 = m^2/v # punto de inicio = estimador de los momentos
  error = 1
  while(error>1E-4){
    alpha1 = GammaNewton(muestra,alpha0)
    error = abs(alpha1-alpha0)
    alpha0 = alpha1
  }
  alpha1
}


#X = muestra.1
#m = mean(X)
#emvalpha = emvalphaGamma(X)
#emvbeta = m/emvalpha

#emvalpha
#emvbeta

#writeLines(paste('n =',n))
#writeLines(paste('alpha =',alpha,' e.m.v.(alpha) =',round(emvalpha,digits=4)))
#writeLines(paste('beta =',beta,' e.m.v.(beta) =',round(emvbeta,digits=4)))
