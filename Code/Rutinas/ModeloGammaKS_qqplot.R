modelo.gamma.KS.qqplot<-function(muestra)
{
  m = mean(muestra)
  emvalpha = emvalphaGamma(muestra)
  emvbeta = m/emvalpha
  emvalpha
  emvbeta
  
  #pvalue.smirnov<-ks.test(muestra, "pgamma",emvalpha,1/emvbeta,alternative = "two.sided")$p.value
  
  #windows(width=6.5, height=6.5, rescale="fit")
  #p<-ppoints(length(muestra),1/2)
  #qqplot(muestra,qgamma(p, emvalpha,1/emvbeta))
  #qqline(muestra, distribution = function(p) qgamma(p, emvalpha,1/emvbeta),
  #       probs = c(0.25, 0.75), col = 2)
  
  return(list("alpha.MV"=emvalpha,"beta.MV"=emvbeta))
              #,"pvalue.smirnov"=pvalue.smirnov))
}


