### PROGRAMA QUE ESTIMA EL NUMERO DE LOOKS - NO CONSIDERA ESTIMACION ROBUSTA
### PARAMETROS DE ENTRADA: IMAGEN Y CANTIDAD DE MUESTRAS


estim.num.look<-function(imagen,cant.muestras,tam)
{
  look.muestras<-rep(0,cant.muestras)
  tams.muestras<-rep(0,cant.muestras)
  media.muestras<-rep(0,cant.muestras)
  desvio.muestras<-rep(0,cant.muestras)
  media.muestras.cuad<-rep(0,cant.muestras)
  desvio.muestras.cuad<-rep(0,cant.muestras)
  
  x1.tot<-rep(0,cant.muestras)
  x2.tot<-rep(0,cant.muestras)
  y1.tot<-rep(0,cant.muestras)
  y2.tot<-rep(0,cant.muestras)
  
  for (i in 1:cant.muestras)
  {
    #col=c("magenta","green","blue","aquamarine3","yellow","red","white","magenta","purple",
    #     "orangered","palegreen","aquamarine1","blueviolet","darkgoldenrod1","firebrick",
    #     "orangered","lightblue1","lightcoral","hotpink","lightgoldenrod1")
    col=c("magenta","green","blue","aquamarine3","yellow","red","white","magenta","purple",
               "orangered","palegreen","aquamarine1","blueviolet","darkgoldenrod1","firebrick")
    #fin<-cant.muestras+1
    #col=2:cant.muestras
    lty=1
    lwd=2
    print("Ingrese 2 puntos en la imagen utilizando el mouse")
    pos<-locator(n=1,type="n")
    pos.x1<-pos$x[1]
    pos.y1<-pos$y[1]
    pos.x2<-pos.x1+(tam-1)
    pos.y2<-pos.y1+(tam-1)
    
    
    #dibuja los rectángulos
    segments(pos.x1,pos.y1,pos.x2,pos.y1,col=col[i],lty=lty,lwd=lwd)
    segments(pos.x1,pos.y2,pos.x2,pos.y2,col=col[i],lty=lty,lwd=lwd)
    segments(pos.x1,pos.y1,pos.x1,pos.y2,col=col[i],lty=lty,lwd=lwd)
    segments(pos.x2,pos.y1,pos.x2,pos.y2,col=col[i],lty=lty,lwd=lwd)
    
    x1<-floor(pos.x1)
    y1<-floor(pos.y1)
    x2<-floor(pos.x2)
    y2<-floor(pos.y2)
    
    y1<-dim(imagen)[1]-y1
    y2<-dim(imagen)[1]-y2
    x1<-x1
    x2<-x2
    
    x1.tot[i]<-x1
    x2.tot[i]<-x2
    y1.tot[i]<-y1
    y2.tot[i]<-y2
    
    
    
    #muestra para estimar los parámetros
    muestra.1<-as.vector(imagen[y1:y2,x1:x2])
    
    n1<-length(muestra.1)
    
    look.muestras[i]<-(mean(muestra.1)/sd(muestra.1))^2
    tams.muestras[i]<-n1
    media.muestras[i]<-mean(muestra.1)
    desvio.muestras[i]<-sd(muestra.1)
    media.muestras.cuad[i]<-media.muestras[i]^2
    desvio.muestras.cuad[i]<-desvio.muestras[i]^2
  }
  n<-sum(tams.muestras)
  salida.lm<-lm(media.muestras.cuad~desvio.muestras.cuad-1)
  L.est.reg<-summary(salida.lm)$coefficients[1]
  L.est.CV<-(sum(look.muestras*tams.muestras))/n
  
  return(list("salida.lm"=salida.lm,"tams.muestras"=tams.muestras,"muestra"=muestra.1,
              "L.est.reg"=L.est.reg,"L.est.CV"=L.est.CV,"medias.cuad"=media.muestras.cuad,"desvios.cuad"=desvio.muestras.cuad,
              "salida.lm"=salida.lm,"x1"=x1.tot,"x2"=x2.tot,"y1"=y1.tot,"y2"=y2.tot))
}
