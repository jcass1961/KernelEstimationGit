### PROGRAMA QUE ESTIMA EL NUMERO DE LOOKS - NO CONSIDERA ESTIMACION ROBUSTA
### PARAMETROS DE ENTRADA: IMAGEN Y CANTIDAD DE MUESTRAS


elige.muestra<-function(imagen,cant.muestras)
{
  x1.tot<-c()
  x2.tot<-c()
  y1.tot<-c()
  y2.tot<-c()
  x1.seg<-c()
  x2.seg<-c()
  y1.seg<-c()
  y2.seg<-c()
  
  
  for (i in 1:cant.muestras)
  {
    fin<-cant.muestras
    col=2:cant.muestras+1
    lty=1
    lwd=2
    print("Ingrese 2 puntos en la imagen utilizando el mouse")
    pos<-locator(n=2,type="n")
    
    
    #dibuja los rectángulos
    segments(pos$x[1],pos$y[1],pos$x[2],pos$y[1],col=col[i],lty=lty,lwd=lwd)
    segments(pos$x[1],pos$y[2],pos$x[2],pos$y[2],col=col[i],lty=lty,lwd=lwd)
    segments(pos$x[1],pos$y[1],pos$x[1],pos$y[2],col=col[i],lty=lty,lwd=lwd)
    segments(pos$x[2],pos$y[1],pos$x[2],pos$y[2],col=col[i],lty=lty,lwd=lwd)
    
    y1<-dim(imagen)[1]-floor(pos$y[1])
    x1<-ceiling(pos$x[1])
    
    y2<-dim(imagen)[1]-floor(pos$y[2])
    x2<-ceiling(pos$x[2])
    
    # y1<-dim(imagen)[1]-floor(pos$y[1])
    # y2<-dim(imagen)[1]-floor(pos$y[2])
    # x1<-x1
    # x2<-x2
    
    ### COORDENADAS PARA EXTRAER LA SUBIMAGEN
    x1.tot[i]<-x1
    x2.tot[i]<-x2
    y1.tot[i]<-y1
    y2.tot[i]<-y2
    
    ### COORDENADAS PARA GRAFICAR LA SUBIMAGEN
    x1.seg[i]<-pos$x[1]
    x2.seg[i]<-pos$x[2]
    y1.seg[i]<-pos$y[1]
    y2.seg[i]<-floor(pos$y[2])
    
    #muestra para estimar los parámetros
    muestra.1<-as.vector(imagen[y1:y2,x1:x2])
    
    n1<-length(muestra.1)
    coord<-data.frame("x1"=y1.tot,"x2"=y2.tot,"y1"=x1.tot,"y2"=y2.tot)
    
    ## Para graficar
    coord.segment<-data.frame("x1.seg"=x1.seg,"x2.seg"=x2.seg,"y1.seg"=y1.seg,"y2.seg"=y2.seg)
  }

  return(list("muestra"=muestra.1,"coordenadas"=coord,"tamaño"=n1,"Coord.Segment"=coord.segment))
}
