### GENERA UN PROCESAMIENTO PIRAMIDAL CON tam VECINOS
### INGRESAR EL TAMAÑO DE LA VENTANA TAM=k

## IMAGEN ORIGINAL DE DIMENSION M*N
## IMAGEN PROCESADA DE DIMENSION (M/k)*(N/k)
## i RECORRE LAS FILAS DE LA IMAGEN PROCESADA
## j RECORRE LAS COLUMNAS DE LA IMAGEN PROCESADA


proc.piram<-function(tam.vent,imagen)
{
  dimension<-dim(imagen)
  fin.i<-floor(dimension[1]/tam.vent)
  fin.j<-floor(dimension[2]/tam.vent)
  
  im.nueva<-matrix(0,fin.i,fin.j)
  
   for(i in 1:fin.i)
  {
    print(i)
    for(j in 1:fin.j)
    {
      coor.i1=(i-1)*tam.vent+1
      coor.i2=i*tam.vent
      coor.j1=(j-1)*tam.vent+1
      coor.j2=j*tam.vent
      im.nueva[i,j]<-mean(imagen[coor.i1:coor.i2,coor.j1:coor.j2])
    }
  }
  return(im.nueva)
}


