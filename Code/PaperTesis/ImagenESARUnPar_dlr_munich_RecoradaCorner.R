setwd("C:/Users/julia/Dropbox/Procesamiento de imagenes/KerEst/")

library(MASS)
library("stats4")
library("compiler")
library("cubature")
library("Conake")

library("caTools")

require(ggplot2)

### CARGO RUTINAS
source("Code/Rutinas/imagematrix.R")
source("Code/Rutinas/myread.ENVI.R")
source("Code/Rutinas/estima numero de looks muestras de tamanio gral_V2.R")

source("Code/Rutinas/ModeloGammaKS_qqplot.R")
source("Code/Rutinas/MV_Gama.R")
source("Code/Rutinas/Define densidad GI0.R")

############################################
## PARA ESTIMAR PARAMETROS 
source("Code/Rutinas/DistanciaTriangularNG1conCte.R")
source("Code/Rutinas/DistanciaTriangularLNconCte.R")
source("Code/Rutinas/LogcumulantOrden1.R")
source("Code/Rutinas/MV Estimador.R")

source("Code/Rutinas/EstimaUnPar_MuestrasReales.R")
source("Code/Rutinas/elige una muestra_V4.R")





source("Code/Rutinas/nucleos.R")                ###############
source("Code/Rutinas/CrossValidationdeR.R")     ###############
source("Code/Rutinas/MOM_1medio.R")
source("Code/Rutinas/Conakereport2conIntegrate.R")                ###############
source("Code/Rutinas/Conakereport2.R") 
source("Code/Rutinas/procesamiento piramidal_V2.R")
source("Code/Rutinas/DensitiesGI0Alejandro.R")

source("Code/Rutinas/GraficaMuestra.R")

#setwd("C:/Users/Usuario/Google Drive/Procesamiento de imagenes/Alejandro/ESAR")
setwd("C:/Users/julia/Dropbox/Procesamiento de imagenes/KerEst/ProgramasparaJSTAR/DatosRadarsat")

a0<-read.ENVI("C:/Users/julia/Dropbox/Procesamiento de imagenes/KerEst/Images/tesis/dlr_munich_4s.flt")

b0<-a0^2
elige.muestra(b0,"red")

windows(width=6.5, height=6.5, rescale="fit")
plot(imagematrix(matrix(ecdf(b0)(b0), nrow=nrow(b0))))
dim(b0)
imagen.recortada<-b0[29:360,3:350]

### GRAFICO LA IMAGEN NORMALIZADA Y ECUALIZADA
windows(width=6.5, height=6.5, rescale="fit")
plot(imagematrix(matrix(ecdf(imagen.recortada)(imagen.recortada), nrow=nrow(imagen.recortada))))

### ESTIMO NUMERO DE LOKS
#looks<-elige.muestra(imagen.recortada)


#mean(looks$muestra)^2/sd(looks$muestra)^2


##########################################################
## APLICO PROCESAMIENTO PIRAMIDAL

piramidal.MUNICH<-proc.piram(2,imagen.recortada)

#View(mapa.media)
dim(piramidal.MUNICH)

### GRAFICO IMAGEN PIRAMIDAL NORMALIZADA Y ECUALIZADA

# windows(width=6.5, height=6.5, rescale="fit")
# plot(imagematrix(normalize(matrix(ecdf(piramidal.MUNICH)(piramidal.MUNICH), nrow=dim(piramidal.MUNICH)[1], ncol=dim(piramidal.MUNICH)[2]))))



#looks.piramidal<-elige.muestra(piramidal.MUNICH)


#L=mean(looks.piramidal$muestra)^2/sd(looks.piramidal$muestra)^2
L=3.21

############################################################
## ESTIMO

windows(width=6.5, height=6.5, rescale="fit")
plot(imagematrix(normalize(matrix(ecdf(piramidal.MUNICH)(piramidal.MUNICH), nrow=dim(piramidal.MUNICH)[1], ncol=dim(piramidal.MUNICH)[2]))))
#View(piramidal.MUNICH)

muestra.r<-elige.muestra(piramidal.MUNICH,"red")
muestra.r<-muestra.r$muestra/mean(muestra.r$muestra)

#recortada1<-piramidal.MUNICH[4:47,4:62]
recortada2<-piramidal.MUNICH[9: 94,26:115]
#recortada1<-piramidal.MUNICH[26:82,7:65]
windows(width=6.5, height=6.5, rescale="fit")
plot(imagematrix(normalize(matrix(ecdf(recortada2)(recortada2), 
                                  nrow=dim(recortada2)[1], ncol=dim(recortada2)[2]))))

grafica.muestra.imagenRecortada(recortada2,60,62,43,45,"magenta",2)
grafica.muestra.imagenRecortada(recortada2,59,63,42,47,"green",2)
grafica.muestra.imagenRecortada(recortada2,58,66,41,49,"yellow",2)
grafica.muestra.imagenRecortada(recortada2,57,68,40,51,"red",2)



muestra1.r2<-elige.muestra(recortada2,"red")
muestra1.m.r2<-muestra1.r2$muestra/mean(muestra1.r2$muestra)

muestra2.r2<-elige.muestra(recortada2,"green")
muestra2.m.r2<-muestra2.r2$muestra/mean(muestra2.r2$muestra)

muestra3.r2<-elige.muestra(recortada2,"yellow")
muestra3.m.r2<-muestra3.r2$muestra/mean(muestra3.r2$muestra)

muestra4.r2<-elige.muestra(recortada2,"yellow")
muestra4.m.r2<-muestra4.r2$muestra/mean(muestra4.r2$muestra)

muestra1.r2
muestra2.r2
muestra3.r2
muestra4.r2

a.estim1.r2<-estima.unpar(as.vector(muestra1.m.r2),L)
a.estim1.r2

a.estim2.r2<-estima.unpar(as.vector(muestra2.m.r2),L)
a.estim2.r2

a.estim3.r2<-estima.unpar(as.vector(muestra3.m.r2),L)
a.estim3.r2

a.estim4.r2<-estima.unpar(as.vector(muestra4.m.r2),L)
a.estim4.r2



m1<-recortada2[43:45,60:62]
m2<-recortada2[42:47,59:63]
m3<-recortada2[41:49,58:66]
m4<-recortada2[40:51,57:68]
m5<-recortada2[43:45,60:63]


a.estim1.r2<-estima.unpar(as.vector(m1)/mean(as.vector(m1)),L)
a.estim1.r2

a.estim2.r2<-estima.unpar(as.vector(m2)/mean(as.vector(m2)),L)
a.estim2.r2

a.estim3.r2<-estima.unpar(as.vector(m3)/mean(as.vector(m3)),L)
a.estim3.r2

a.estim4.r2<-estima.unpar(as.vector(m4)/mean(as.vector(m4)),L)
a.estim4.r2

a.estim5.r2<-estima.unpar(as.vector(m5)/mean(as.vector(m5)),L)
a.estim5.r2
