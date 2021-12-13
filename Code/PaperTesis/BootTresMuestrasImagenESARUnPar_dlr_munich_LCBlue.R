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
source("Code/Rutinas/elige una muestra_V4.R")
source("Code/Rutinas/ModeloGammaKS_qqplot.R")
source("Code/Rutinas/MV_Gama.R")
source("Code/Rutinas/Define densidad GI0.R")

############################################
## PARA ESTIMAR PARAMETROS 
source("Code/Rutinas/Genera GI en funcion de alfa.R")
source("Code/Rutinas/Define densidad GI0.R")
source("Code/Rutinas/MV Estimador.R")
source("Code/Rutinas/DistanciaTriangularNG1conCte.R")
source("Code/Rutinas/DistanciaTriangularLNconCte.R")
source("Code/Rutinas/LogcumulantOrden1.R")
source("Code/Rutinas/MV Estimador.R")

source("Code/Rutinas/EstimaUnPar_MuestrasReales.R")




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

plot(imagematrix(matrix(ecdf(b0)(b0), nrow=nrow(b0))))

imagen.recortada<-b0[180:400,20:250]
#imagen.recortada<-b0[180:450,20:250]

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


# 
# ### Muestra que da mal la estimacion
# muestra1<-piramidal.MUNICH[76:79,19:22]/mean(piramidal.MUNICH[76:79,19:22])
# muestra2<-piramidal.MUNICH[75:80,18:23]/mean(piramidal.MUNICH[75:80,18:23])
# muestra3<-piramidal.MUNICH[74:81,17:24]/mean(piramidal.MUNICH[74:81,17:24])
# #muestra4<-piramidal.MUNICH[73:82,16:25]/mean(piramidal.MUNICH[73:82,16:25])

muestra1<-piramidal.MUNICH[76:79,19:22]/mean(piramidal.MUNICH[76:79,19:22])##Amarilla
a.estim1<-estima.unpar(as.vector(muestra1),L)
a.estim1

muestra2<-piramidal.MUNICH[77:79,19:22]/mean(piramidal.MUNICH[77:79,19:22])##Azul
a.estim2<-estima.unpar(as.vector(muestra2),L)
a.estim2

muestra3<-piramidal.MUNICH[77:80,19:22]/mean(piramidal.MUNICH[77:80,19:22])## Roja
a.estim3<-estima.unpar(as.vector(muestra3),L)
a.estim3

###########################################################################
r<-2000
L<-3.21

nucleo<-"LC"
muestra<-"blue"

repli.L.alfa<-data.frame()

setwd("G:/Mi unidad/Github/KernelEstimationGit/Data/PaperTesis/Bootstrap")
nombre<-paste("BootTresMuestras",muestra,nucleo,r,sep = "")


alfa=a.estim2$alfa.LC[1]
n=length(muestra2)

base1<-c()
alfa.GA<-c()
alfa.LN<-c()
alfa.MV<-c()
alfa.LC<-c()

boot1<-function(alfa,n,L,ker)
{
  #browser()
  for(i in 1:r)
  {
    print(i)
    datosGI<-generoGI(alfa,n,L)
    
    if (mom_1_2(datosGI,L)!=0) x0<-mom_1_2(datosGI,L)
    else x0<--1.5
    
    switch(ker,
           MV=alfa.MV[i]<-MV(a,datosGI,n,L)@coef[[1]],
           LC=alfa.LC[i]<-logcum.orden1(datosGI,L))
    base1[i]<-alfa.LC[i]
    write.csv(base1, file = nombre)
  }
  switch(ker,
         MV=base1<-return(alfa.MV),
         LC=base1<-return(alfa.LC))
  write.csv(base1, file = nombre)
  
}

repli<-boot1(alfa,n,L,nucleo)
write.csv(repli,nombre)
