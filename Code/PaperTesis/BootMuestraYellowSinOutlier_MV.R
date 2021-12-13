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


imagen.recortada<-b0[180:450,20:250]

##########################################################
## APLICO PROCESAMIENTO PIRAMIDAL

piramidal.MUNICH<-proc.piram(2,imagen.recortada)
############################################################
## ESTIMO

L=3.21

############################################################
## ESTIMO

### Muestra que da mal la estimacion
muestra1.mat<-piramidal.MUNICH[76:79,19:22]
muestra1.mat

muestra1<-as.vector(muestra1.mat)/mean(as.vector(muestra1.mat))

muestra5<-muestra1[-1]

a.estim5<-estima.unpar(muestra5,L)
a.estim5

############################################################
## Bootstrap
r<-2000
L<-3.21

setwd("G:/Mi unidad/Github/KernelEstimationGit/Data/PaperTesis/Bootstrap")

muestra<-"Yellow"
nucleo<-"MV"


repli.L.alfa<-data.frame()

nombre<-paste("BootMuestraYellowSinOutlier",nucleo,r,sep = "")


alfa=a.estim5$alfa.MV[1]
n=length(muestra5)

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