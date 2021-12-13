setwd("C:/Users/usuario/Dropbox/Procesamiento de imagenes/KerEst/")

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
source("Code/Rutinas/Genera GI en funcion de alfa.R")
source("Code/Rutinas/Define densidad GI0.R")
source("Code/Rutinas/MV Estimador.R")
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
setwd("C:/Users/usuario/Dropbox/Procesamiento de imagenes/KerEst/ProgramasparaJSTAR/DatosRadarsat")

a0<-read.ENVI("C:/Users/usuario/Dropbox/Procesamiento de imagenes/KerEst/Images/tesis/dlr_munich_4s.flt")

b0<-a0^2
elige.muestra(b0,"red")

imagen.recortada<-b0[29:360,3:350]

##########################################################
## APLICO PROCESAMIENTO PIRAMIDAL

piramidal.MUNICH<-proc.piram(2,imagen.recortada)

L=3.21

############################################################
## ESTIMO

recortada2<-piramidal.MUNICH[9: 94,26:115]

m1<-recortada2[43:45,60:62]
m2<-recortada2[42:47,59:63]
m3<-recortada2[41:49,58:66]
m4<-recortada2[40:51,57:68]
m5<-recortada2[43:45,60:63]


a.estim1.r2<-estima.unpar(as.vector(m1)/mean(as.vector(m1)),L)
a.estim1.r2

a.estim2.r2<-estima.unpar(as.vector(m2)/mean(as.vector(m2)),L)#green
a.estim2.r2

a.estim3.r2<-estima.unpar(as.vector(m3)/mean(as.vector(m3)),L)#yellow
a.estim3.r2

a.estim4.r2<-estima.unpar(as.vector(m4)/mean(as.vector(m4)),L)#red
a.estim4.r2

a.estim5.r2<-estima.unpar(as.vector(m5)/mean(as.vector(m5)),L)
a.estim5.r2
##########################################################
### Muestras

green<-a.estim2.r2
yellow<-a.estim3.r2
red<-a.estim4.r2



##############################################
##### 

library(tidyverse)
library("ggthemes")
library("wesanderson")

r<-2000
L<-3.21

nucleo<-"LN"
muestra<-"Red"

repli.L.alfa<-data.frame()

setwd("G:/Mi unidad/Github/KernelEstimationGit/Data/PaperTesis/Bootstrap")
nombre<-paste("BootMuestra_Corner_V2",muestra,nucleo,r,sep = "")
nombre


alfa=red$alfa.LN[1]
n=length(m4)
alfa
n

base1<-c()
alfa.GA<-c()
alfa.LN<-c()
alfa.MV<-c()
alfa.LC<-c()

random<-replicate(1000,generoGI(alfa,n,L))

boot1<-function(alfa,n,L,ker)
{
  #browser()
  for(i in 1:r)
  {
    print(i)
    datosGI<-generoGI(alfa,n,L)
    
    if (mom_1_2(datosGI,L)!=0) x0<-mom_1_2(datosGI,L)
    else x0<--1.5
    
    b<-cv(datosGI,ker=ker)$hcv
    
    const<-Conakereport2(datosGI,ker=ker,nx=100)$C_n
    
    switch(ker,
           GA=f2.GA<-function(a) DT.NG1.Cte(a,L,datosGI,b,const),
           LN=f2.LN<-function(a) DT.LN.Cte(a,L,datosGI,b,const))
    
    
    switch(ker,
           GA=alfa.GA[i]<-optim(x0,f2.GA,method = "L-BFGS-B",lower = -20, upper = -1.00000001)$par,
           LN=alfa.LN[i]<-optim(x0,f2.LN,method = "L-BFGS-B",lower = -20, upper = -1.00000001)$par)
    base1[i]<-alfa.LN[i]
    write.csv(base1, file = nombre)
  }
  switch(ker,
         GA=base1<-return(alfa.GA),
         LN=base1<-return(alfa.LN))
  write.csv(base1, file = nombre)
  
}

repli<-boot1(alfa,n,L,nucleo)
write.csv(repli,nombre)
