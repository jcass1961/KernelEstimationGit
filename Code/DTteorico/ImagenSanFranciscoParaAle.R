####################################################
### UBICARSE EN EL DIRECOTRIO KernelEstimationGit
####################################################

#setwd("/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimationGit")

getwd()

library(MASS)
library("stats4")
library("compiler")
library("cubature")
library("Conake")

### CARGO RUTINAS
source("Code/Rutinas/imagematrix.R")
source("Code/Rutinas/estima numero de looks muestras de tamanio gral_V2.R")
source("Code/Rutinas/elige una muestra_V3.R")
source("Code/Rutinas/ModeloGammaKS_qqplot.R")
source("Code/Rutinas/MV_Gama.R")

############################################
## PARA ESTIMAR PARAMETROS 
source("Code/Rutinas/Alfa0Gama0PorMomentos.R")
source("Code/Rutinas/LogCumulantAlfayGamma.R")
source("Code/Rutinas/DefineDensidadGI_alfaygamma.R")
source("Code/Rutinas/LogLikelihoodGI0.R")
source("Code/Rutinas/DistanciaTriangularNG1conCteAlfaGama.R")


source("Code/Rutinas/nucleos.R")                ###############
#source("Code/Rutinas/CrossValidationdeR.R")     ###############
source("Code/Rutinas/MOM_1medio.R")
#source("Code/Rutinas/Conakereport2conIntegrate.R")                ###############
source("Code/Rutinas/Conakereport2.R") 
source("Code/Rutinas/boptimoNucleoGamma_alfaygama.R")
source("Code/Rutinas/EstimaParametrosMuestrasReales.R")
# source("Code/Rutinas/EstimaParametrosMuestrasRealesDT.R")
# source("Code/Rutinas/EstimaParametrosMuestrasRealesMV.R")
source("Code/Rutinas/procesamiento piramidal_V2.R")

### CARGO IMAGEN RECORTADA DE LA BAHIA DE SAN FRANCISCO
### ES UNA IMAGEN POLARIMETRICA EN FORMATO INTENSIDAD, ME QUEDE CON LA COMPONENTE HV

load("Images/DTterico/DatosImagen/ImagenRecortadaSF.RData")

aeq.imagen.recortada<-normalize(matrix(ecdf(imagen.recortada)(imagen.recortada), 
                                       nrow=dim(imagen.recortada)[1], ncol=dim(imagen.recortada)[2]))

### GRAFICO LA IMAGEN RECORTADA NORMALIZADA Y ECUALIZADA
x11(width=6.5, height=6.5)
plot(imagematrix(aeq.imagen.recortada,ncol=dim(aeq.imagen.recortada)[1], nrow=dim(aeq.imagen.recortada)[2]))

### ESTIMO NUMERO DE LOKS - HACER 6 CLIKS EN ZONAS HOMOGENEAS 
### PARA OBTENER SEIS MUESTRAS Y ESTIMAR EL NÚMERO DE LOOKS

looks<-estim.num.look(imagen.recortada,6,10)

names(looks)
looks

## Coordenadas muestras para estimar número de looks en imagen original
# $x1
# [1]  72  98 121 133 135  99
# 
# $x2
# [1]  81 107 130 142 144 108
# 
# $y1
# [1] 108 101 110 137 168 172
# 
# $y2
# [1]  99  92 101 128 159 163

looks.reg<-looks$L.est.reg
looks.CV<-looks$L.est.CV

looks.reg
looks.CV

# > looks.reg
# [1] 0.9922337
# > looks.CV
# [1] 1.062319

############################################################################
## COMPRUEBO AJUSTE MODELO GAMMA - Elijo una muestra homogénea y hago histograma

x11(width=6.5, height=6.5)
plot(imagematrix(normalize(matrix(ecdf(imagen.recortada)(imagen.recortada), 
                                  nrow=dim(imagen.recortada)[1], ncol=dim(imagen.recortada)[2]))))

### ELIJO MUESTRA PARA HOMOGENEA PARA COMPROBAR EL MODELO GAMMA
muestra.homogenea<-elige.muestra(imagen.recortada,1)
coord<-muestra.homogenea$coordenadas

# > coord
# x1  x2 y1  y2
# 1 103 118 73 118

## ESTIMO MODELO GAMMA
muestra<-muestra.homogenea$muestra
m = mean(muestra)
emvalpha = emvalphaGamma(muestra)
emvbeta = m/emvalpha
emvalpha
emvbeta


## GRAFICO HIST GAMMA Y CURVA TEORICA GAMMA

x11(width=6.5, height=6.5)
hist(muestra.homogenea$muestra,probability=TRUE,ylim=c(0,60),
     main="Histogram Gamma Model and True GI0 density - One look")
curve(dgamma(x,shape=emvalpha,rate=1/emvbeta),col="red",add=T)

##########################################################
## APLICO PROCESAMIENTO PIRAMIDAL

## con ventana de 2x2

imagen.piramidal<-proc.piram(2,imagen.recortada)

### GRAFICO IMAGEN PIRAMIDAL NORMALIZADA Y ECUALIZADA

x11(width=6.5, height=6.5)
plot(imagematrix(normalize(matrix(ecdf(imagen.piramidal)(imagen.piramidal), 
                                  nrow=dim(imagen.piramidal)[1], ncol=dim(imagen.piramidal)[2]))))

### ELIJO 5 MUESTRAS PARA ESTIMAR EL NÚMERO DE LOOKS EN IMAGEN PIRAMIDAL
looks.piramidal<-estim.num.look(imagen.piramidal,5,10)

## Coordenadas muestra para estimar el número de looks 
# $x1
# [1] 36 57 65 59 44
# 
# $x2
# [1] 45 66 74 68 53
# 
# $y1
# [1]  61  53  89 126 160
# 
# $y2
# [1]  52  44  80 117 151

looks.reg.piramidal<-looks.piramidal$L.est.reg
looks.CV.piramidal<-looks.piramidal$L.est.CV

looks.reg.piramidal
looks.CV.piramidal

# > looks.reg.piramidal
# [1] 2.381883
# > looks.CV.piramidal
# [1] 2.398127

############################################################################
## COMPRUEBO AJUSTE MODELO GAMMA EN IMAGEN PIRAMIDAL

x11(width=6.5, height=6.5)
plot(imagematrix(normalize(matrix(ecdf(imagen.piramidal)(imagen.piramidal), 
                                  nrow=dim(imagen.piramidal)[1], ncol=dim(imagen.piramidal)[2]))))

muestra.homogenea<-elige.muestra(imagen.piramidal,1)
coord<-muestra.homogenea$coordenadas

# > coord
# x1 x2 y1 y2
# 1 53 58 37 58

## ESTIMO MODELO GAMMA
muestra<-muestra.homogenea$muestra
m = mean(muestra)
emvalpha = emvalphaGamma(muestra)
emvbeta = m/emvalpha
emvalpha
emvbeta

## GRAFICO HIST GAMMA Y CURVA TEORICA GAMMA
x11(width=6.5, height=6.5)
hist(muestra.homogenea$muestra,probability=TRUE, ylim=c(0,60),
     breaks="FD",main="Gamma Model and True GI0 Piramidal Image")
curve(dgamma(x,shape=emvalpha,rate=1/emvbeta),col="red",add=T)

##################################################################################################

#### ESTIMO
##########################################################
x11(width=6.5, height=6.5)
plot(imagematrix(normalize(matrix(ecdf(imagen.piramidal)(imagen.piramidal), 
                                  nrow=dim(imagen.piramidal)[1], ncol=dim(imagen.piramidal)[2]))))

muestra.estima1<-elige.muestra(imagen.piramidal,1)
coord<-muestra.estima1$coordenadas
coord

# > coord
# x1 x2 y1 y2
# 1 33 37 39 37

muestra1<-muestra.estima1$muestra

### GRABO LA IMAGEN RECORTADA
#save(muestra1,file="C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimationGit/Figures/DTTeorico/DosPar/ImagenReal/muestra1.RData")


L=mean(looks.piramidal$L.est.reg,looks.piramidal$L.est.CV)

ker="GA"
dist="DT"

a<-estimadores.tiempo2(muestra1,L,ker,dist)
a

# > a
# L   alfa.MV gama.MV   alfa.DT  gama.DT   alfa.LC gama.LC
# 1 2.381883 -2.956757 1.55967 -2.951008 1.447395 -2.791918     0.1

x11(width=6.5, height=6.5)
hist(muestra1,probability=TRUE,breaks="FD",ylim=c(0,2),main="Histogram Muestra and Theoretical Densities")
#curve(GI0.alfagama(x,a$alfa.LC,a$gama.LC,L),col="red",add=T,lwd=2)
curve(GI0.alfagama(x,a$alfa.MV,a$gama.MV,L),col="green",add=T,lwd=2)
curve(GI0.alfagama(x,a$alfa.DT,a$gama.DT,L),col="magenta",add=T,lwd=2)
legend(2, 1.9, c("ML", "DT"), col=c("green", "magenta"),lwd=2, lty=1, cex=0.8)


##########################################################################
x11(width=6.5, height=6.5)
plot(imagematrix(normalize(matrix(ecdf(imagen.piramidal)(imagen.piramidal), 
                                  nrow=dim(imagen.piramidal)[1], ncol=dim(imagen.piramidal)[2]))))

muestra.estima2<-elige.muestra(imagen.piramidal,1)
coord3<-muestra.estima2$coordenadas
coord3

# > coord3
# x1  x2 y1  y2
# 1 180 189 78 189

muestra2<-muestra.estima2$muestra

#save(muestra2,file="C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimationGit/Figures/DTTeorico/DosPar/ImagenReal/muestra2.RData")

a3<-estimadores.tiempo2(muestra2,L,ker,dist)
a3

# > a3
# L   alfa.MV    gama.MV   alfa.DT    gama.DT alfa.LC gama.LC
# 1 2.381883 -1.881729 0.04884692 -1.881729 0.04884692     0.1     0.1

x11(width=6.5, height=6.5)
hist(muestra2,probability=TRUE,breaks="FD",main="Histogram Muestra and Theoretical Densities")
#curve(GI0.alfagama(x,a3$alfa.LC,a$gama.LC,L),col="red",add=T,lwd=2)
curve(GI0.alfagama(x,a3$alfa.MV,a$gama.MV,L),col="green",add=T,lwd=2)
curve(GI0.alfagama(x,a3$alfa.DT,a$gama.DT,L),col="blue",add=T,lwd=2)