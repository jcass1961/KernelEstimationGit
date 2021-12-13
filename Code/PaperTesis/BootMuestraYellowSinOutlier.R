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

imagen.recortada<-b0[180:450,20:250]

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


L=mean(looks.piramidal$muestra)^2/sd(looks.piramidal$muestra)^2
L=3.21
#L=2.745
############################################################
## ESTIMO

windows(width=6.5, height=6.5, rescale="fit")
plot(imagematrix(normalize(matrix(ecdf(piramidal.MUNICH)(piramidal.MUNICH), nrow=dim(piramidal.MUNICH)[1], ncol=dim(piramidal.MUNICH)[2]))))
#View(piramidal.MUNICH)

grafica.muestra.imagenRecortada(piramidal.MUNICH,19,22,77,80,"red")#muestra2
grafica.muestra.imagenRecortada(piramidal.MUNICH,19,22,77,79,"blue")#muestra3
grafica.muestra.imagenRecortada(piramidal.MUNICH,19,22,76,79,"yellow")#muestra1



muestra10<-elige.muestra(piramidal.MUNICH,"red")

#piramidal.MUNICH[23:29,39:45]<-7000000

#grafica.muestra.imagenRecortada(piramidal.MUNICH,19,19,79,79,"blue")

### Muestra que da mal la estimacion
muestra1.mat<-piramidal.MUNICH[76:79,19:22]
muestra1.mat


muestra2.mat<-piramidal.MUNICH[77:80,19:22]
muestra2.mat

muestra3.mat<-piramidal.MUNICH[77:79,19:22]
muestra3.mat

muestra4.mat<-piramidal.MUNICH[75:80,19:22]
muestra4.mat



muestra1<-as.vector(muestra1.mat)/mean(as.vector(muestra1.mat))
muestra2<-as.vector(muestra2.mat)/mean(as.vector(muestra2.mat))
muestra3<-as.vector(muestra3.mat)/mean(as.vector(muestra3.mat))
muestra4<-as.vector(muestra4.mat)/mean(as.vector(muestra4.mat))
muestra5<-muestra1[-1]
muestra6<-muestra1[-16]

sort(muestra1)

length(muestra1)
length(muestra2)
length(muestra3)



a.estim1<-estima.unpar(muestra1,L)
a.estim1

a.estim2<-estima.unpar(muestra2,L)
a.estim2

a.estim3<-estima.unpar(muestra3,L)
a.estim3

a.estim4<-estima.unpar(muestra4,L)
a.estim4

a.estim5<-estima.unpar(muestra5,L)
a.estim5

a.estim6<-estima.unpar(muestra6,L)
a.estim6





alfa.estim<-a.estim1$alfa.LN
gama.estim<--alfa.estim-1
n=length(muestra1)
df1=2*L
df2=-2*alfa.estim
pos<-seq(1,n,1)
z<-(pos-1/3)/(n+1/3)
cuantil.T<-qf(z,df1,df2)*gama.estim/(-alfa.estim)

qqplot.casero(a.estim1$alfa.LN,sort(muestra1),L)
identify(cuantil.T,sort(muestra1))

cuantiles.df<-data.frame(cuantil.T=cuantil.T,cuantil.m=sort(muestra1))
cuantiles.df

qqplot(a.estim.dostercios$alfa.LN,muestra36.1tercios)

# 
# 
# 
# 
# a.estim3<-estima.unpar(as.vector(muestra3),L)
# a.estim3
# 
# a.estim4<-estima.unpar(as.vector(muestra4),L)
# a.estim4



observaciones<-as.vector(muestra3)
binwidth <- 2 * IQR(observaciones) * length(observaciones)^(-1/3)

alfaMV.graf<-a.estim1$alfa.MV
gamaMV.graf<--alfaMV.graf-1

alfaGA.graf<-a.estim1$alfa.GA
gamaGA.graf<--alfaGA.graf-1

alfaLC.graf<-a.estim1$alfa.LC
gamaLC.graf<--alfaLC.graf-1

alfaLN.graf<-a.estim1$alfa.LN
gamaLN.graf<--alfaLN.graf-1

observaciones.df<-data.frame(observaciones=observaciones)
#x1<-observaciones[observaciones<1000]
windows(width=6.5, height=6.5, rescale="fit")
ggplot(observaciones.df, aes(x=observaciones)) +
  geom_histogram(aes(y = ..density..), bins=8,color="black", fill="white")+
  stat_function(fun=dGI0, args=list(alfaMV.graf, gamaMV.graf, L), n=1000, aes(colour="alfa.MV"), size=1.3) + 
  stat_function(fun=dGI0, args=list(alfaGA.graf, gamaGA.graf, L), n=1000, aes(colour="alfa.DT"), size=1.3) +
  stat_function(fun=dGI0, args=list(alfaLN.graf, gamaLN.graf, L), n=1000, aes(colour="alfa.LC"), size=1.3) +
  #stat_function(fun=dGI0, args=list(a$alfa.ini, a$gama.ini,L), n=1000, aes(colour="alfa.MOM"), size=1.3) +
  scale_x_continuous(name = "z",limits = c(0, 3)) +
  labs(colour=expression(alpha),y="densidad",x="z")


setwd("C:/Users/usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Figures/Tesis/ImagenReal")
ggsave("histMuestraVerde.pdf", plot = last_plot(), device = "pdf",scale=1.2)

