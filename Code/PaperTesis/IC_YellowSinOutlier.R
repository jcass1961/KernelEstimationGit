library(tidyverse)
library("ggthemes")
library("wesanderson")

getwd()
enableJIT(3)

setwd("G:/Mi unidad/Github/KernelEstimationGit/Data/PaperTesis/Bootstrap")

## Junto bases

nucleo1<-"MV"
nucleo2<-"GA"
nucleo3<-"LN"
nucleo4<-"LC"


####################################################
color<-"yellow"

nombre1<-paste("BootMuestraYellowSinOutlier",nucleo1,"2000",sep="")
nombre2<-paste("BootMuestraYellowSinOutlier",nucleo2,"2000",sep="")
nombre3<-paste("BootMuestraYellowSinOutlier",nucleo3,"2000",sep="")
nombre4<-paste("BootMuestraYellowSinOutlier",nucleo4,"2000",sep="")

muestra1.yellow<-read_delim(nombre1)[,-1]
muestra2.yellow<-read_delim(nombre2)[,-1]
muestra3.yellow<-read_delim(nombre3)[,-1]
muestra4.yellow<-read_delim(nombre4)[,-1]


muestra.MV.yellow<-muestra1.yellow%>%mutate(metodo=nucleo1,alfa.est=x)%>%dplyr::select(metodo,alfa.est)
muestra.GA.yellow<-muestra2.yellow%>%mutate(metodo=nucleo2,alfa.est=x)%>%dplyr::select(metodo,alfa.est)
muestra.LN.yellow<-muestra3.yellow%>%mutate(metodo=nucleo3,alfa.est=x)%>%dplyr::select(metodo,alfa.est)
muestra.LC.yellow<-muestra4.yellow%>%mutate(metodo=nucleo4,alfa.est=x)%>%dplyr::select(metodo,alfa.est)

muestra<-rbind(muestra.MV.yellow,muestra.GA.yellow,muestra.LN.yellow,muestra.LC.yellow)

View(muestra)



################################################
## Cuento -20
cuenta.menos20<-muestra%>%filter(alfa.est==-20)%>%group_by(metodo)%>%
  tally()%>%mutate(cant.efect=2000-n)

cuenta.menos20
################################################
## Cuento 0
cuenta.0<-muestra%>%filter(alfa.est==0)%>%group_by(metodo)%>%
  tally()%>%mutate(cant.efect=2000-n)
cuenta.0
################################################
## Quito -20
base<-muestra%>%filter(alfa.est!=-20 & alfa.est!=0)

head(base)


## Calculo percentiles muestrales

per.LI<-base%>%group_by(metodo)%>%
  summarise(per=quantile(alfa.est,0.025))
per.LS<-base%>%group_by(metodo)%>%summarise(per=quantile(alfa.est,0.975))

min(per.LS$per-per.LI$per)
max(per.LS$per-per.LI$per)

percentiles<-left_join(per.LI,per.LS,by=c('metodo'))%>%mutate(per.y-per.x)
percentiles
