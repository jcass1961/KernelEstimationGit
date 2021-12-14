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
color<-"Yellow"


nombre1<-paste("BootMuestra_Corner",color,nucleo1,"2000",sep="")
nombre2<-paste("BootMuestra_Corner",color,nucleo2,"2000",sep="")
nombre3<-paste("BootMuestra_Corner",color,nucleo3,"2000",sep="")
nombre4<-paste("BootMuestra_Corner",color,nucleo4,"2000",sep="")

muestra1.yellow<-read_delim(nombre1)[,-1]
muestra2.yellow<-read_delim(nombre2)[,-1]
muestra3.yellow<-read_delim(nombre3)[,-1]
muestra4.yellow<-read_delim(nombre4)[,-1]


muestra.MV.yellow<-muestra1.yellow%>%mutate(color=color,metodo=nucleo1,alfa.est=x)%>%select(metodo,color,alfa.est)
muestra.GA.yellow<-muestra2.yellow%>%mutate(color=color,metodo=nucleo2,alfa.est=x)%>%select(metodo,color,alfa.est)
muestra.LN.yellow<-muestra3.yellow%>%mutate(color=color,metodo=nucleo3,alfa.est=x)%>%select(metodo,color,alfa.est)
muestra.LC.yellow<-muestra4.yellow%>%mutate(color=color,metodo=nucleo4,alfa.est=x)%>%select(metodo,color,alfa.est)

muestra.yellow<-rbind(muestra.MV.yellow,muestra.GA.yellow,muestra.LN.yellow,muestra.LC.yellow)

View(muestra.yellow)

####################################################
color<-"yellow"

nombre1<-paste("BootMuestra_Corner",color,nucleo1,"2000",sep="")
nombre2<-paste("BootMuestra_Corner",color,nucleo2,"2000",sep="")
nombre3<-paste("BootMuestra_Corner",color,nucleo3,"2000",sep="")
nombre4<-paste("BootMuestra_Corner",color,nucleo4,"2000",sep="")

muestra1.yellow<-read_delim(nombre1)[,-1]
muestra2.yellow<-read_delim(nombre2)[,-1]
muestra3.yellow<-read_delim(nombre3)[,-1]
muestra4.yellow<-read_delim(nombre4)[,-1]


muestra.MV.yellow<-muestra1.yellow%>%mutate(color=color,metodo=nucleo1,alfa.est=x)%>%select(metodo,color,alfa.est)
muestra.GA.yellow<-muestra2.yellow%>%mutate(color=color,metodo=nucleo2,alfa.est=x)%>%select(metodo,color,alfa.est)
muestra.LN.yellow<-muestra3.yellow%>%mutate(color=color,metodo=nucleo3,alfa.est=x)%>%select(metodo,color,alfa.est)
muestra.LC.yellow<-muestra4.yellow%>%mutate(color=color,metodo=nucleo4,alfa.est=x)%>%select(metodo,color,alfa.est)

muestra.yellow<-rbind(muestra.MV.yellow,muestra.GA.yellow,muestra.LN.yellow,muestra.LC.yellow)

View(muestra.yellow)

####################################################
color<-"red"

nombre1<-paste("BootMuestra_Corner",color,nucleo1,"2000",sep="")
nombre2<-paste("BootMuestra_Corner",color,nucleo2,"2000",sep="")
nombre3<-paste("BootMuestra_Corner",color,nucleo3,"2000",sep="")
nombre4<-paste("BootMuestra_Corner",color,nucleo4,"2000",sep="")

muestra1.red<-read_delim(nombre1)[,-1]
muestra2.red<-read_delim(nombre2)[,-1]
muestra3.red<-read_delim(nombre3)[,-1]
muestra4.red<-read_delim(nombre4)[,-1]


muestra.MV.red<-muestra1.red%>%mutate(tam.muest=length(muestra1.red),color=color,metodo=nucleo1,alfa.est=x)%>%
  select(metodo,color,alfa.est)
muestra.GA.red<-muestra2.red%>%mutate(color=color,metodo=nucleo2,alfa.est=x)%>%select(metodo,color,alfa.est)
muestra.LN.red<-muestra3.red%>%mutate(color=color,metodo=nucleo3,alfa.est=x)%>%select(metodo,color,alfa.est)
muestra.LC.red<-muestra4.red%>%mutate(color=color,metodo=nucleo4,alfa.est=x)%>%select(metodo,color,alfa.est)

muestra.red<-rbind(muestra.MV.red,muestra.GA.red,muestra.LN.red,muestra.LC.red)

View(muestra.red)

################################################
## Junto todas las bases

muestra<-rbind(muestra.yellow,muestra.yellow,muestra.red)
head(muestra)


################################################
## Cuento -20
cuenta.menos20<-muestra%>%filter(alfa.est==-20)%>%group_by(metodo,color)%>%
  tally()%>%mutate(cant.efect=2000-n)

cuenta.menos20%>%spread(key=color,value=cant.efect)

################################################
## Cuento 0
cuenta.0<-muestra%>%filter(alfa.est==0)%>%group_by(metodo,color)%>%
  tally()%>%mutate(cant.efect=2000-n)
cuenta.0
################################################
## Quito -20
base<-muestra%>%filter(alfa.est!=-20 & alfa.est!=0)

head(base)


## Calculo percentiles muestrales

per.LI<-base%>%group_by(metodo,color)%>%
  summarise(per=quantile(alfa.est,0.025))
per.LS<-base%>%group_by(metodo,color)%>%summarise(per=quantile(alfa.est,0.975))

min(per.LS$per-per.LI$per)
max(per.LS$per-per.LI$per)

percentiles<-left_join(per.LI,per.LS,by=c('metodo','color'))%>%mutate(per.y-per.x)
