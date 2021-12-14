library(MASS)
library("stats4")
library(caTools)
require(ggplot2)
require(ggthemes)

######################################################################################################
######################################################################################################
##### SETEO DE DIRECTORIOS Y DEFINICION DE NOMBRES DE ARCHIVOS

### DIRECTORIO DONDE ESTAN LAS BASES

setwd("C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Data/Tesis/BasesFinalesUnParametroTesis")

L=3

######################################################################################################
######################################################################################################

### NOMBRE BASE A GRAFICAR
nombre.base<-paste("base500_CONT_L",L,"MVyGAyLNyLC_Caso1_FINAL.csv", sep = "")
nombre.base


### NOMBRE DE LOS GRAFICOS
graf.alfa<-paste("GraficoSesgoMVyGAyLNyLC_L=",L,"Cont_BarrasError.pdf",sep = "")
graf.ECM<-paste("GraficoECMMVyGAyLNyLC_L=",L,"Cont_BarrasError.pdf",sep = "")

##########################################################################################
###########################################################################################

### COMIENZO DEL PROGRAMA
base00 <- read.csv(nombre.base)
head(base00)

base<-subset(base00,alfa.MV!=-20 & alfa.DT.GA.BFGS!=-20
             & alfa.DT.LN.BFGS!=-20 & alfa.LC!=-0)

head(base)
#View(base)

dim(base00)
dim(base)

cant.L<-1
cant.n<-6
cant.alfas<-4
cant<-cant.L*cant.n*cant.alfas

##########################################################################################
### AGREGA EL ECM PARA TODOS LOS ESTIMADORES 

ecm.MV<-(base$alfa.MV-base$alfa)^2
ecm.GA<-(base$alfa.DT.GA.BFGS-base$alfa)^2
ecm.LN<-(base$alfa.DT.LN.BFGS-base$alfa)^2
ecm.LC<-(base$alfa.LC-base$alfa)^2

sesgo.MV<-base$alfa.MV-base$alfa
sesgo.GA<-base$alfa.DT.GA.BFGS-base$alfa
sesgo.LN<-base$alfa.DT.LN.BFGS-base$alfa
sesgo.LC<-base$alfa.LC-base$alfa


base.con.ecm<-cbind(base,sesgo.MV,sesgo.GA,sesgo.LN,sesgo.LC,ecm.MV,ecm.GA,ecm.LN,ecm.LC)

##########################################################################################
### GENERA PROMEDIOS por L,n y alfa DEL ESTIMADOR, ECM Y TIEMPOS

library(plyr)

medias0<-ddply(base.con.ecm,.(L,n,alfa),summarize,
               mean.sesgo.MV=mean(sesgo.MV),mean.sesgo.GA=mean(sesgo.GA),
               mean.sesgo.LN=mean(sesgo.LN),mean.sesgo.LC=mean(sesgo.LC),
               mean.ecm.MV=mean(ecm.MV),mean.ecm.GA=mean(ecm.GA),
               mean.ecm.LN=mean(ecm.LN),mean.ecm.LC=mean(ecm.LC),
               sd.sesgo.MV=sd(sesgo.MV),sd.sesgo.GA=sd(sesgo.GA),
               sd.sesgo.LN=sd(sesgo.LN),sd.sesgo.LC=sd(sesgo.LC),
               sd.ecm.MV=sd(ecm.MV),sd.ecm.GA=sd(ecm.GA),
               sd.ecm.LN=sd(ecm.LN),sd.ecm.LC=sd(ecm.LC),
               pi.MV.sesgo=quantile(sesgo.MV,0.05),
               ps.MV.sesgo=quantile(sesgo.MV,0.95),
               pi.GA.sesgo=quantile(sesgo.GA,0.05),
               ps.GA.sesgo=quantile(sesgo.GA,0.95),
               pi.LN.sesgo=quantile(sesgo.LN,0.05),
               ps.LN.sesgo=quantile(sesgo.LN,0.95),
               pi.LC.sesgo=quantile(sesgo.LC,0.05),
               ps.LC.sesgo=quantile(sesgo.LC,0.95),
               pi.MV.ecm=quantile(ecm.MV,0.05),
               ps.MV.ecm=quantile(ecm.MV,0.95),
               pi.GA.ecm=quantile(ecm.GA,0.05),
               ps.GA.ecm=quantile(ecm.GA,0.95),
               pi.LN.ecm=quantile(ecm.LN,0.05),
               ps.LN.ecm=quantile(ecm.LN,0.95),
               pi.LC.ecm=quantile(ecm.LC,0.05),
               ps.LC.ecm=quantile(ecm.LC,0.95))



medias<-medias0[,-c(12:19)]
head(medias)
View(medias)
##########################################################################################
### GRABA TIEMPOS
#write.csv(medias.tiempos.L3, file =nombre.tiempos)

##########################################################################################
### Armo data para graficos

a1<-cbind(medias[,1],medias[,2],medias[,3],medias[,4])#alfa_MV
a2<-cbind(medias[,1],medias[,2],medias[,3],medias[,5])#alfa_GA
a3<-cbind(medias[,1],medias[,2],medias[,3],medias[,6])#alfa_LN
a4<-cbind(medias[,1],medias[,2],medias[,3],medias[,7])#alfa_LC
a5<-cbind(medias[,1],medias[,2],medias[,3],medias[,8])#ecm_MV
a6<-cbind(medias[,1],medias[,2],medias[,3],medias[,9])#ecm_GA
a7<-cbind(medias[,1],medias[,2],medias[,3],medias[,10])#ecm_LN
a8<-cbind(medias[,1],medias[,2],medias[,3],medias[,11])#ecm_LC
a9<-cbind(medias[,1],medias[,2],medias[,3],medias[,12])#li.MV.sesgo
a10<-cbind(medias[,1],medias[,2],medias[,3],medias[,13])#ls.MV.sesgo
a11<-cbind(medias[,1],medias[,2],medias[,3],medias[,14])#li.GA.sesgo
a12<-cbind(medias[,1],medias[,2],medias[,3],medias[,15])#ls.GA.sesgo
a13<-cbind(medias[,1],medias[,2],medias[,3],medias[,16])#li.LN.sesgo
a14<-cbind(medias[,1],medias[,2],medias[,3],medias[,17])#ls.LN.sesgo
a15<-cbind(medias[,1],medias[,2],medias[,3],medias[,18])#li.LC.sesgo
a16<-cbind(medias[,1],medias[,2],medias[,3],medias[,19])#ls.LC.sesgo

a17<-cbind(medias[,1],medias[,2],medias[,3],medias[,20])#li.MV.ecm
a18<-cbind(medias[,1],medias[,2],medias[,3],medias[,21])#ls.MV.ecm
a19<-cbind(medias[,1],medias[,2],medias[,3],medias[,22])#li.GA.ecm
a20<-cbind(medias[,1],medias[,2],medias[,3],medias[,23])#ls.GA.ecm
a21<-cbind(medias[,1],medias[,2],medias[,3],medias[,24])#li.LN.ecm
a22<-cbind(medias[,1],medias[,2],medias[,3],medias[,25])#ls.LN.ecm
a23<-cbind(medias[,1],medias[,2],medias[,3],medias[,26])#li.LC.ecm
a24<-cbind(medias[,1],medias[,2],medias[,3],medias[,27])#ls.LC.ecm

max(a16[,4])

a<-rbind(a1,a2,a3,a4) #estos son los valores de alfa estimados

b<-rbind(a5,a6,a7,a8) #estos los ecm

c<-rbind(a9,a11,a13,a15)#li.sesgo
d<-rbind(a10,a12,a14,a16)#ls.sesgo

e<-rbind(a17,a19,a21,a23)#li.ecm
f<-rbind(a18,a20,a22,a24)#ls.ecm

metodo<-c(rep("MV",cant),rep("GA",cant),rep("LN",cant),rep("LC",cant))

datos.grafico0<-data.frame(L=a1[,1],n=a1[,2],alfa=a1[,3],metodo=metodo,sesgo=a[,4],
                           ecm=b[,4],li.sesgo=c[,4],ls.sesgo=d[,4],
                           li.ecm=e[,4],ls.ecm=f[,4])
#View(datos.grafico0)
#View(datos.grafico0)

#### AGREGO VARIABLES PARA CARTELES

base.datos<-datos.grafico0
L.graf<-rep("0",dim(base.datos)[1])
n.graf<-rep("0",dim(base.datos)[1])
alfa.graf<-rep("0",dim(base.datos)[1])

base.datos<-data.frame(base.datos,L.graf=L.graf,n.graf=n.graf,alfa.graf=alfa.graf)

base.datos$L.graf<-ifelse(base.datos$L==1,"L=1",base.datos$L.graf)
base.datos$L.graf<-ifelse(base.datos$L==2,"L=2",base.datos$L.graf)
base.datos$L.graf<-ifelse(base.datos$L==3,"L=3",base.datos$L.graf)
base.datos$L.graf<-ifelse(base.datos$L==5,"L=5",base.datos$L.graf)
base.datos$L.graf<-ifelse(base.datos$L==8,"L=8",base.datos$L.graf)

base.datos$n.graf<-ifelse(base.datos$n==9,"n=9",base.datos$n.graf)
base.datos$n.graf<-ifelse(base.datos$n==25,"n=25",base.datos$n.graf)
base.datos$n.graf<-ifelse(base.datos$n==49,"n=49",base.datos$n.graf)
base.datos$n.graf<-ifelse(base.datos$n==81,"n=81",base.datos$n.graf)
base.datos$n.graf<-ifelse(base.datos$n==49,"n=49",base.datos$n.graf)
base.datos$n.graf<-ifelse(base.datos$n==81,"n=81",base.datos$n.graf)
base.datos$n.graf<-ifelse(base.datos$n==121,"n=121",base.datos$n.graf)
base.datos$n.graf<-ifelse(base.datos$n==1000,"n=1000",base.datos$n.graf)

base.datos$alfa.graf<-ifelse(base.datos$alfa==-1.5,"alpha=-1.5",base.datos$alfa.graf)
base.datos$alfa.graf<-ifelse(base.datos$alfa==-3,"alpha=-3",base.datos$alfa.graf)
base.datos$alfa.graf<-ifelse(base.datos$alfa==-5,"alpha=-5",base.datos$alfa.graf)
base.datos$alfa.graf<-ifelse(base.datos$alfa==-8,"alpha=-8",base.datos$alfa.graf)


datos.grafico<-base.datos
head(datos.grafico)
View(datos.grafico)

###########################################################################


labels.graf = c("alpha==-1.5","alpha==-3","alpha==-5","alpha==-8")

datos.grafico$alfa.graf<-factor(datos.grafico$alfa.graf,labels=labels.graf)
#head(datos.grafico)

##########################################

#datos.grafico<-subset(datos.grafico,alfa==-1.5 | alfa==-3)
head(datos.grafico)
########################################################################################
#### Graficos
#### ALFA

setwd("C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Figures/PaperTesis")
getwd()

legenda.nomb<-c("MV"=expression(paste("  ",widehat(alpha)[ML])), "GA"=expression(paste("  ",widehat(alpha)[Gamma])), 
                "LN"=expression(paste("  ",widehat(alpha)[LN])),"LC"=expression(paste("  ",widehat(alpha)[LC])))

pp1<-ggplot(data = datos.grafico, aes(x = n, y = 0)) +
  geom_line(size=1, color="blue") +
  facet_wrap( ~ alfa.graf,labeller = label_parsed)

pp1

pp1+geom_line(data = datos.grafico, aes(x = n, y = sesgo, color=metodo,linetype=metodo),size=2) +
  geom_point(data = datos.grafico, aes(x = n, y = sesgo, color=metodo,shape=metodo),size=3.5) +
  geom_errorbar(aes(ymin=li.sesgo, ymax=ls.sesgo,color=metodo), width=.1,#'#0072B2'
                position=position_dodge(.08))+ 
  #scale_color_manual(values = c("#56B4E9","coral", "magenta","#009E73"))+
  facet_wrap( ~ alfa.graf,labeller = label_parsed)+
  labs(x = "n",              # t?tulo del eje x
       y = "Bias") +  # t?tulo del eje y
  scale_x_continuous(trans="log10",breaks=c(9,25 ,49,81,121,500))+
  scale_colour_manual(name = "", 
                      #values=wes_palette("Darjeeling", n = 4),
                      #values = c("#01AFBB","#DC4E07", "#668cff","magenta"),
                      values = c("#56B4E9","coral", "magenta","#009E73"),labels=legenda.nomb)+
  scale_linetype_manual(name = "", 
                        values = c("dashed", "twodash" ,"dotted","longdash"),labels=legenda.nomb)+
  scale_shape_manual(name = " ", 
                     values = c(17, 19, 18,15),labels=legenda.nomb)+
  # scale_colour_manual(name = "Método", 
  #                     #values=wes_palette("Darjeeling", n = 4),
  #                     #values = c("#01AFBB","#DC4E07", "#668cff","magenta"),
  #                     values = c("#56B4E9","coral", "magenta","#009E73"),
  #                     labels = legenda.nomb)+
  # scale_linetype_manual(name = "Método", 
  #                       values = c("dashed", "twodash" ,"dotted","longdash"),
  #                       labels = legenda.nomb)+
  # scale_shape_manual(name = "Método", 
  #                       values = c(17, 19, 18,15),
  #                       labels = legenda.nomb)+
theme_few()+
  theme(legend.position="top",
        legend.text = element_text( size=20),
        legend.title = element_text( size=20),
        axis.text.y = element_text( size = 20 ),
        axis.text.x = element_text(angle=70,hjust = 1, size = 20),
        axis.title.y = element_text( size = 20 ),
        axis.title.x = element_text( size = 20 ),
        strip.text = element_text(size = 20))

ggsave(graf.alfa, plot = last_plot(), device = "pdf",scale=1.2)
##########################################################
#########################################################

####### ECM
# labels.graf = c("alpha==-1.5","alpha==-3","alpha==-5")
# 
# datos.grafico$alfa.graf<-factor(datos.grafico$alfa.graf,labels=labels.graf)
# #head(datos.grafico)


pp1+geom_line(data = datos.grafico, aes(x = n, y = ecm, color=metodo,linetype=metodo),size=2) +
  #geom_errorbar(aes(ymin=li.ecm, ymax=ls.ecm,color=metodo), width=.1,#'#0072B2'
  #              position=position_dodge(.1))+  
  geom_point(data = datos.grafico, aes(x = n, y = ecm, color=metodo,shape=metodo),size=3.5) +
  facet_wrap( ~ alfa.graf,labeller = label_parsed)+ 
  labs( x = "n", y = "MSE")+ 
  scale_x_continuous(trans="log10",breaks=c(9,25 ,49,81,121,500))+
  scale_colour_manual(name = "", 
                      #values=wes_palette("Darjeeling", n = 4),
                      #values = c("#01AFBB","#DC4E07", "#668cff","magenta"),
                      values = c("#56B4E9","coral", "magenta","#009E73"),
                      labels = legenda.nomb)+
  scale_linetype_manual(name = "", 
                        values = c("dashed", "twodash" ,"dotted","longdash"),
                        labels = legenda.nomb)+
  scale_shape_manual(name = " ", 
                     values = c(17, 19, 18,15),
                     labels = legenda.nomb)+
  theme_few()+
  theme(legend.position="top",
        legend.text = element_text( size=20),
        legend.title = element_text( size=20),
        axis.text.y = element_text( size = 20 ),
        axis.text.x = element_text(angle=70,hjust = 1, size = 20),
        axis.title.y = element_text( size = 20 ),
        axis.title.x = element_text( size = 20 ),
        strip.text = element_text(size = 20))


ggsave(graf.ECM, plot = last_plot(), device = "pdf",scale=1.2)
