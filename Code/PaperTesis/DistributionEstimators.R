library(MASS)
library("stats4")
library(caTools)
require(ggplot2)
require(ggthemes)

######################################################################################################
######################################################################################################
##### SETEO DE DIRECTORIOS Y DEFINICION DE NOMBRES DE ARCHIVOS

### DIRECTORIO DONDE ESTAN LAS BASES

setwd("C:/Users/julia/Dropbox/Procesamiento de imagenes/KerEst/Data/Tesis/BasesFinalesUnParametroTesis")

L=3

######################################################################################################
######################################################################################################

### NOMBRE BASE A GRAFICAR
nombre.base<-paste("base500_NoCont_L",L,"MVyGAyLNyLC_OPTIM_MOM1medioCONST_VERSION2_FINAL.csv", sep = "")
nombre.base


##########################################################################################
###########################################################################################

### COMIENZO DEL PROGRAMA
base00 <- read.csv(nombre.base)
head(base00)

base0<-subset(base00,alfa.MV!=-20 & alfa.DT.GA.BFGS!=-20
              & alfa.DT.LN.BFGS!=-20 & alfa.LC!=-0)[,1:7]
head(base0)
colnames(base0)<-c("L","alfa","n","alfa.MV","alfa.GA","alfa.LN","alfa.LC")

head(base0)
#View(base)



#metodo<-c(rep("MV",dim(base0)[1]),rep("GA",dim(base0)[1]),rep("LN",dim(base0)[1]))

base.MV<-base0[,c(1:4)]
colnames(base.MV)<-c("L","alfa","n","alfa.est")
base.GA<-base0[,c(1:3,5)]
colnames(base.GA)<-c("L","alfa","n","alfa.est")
base.LN<-base0[,c(1:3,6)]
colnames(base.LN)<-c("L","alfa","n","alfa.est")
base.LC<-base0[,c(1:3,7)]
colnames(base.LC)<-c("L","alfa","n","alfa.est")

metodo<-c(rep("MV",dim(base.MV)[1]),rep("GA",dim(base.GA)[1]),rep("LN",dim(base.LN)[1]),rep("LC",dim(base.LC)[1]))

# head(base.MV)
# head(base.GA)
# head(base.LN)
# head(base.LC)

base0<-rbind(base.MV,base.GA,base.LN,base.LC)
#base0<-rbind(base.MV,base.GA,base.LN)
base<-data.frame(base0,metodo)

dim(base)
length(metodo)
head(base)
#View(base)

# base9<-subset(base,base$n==9  & base$alfa==-3)
# base25<-subset(base,base$n==25  & base$alfa==-3)
# base49<-subset(base,base$n==49  & base$alfa==-3)
# base81<-subset(base,base$n==81  & base$alfa==-3)
# base121<-subset(base,base$n==121  & base$alfa==-3)
# base500<-subset(base,base$n==500  & base$alfa==-3)

base.datos<-subset(base,base$alfa==-3)

base.datos<-data.frame(base.datos,n.graf=base.datos$n)

base.datos$n.graf<-ifelse(base.datos$n==9,"n=9",base.datos$n.graf)
base.datos$n.graf<-ifelse(base.datos$n==25,"n=25",base.datos$n.graf)
base.datos$n.graf<-ifelse(base.datos$n==49,"n=49",base.datos$n.graf)
base.datos$n.graf<-ifelse(base.datos$n==81,"n=81",base.datos$n.graf)
base.datos$n.graf<-ifelse(base.datos$n==49,"n=49",base.datos$n.graf)
base.datos$n.graf<-ifelse(base.datos$n==81,"n=81",base.datos$n.graf)
base.datos$n.graf<-ifelse(base.datos$n==121,"n=121",base.datos$n.graf)
base.datos$n.graf<-ifelse(base.datos$n==500,"n=500",base.datos$n.graf)

# base25.MV<-subset(base25,metodo="MV")
# mean(base.MV$alfa.est)
# 
# base25.GA<-subset(base25,metodo="GA")
# mean(base.GA$alfa.est)
# 
# base25.LN<-subset(base25,metodo="LN")
# mean(base.LN$alfa.est)
# 
# base25.LC<-subset(base25,metodo="LC")
# mean(base.LC$alfa.est)
# 
head(base.datos)

nombre.ticks<-c(-5,-3,-1)
n<-dim(base)[1]

LegendTitle = "Método"
legenda.nomb<-c("MV"="MV","GA"=expression(Gamma),"LN"="LN","LC"="LC")

n.labs<-c("n=9","n=25","n=49","n=81","n=121","n=500")

base.datos$n <- factor(base.datos$n, labels = c("n=9","n=25","n=49","n=81","n=121","n=500"))

setwd("C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Figures/Presentacion")

p<-ggplot(base.datos, aes(x=alfa.est,color=metodo)) + 
  facet_wrap(~n,labeller = label_value,ncol=3) + 
  geom_density(size=1.2)+
  scale_x_continuous(breaks = nombre.ticks,limits = c(-8,0))+
  scale_color_discrete(name = LegendTitle,labels = legenda.nomb)+
  theme_few()+
  theme(legend.position="right",
        legend.text = element_text( size=20),
        legend.title = element_text( size=20),
        axis.text.y = element_text( size = 20 ),
        axis.text.x = element_text(hjust = 1, size = 20),
        axis.title.y = element_text( size = 20 ),
        axis.title.x = element_text( size = 20 ),
        axis.ticks.length=unit(0.5,"cm"),
        strip.text = element_text(size = 20))+
  labs(x=expression(paste(alpha)), y = "densidad")
p
p+ geom_vline(aes(xintercept=-3),
              color="blue", linetype="dashed", size=1)+ geom_hline(aes(yintercept=0),
                                                                   color="black",  size=1)+
  facet_wrap(~n,labeller = label_value,ncol=3)  
######################################## 


#ggsave("DensidadEstimadorNoCont.pdf", plot = last_plot(), device = "pdf",scale=2)
