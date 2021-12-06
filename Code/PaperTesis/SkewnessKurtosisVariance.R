library(MASS)
library("stats4")
library(caTools)
require(ggplot2)
require(ggthemes)

######################################################################################################
######################################################################################################
##### SETEO DE DIRECTORIOS Y DEFINICION DE NOMBRES DE ARCHIVOS

### DIRECTORIO DONDE ESTAN LAS BASES

setwd("G:/Mi unidad/Github/KernelEstimationGit/Data/PaperTesis")

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

base.df<-base0%>% gather(.,key=metodo,value=alfa.est,4:7) %>% filter(n==500)%>%
  mutate(alfa2=alfa,alfa.fact=as.factor(alfa2))
base.df

LegendTitle = "Method"
legenda.nomb<-c("alfa.MV"="ML","alfa.GA"=expression(Gamma),"alfa.LN"="LN","alfa.LC"="LC")

base.df$alfa <- factor(base.df$alfa, levels = c("-1.5","-3","-5","-8"),
                       ordered = TRUE,
                       labels=c(expression(alpha==-1.5),
                                expression(alpha==-3),
                                expression(alpha==-5),
                                expression(alpha==-8)))
#nombre.ticks<-c(-5,-3,-1)

head(base.df)

###################################################
### Asismetria y curtosis
library(moments)

base.asKur<-base0%>% gather(.,key=metodo,value=alfa.est,4:7)%>%group_by(L,alfa,n,metodo)%>%
  summarise(asimetria=skewness(alfa.est),curtosis=kurtosis(alfa.est),varianza=var(alfa.est))
base.asKur%>%filter(n==500,metodo=="alfa.LN")

base.asKur.ext<-base.asKur%>%pivot_wider(names_from = metodo, values_from = c(asimetria,curtosis,varianza))

write.csv(base.asKur.ext,file="G:/Mi unidad/Github/KernelEstimationGit/Data/PaperTesis/Asim_curt")



