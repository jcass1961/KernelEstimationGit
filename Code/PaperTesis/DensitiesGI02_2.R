require(ggplot2)
require(ggthemes)

source("C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Code/Rutinas/DefineDensidadGI_alfaygamma.R")
setwd("C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Figures/Tesis/Capitulo5")


# Densidades de la GI0(a, g*, L)
  
alfa1<--20
gama1<--alfa1-1
alfa2<--30
gama2<--alfa2-1

L=3

rango.x<-seq(0,4,0.005)

GI0.1<-function(z) GI0.alfagama(z,alfa1,gama1,L)
GI0.2<-function(z) GI0.alfagama(z,alfa2,gama2,L)


y1<-sapply(rango.x,GI0.1)
y2<-sapply(rango.x,GI0.2)


data1<-data.frame(alfa=factor(alfa1),rango.x,y=y1)
data2<-data.frame(alfa=factor(alfa2),rango.x,y=y2)


resultados<-as.data.frame(rbind(data1,data2))
resultados$alfa<-factor(resultados$alfa)
#View(resultados)
#head(resultados)

#########################################################

LegendTitle = expression(paste(alpha ))

ggplot(resultados, aes(x=rango.x, y=y, group=alfa)) +
  geom_line(aes(linetype=alfa,color=alfa,size=alfa))+
  labs(x="z", y = expression("density"))+
  scale_linetype_manual(name = LegendTitle,values=c("solid", "solid"))+
  scale_color_manual(name = LegendTitle,values=alpha(c("steelblue", "white"), 1))+
  scale_size_manual(name = LegendTitle,values=c(3, 1))+
  theme_few()+
  theme(legend.position="top",
        legend.text = element_text( size=20),
        legend.title = element_text( size=20),
        axis.text.y = element_text( size = 20 ),
        axis.text.x = element_text(hjust = 1, size = 20),
        axis.title.y = element_text( size = 20 ),
        axis.title.x = element_text( size = 20 ),
        strip.text = element_text(size = 20))



ggplot(resultados, aes(x=rango.x, y=y, group=alfa)) +
  geom_line(aes(linetype=alfa,color=alfa,size=alfa,alpha=alfa))+
  labs(x="z", y = expression("density"))+
  scale_alpha_discrete(values=c(0.4,1))+
  scale_size_manual(values=c(2.5, 1))+
  scale_color_manual(values=c("magenta","blue"))+
  scale_linetype_manual(values=c("solid", "solid"))+
  theme_few()+
  theme(legend.position="top",
        legend.text = element_text( size=20),
        legend.title = element_text( size=20),
        axis.text.y = element_text( size = 20 ),
        axis.text.x = element_text(hjust = 1, size = 20),
        axis.title.y = element_text( size = 20 ),
        axis.title.x = element_text( size = 20 ),
        strip.text = element_text(size = 20))
#p+stat_function(fun=dgamma, args=list(L, L), n=1000, aes(colour="-20"), linetype="dashed", size=1.3) +

setwd("C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Figures/PaperTesis")
getwd()

graf.alfa<-paste("DensidadGI0L",L,".pdf",sep="")
ggsave(graf.alfa, plot = last_plot(), device = "pdf",scale=1.2)

