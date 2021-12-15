library("ggplot2")
library("wesanderson")


library(MASS)
library("stats4")


set.seed(1234)


setwd("C:/Users/julia/Dropbox/Procesamiento de imagenes/KerEst/Data/Tesis/Cuantiles")


L=8
alfa=-5
alfa.nomb="-5"
nucleo1<-"MVyGAyLNyLC"
n=25

nombre1<-paste("../Contaminacion/CurvaDeInfluencia/INFL_",nucleo1,"_alfa",alfa.nomb,"L",L,"n=",n,"_FINAL.csv",sep = "")
nombre.cuantil<-paste("Cuantil_L",L,"_n_",n,".csv",sep="")


Cuantil<-read.csv(nombre.cuantil)
datos1 <- read.csv(nombre1)
Cuantil.alfa<-t(round(Cuantil$alfa5,2))

head(datos1)

legenda.nomb<-c("Set1"=expression(paste("  ",widehat(alpha)[Gamma])),
                "Set2"=expression(paste("  ",widehat(alpha)[LC])),
                "Set3"=expression(paste("  ",widehat(alpha)[LN])),
                "Set4"=expression(paste("  ",widehat(alpha)[ML])))

ticks<-c(Cuantil.alfa,seq(8,20,2))


nombre.ticks<-c(min(Cuantil.alfa),max(Cuantil.alfa),3,5,7,9,11)
cant.black<-length(nombre.ticks)-2

nombre.x<-expression(italic(z))

pp1<-ggplot(datos1, aes(x=grilla)) +
  geom_line(aes(y=alfa.GA, colour="Set1",linetype="Set1"),size=2) +
  geom_line(aes(y=alfa.LC, colour="Set2",linetype="Set2"),size=2) +
  geom_line(aes(y=alfa.LN, colour="Set3",linetype="Set3"),size=2) +
  geom_line(aes(y=MV, colour="Set4",linetype="Set4"),size=2) +
  labs(x=nombre.x, y = expression(paste(widehat(alpha))))+
  scale_x_continuous(breaks = nombre.ticks)+
  scale_colour_manual(name = " ", 
                      #values=wes_palette("Darjeeling", n = 4),
                      #values = c("#01AFBB","#DC4E07", "#668cff","magenta"),
                      values = c("#56B4E9","coral", "magenta","#009E73"),
                      labels = legenda.nomb)+
  scale_linetype_manual(name = " ", 
                        values = c("Set1" ="dashed", "Set2"="twodash" ,"Set3"= "dotted","Set4"="longdash"),
                        labels = legenda.nomb)+
  theme_few()+
  theme(text=element_text(size=35, family="serif"),
        legend.position="top",
        legend.text = element_text( size=35),
        legend.title = element_text( size=35),
        axis.text.y = element_text( size = 35 ),
        axis.text.x = element_text(colour = c("red","red",rep("black",cant.black)),
                                   angle=70,hjust = 1, size = 35),
        axis.title.y = element_text( size = 35 ),
        axis.title.x = element_text( size = 35 ),
        strip.text = element_text(size = 35))



pp1+geom_line(data=datos1, aes(x = grilla,y=alfa),size=2, color="blue") 


##################### GRABA GRAFICOS
setwd("G:/Mi unidad/Github/KernelEstimationGit/figures/PaperTesis")
getwd()

graf.alfa<-paste("CurvaInfluenciaAlfa",alfa.nomb,"L",L,"n",n,".pdf",sep="")
ggsave(graf.alfa, plot = last_plot(), device = "pdf",scale=2)
