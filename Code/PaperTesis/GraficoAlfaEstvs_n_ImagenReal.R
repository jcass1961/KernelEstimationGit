library("ggplot2")
library("ggthemes")
library("wesanderson")

enableJIT(3)

setwd("G:/Mi unidad/Procesamiento de imagenes/KerEst/Data/Tesis/ImagenReal")
#setwd("C:/Users/julia/Dropbox/Procesamiento de imagenes/KernelEstimation/Code/OtroPaper")

muestra1<-read.csv("muestra1")[,-1]
muestra2<-read.csv("muestra2")[,-1]
muestra3<-read.csv("muestra3")[,-1]
muestra4<-read.csv("muestra4")[,-1]
muestra5<-read.csv("muestra5")[,-1]
per<-read.csv("percentiles_NoPar.csv")

muestra1
muestra2
muestra3
muestra4
muestra5



muestra1.MV<-muestra1[c(1,6,2)]
muestra2.MV<-muestra2[c(1,6,2)]
muestra3.MV<-muestra3[c(1,6,2)]
muestra4.MV<-muestra4[c(1,6,2)]
muestra5.MV<-muestra5[c(1,6,2)]

muestra1.GA<-muestra1[c(1,6,3)]
muestra2.GA<-muestra2[c(1,6,3)]
muestra3.GA<-muestra3[c(1,6,3)]
muestra4.GA<-muestra4[c(1,6,3)]
muestra5.GA<-muestra5[c(1,6,3)]

muestra1.LN<-muestra1[c(1,6,4)]
muestra2.LN<-muestra2[c(1,6,4)]
muestra3.LN<-muestra3[c(1,6,4)]
muestra4.LN<-muestra4[c(1,6,4)]
muestra5.LN<-muestra5[c(1,6,4)]

muestra1.LC<-muestra1[c(1,6,5)]
muestra2.LC<-muestra2[c(1,6,5)]
muestra3.LC<-muestra3[c(1,6,5)]
muestra4.LC<-muestra4[c(1,6,5)]
muestra5.LC<-muestra5[c(1,6,5)]

metodo<-c(rep("MV",5),rep("GA",5),rep("LN",5),rep("LC",5))
y.MV<-rbind(muestra1.MV,muestra2.MV,muestra3.MV,muestra4.MV,muestra5.MV)
colnames(y.MV)<-c("L","n","alfa.est")

y.GA<-rbind(muestra1.GA,muestra2.GA,muestra3.GA,muestra4.GA,muestra5.GA)
colnames(y.GA)<-c("L","n","alfa.est")

y.LN<-rbind(muestra1.LN,muestra2.LN,muestra3.LN,muestra4.LN,muestra5.LN)
colnames(y.LN)<-c("L","n","alfa.est")

y.LC<-rbind(muestra1.LC,muestra2.LC,muestra3.LC,muestra4.LC,muestra5.LC)
colnames(y.LC)<-c("L","n","alfa.est")

y.LI<-per[,3]
y.LS<-per[,4]


y.est<-rbind(y.MV,y.GA,y.LN,y.LC)
colnames(y.est)<-c("L","n","alfa.est")

length(metodo)
datos.grafico<-data.frame(y.est,metodo,per[,3],per[,4])
names(datos.grafico)
colnames(datos.grafico)<-c("L","n","alfa.est","metodo","li","ls")


n=c(muestra1$n,muestra2$n,muestra3$n,muestra4$n,muestra5$n)
#metodo<-c("MV","GA","LN","LC")
datos.grafico$li

legenda.nomb<-c("MV"="MV","GA"=expression(paste("  ",Gamma)),
                "LN"="LN","LC"="LC")

ticks<-n
nombre.ticks<-c(9,25,49,81,121)


ggplot()+geom_line(data = datos.grafico, aes(x = n, y = alfa.est, color=metodo,linetype=metodo),size=2) +
  geom_point(data = datos.grafico, aes(x = n, y = alfa.est, color=metodo,shape=metodo),size=3.5) +
  geom_errorbar(data = datos.grafico,aes(x = n,  ymin=li, ymax=ls,color=metodo), width=.1,#'#0072B2'
               position=position_dodge(.03))+ 
  labs(x = "n",              # t?tulo del eje x
       y = expression(paste(widehat(alpha)))) +  # t?tulo del eje y
  scale_x_continuous(trans="log10",breaks=c(9,25 ,49,81,121,500))+
  scale_colour_manual(name = "Método", 
                      #values=wes_palette("Darjeeling", n = 4),
                      #values = c("#01AFBB","#DC4E07", "#668cff","magenta"),
                      values = c("#56B4E9","coral", "magenta","#009E73"),
                      labels = legenda.nomb)+
  scale_linetype_manual(name = "Método", 
                        values = c("dashed", "twodash" ,"dotted","longdash"),
                        labels = legenda.nomb)+
  scale_shape_manual(name = "Método", 
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
#####################################################################################

setwd("C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Figures/Tesis/ImagenReal")

ggsave("AlfaVsTamCincoMuestras_v2.pdf", plot = last_plot(), device = "pdf",scale=1.2)





# 
# 
# 
# 
# 
# ggplot(resultados, aes(x=resultados$n)) +
#   geom_line(aes(y=resultados$GA, colour="Set1",linetype="Set1"),size=2) +
#   geom_line(aes(y=resultados$LC, colour="Set2",linetype="Set2"),size=2) +
#   geom_line(aes(y=resultados$LN, colour="Set3",linetype="Set3"),size=2) +
#   geom_line(aes(y=resultados$MV, colour="Set4",linetype="Set4"),size=2) +
#   geom_point(aes(y=resultados$GA, colour="Set1",shape="15"),size=3.5) +
#   geom_point(aes(y=resultados$LC, colour="Set2",shape="17"),size=3.5) +
#   geom_point(aes(y=resultados$LN, colour="Set3",shape="18"),size=3.5) +
#   geom_point(aes(y=resultados$MV, colour="Set4",shape="19"),size=3.5) +
#   labs(x="n", y = expression(paste(widehat(alpha))))+
#   scale_x_continuous(breaks = nombre.ticks)+
#   scale_colour_manual(name = "Método", 
#                       #values=wes_palette("Darjeeling", n = 4),
#                       #values = c("#01AFBB","#DC4E07", "#668cff","magenta"),
#                       values = c("#56B4E9","coral", "magenta","#009E73"),
#                       labels = legenda.nomb)+
#   scale_linetype_manual(name = "Método", 
#                         values = c("Set1" ="dashed", "Set2"="twodash" ,"Set3"= "dotted","Set4"="longdash"),
#                         labels = legenda.nomb)+
#   scale_shape_manual(name = "Método",values = c(15, 17 ,18,19), guide=FALSE,
#                      labels = legenda.nomb)+
#   theme_few()+
#   theme(legend.position="top",
#         legend.text = element_text( size=20),
#         legend.title = element_text( size=20),
#         axis.text.y = element_text( size = 20 ),
#         axis.text.x = element_text(angle=70,hjust = 1, size = 20),
#         axis.title.y = element_text( size = 20 ),
#         axis.title.x = element_text( size = 20 ),
#         strip.text = element_text(size = 20))
# 
# 
# 
# # ggplot(resultados, aes(x=resultados$n)) +
# #   geom_line(aes(y=resultados$GA, colour="Set1",linetype="Set1"),size=2) +
# #   geom_line(aes(y=resultados$LC, colour="Set2",linetype="Set2"),size=2) +
# #   geom_line(aes(y=resultados$LN, colour="Set3",linetype="Set3"),size=2) +
# #   geom_line(aes(y=resultados$MV, colour="Set4",linetype="Set4"),size=2) +
# #   geom_point(aes(y=resultados$GA, colour="Set1",shape="15"),size=3.5) +
# #   geom_point(aes(y=resultados$LC, colour="Set2",shape="17"),size=3.5) +
# #   geom_point(aes(y=resultados$LN, colour="Set3",shape="18"),size=3.5) +
# #   geom_point(aes(y=resultados$MV, colour="Set4",shape="19"),size=3.5) +
# #   labs(x="n", y = expression(paste(widehat(alpha))))+
# #   scale_x_continuous(breaks = nombre.ticks)+
# #   scale_colour_manual(name = "Método", 
# #                       #values=wes_palette("Darjeeling", n = 4),
# #                       #values = c("#01AFBB","#DC4E07", "#668cff","magenta"),
# #                       values = c("#56B4E9","coral", "magenta","#009E73"),
# #                       labels = legenda.nomb)+
# #   scale_linetype_manual(name = "Método", 
# #                         values = c("Set1" ="dashed", "Set2"="twodash" ,"Set3"= "dotted","Set4"="longdash"),
# #                         labels = legenda.nomb)+
# #   scale_shape_manual(name = "Método", 
# #                         values = c("Set1" ="15", "Set2"="17" ,"Set3"= "18","Set4"="19"),
# #                         labels = legenda.nomb)+
# #   theme_few()+
# #   theme(legend.position="top",
# #         legend.text = element_text( size=20),
# #         legend.title = element_text( size=20),
# #         axis.text.y = element_text( size = 20 ),
# #         axis.text.x = element_text(angle=70,hjust = 1, size = 20),
# #         axis.title.y = element_text( size = 20 ),
# #         axis.title.x = element_text( size = 20 ),
# #         strip.text = element_text(size = 20))
# 
# 
# ##################### GRABA GRAFICOS
# setwd("C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Figures/Tesis/ImagenReal")
# 
# ggsave("AlfaVsTamCincoMuestras_v2.pdf", plot = last_plot(), device = "pdf",scale=1.2)