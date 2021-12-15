library(MASS)
library(stats4)
library(caTools)
require(ggplot2)
require(ggthemes)
require(tidyr)
require(rstudioapi)
library(tidyverse)

######################################################################################################
######################################################################################################
##### SETEO DE DIRECTORIOS Y DEFINICION DE NOMBRES DE ARCHIVOS

### DIRECTORIO DONDE ESTAN LAS BASES

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../../Data/PaperTesis")


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
#legenda.nomb<-c("alfa.MV"="ML","alfa.GA"=expression(Gamma),"alfa.LN"="LN","alfa.LC"="LC")
legenda.nomb<-c("alfa.MV"=expression(paste("  ",widehat(alpha)[ML])), 
                "alfa.GA"=expression(paste("  ",widehat(alpha)[Gamma])), 
                "alfa.LN"=expression(paste("  ",widehat(alpha)[LN])),
                "alfa.LC"=expression(paste("  ",widehat(alpha)[LC])))


base.df$alfa <- factor(base.df$alfa, levels = c("-1.5","-3","-5","-8"),
                  ordered = TRUE,
                  labels=c(expression(alpha==-1.5),
                           expression(alpha==-3),
                           expression(alpha==-5),
                           expression(alpha==-8)))
#nombre.ticks<-c(-5,-3,-1)

head(base.df)

p3<-ggplot(base.df, aes(x=alfa.est, color=metodo, group =metodo)) + 
  facet_wrap(~alfa,labeller = label_parsed, ncol=2, scales = 'free_y') +
  geom_line(stat='density', size = 2) +
  geom_vline(aes(xintercept=alfa2),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(breaks=c(-1.5,-3,-5,-8),limits = c(-12,-1))+
  #scale_color_discrete(name = LegendTitle,labels = legenda.nomb)+
  scale_colour_manual(name = " ",
                      values = c("#56B4E9","coral", "magenta","#009E73"),
                      labels = legenda.nomb)+
  scale_linetype_manual(name = " ",
                        values = c("dashed", "twodash" ,"dotted","longdash"),
                        labels = legenda.nomb)+
  scale_shape_manual(name = " ",
                     values = c(17, 19, 18,15),
                     labels = legenda.nomb)+
  theme_few()+
  theme(text=element_text(size=35, family="serif"),
        legend.position="top",
        legend.text = element_text( size=35),
        legend.title = element_text( size=35),
        axis.text.y = element_text( size = 35 ),
        axis.text.x = element_text(hjust = 1, size = 35,angle=45),
        axis.title.y = element_text( size = 35 ),
        axis.title.x = element_text( size = 35 ),
        #axis.ticks.length=unit(0.5,"cm"),
        strip.text = element_text(size = 35))+
  labs(x=" ", y = "Density")
  #labs(x=expression(paste(widehat(alpha))), y = "Density")
  p3

######################################## 
ggsave("../../Figures/PaperTesis/Asymptotic_n500_TodoAlfa.pdf", 
       plot = last_plot(), 
       device = "pdf", 
       scale=2)
#system("convert ../../Figures/PaperTesis/Asymptotic_n500_TodoAlfa.eps ../../Figures/PaperTesis/Asymptotic_n500_TodoAlfa.pdf
  #")

############################################################
############################################################

# scaleFUN <- function(x) sprintf("%.1f", x)
# 
# p1<-ggplot(base.df%>%filter(alfa2!=-5,alfa2!=-8), aes(x=alfa.est,color=metodo)) + 
#   facet_wrap(~alfa,labeller = label_parsed,ncol=2, scales = 'free_y') +
#   geom_line(stat='density', aes(linetype = metodo), size = 2) +
#   geom_vline(aes(xintercept=alfa2),
#              color="blue", linetype="dashed", size=1)+
#   scale_x_continuous(breaks=c(-1.5,-3,-5,-8),limits = c(-15,0))+
#   #scale_color_discrete(name = LegendTitle,labels = legenda.nomb)+
#   scale_colour_manual(name = "",
#                       values = c("#56B4E9","coral", "magenta","#009E73"),
#                       labels = legenda.nomb)+
#   scale_linetype_manual(name = "",
#                         values = c("dashed", "twodash" ,"dotted","longdash"),
#                         labels = legenda.nomb)+
#   scale_shape_manual(name = " ",
#                      values = c(17, 19, 18,15),
#                      labels = legenda.nomb)+
#   theme_few()+
#   theme(legend.position="top",
#         legend.text = element_text( size=20),
#         legend.title = element_text( size=20),
#         axis.text.y = element_text( size = 18 ),
#         axis.text.x = element_text(hjust = 1, size = 18,angle=45),
#         axis.title.y = element_text( size = 20 ),
#         axis.title.x = element_text( size = 20 ),
#         #axis.ticks.length=unit(0.5,"cm"),
#         strip.text = element_text(size = 0))+
#   labs(x=expression(paste(widehat(alpha))), y = "Density")
# p1
# 
# ######################################## 
# getwd()
# ggsave("G:/Mi unidad/Github/KernelEstimationGit/figures/PaperTesis/Asymptotic_n500_-1punto5_-3.eps", plot = last_plot(), device = "eps",scale=2)
# 
# 
# #######################################
# 
# 
# p2<-ggplot(base.df%>%filter(alfa2!=-1.5,alfa2!=-3), aes(x=alfa.est,color=metodo)) + 
#   facet_wrap(~alfa,labeller = label_parsed,ncol=2, scales = 'free_y') +
#   geom_line(stat='density', aes(linetype = metodo), size = 2) +
#   geom_vline(aes(xintercept=alfa2),
#              color="blue", linetype="dashed", size=1)+
#   scale_x_continuous(breaks=c(-1.5,-3,-5,-8),limits = c(-15,0))+
#   #scale_color_discrete(name = LegendTitle,labels = legenda.nomb)+
#   scale_colour_manual(name = "",
#                       values = c("#56B4E9","coral", "magenta","#009E73"),
#                       labels = legenda.nomb)+
#   scale_linetype_manual(name = "",
#                         values = c("dashed", "twodash" ,"dotted","longdash"),
#                         labels = legenda.nomb)+
#   scale_shape_manual(name = " ",
#                      values = c(17, 19, 18,15),
#                      labels = legenda.nomb)+
#   theme_few()+
#   theme(legend.position="top",
#         legend.text = element_text( size=20),
#         legend.title = element_text( size=20),
#         axis.text.y = element_text( size = 18 ),
#         axis.text.x = element_text(hjust = 1, size = 18,angle=45),
#         axis.title.y = element_text( size = 20 ),
#         axis.title.x = element_text( size = 20 ),
#         #axis.ticks.length=unit(0.5,"cm"),
#         strip.text = element_text(size = 20))+
#   labs(x=expression(paste(widehat(alpha))), y = "Density")
# p2
# 
# ######################################## 
# getwd()
# ggsave("G:/Mi unidad/Github/KernelEstimationGit/figures/PaperTesis/Asymptotic_n500_-5_-8.eps", plot = last_plot(), device = "eps",scale=2)

############################################################


#####################################################
# 
# p1<-ggplot(base.df%>%filter(alfa2==-1.5), aes(x=alfa.est,color=metodo,group =metodo)) + 
#   facet_wrap(~alfa,labeller = label_parsed,ncol=2, scales = 'free_y') +
#   geom_line(stat='density', aes(linetype = metodo), size = 2) +
#   geom_vline(aes(xintercept=alfa2),
#              color="blue", linetype="dashed", size=1)+
#   #scale_x_continuous(breaks={},limits = c(-12,0))+
#   #scale_color_discrete(name = LegendTitle,labels = legenda.nomb)+
#   scale_colour_manual(name = "Method",
#                       values = c("#56B4E9","coral", "magenta","#009E73"),
#                       labels = legenda.nomb)+
#   scale_linetype_manual(name = "Method",
#                         values = c("dashed", "twodash" ,"dotted","longdash"),
#                         labels = legenda.nomb)+
#   scale_shape_manual(name = "Method",
#                      values = c(17, 19, 18,15),
#                      labels = legenda.nomb)+
#   theme_few()+
#   theme(legend.position="top",
#         legend.text = element_text( size=20),
#         legend.title = element_text( size=20),
#         #axis.text.y = element_text( size = 20 ),
#         axis.text.x = element_text(hjust = 1, size = 20),
#         axis.title.y = element_text( size = 20 ),
#         axis.title.x = element_text( size = 20 ),
#         #axis.ticks.length=unit(0.5,"cm"),
#         strip.text = element_text(size = 20))+
#   labs(x=expression(paste(widehat(alpha))), y = "Density")
# p1
# 
# p2<-ggplot(base.df%>%filter(alfa2==-3), aes(x=alfa.est,color=metodo,group =metodo)) + 
#   facet_wrap(~alfa,labeller = label_parsed,ncol=2, scales = 'free_y') +
#   geom_line(stat='density', aes(linetype = metodo), size = 2) +
#   geom_vline(aes(xintercept=alfa2),
#              color="blue", linetype="dashed", size=1)+
#   #scale_x_continuous(breaks={},limits = c(-12,0))+
#   #scale_color_discrete(name = LegendTitle,labels = legenda.nomb)+
#   scale_colour_manual(name = "Method",
#                       values = c("#56B4E9","coral", "magenta","#009E73"),
#                       labels = legenda.nomb)+
#   scale_linetype_manual(name = "Method",
#                         values = c("dashed", "twodash" ,"dotted","longdash"),
#                         labels = legenda.nomb)+
#   scale_shape_manual(name = "Method",
#                      values = c(17, 19, 18,15),
#                      labels = legenda.nomb)+
#   theme_few()+
#   theme(legend.position="top",
#         legend.text = element_text( size=20),
#         legend.title = element_text( size=20),
#         #axis.text.y = element_text( size = 20 ),
#         axis.text.x = element_text(hjust = 1, size = 20),
#         axis.title.y = element_text( size = 20 ),
#         axis.title.x = element_text( size = 20 ),
#         #axis.ticks.length=unit(0.5,"cm"),
#         strip.text = element_text(size = 20))+
#   labs(x=expression(paste(widehat(alpha))), y = "Density")
# p2
# 
# p3<-ggplot(base.df%>%filter(alfa2==-5), aes(x=alfa.est,color=metodo,group =metodo)) + 
#   facet_wrap(~alfa,labeller = label_parsed,ncol=2, scales = 'free_y') +
#   geom_line(stat='density', aes(linetype = metodo), size = 2) +
#   geom_vline(aes(xintercept=alfa2),
#              color="blue", linetype="dashed", size=1)+
#   #scale_x_continuous(breaks={},limits = c(-12,0))+
#   #scale_color_discrete(name = LegendTitle,labels = legenda.nomb)+
#   scale_colour_manual(name = "Method",
#                       values = c("#56B4E9","coral", "magenta","#009E73"),
#                       labels = legenda.nomb)+
#   scale_linetype_manual(name = "Method",
#                         values = c("dashed", "twodash" ,"dotted","longdash"),
#                         labels = legenda.nomb)+
#   scale_shape_manual(name = "Method",
#                      values = c(17, 19, 18,15),
#                      labels = legenda.nomb)+
#   theme_few()+
#   theme(legend.position="top",
#         legend.text = element_text( size=20),
#         legend.title = element_text( size=20),
#         #axis.text.y = element_text( size = 20 ),
#         axis.text.x = element_text(hjust = 1, size = 20),
#         axis.title.y = element_text( size = 20 ),
#         axis.title.x = element_text( size = 20 ),
#         #axis.ticks.length=unit(0.5,"cm"),
#         strip.text = element_text(size = 20))+
#   labs(x=expression(paste(widehat(alpha))), y = "Density")
# p3
# 
# p4<-ggplot(base.df%>%filter(alfa2==-8), aes(x=alfa.est,color=metodo,group =metodo)) + 
#   facet_wrap(~alfa,labeller = label_parsed,ncol=2, scales = 'free_y') +
#   geom_line(stat='density', aes(linetype = metodo), size = 2) +
#   geom_vline(aes(xintercept=alfa2),
#              color="blue", linetype="dashed", size=1)+
#   #scale_x_continuous(breaks={},limits = c(-12,0))+
#   #scale_color_discrete(name = LegendTitle,labels = legenda.nomb)+
#   scale_colour_manual(name = "Method",
#                       values = c("#56B4E9","coral", "magenta","#009E73"),
#                       labels = legenda.nomb)+
#   scale_linetype_manual(name = "Method",
#                         values = c("dashed", "twodash" ,"dotted","longdash"),
#                         labels = legenda.nomb)+
#   scale_shape_manual(name = "Method",
#                      values = c(17, 19, 18,15),
#                      labels = legenda.nomb)+
#   theme_few()+
#   theme(legend.position="top",
#         legend.text = element_text( size=20),
#         legend.title = element_text( size=20),
#         #axis.text.y = element_text( size = 20 ),
#         axis.text.x = element_text(hjust = 1, size = 20),
#         axis.title.y = element_text( size = 20 ),
#         axis.title.x = element_text( size = 20 ),
#         #axis.ticks.length=unit(0.5,"cm"),
#         strip.text = element_text(size = 20))+
#   labs(x=expression(paste(widehat(alpha))), y = "Density")
# p4
# 
# p1+p2+p3+p3+p4+plot_layout(ncol = 2)
# 
# ######################################## 
# getwd()
# ggsave("G:/Mi unidad/Github/KernelEstimationGit/figures/PaperTesis/Asymptotic_n500_-5_-8.eps", plot = last_plot(), device = "eps",scale=2)
# 
# 
