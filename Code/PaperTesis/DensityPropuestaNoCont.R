library(MASS)
library(stats4)
library(caTools)
require(ggplot2)
require(ggthemes)
require(tidyr)
require(rstudioapi)

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

base.df<-base0%>% gather(.,key=metodo,value=alfa.est,4:7) %>% filter(alfa==-3)
base.df

LegendTitle = "Method"
legenda.nomb<-c("alfa.MV"=expression(paste("  ",widehat(alpha)[ML])), 
                "alfa.GA"=expression(paste("  ",widehat(alpha)[Gamma])), 
                "alfa.LN"=expression(paste("  ",widehat(alpha)[LN])),
                "alfa.LC"=expression(paste("  ",widehat(alpha)[LC])))

#legenda.nomb<-c("alfa.MV"="ML","alfa.GA"=expression(Gamma),"alfa.LN"="LN","alfa.LC"="LC")

n.labs = c("9"="n=9","25"="n=25","49"="n=49","81"="n=81","121"="n=121","500"="n=500")
nombre.ticks<-c(-5,-3,-1)

p<-ggplot(base.df, aes(x=alfa.est,color=metodo)) + 
  facet_wrap(~n,labeller = labeller(n=n.labs),ncol=3) + 
  geom_line(stat='density', aes(linetype = metodo), size = 2) +
  scale_x_continuous(breaks=c(-3),limits = c(-10,0), labels=scaleFUN)+
  #scale_color_discrete(name = LegendTitle,labels = legenda.nomb)+
  scale_colour_manual(name = "",
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
        legend.text = element_text( size=25),
        legend.title = element_text( size=25),
        axis.text.y = element_text( size = 25 ),
        axis.text.x = element_text(hjust = 1, size = 25,angle=45),
        axis.title.y = element_text( size = 25 ),
        axis.title.x = element_text( size = 25 ),
        #axis.ticks.length=unit(0.5,"cm"),
        strip.text = element_text(size = 25))+
  labs(x=expression(paste(widehat(alpha))), y = "Density")
p
p+ geom_vline(aes(xintercept=-3),
                 color="blue", linetype="dashed", size=1)+ geom_hline(aes(yintercept=0),
             color="black",  size=1)+
  facet_wrap(~n,labeller = labeller(n=n.labs),ncol=3)  
######################################## 
getwd()
ggsave("G:/Mi unidad/Github/KernelEstimationGit/figures/PaperTesis/DensidadEstimadorNoCont.eps", plot = last_plot(), device = "eps",scale=2)



