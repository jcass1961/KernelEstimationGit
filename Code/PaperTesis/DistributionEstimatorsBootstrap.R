library(tidyverse)
library("ggthemes")
library("wesanderson")

getwd()
enableJIT(3)

setwd("G:/Mi unidad/Github/KernelEstimationGit/Data/PaperTesis")

## Estimaciones de alfa para cada una de las muestras elegidas

base<-read.csv("Bootstrap2000_FinalCon121.csv",sep=";")
head(base)

base.df<-base%>% gather(.,key=metodo,value=alfa.est,3:6)
base.df

LegendTitle = "Método"
legenda.nomb<-c("MV"="MV","GA"=expression(Gamma),"LN"="LN","LC"="LC")

n.labs = c("9"="n=9","25"="n=25","49"="n=49","81"="n=81","121"="n=121","500"="n=500")
nombre.ticks<-c(-5,-3,-1)

p<-ggplot(base.df, aes(x=alfa.est,color=metodo)) + 
  facet_wrap(~n,labeller = labeller(n=n.labs),ncol=3) + 
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
  labs(x=expression(paste(widehat(alpha))), y = "Density")
p
# p+ geom_vline(aes(xintercept=-3),
#               color="blue", linetype="dashed", size=1)+ geom_hline(aes(yintercept=0),
#                                                                    color="black",  size=1)+
#   facet_wrap(~n,labeller = labeller(n=n.labs),ncol=3)  
######################################## 
