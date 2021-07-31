library(ggplot2)
library(tidyverse)
library(caret)
library(data.table)
library(e1071)
library(extrafont) 
#loadfonts(device = "win")


setwd("./Data/CISS2021")

acc0 <- fread("../../../Data/CISS2021/Accuracy_L2.csv")

head(acc0)

acc<-as.data.frame(acc0)
head(acc)
acc.graf<-acc%>% gather(.,key=Estimator,value=acc,3:8) 
acc.graf


#setwd("C:/Users/Usuario/Google Drive (jacassetti@docentes.unm.edu.ar)/AndreDaiJulia/Figures/CISS2021")

legenda <- c(expression(italic(H)[A0[1]]),
             expression(italic(H)[C]),
             expression(italic(H)[ML]),
             expression(italic(H)["NA"]),
             expression(italic(H)[V]),
             expression(italic(H)[VE]))
### acc
#setwd("C:/Users/Usuario/Google Drive (jacassetti@docentes.unm.edu.ar)/AndreDaiJulia/Figures/CISS2021")

par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(0,0,0,0))

ggplot(acc.graf) +
  geom_line(aes(x=n, y=acc, colour = Estimator), size=1) +
  geom_point(aes(x=n, y=acc, colour = Estimator)) +
  ylab("") +
  xlab("sample size") +
  scale_x_continuous(breaks = c(9,25,49,81,121)) +
  scale_color_discrete(name="Estimator",labels = legenda)+
  theme(text=element_text(size=12,  family="serif"),
        legend.position = "bottom",
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        strip.background =element_rect(fill="white"),
        strip.placement = "outside",
        strip.text = element_text(size=10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) +
  guides(colour = guide_legend(nrow = 1)) -> accplot

ggsave(plot=accplot, 
       file="../../../Figures/CISS2021/Accuracy_L2.pdf", width=12, height=12, units="cm")


