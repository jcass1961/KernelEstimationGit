library(ggplot2)
library(tidyverse)
library(caret)
library(data.table)
library(e1071)
library(extrafont) 
#loadfonts(device = "win")

getwd()
setwd("./Data/NuevoAnalisisFinal/medidas")

acc0 <- fread("Accuracy_L2.csv")
sse0 <- fread("sse_L2.csv")
head(acc0)
head(sse0)


acc<-as.data.frame(acc0)
head(acc)
acc.graf<-acc%>% gather(.,key=Estimator,value=acc,3:8) 
acc.graf

sse<-as.data.frame(sse0)
sse.graf<-sse%>% gather(.,key=Estimator,value=sse,2:7) %>% arrange(n)
sse.graf


setwd("C:/Users/Usuario/Google Drive (jacassetti@docentes.unm.edu.ar)/AndreDaiJulia/Figures/CISS2021")

legenda <- c(expression(H[A0[1]]),expression(H[C]),
             expression(H[ML]),expression(H["NA"]),
             expression(H[V]),expression(H[VE]))
### acc
#setwd("C:/Users/Usuario/Google Drive (jacassetti@docentes.unm.edu.ar)/AndreDaiJulia/Figures/CISS2021")
pdf("Accuracy_L2.pdf", height = 8, width = 8, pointsize=10)

par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(0,0,0,0))

ggplot(acc.graf) +
  geom_line(aes(x=n, y=acc, colour = Estimator), size=1.2) +
  geom_point(aes(x=n, y=acc, colour = Estimator), size=1.4) +
  ylab("") +
  xlab("sample size") +
  scale_x_continuous(breaks = c(9,25,49,81,121)) +
  scale_color_discrete(name="Estimator",labels = legenda)+
  theme(text=element_text(size=22,  family="serif"),
        legend.position = "bottom",
        axis.title.x = element_text(size = 24),
        axis.text.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        axis.text.y = element_text(size = 22),
        strip.background =element_rect(fill="white"),
        strip.placement = "outside",
        strip.text = element_text(size=22),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) +
  guides(colour = guide_legend(nrow = 1))
dev.off()

#ggsave("Accuracy_L2.pdf", plot = p1, device = "pdf",scale=1)


