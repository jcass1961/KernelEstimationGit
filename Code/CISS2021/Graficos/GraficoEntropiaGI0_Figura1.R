
# setwd("C:/Users/Usuario/Google Drive (jacassetti@docentes.unm.edu.ar)/Daiana")
# 
# setwd("G:/Mi unidad/Daiana")

library(tidyverse)
library(plotly)

#source("Code/Rutinas/EntropiaFisherGI0_2par.R")

source("./Code/CISS2021/EntropiaFisherGI0_2par.R")

alfa<-seq(-15,-0.5,by=0.05)
gamma<-seq(1,100,by=0.05)

EntropiaGI0.2P.L2<-function(alfa,gama) EntropiaGI0.2P(alfa,gama,2)

H=outer(alfa,gamma,EntropiaGI0.2P.L2)
max(H)


axx <- list(
  nticks = 4,
  range = c(-15,-0.5),
  title=paste("\U03B1"),
  titlefont = 20
)

axy <- list(
  nticks = 6,
  range = c(0,100),title=paste("\U03B3")
)

axz <- list(
  nticks = 6,
  range = c(0,10),title=""
)

H=outer(alfa,gamma,Vectorize(EntropiaGI0.2P.L2))%>% t()

# Change font for whole plot
p<-plot_ly(z = H,x=alfa,y=gamma,type = "surface",colorscale = list(c(0, 1), c("black", "cyan")),showscale=FALSE,
         contours=list(z=list(show=TRUE,start=0.5,end=8,size=0.5))) %>% 
         layout(scene=list(xaxis=axx,yaxis=axy,zaxis=axz),
         title=list(font=list(color="black")),
         font = list(family = "Serif"))
p
# setwd("C:/Users/Usuario/Google Drive (jacassetti@docentes.unm.edu.ar)/AndreDaiJulia/Figures/CISS2021")
# plotly_IMAGE(p, format = "png",  out_file = "entropy2.png")


