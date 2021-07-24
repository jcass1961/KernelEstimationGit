setwd("C:/Users/Usuario/Google Drive (jacassetti@docentes.unm.edu.ar)/Daiana")


library(tidyverse)
library(plotly)

source("Code/Rutinas/EntropiaFisherGI0_2par.R")

##############################################

alfa<-seq(-15,-0.5,by=0.5)
gamma<-seq(1,100,by=1)

EntropiaGI0.2P.L2<-function(alfa,gama) EntropiaGI0.2P(alfa,gama,2)

H=outer(alfa,gamma,EntropiaGI0.2P.L2)
png(file="prueba.png",width=600, height=350)
plot=persp3d(alfa,gamma,H,col="skyblue", col.grid = "grey", shade = 0.5, bty = "f",
       xlab = "", ylab = "", zlab = "",
       axes = FALSE,box=TRUE,
       size = .75, type = "s", lit = FALSE)
contourLines3d(plot,nlevels=10)
# Add axes to specific sides. Possible values are "x--", "x-+", "x+-", and "x++".
axes3d(edges = c("x--", "y+-", "z--"),
       ntick = 6,                       # Attempt 6 tick marks on each side
       cex = 1.5,
       family="serif")                       # Smaller font

# Add axis labels. 'line' specifies how far to set the label from the axis.
mtext3d(expression(alpha), edge = "x--", line = 2,cex=2)
mtext3d(expression(gamma), edge = "y+-",line = 2,cex=2)
mtext3d("H",edge = "z--", line = 2,cex=2,family="serif")


rgl.snapshot('3dplot.png', fmt = 'png')



