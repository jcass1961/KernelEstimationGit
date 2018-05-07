library("ggplot2")

############################################################################
#### DEFINE DENSIDAD GI0 CONSIDERANDO ESPERANZA 1
#### alfa=-gamma-1
GI2<-function(z,alfap,L)
{L^L*gamma(L - alfap)/((-alfap - 1)^alfap*gamma(L)*gamma(-alfap))*z^( L - 1)/((-alfap - 1) + L*z)^(L - alfap)}
############################################################################

epsilon<-0.1

L<-3
alpha1<--8
alpha2<--20
alpha.ga<-"ga"

alfa1.fact<-as.factor(alpha1)
alfa2.fact<-as.factor(alpha2)
alfaGa.fact<-as.factor(alpha.ga)

rango<-seq(0,5,0.01)
gi.1<-data.frame(L=L,alfa=alfa1.fact,rango=rango,GI=GI2(rango,alpha1,L))
gi.2<-data.frame(L=L,alfa=alfa2.fact,rango=rango,GI=GI2(rango,alpha2,L))

#gi.ga<-data.frame(L=L,alfa=alfaGa.fact,rango=rango,GI.ga=dgamma(rango,shape=L,scale=1/L))

gi.ga<-data.frame(L=L,alfa=alfaGa.fact,rango=rango,GI=dgamma(rango,shape=L,scale=1/L))


head(gi.1)
head(gi.2)
head(gi.ga)

df<-rbind(gi.1,gi.2,gi.ga)
head(df)
View(df)

df2<-cbind(gi.ga,GI.1=GI2(rango,alpha1,L),GI.2=GI2(rango,alpha2,L))
head(df2)
#View(df2)

Gi.ga<-rep(dgamma(rango,shape=L,scale=1/L),3)

df3<-cbind(df,Gi.ga)
head(df3)

##########################################################
windows(width=10, height=4, rescale="fit")

d1<-data.frame(x1=GI2(rango,alpha1,L))
d2<-data.frame(x2=GI2(rango,alpha2,L))
d3<-data.frame(x3=dgamma(rango,shape=L,scale=1/L))

p2<-ggplot(df3,aes(x=rango))+
  geom_ribbon(aes(ymin = Gi.ga - epsilon, ymax = Gi.ga + epsilon), fill = "grey80") 

p2

p2+geom_line(data = d1, aes(rango,y = x1, colour = "Set 1", linetype = "Set 1"),size=1.2) +
  geom_line(data = d2, aes(rango,y = x2, colour = "Set 2", linetype = "Set 2"),size=1.2) +
  geom_line(data = d3, aes(rango,y = x3, colour = "Set 3", linetype = "Set 3"),size=1) +
  #scale_x_continuous(limits=c(0,10), breaks=seq(0,10,.5))             +
  labs(x="X-axis Label", y="Y-axis Label")                            +
  scale_colour_manual(name = "Densities", 
                      values = c("Set 1" ="blue", "Set 2" = "red","Set 3"="black"),
                      labels = c(expression(paste(alpha," =-20    ")), 
                                 expression(paste(alpha," =-8      ")), 
                                 expression(paste("      ", Gamma(1,1/3)))))+
  scale_linetype_manual(name = "Densities", values = c("Set 1" ="dotted", "Set 2" = "dashed","Set 3"="solid"),
                        labels = c(expression(paste(alpha," =-20    ")), 
                                   expression(paste(alpha," =-8      ")),
                                   expression(paste("      ", Gamma(1,1/3)))))
#######################################################################
#PDF
trellis.device("pdf", file=graf.alfa.pdf,col = T, width=6, height=4)
print(graf.alfas+plog)
dev.off()
