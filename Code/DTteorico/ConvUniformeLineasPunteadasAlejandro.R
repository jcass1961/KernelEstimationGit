require("ggplot2")

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
# alpha3<--30
# alpha4<--50
alpha.ga<-"ga"

alfa1.fact<-as.factor(alpha1)
alfa2.fact<-as.factor(alpha2)
alfaGa.fact<-as.factor(alpha.ga)

rango<-seq(0,5,0.01)
gi.1<-data.frame(L=L,alfa=alfa1.fact,rango=rango,GI=GI2(rango,alpha1,L))
gi.2<-data.frame(L=L,alfa=alfa2.fact,rango=rango,GI=GI2(rango,alpha2,L))
gi.ga<-data.frame(L=L,alfa=alfaGa.fact,rango=rango,GI.ga=dgamma(rango,shape=L,scale=1/L))

#head(gi.1)
#head(gi.2)
#head(gi.ga)

df2<-cbind(gi.ga,GI.1=GI2(rango,alpha1,L),GI.2=GI2(rango,alpha2,L))
head(df2)
#View(df)

p<-ggplot(df2,aes(x=rango))+
  geom_ribbon(aes(ymin = GI.ga - epsilon, ymax = GI.ga + epsilon), fill = "grey90") 
  
p

p+geom_line(aes(y = GI.ga,color="Ga"),size=1)+
  geom_line(aes(y = GI.1,color="-8"),linetype="dashed",size=1)+
  geom_line(aes(y = GI.2,color="-20"),linetype="twodash",size=1)+
  #scale_linetype_manual(values=c( "twodash", "dotted", "twodash"))+
  scale_color_manual(name="   Densities",values = c("red", "blue", "black"), # Color specification
                     breaks=c("-20", "-8", "Ga"),
                     labels = c(expression(paste(alpha," =-20    ")), 
                                expression(paste(alpha," =-8      ")), 
                                expression(paste("      ", Gamma(1,1/3)))))+
  scale_linetype_manual(values=c( "twodash", "dotted", "twodash"))+
  theme_minimal()+
  labs(x="x", y = "Densities")

