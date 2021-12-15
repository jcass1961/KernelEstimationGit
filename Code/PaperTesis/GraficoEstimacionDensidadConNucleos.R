library("cubature")
library("Conake")
require(ggplot2)
require(ggfortify)
require(ggthemes)
require("statmod") ## nucleo IG


enableJIT(3)

setwd("C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Code/OtroPaper")
#setwd("C:/Users/julia/Dropbox/Procesamiento de imagenes/KernelEstimation/Code/OtroPaper")

source("C:../Rutinas/Genera GI en funcion de alfa y gamma.R")
source("C:../Rutinas/DefineDensidadGI_alfaygamma.R")
source("C:../Rutinas/Alfa0Gama0PorMomentos_V2.R")

source("C:../Rutinas/DistanciaTriangularNG1conCteAlfaGama.R")


source("C:../Rutinas/nucleos.R")                ###############
source("../Rutinas/CrossValidationdeR.R")     ###############
source("../Rutinas/MOM_1medio.R")
#source("C:../Rutinas/Conakereport2conIntegrate.R")                ###############
source("C:../Rutinas/Conakereport2.R") 
source("C:../Rutinas/boptimoNucleoGamma_alfaygama.R")

getwd()
set.seed(1775)

L<-8
alfa<--5
gama<--alfa-1


n<-25
random<-replicate(5,generoGI.alfagama(alfa,gama,n,L))

############################################################################☺
dGI0 <- function(z, p_alpha, p_gamma, p_Looks, log=FALSE) {
  
  if(log==TRUE) {
    return(
      (p_Looks*log(p_Looks) + lgamma(p_Looks-p_alpha) + (p_Looks-1)*log(z) ) - 
        (p_alpha*log(p_gamma) + lgamma(-p_alpha) + lgamma(p_Looks) + 
           (p_Looks-p_alpha)*log(p_gamma + z*p_Looks) ) 
    )   
  }
  else { return( 
    ( p_Looks^p_Looks * gamma(p_Looks-p_alpha) * z^(p_Looks-1) ) / 
      (p_gamma^p_alpha * gamma(-p_alpha) * gamma(p_Looks) * (p_gamma + z*p_Looks)^(p_Looks-p_alpha)) 
  )
  }
}

############################################################################
EstimadorNucleoGama1<-function(x,datos,ancho,cte)
{
  n<-length(datos)
  EstNuGa<-1/(n*cte)*sum(sapply(x,function(x) dgamma(datos,shape=x/ancho+1,scale=ancho)))
  return(EstNuGa)
}

############################################################################
#### DEFINE ESTIMADOR NUCLEO LOGNORMAL
EstimadorNucleoLN<-function(x,datos,b,Cte)
{
  n<-length(datos)
  EstNuGa<-1/(n*Cte)*sum(sapply(x,function(x) dlnorm(datos,log(x)+b^2,b)))
  return(EstNuGa)
}

############################################################################
#### DEFINE ESTIMADOR NUCLEO INVERSO GAUSSIANO
EstimadorNucleoIG<-function(x,datos,ancho,cte)
{
  n<-length(datos)
  EstNuIG<-1/(n*cte)*sum(sapply(x,function(x) dinvgauss(datos,mean=x,shape=1/ancho)))
  return(EstNuIG)
}

############################################################################

############################################################################
##### PROGRAMA PRINCIPAL

datosGI<-generoGI.alfagama(alfa,gama,n,L)
datosGI.sort<-(sort(datosGI))
datosGI.sort


ancho.GA<-cv(datosGI.sort,ker="GA")$hcv
ancho.LN<-cv(datosGI.sort,ker="LN")$hcv
ancho.IG<-cv(datosGI.sort,ker="IG")$hcv

const.GA<-Conakereport2(datosGI.sort,ker="GA",h=ancho.GA,nx=100,a = 0, b = 1)$C_n
const.LN<-Conakereport2(datosGI.sort,ker="LN",h=ancho.LN,nx=100,a = 0, b = 1)$C_n
const.IG<-Conakereport2(datosGI.sort,ker="IG",h=ancho.IG,nx=100,a = 0, b = 1)$C_n
const.LN
ancho.LN

## Versión 2
rangox<-seq(0,10,0.001)

#t<-seq(0,4,0.001)


fest0.GA<-function(x) EstimadorNucleoGama1(x,datosGI.sort,ancho.GA,const.GA)
fest0.LN<-function(x) EstimadorNucleoLN(x,datosGI.sort,ancho.LN,const.LN)
fest0.IG<-function(x) EstimadorNucleoIG(x,datosGI.sort,ancho.IG,const.IG)

fest.GA<-sapply(rangox,fest0.GA)
fest.LN<-sapply(rangox,fest0.LN)
fest.IG<-sapply(rangox,fest0.IG)


resultados <- data.frame(rangox, fest.GA, fest.LN, fest.IG)

legenda.nomb<-c("Set1"=expression(Gamma),"Set2"="LN","Set3"="IG")

ggplot(resultados, aes(x=rangox)) +
  geom_line(aes(y=fest.GA, colour="Set1",linetype="Set1",alpha =I(0.7)),size=2.5) +
  geom_line(aes(y=fest.LN, colour="Set2",linetype="Set2",alpha =I(0.7)),size=2) +
  geom_line(aes(y=fest.IG, colour="Set3",linetype="Set3",alpha =I(0.9)),size=1.8) +
  stat_function(fun=dGI0, args=list(alfa,gama,L), n=1000, colour="black", size=1.2) + 
  xlim(0,5)+
  xlab("x")+
  ylab(expression(widehat(f)[G[I]^0]))+
  geom_hline(yintercept=0) +
scale_colour_manual(name = "Kernels",
                    values = c("blue","red","green"),
                    labels = legenda.nomb)+
  scale_linetype_manual(name = "Kernels", 
                        values = c("Set1" ="solid", "Set2"="solid" ,"Set3"= "solid"),
                        labels = legenda.nomb)+
  theme_few()+
  theme(legend.position="top",
        legend.text = element_text( size=20),
        legend.title = element_text( size=20),
        axis.text.y = element_text( size = 20 ),
        axis.title.y = element_text( size = 20 ),
        axis.title.x = element_text( size = 20 ),
        strip.text = element_text(size = 20))

setwd("C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Figures/PaperTesis")
getwd()
graf.nucleos<-"NucleosGALNyIG.pdf"
ggsave(graf.nucleos, plot = last_plot(), device = "pdf",scale=1.2)
