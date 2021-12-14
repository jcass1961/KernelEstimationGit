library("plyr")
library("xtable")
setwd("C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/data/Tesis/OtroPaper/SinContConConst")

getwd()



### VALOR DE L
L=3

######################################################################################################
######################################################################################################

### NOMBRE BASE 
# nombre.base1<-paste("base500_NOCONT_MVyLNyNG1_L",L, sep = "")
# nombre.base2<-paste("base_NOCONT_MVyLNyNG1_L",L,"_TODOalfa_n=1000v2.csv", sep = "")

nombre.base1<-paste("base500_NoCont_L3_TODOESTIM_OPTIM_ConCONST_hasta121_MOM1mediotodoalfa_VERSION2FINAL.csv",sep = "")


##########################################################################################
###########################################################################################

### COMIENZO DEL PROGRAMA
# base1 <- read.csv(nombre.base1)[,2:7]
# base2 <- read.csv(nombre.base2)
base0<-read.csv(nombre.base1)

names(base0)

base<-data.frame(base0[,1:4],base0$alfa.DT.GA.BFGS,base0$alfa.DT.LN.BFGS,
                 base0$alfa.DT.IGJstar.BFGS,base0$alfa.DT.IG.BFGS,base0$alfa.LC)
head(base)




menos20.col1<-subset(base,base[,4]==-20)
menos20.col2<-subset(base,base[,5]==-20)
menos20.col3<-subset(base,base[,6]==-20)
menos20.col4<-subset(base,base[,7]==-20)
menos20.col5<-subset(base,base[,8]==-20)
menos20.col6<-subset(base,base[,9]==0)



ordena.tabla<-function(datos)
{
  #tabla.nom00<-table(datos$n,datos$alfa)
  tabla0<-as.data.frame(table(datos$n,datos$alfa))
  names(tabla0)<-c("n","alfa","frec")
  tabla<-data.frame(alfa=tabla0$alfa,
                    n=tabla0$n,
                    frec=tabla0$frec)
  
  orden<-arrange(tabla,desc(tabla$alfa),tabla$n)
  return(orden)
}

ordena.1<-ordena.tabla(menos20.col1)
ordena.2<-ordena.tabla(menos20.col2)
ordena.3<-ordena.tabla(menos20.col3)
ordena.4<-ordena.tabla(menos20.col4)
ordena.5<-ordena.tabla(menos20.col5)
ordena.6<-ordena.tabla(menos20.col6)

junto2y3 <- merge(ordena.2, ordena.3, by.x=c("alfa","n"), 
                  by.y=c("alfa","n"),all=TRUE)

junto2y3 <- merge(ordena.2, ordena.3, by.x=c("alfa","n"), 
                     by.y=c("alfa","n"),all=TRUE)
junto4y5 <- merge(ordena.4, ordena.5, by.x=c("alfa","n"), 
                  by.y=c("alfa","n"),all=TRUE)

tabla2y3<-data.frame(alfa=junto2y3$alfa,
                  n=junto2y3$n,
                  frec.2=junto2y3$frec.x,
                  frec.3=junto2y3$frec.y)
tabla4y5<-data.frame(alfa=junto4y5$alfa,
                     n=junto4y5$n,
                     frec.4=junto4y5$frec.x,
                     frec.5=junto4y5$frec.y)

ordena2y3<-arrange(tabla2y3,desc(tabla2y3$alfa),tabla2y3$n)
ordena4y5<-arrange(tabla4y5,desc(tabla4y5$alfa),tabla4y5$n)


junto2345<-merge(ordena2y3, ordena4y5, by.x=c("alfa","n"), 
                  by.y=c("alfa","n"),all=TRUE)
ordena2345<-arrange(junto2345,desc(junto2345$alfa),desc(junto2345$n))

junto23456<-merge(ordena2345, ordena.6, by.x=c("alfa","n"), 
                 by.y=c("alfa","n"),all=TRUE)
ordena23456<-arrange(junto23456,desc(junto23456$alfa),desc(junto23456$n))

#colnames(ordena23456)<-c("alfa","n","1","1.5","2","3","4")

data.ord<-arrange(ordena23456,desc(ordena23456$alfa),ordena23456$n)
dim(data.ord)
data.ord2<-data.ord[,c(-1,-7)]
data.ord2

names(data.ord2)
colnames(data.ord2)<-c("n","GA","LN","IGJstar","IG")

data.ord2

##############################################################################
########### ARMO LA TABLA PARA LATEX

data.ord2.table<-xtable(data.ord2)
align(data.ord2.table)<-xalign(data.ord2.table)
digits(data.ord2.table)<-xdigits(data.ord2.table)
display(data.ord2.table)<-xdisplay(data.ord2.table)
print(data.ord2.table, include.rownames = FALSE,booktabs=TRUE,floating=FALSE,latex.environments=NULL)

