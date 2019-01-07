#####################################
#### Define UNA DISTRIBUCION GI #####
#### PARAMETROS: alfa, gamma y L ####
#####################################

GI0.alfagama<-function(z,alfap,gamap,L)
{L^L*gamma(L - alfap)/((gamap)^alfap*gamma(L)*gamma(-alfap))*z^( L - 1)/((gamap) + L*z)^(L - alfap)}
