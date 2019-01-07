#######################################################################################
#######################################################################################
#### GENERA UN REPORTE: ENCUENTRA EL BANDWITH, LA CONSTANTE DE NORMALIZACIÓN
#### Y LOS VALORES DE LA FUNCIÒN DE DENSIDAD EN UNA GRILLA
#### 
#### REQUIERE: LIBRERIA CONAKE, Y FUNCION kef2
#######################################################################################
#######################################################################################

dke2<-function (vec_data, ker, bw, x = NULL,nx, a = 0, b = 1) 
{
  n <- length(vec_data)
  if (is.null(x)) {
    x = seq(min(vec_data), max(vec_data), length.out = nx)
  }
  aux <- matrix(data = vec_data, nrow = nx, ncol = length(vec_data), 
                byrow = TRUE)
  dim(aux)
  aux <- kef2(x, aux, bw, ker, a, b)
  res <- apply(aux, 1, mean)
  C <- simp_int(x, res)
  result <- res/C
  result <- res/C
  return(list(C_n = C, f_n = result))
}

Conakereport2<-function (Vec, ker, h = NULL, nx,a = 0, b = 1) 
{
  if (missing(h)) {
    h1 = cv(Vec, NULL, ker, a, b)
    h = h1$hcv
    bilan = dke2(Vec, ker, h, NULL, nx, a, b)
    message("f_n is the estimated p.d.f. obtained")
    if (ker == "GA") {
      message("using gamma kernel and h_n by cross validation technique.")
      return(list(hn_cv = h, C_n = bilan$C_n))
    }
    else if (ker == "BE") {
      message("using extended beta kernel and h_n by cross validation technique.")
      return(list(hn_cv = h, C_n = bilan$C_n))
    }
    else if (ker == "LN") {
      message("using lognormal kernel and h_n by cross validation technique.")
      return(list(hn_cv = h, C_n = bilan$C_n))
    }
    else if (ker == "IG") {
      message(paste("using inverse Gaussian and h_n by cross validation technique."))
      return(list(hn_cv = h, C_n = bilan$C_n))
    }
    else if (ker == "IGJstar") {
      message(paste("using inverse Gaussian and h_n by cross validation technique."))
      return(list(hn_cv = h, C_n = bilan$C_n))
    }
  }
  else {
    bilan = dke2(Vec, ker, h, NULL, nx, a, b)
    message("f_n is the estimated p.d.f. obtained")
    if (ker == "GA") {
      message("with a gamma kernel and a given bandwidth h=", 
              h)
      return(list(h = h, C_n = bilan$C_n))
    }
    else if (ker == "BE") {
      message("with an extended beta kernel and a given bandwidth h=", 
              h)
      return(list(h = h, C_n = bilan$C_n))
    }
    else if (ker == "LN") {
      message("with a lognormal kernel and a given bandwidth h=", 
              h)
      return(list(h = h, C_n = bilan$C_n))
    }
    else if (ker == "IG") {
      message(paste("using inverse Gaussian and h_n by cross validation technique."))
      return(list(hn_cv = h, C_n = bilan$C_n))
    }
    else if (ker == "IGJstar") {
      message("with a reciprocal Gaussian kernel and a given bandwidth h=", 
              h)
      return(list(h = h, C_n = bilan$C_n))
    }
  }
}