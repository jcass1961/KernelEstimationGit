kef2<-function (x, t, h, ker, a = 0, b = 1) 
{
  if (ker == "BE") {
    result <- t
    Logic0 <- ((a <= t) & (t <= b))
    Logic1 <- ((t < a) | (b < t))
    tval <- result[Logic0]
    result[Logic1] = 0
    result[Logic0] <- ((1/((b - a)^(1 + h^(-1)) * 
                      beta(((x -a)/((b - a) * h)) + 1, ((b - x)/((b - a) * h)) + 1)))) * 
                      ((tval - a)^((x - a)/((b - a) * h))) * ((b -tval)^((b - x)/((b - a) * h)))
    return(result)
  }
  else if (ker == "GA") {
    result <- t
    Logic0 <- (0 <= t)
    Logic1 <- (t < 0)
    tval <- result[Logic0]
    result[Logic1] = 0
    result[Logic0] <- dgamma(tval, (x/h) + 1, 1/h)
    return(result)
  }
  else if (ker == "LN") {
    result <- t
    Logic0 <- (0 <= t)
    Logic1 <- (t < 0)
    tval <- result[Logic0]
    result[Logic1] = 0
    result[Logic0] <- dlnorm(tval, meanlog = log(x) + h^2,sdlog = h)
    return(result)
  }
  else if (ker == "LN2") {
    result <- t
    Logic0 <- (0 <= t)
    Logic1 <- (t < 0)
    tval <- result[Logic0]
    result[Logic1] = 0
    result[Logic0] <- dlnorm(tval, meanlog = log(x) ,sdlog = 4*log(1+h))
    return(result)
  }
  else if (ker == "BS") {
    result <- t
    Logic0 <- (0 <= t)
    Logic1 <- (t < 0)
    tval <- result[Logic0]
    result[Logic1] = 0
    result[Logic0] <- dcbs(tval,  h^(1/2) , x)
    return(result)
  }
  else if (ker == "GA2") {
    result <- t
    Logic0 <- (0 <= t & t < 2*h)
    Logic1 <- (2*h <= t)
    Logic2 <- (t < 0)
    tval <- result[Logic0]
    tval1 <- result[Logic1]
    result[Logic2] = 0
    result[Logic0] <- dgamma(tval, 1/4*(x/h)^2 + 1, 1/h)
    result[Logic1] <- dgamma(tval1, x/h, 1/h)
    return(result)
  }
  else if (ker == "IG") {
    result <- t
    Logic0 <- (0 < t)
    Logic1 <- (t <= 0)
    tval <- result[Logic0]
    result[Logic1] <- 0
    result[Logic0] <- dinvgauss(tval ,mean=x,shape=1/h)
    return(result)
  }
  else if (ker == "IGJstar") {
    result <- t
    Logic0 <- (0 < t)
    Logic1 <- (t <= 0)
    tval <- result[Logic0]
    result[Logic1] <- 0
    result[Logic0] <- dinvgauss(tval ,mean=x,shape=1/h)
    return(result)
  }
}