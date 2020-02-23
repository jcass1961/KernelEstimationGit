

dGI0fromF<-function(z,alfap,gamap,L)
{
  df(-alfap/gamap*z,2*L,-2*alfap)*(-alfap/gamap)
}

rGI0fromF<-function(n,alfap,gamap,L)
{
  (-gamap/alfap)*rf(n,2*L,-2*alfap)
}
  
pGI0fromF<-function(q,alfap,gamap,L)
{
  pf(-alfap/gamap*q,2*L,-2*alfap)
}

qGI0fromF<-function(p,alfap,gamap,L)
{
  qf(p,2*L,-2*alfap)*(gamap/-alfap)
}

