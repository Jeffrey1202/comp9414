edge(d,f).
edge(d,h).
edge(f,g).
edge(g,h).
edge(h,i).
c(X,Y):-edge(X,Y).
c(X,Y):-edge(X,Z),c(Z,Y).