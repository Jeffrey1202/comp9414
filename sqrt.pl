sqrt_table(N,N,R):-
   Result is sqrt(N),
   R=[[N,Result]].
sqrt_table(N,M,R):-
      N>M,
      M>0,
      Result is sqrt(N),
      Head=[N,Result],
      N1 is N-1,
      sqrt_table(N1,M,R1),
      R=[Head|R1].