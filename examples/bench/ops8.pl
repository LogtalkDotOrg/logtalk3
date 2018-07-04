% generated: 25 October 1989
% option(s): 
%
%   (deriv) ops8
%
%   David H. D. Warren
%
%   symbolic derivative of (x+1)*((^(x,2)+2)*(^(x,3)+3))

top:-ops8.

ops8 :- d((x+1)*((^(x,2)+2)*(^(x,3)+3)),x,_).

d(U+V,X,DU+DV) :- !, 
    d(U,X,DU),
    d(V,X,DV).
d(U-V,X,DU-DV) :- !,
    d(U,X,DU),
    d(V,X,DV).
d(U*V,X,DU*V+U*DV) :- !,
    d(U,X,DU),
    d(V,X,DV).
d(U/V,X,(DU*V-U*DV)/(^(V,2))) :- !,
    d(U,X,DU),
    d(V,X,DV).
d(^(U,N),X,DU*N*(^(U,N1))) :- !, 
    integer(N),
    N1 is N-1,
    d(U,X,DU).
d(-U,X,-DU) :- !,
    d(U,X,DU).
d(exp(U),X,exp(U)*DU) :- !,
    d(U,X,DU).
d(log(U),X,DU/U) :- !,
    d(U,X,DU).
d(X,X,1) :- !.
d(_,_,0).
