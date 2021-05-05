%
%  06-meta.pl
%  marelle-deps
%

:- multifile meta_pkg/2.

pkg(P) :- meta_pkg(P, _).

met(P, _) :- meta_pkg(P, Deps), !,
    maplist(cached_met, Deps).

meet(P, _) :- meta_pkg(P, _), !.

depends(P, _, Deps) :- meta_pkg(P, Deps).
