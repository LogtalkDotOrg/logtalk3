%
%  06-meta.pl
%  marelle-deps
%

% meta_pkg(Name, Plat, Deps).
%   On platform Plat, you can set up Name by meeting Deps.
:- multifile meta_pkg/3.


% meta_pkg(Name, Deps).
%   On any platform, you can set up Name by meeting Deps.
:- multifile meta_pkg/2.

meta_pkg(P, _, Deps) :- meta_pkg(P, Deps).


pkg(P) :- meta_pkg(P, _, _).

met(P, Plat) :- meta_pkg(P, Plat, Deps), !,
    maplist(cached_met, Deps).

meet(P, Plat) :- meta_pkg(P, Plat, _), !.

depends(P, Plat, Deps) :- meta_pkg(P, Plat, Deps).
