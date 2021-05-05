%
%  05-git.pl
%  marelle-deps
%

:- multifile git_step/3.

pkg(P) :- git_step(P, _, _).

met(P, _) :-
    git_step(P, _, Dest0),
    join([Dest0, '/.git'], Dest),
    isdir(Dest).

meet(P, _) :-
    git_step(P, Repo, Dest0),
    expand_path(Dest0, Dest),
    git_clone(Repo, Dest).

git_clone(Source, Dest) :-
    sh(['git clone --recursive ', Source, ' ', Dest]).
