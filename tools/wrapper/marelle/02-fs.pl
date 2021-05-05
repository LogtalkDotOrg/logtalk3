%
%  02-fs.pl
%  marelle-deps
%

:- multifile symlink_step/3.

pkg(P) :- symlink_step(P, _, _).
met(P, _) :-
    symlink_step(P, Dest0, Link0), !,
    expand_path(Dest0, Dest),
    expand_path(Link0, Link),
    read_link(Link, _, Dest).
meet(P, _) :-
    symlink_step(P, Dest, Link), !,
    join(['ln -s ', Dest, ' ', Link], Cmd),
    bash(Cmd).
