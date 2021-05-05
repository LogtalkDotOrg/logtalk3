%
%  02-fs.pl
%  marelle-deps
%

:- multifile symlink_step/3.

pkg(P) :- symlink_step(P, _, _).
met(P, _) :-
    symlink_step(P, Dest, Link), !,
    is_symlinked(Dest, Link).
meet(P, _) :-
    symlink_step(P, Dest, Link), !,
    symlink(Dest, Link).

% is_symlinked(+Dest, +Link) is semidet.
%   Check if a desired symlink already exists.
is_symlinked(Dest0, Link0) :-
    expand_path(Dest0, Dest),
    expand_path(Link0, Link),
    read_link(Link, _, Dest).

% symlink(+Dest, +Link) is semidet.
%   Create a symbolic link pointing to Dest. May fail if a file already
%   exists in the location Link.
symlink(Dest0, Link0) :-
    expand_path(Dest0, Dest),
    expand_path(Link0, Link),
    sh(['ln -s ', Dest, ' ', Link]).
