%
%  util.pl
%  marelle-deps
%
%  Utility methods common to multiple deps.
%

expand_path(Path0, Path) :-
    ( atom_concat('~/', Suffix, Path0) ->
        getenv('HOME', Home),
        join([Home, '/', Suffix], Path)
    ;
        Path = Path0
    ).

isfile(Path0) :-
    expand_path(Path0, Path),
    exists_file(Path).

isdir(Path0) :-
    expand_path(Path0, Path),
    exists_directory(Path).

make_executable(Path) :-
    sh(['chmod a+x ', Path]).

curl(Source, Dest) :-
    sh(['curl -s -o ', Dest, ' ', Source]).

% sformat(+S0, +Vars, -S) is semidet.
%   String interpolation, where {} is replaced by an argument in the list.
%   Will fail if the number of {} is not the same as the number of vars passed
%   in.
%
%   sformat('Hello ~a!', ['Bob'], 'Hello Bob!').
%
sformat(S0, Vars, S) :-
    atomic_list_concat(Parts, '~a', S0),
    ( length(Vars, N), N1 is N + 1, length(Parts, N1) ->
        true
    ;
        throw('wrong number of arguments in interpolation')
    ),
    interleave(Parts, Vars, S1),
    atomic_list_concat(S1, '', S).

interleave(Xs, Ys, Zs) :-
    ( Ys = [] ->
        Zs = Xs
    ;
        Ys = [Y|Yr],
        Xs = [X|Xr],
        Zs = [X, Y|Zr],
        interleave(Xr, Yr, Zr)
    ).
