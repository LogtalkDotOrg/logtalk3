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
    join(['chmod a+x ', Path], Cmd),
    bash(Cmd).

curl(Source, Dest) :-
    join(['curl -s -o ', Dest, ' ', Source], Cmd),
    bash(Cmd).
