%
%  marelle
%
%  Test driven system administration.
%

%
%  WRITING DEPS
%
%  You need one each of these three statements. E.g.
%
%  pkg(python).
%  met(python, _) :- which(python, _).
%  meet(python, osx) :- bash('brew install python').
%
:- multifile pkg/1.
:- multifile meet/2.
:- multifile met/2.
:- multifile depends/3.

:- dynamic platform/1.

% pkg(?Pkg) is nondet.
%   Is this a defined package name?

% met(+Pkg, +Platform) is semidet.
%   Determine if the package is already installed.

% meet(+Pkg, +Platform) is semidet.
%   Try to install this package.

% where to look for dependencies
marelle_search_path('~/.marelle/deps').
marelle_search_path('marelle-deps').
marelle_search_path('deps').

%
%  CORE CODE
%
%

main :-
    ( current_prolog_flag(os_argv, Argv) ->
        true
    ;
        current_prolog_flag(argv, Argv)
    ),
    append([_, _, _, _, _, _], Rest, Argv),
    detect_platform,
    load_deps,
    ( Rest = [Command|SubArgs] ->
        main(Command, SubArgs)
    ;
        usage
    ).

main(scan, Rest) :-
    ( Rest = ['--all'] ->
        scan_packages(all)
    ; Rest = ['--missing'] ->
        scan_packages(missing)
    ; Rest = [] ->
        scan_packages(unprefixed)
    ).

main(list, Rest) :-
    ( Rest = [] ; Rest = [Pattern] ),
    !,
    ( Rest = [] ->
        findall(P, (pkg(P), \+ ishidden(P)), Ps0)
    ; Rest = [Pattern] ->
        join(['*', Pattern, '*'], Glob),
        findall(P, (pkg(P), wildcard_match(Glob, P), \+ ishidden(P)), Ps0)
    ),
    sort(Ps0, Ps),
    (
        member(P, Ps),
        writeln(P),
        fail
    ;
        true
    ).

main(met, [Pkg]) :-
    !,
    ( pkg(Pkg) ->
        ( met(Pkg) ->
            writeln('ok')
        ;
            writeln('not met'),
            fail
        )
    ;
        join(['ERROR: ', Pkg, ' is not defined as a dep'], Msg),
        writeln(Msg),
        fail
    ).

main(met, ['-q', Pkg]) :- !, met(Pkg).

main(meet, Pkgs) :- !, maplist(meet_recursive, Pkgs).

main(platform, []) :- !, platform(Plat), writeln(Plat).

% start an interactive prolog shell
main(debug, []) :- !, prolog.

% run the command with profiling
main(profile, [Cmd|Rest]) :- !, profile(main(Cmd, Rest)).

% time the command and count inferences
main(time, [Cmd|Rest]) :- !, time(main(Cmd, Rest)).

main(_, _) :- !, usage.

meet_recursive(Pkg) :- meet_recursive(Pkg, 0).

meet_recursive(Pkg, Depth0) :-
    ( pkg(Pkg) ->
        ( cached_met(Pkg) ->
            join([Pkg, ' ✓'], M0),
            writeln_indent(M0, Depth0)
        ; ( join([Pkg, ' {'], M2),
            writeln_indent(M2, Depth0),
            force_depends(Pkg, Deps),
            Depth is Depth0 + 1,
            length(Deps, L),
            repeat_val(Depth, L, Depths),
            maplist(meet_recursive, Deps, Depths),
            meet(Pkg),
            cached_met(Pkg)
        ) ->
            join(['} ok ✓'], M4),
            writeln_indent(M4, Depth0)
        ;
            join(['} fail ✗'], M5),
            writeln_indent(M5, Depth0),
            fail
        )
    ;
        join(['ERROR: ', Pkg, ' is not defined as a dep'], M6),
        writeln_indent(M6, Depth0),
        fail
    ).

repeat_val(X, N, Xs) :-
    repeat_val(X, N, [], Xs).
repeat_val(X, N0, Xs0, Xs) :-
    ( N0 = 0 ->
        Xs = Xs0
    ;
        N is N0 - 1,
        repeat_val(X, N, [X|Xs0], Xs)
    ).


met(Pkg) :-
    platform(P),
    met(Pkg, P).

meet(Pkg) :-
    platform(P),
    meet(Pkg, P).

:- dynamic already_met/1.

cached_met(Pkg) :-
    ( already_met(Pkg) ->
        true
    ; met(Pkg) ->
        assertz(already_met(Pkg))
    ).

% force_depends(+Pkg, -Deps) is det.
%   Get a list of dependencies for the given package on this platform. If
%   none exist, return an empty list. Supports multiple matching depends/3
%   statements for a package, or none.
force_depends(Pkg, Deps) :-
    platform(P),
    findall(DepSet, depends(Pkg, P, DepSet), DepSets),
    flatten(DepSets, Deps0),
    list_to_set(Deps0, Deps).

% scan_packages(+Visibility) is det.
%   Print all supported packages, marking installed ones with an asterisk.
scan_packages(Visibility) :-
    writeln_stderr('Scanning packages...'),
    findall(P, package_state(P), Ps0),
    sort(Ps0, Ps1),
    ( Visibility = all ->
        Ps = Ps1
    ; Visibility = missing ->
        include(ismissing_ann, Ps1, Ps2),
        exclude(ishidden_ann, Ps2, Ps)
    ;
        exclude(ishidden_ann, Ps1, Ps)
    ),
    maplist(writepkg, Ps).

ishidden(P) :- atom_concat('__', _, P).

ishidden_ann(pkg(P, _)) :- ishidden(P).

ismissing_ann(pkg(_, unmet)).

% package_state(-Ann) is nondet
%   Find a package and it's current state as either met or unmet.
package_state(Ann) :-
    pkg(Pkg),
    ground(Pkg),
    ( cached_met(Pkg) ->
        Ann = pkg(Pkg, met)
    ;
        Ann = pkg(Pkg, unmet)
    ).

% load_deps is det.
%   Looks for dependency files to load from a per-user directory and from
%   a project specific directory.
load_deps :-
    findall(P, (
        marelle_search_path(P0),
        expand_path(P0, P),
        exists_directory(P)
    ), Ps),
    ( maplist(load_deps, Ps) ->
        true
    ;
        true
    ).

load_deps(Dir) :-
    join([Dir, '/*.pl'], Pattern),
    expand_file_name(Pattern, Deps),
    load_files(Deps).

usage :-
    writeln('Usage: marelle list [pattern]'),
    writeln('       marelle scan [--all | --missing]'),
    writeln('       marelle met [-q] <target>'),
    writeln('       marelle meet <target>'),
    writeln('       marelle platform'),
    writeln(''),
    writeln('Detect and meet dependencies. Searches ~/.marelle/deps and the folder'),
    writeln('marelle-deps in the current directory if it exists.').

% which(+Command, -Path) is semidet.
%   See if a command is available in the current PATH, and return the path to
%   that command.
which(Command, Path) :-
    join(['which ', Command], C),
    bash_output(C, Path).

% which(+Command) is semidet.
%   See if a command is available in the current PATH.
which(Command) :- which(Command, _).

% platform(-Platform).
%   Determines the current platform (e.g. osx, ubuntu). Needs to be called
%   after detect_platform/0 has set the platform.
platform(_) :- fail.

% detect_platform is det.
%   Sets platform/1 with the current platform.
detect_platform :-
    bash_output('uname -s', OS),
    ( OS = 'Linux' ->
        linux_codename(Codename),
        Platform = linux(Codename)
    ; OS = 'Darwin' ->
        Platform = osx
    ;
        Platform = unknown
    ),
    retractall(platform(_)),
    assertz(platform(Platform)).

join(L, R) :- atomic_list_concat(L, R).

% linux_codename(-Codename).
%   Determine the codename of the linux release (e.g. precise).
linux_codename(Codename) :-
    ( ( which('lsb_release', _),
        bash_output('lsb_release -c | sed \'s/^[^:]*:\\s//g\'', Codename)
      ) ->
      true
    ;
        Codename = unknown
    ).

writeln_indent(L, D) :- write_indent(D), writeln(L).
writeln_star(L) :- write(L), write(' *\n').
write_indent(D) :-
    ( D = 0 ->
        true
    ;
        D1 is D - 1,
        write('  '),
        write_indent(D1)
    ).

writepkg(pkg(P, met)) :- writeln_star(P).
writepkg(pkg(P, unmet)) :- writeln(P).

install_apt(Name) :-
    ( bash_output('whoami', root) ->
        Sudo = ''
    ;
        Sudo = 'sudo '
    ),
    join([Sudo, 'apt-get install -y ', Name], Cmd),
    bash(Cmd).

install_brew(Name) :-
    join(['brew install ', Name], Cmd),
    bash(Cmd).

home_dir(D0, D) :-
    getenv('HOME', Home),
    join([Home, '/', D0], D).

git_clone(Source, Dest) :-
    join(['git clone --recursive ', Source, ' ', Dest], Cmd),
    bash(Cmd).

%  command packages: met when their command is in path
:- multifile command_pkg/1.
:- multifile command_pkg/2.

pkg(P) :- command_pkg(P, _).
met(P, _) :- command_pkg(P, Cmd), which(Cmd).

command_pkg(P, P) :- command_pkg(P).

writeln_stderr(S) :-
    open('/dev/stderr', write, Stream),
    write(Stream, S),
    write(Stream, '\n'),
    close(Stream).

% bash(+Cmd, -Code) is semidet.
%   Execute the given command in shell. Catch signals in the subshell and
%   cause it to fail if CTRL-C is given, rather than becoming interactive.
%   Code is the exit code of the command.
bash(Cmd0, Code) :-
    ( is_list(Cmd0) ->
        join(Cmd0, Cmd)
    ;
        Cmd = Cmd0
    ),
    catch(shell(Cmd, Code), _, fail).

% bash(+Cmd) is semidet.
%   Run the command in shell and fail unless it returns with exit code 0.
bash(Cmd) :- bash(Cmd, 0).

% bash_output(+Cmd, -Output) is semidet.
%   Run the command in shell and capture its stdout, trimming the last
%   newline. Fails if the command doesn't return status code 0.
bash_output(Cmd, Output) :-
    tmp_file(syscmd, TmpFile),
    join([Cmd, ' >', TmpFile], Call),
    bash(Call),
    read_file_to_codes(TmpFile, Codes, []),
    atom_codes(Raw, Codes),
    atom_concat(Output, '\n', Raw).

:- dynamic marelle_has_been_updated/0.

pkg(selfupdate).
met(selfupdate, _) :- marelle_has_been_updated.
meet(selfupdate, _) :-
    bash('cd ~/.local/marelle && git pull'),
    assertz(marelle_has_been_updated).

%:- include('00-util').
%:- include('01-python').
%:- include('02-fs').
%:- include('03-homebrew').
%:- include('04-apt').
%:- include('05-git').
%:- include('06-meta').
%:- include('07-managed').
%:- include('sudo').
