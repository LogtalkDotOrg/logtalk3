%
%  04-apt.pl
%  marelle-deps
%

% installs_with_apt(Pkg).
%   Pkg installs with apt package of same name on all Ubuntu/Debian flavours
:- multifile installs_with_apt/1.

% installs_with_apt(Pkg, AptName).
%   Pkg installs with apt package called AptName on all Ubuntu/Debian
%   flavours. AptName can also be a list of packages.
:- multifile installs_with_apt/2.

installs_with_apt(P, P) :- installs_with_apt(P).

% installs_with_apt(Pkg, Codename, AptName).
%   Pkg installs with apt package called AptName on given Ubuntu/Debian
%   variant with given Codename.
:- multifile installs_with_apt/3.

installs_with_apt(P, _, AptName) :- installs_with_apt(P, AptName).

depends(P, linux(_), ['apt-get-update']) :-
    isfile('/usr/bin/apt-get'),
    installs_with_apt(P, _, _).

:- dynamic apt_updated/0.

pkg('apt-get-update').
met('apt-get-update', linux(_)) :-
    isfile('/usr/bin/apt-get'),
    apt_updated.
meet('apt-get-update', linux(_)) :-
    isfile('/usr/bin/apt-get'),
    sh('sudo apt-get update'),
    assertz(apt_updated).

met(P, linux(Codename)) :-
    isfile('/usr/bin/apt-get'),
    installs_with_apt(P, Codename, PkgName), !,
    ( is_list(PkgName) ->
        maplist(check_dpkg, PkgName)
    ;
        check_dpkg(PkgName)
    ).

meet(P, linux(Codename)) :-
    isfile('/usr/bin/apt-get'),
    installs_with_apt(P, Codename, PkgName), !,
    ( is_list(PkgName) ->
        maplist(install_apt, PkgName)
    ;
        install_apt(PkgName)
    ).

check_dpkg(PkgName) :-
    join(['dpkg -s ', PkgName, ' >/dev/null 2>/dev/null'], Cmd),
    sh(Cmd).

install_apt(Name) :-
    sudo_or_empty(Sudo),
    sh([Sudo, 'apt-get install -y ', Name]).
