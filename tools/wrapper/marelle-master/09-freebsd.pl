%
%  09-freebsd.pl
%  marelle
%

% installs_with_pkgng(Pkg).
%  Pkg installs with pkgng package of same name.
:- multifile installs_with_pkgng/1.

% installs_with_pkgng(Pkg, PkgName).
%  Pkg installs with pkgng package called PkgName.
:- multifile installs_with_pkgng/2.

% installs_with_ports(Pkg, PortName).
%  Pkg installs with FreeBSD port called PortName.
:- multifile installs_with_ports/2.

% installs_with_ports(Pkg, PortName, Options).
%  Pkg installs with FreeBSD port called PortName and with Options.
:- multifile installs_with_ports/3.

installs_with_pkgng(P, P) :- installs_with_pkgng(P).
installs_with_ports(P, N, '') :- installs_with_ports(P, N).

exists_pkgng(Name) :- sh(['pkg info ', Name, ' >/dev/null 2>/dev/null']).

met(P, freebsd) :-
    ( installs_with_pkgng(P, PkgName) ->
        exists_pkgng(PkgName)
    ; installs_with_ports(P, PortName, _) ->
        exists_pkgng(PortName)
    ).

meet(P, freebsd) :-
    ( installs_with_pkgng(P, PkgName) ->
        install_pkgng(PkgName)
    ; installs_with_ports(P, PortName, Options) ->
        install_ports(PortName, Options)
    ).

install_pkgng(Name) :-
    sudo_or_empty(Sudo),
    sh([Sudo, 'pkg install -y ', Name]).

install_ports(Name, Options) :-
    sudo_or_empty(Sudo),
    sh([Sudo, 'make BATCH=yes ', Options, ' -C/usr/ports/', Name, ' install clean']).
