%
%  03-homebrew.pl
%  marelle-deps
%
%  Helpers for working with Homebrew.
%  http://mxcl.github.io/homebrew/
%

command_pkg(brew).

meet(brew, osx) :-
    bash('ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"').

% installs_with_brew(Pkg).
%  Pkg installs with homebrew package of same name.
:- multifile installs_with_brew/1.

% installs_with_brew(Pkg, BrewName).
%  Pkg installs with homebrew package called BrewName.
:- multifile installs_with_brew/2.

installs_with_brew(P, P) :- installs_with_brew(P).

depends(P, osx, [brew, 'brew-update']) :- installs_with_brew(P, _).

:- dynamic brew_updated/0.

pkg('brew-update').
met('brew-update', osx) :- brew_updated.
meet('brew-update', osx) :-
    bash('brew update'),
    assertz(brew_updated).

met(P, osx) :-
    installs_with_brew(P, PkgName), !,
    join(['/usr/local/Cellar/', PkgName], Dir),
    isdir(Dir).

meet(P, osx) :-
    installs_with_brew(P, PkgName), !,
    install_brew(PkgName).

% brew_tap(TapName).
%   An extra set of Homebrew packages.
:- multifile brew_tap/2.

% taps are targets
pkg(P) :- brew_tap(P, _).

met(P, osx) :-
    brew_tap(P, TapName), !,
    atomic_list_concat([Prefix, Suffix], '/', TapName),
    join(['/usr/local/Library/Taps/', Prefix, '-', Suffix], Path),
    isdir(Path).

meet(P, osx) :-
    brew_tap(P, TapName), !,
    bash(['brew tap ', TapName]).
