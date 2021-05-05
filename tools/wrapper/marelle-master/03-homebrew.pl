%
%  03-homebrew.pl
%  marelle-deps
%
%  Helpers for working with Homebrew.
%  http://mxcl.github.io/homebrew/
%

command_pkg(brew).

meet(brew, osx) :-
    sh('ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"').

% installs_with_brew(Pkg).
%  Pkg installs with homebrew package of same name.
:- multifile installs_with_brew/1.

% installs_with_brew(Pkg, BrewName).
%  Pkg installs with homebrew package called BrewName.
:- multifile installs_with_brew/2.

% installs_with_brew(Pkg, BrewName, Options).
%  Pkg installs with homebrew package called BrewName and with Options.
:- multifile installs_with_brew/3.

installs_with_brew(P, P) :- installs_with_brew(P).
installs_with_brew(P, N, '') :- installs_with_brew(P, N).

depends(P, osx, [brew, 'brew-update']) :- installs_with_brew(P, _).

:- dynamic brew_updated/0.

pkg('brew-update').
met('brew-update', osx) :- brew_updated.
meet('brew-update', osx) :-
    sh('brew update'),
    assertz(brew_updated).

met(P, osx) :-
    installs_with_brew(P, PkgName, _), !,
    join(['/usr/local/Cellar/', PkgName], Dir),
    isdir(Dir).

meet(P, osx) :-
    installs_with_brew(P, PkgName, Options), !,
    install_brew(PkgName, Options).

install_brew(Name, Options) :-
    sh(['brew install ', Name, ' ', Options]).

% brew_tap(P, TapName).
%   An extra set of Homebrew packages.
:- multifile brew_tap/2.

% taps are targets
pkg(P) :- brew_tap(P, _).

met(P, osx) :-
    brew_tap(P, TapName), !,
    join(['/usr/local/Library/Taps/', TapName], Path),
    isdir(Path).

meet(P, osx) :-
    brew_tap(P, TapName), !,
    sh(['brew tap ', TapName]).


brew_tap('brew-cask-tap', 'caskroom/homebrew-cask').
pkg('brew-cask').
depends('brew-cask', osx, ['brew-cask-tap']).
installs_with_brew('brew-cask').

pkg('brew-cask-configured').
depends('brew-cask-configured', osx, ['brew-cask']).
met('brew-cask-configured', osx) :- isdir('/opt/homebrew-cask/Caskroom').
meet('brew-cask-configured', osx) :- sh('brew cask').

% installs_with_brew_cask(Pkg).
%  Pkg installs with homebrew-cask package of same name.
:- multifile installs_with_brew_cask/1.

% installs_with_brew_cask(Pkg, BrewName).
%  Pkg installs with homebrew-cask package called BrewName.
:- multifile installs_with_brew_cask/2.

% installs_with_brew_cask(Pkg, BrewName, Options).
%  Pkg installs with homebrew-cask package called BrewName and with Options.
:- multifile installs_with_brew_cask/3.

:- multifile cask_pkg/1.

:- multifile cask_pkg/2.

cask_pkg(P, P) :- cask_pkg(P).
pkg(P) :- cask_pkg(P, _).
installs_with_brew_cask(P, BrewName) :- cask_pkg(P, BrewName).
installs_with_brew_cask(P, P) :- installs_with_brew_cask(P).
installs_with_brew_cask(P, N, '') :- installs_with_brew_cask(P, N).
depends(P, osx, ['brew-cask-configured', 'brew-update']) :- cask_pkg(P, _).

met(P, osx) :-
    installs_with_brew_cask(P, PkgName, _), !,
    join(['/opt/homebrew-cask/Caskroom/', PkgName], Dir),
    isdir(Dir).

meet(P, osx) :-
    installs_with_brew_cask(P, PkgName, Options), !,
    install_brew_cask(PkgName, Options).

install_brew_cask(Name, Options) :-
    sh(['brew cask install ', Name, ' ', Options]).
