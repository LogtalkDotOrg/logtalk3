:- object('08-pacman').

:- uses(marelle,[sh/1]).

:- uses(sudo,[sudo_or_empty/1]).

:- private([pacman_updated/0]).

:- discontiguous met/2.

:- discontiguous meet/2.

% before the directive:
% :- multifile installs_with_pacman/1
% add the directive:

:- public installs_with_pacman/1.

% before the directive:
% :- multifile installs_with_pacman/2
% add the directive:

:- public installs_with_pacman/2.

:- include('08-pacman.pl').

:- end_object.

