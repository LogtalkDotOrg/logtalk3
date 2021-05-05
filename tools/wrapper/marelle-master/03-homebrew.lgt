:- object('03-homebrew').

:- uses('00-util',[isdir/1]).

:- uses(marelle,[join/2,sh/1]).

:- private([brew_updated/0]).

:- discontiguous meet/2.

:- discontiguous met/2.

:- discontiguous pkg/1.

:- discontiguous depends/3.

% before the directive:
% :- multifile installs_with_brew/1
% add the directive:

:- public installs_with_brew/1.

% before the directive:
% :- multifile installs_with_brew/2
% add the directive:

:- public installs_with_brew/2.

% before the directive:
% :- multifile installs_with_brew/3
% add the directive:

:- public installs_with_brew/3.

% before the directive:
% :- multifile brew_tap/2
% add the directive:

:- public brew_tap/2.

% before the directive:
% :- multifile installs_with_brew_cask/1
% add the directive:

:- public installs_with_brew_cask/1.

% before the directive:
% :- multifile installs_with_brew_cask/2
% add the directive:

:- public installs_with_brew_cask/2.

% before the directive:
% :- multifile installs_with_brew_cask/3
% add the directive:

:- public installs_with_brew_cask/3.

% before the directive:
% :- multifile cask_pkg/1
% add the directive:

:- public cask_pkg/1.

% before the directive:
% :- multifile cask_pkg/2
% add the directive:

:- public cask_pkg/2.

:- include('03-homebrew.pl').

:- end_object.

