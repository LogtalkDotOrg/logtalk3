:- object('03-homebrew').

:- uses(user,[atomic_list_concat/3]).

:- uses('00-util',[isdir/1]).

:- uses(marelle,[bash/1,install_brew/1,join/2]).

:- private([brew_updated/0]).

:- discontiguous meet/2.

:- discontiguous met/2.

:- discontiguous pkg/1.

% before the directive:
% :- multifile installs_with_brew/1
% add the directive:

:- public installs_with_brew/1.

% before the directive:
% :- multifile installs_with_brew/2
% add the directive:

:- public installs_with_brew/2.

% before the directive:
% :- multifile brew_tap/2
% add the directive:

:- public brew_tap/2.

:- include('03-homebrew.pl').

:- end_object.

