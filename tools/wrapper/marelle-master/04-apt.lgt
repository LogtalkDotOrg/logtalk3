:- object('04-apt').

:- uses(user,[is_list/1]).

:- uses('00-util',[isfile/1]).

:- uses(marelle,[join/2,sh/1]).

:- uses(sudo,[sudo_or_empty/1]).

:- use_module(apply,[maplist/2]).

:- public[install_apt/1].

:- private([apt_updated/0]).

:- discontiguous met/2.

:- discontiguous meet/2.

% before the directive:
% :- multifile installs_with_apt/1
% add the directive:

:- public installs_with_apt/1.

% before the directive:
% :- multifile installs_with_apt/2
% add the directive:

:- public installs_with_apt/2.

% before the directive:
% :- multifile installs_with_apt/3
% add the directive:

:- public installs_with_apt/3.

:- include('04-apt.pl').

:- end_object.

