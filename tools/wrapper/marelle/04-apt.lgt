:- object('04-apt').

:- uses(user,[is_list/1]).

:- uses(marelle,[bash/1,install_apt/1,join/2]).

:- use_module(apply,[maplist/2]).

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

