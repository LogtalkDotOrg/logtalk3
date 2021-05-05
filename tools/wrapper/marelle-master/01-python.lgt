:- object('01-python').

:- uses(user,[access_file/2,writeln/1]).

:- uses('04-apt',[install_apt/1]).

:- uses(marelle,[join/2,sh/1,which/2]).

:- discontiguous pkg/1.

:- discontiguous depends/3.

:- discontiguous met/2.

:- discontiguous meet/2.

% before the directive:
% :- multifile python_pkg/1
% add the directive:

:- public python_pkg/1.

% before the directive:
% :- multifile python_pkg/2
% add the directive:

:- public python_pkg/2.

% before the directive:
% :- multifile pip_pkg/1
% add the directive:

:- public pip_pkg/1.

% before the directive:
% :- multifile pip_pkg/2
% add the directive:

:- public pip_pkg/2.

% before the directive:
% :- multifile pip_pkg/3
% add the directive:

:- public pip_pkg/3.

% before the directive:
% :- multifile installs_with_pip/2
% add the directive:

:- public installs_with_pip/2.

% before the directive:
% :- multifile installs_with_pip/1
% add the directive:

:- public installs_with_pip/1.

:- include('01-python.pl').

:- end_object.

