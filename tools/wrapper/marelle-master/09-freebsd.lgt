:- object('09-freebsd').

:- uses(marelle,[sh/1]).

:- uses(sudo,[sudo_or_empty/1]).

% before the directive:
% :- multifile installs_with_pkgng/1
% add the directive:

:- public installs_with_pkgng/1.

% before the directive:
% :- multifile installs_with_pkgng/2
% add the directive:

:- public installs_with_pkgng/2.

% before the directive:
% :- multifile installs_with_ports/2
% add the directive:

:- public installs_with_ports/2.

% before the directive:
% :- multifile installs_with_ports/3
% add the directive:

:- public installs_with_ports/3.

:- include('09-freebsd.pl').

:- end_object.

