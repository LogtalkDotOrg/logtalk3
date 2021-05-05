:- object('07-managed').

% before the directive:
% :- multifile managed_pkg/1
% add the directive:

:- public managed_pkg/1.

:- include('07-managed.pl').

:- end_object.

