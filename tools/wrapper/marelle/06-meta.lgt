:- object('06-meta').

:- use_module(apply,[maplist/2]).

% before the directive:
% :- multifile meta_pkg/2
% add the directive:

:- public meta_pkg/2.

:- include('06-meta.pl').

:- end_object.

