:- object('05-git').

:- uses('00-util',[expand_path/2,isdir/1]).

:- uses(marelle,[join/2,sh/1]).

% before the directive:
% :- multifile git_step/3
% add the directive:

:- public git_step/3.

:- include('05-git.pl').

:- end_object.

