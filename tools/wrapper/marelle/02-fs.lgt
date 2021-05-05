:- object('02-fs').

:- uses(user,[read_link/3]).

:- uses('00-util',[expand_path/2]).

:- uses(marelle,[bash/1,join/2]).

% before the directive:
% :- multifile symlink_step/3
% add the directive:

:- public symlink_step/3.

:- include('02-fs.pl').

:- end_object.

