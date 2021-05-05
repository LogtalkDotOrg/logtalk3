:- object(marelle).

:- uses(user,[atomic_list_concat/2,downcase_atom/2,exists_directory/1,expand_file_name/2,getenv/2,is_list/1,length/2,load_files/1,prolog/0,shell/2,tmp_file/2,wildcard_match/2,writeln/1]).

:- uses('00-util',[expand_path/2]).

:- use_module(apply,[exclude/3,include/3,maplist/2,maplist/3]).

:- use_module(lists,[append/3,flatten/2,list_to_set/2,member/2]).

:- public[join/2,join_if_list/2,sh/1,sh_output/2,which/2].

:- private([already_met/1,marelle_has_been_updated/0,platform/1]).

:- discontiguous pkg/1.

:- discontiguous met/2.

% before the directive:
% :- multifile pkg/1
% add the directive:

:- public pkg/1.

% before the directive:
% :- multifile meet/2
% add the directive:

:- public meet/2.

% before the directive:
% :- multifile met/2
% add the directive:

:- public met/2.

% before the directive:
% :- multifile depends/3
% add the directive:

:- public depends/3.

% before the directive:
% :- multifile command_pkg/1
% add the directive:

:- public command_pkg/1.

% before the directive:
% :- multifile command_pkg/2
% add the directive:

:- public command_pkg/2.

:- include('marelle.pl').

:- end_object.

