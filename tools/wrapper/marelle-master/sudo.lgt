:- object(sudo).

:- uses(user,[delete_file/1,tell/1,tmp_file_stream/3]).

:- uses('00-util',[expand_path/2]).

:- uses(marelle,[join/2,join_if_list/2,sh/1,sh_output/2,which/2]).

:- public[sudo_or_empty/1].

:- include('sudo.pl').

:- end_object.

