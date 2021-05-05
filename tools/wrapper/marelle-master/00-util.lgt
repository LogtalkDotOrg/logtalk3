:- object('00-util').

:- uses(user,[atomic_list_concat/3,exists_directory/1,exists_file/1,getenv/2,length/2]).

:- uses(marelle,[join/2,sh/1]).

:- public([expand_path/2,isdir/1,isfile/1]).

:- include('00-util.pl').

:- end_object.

