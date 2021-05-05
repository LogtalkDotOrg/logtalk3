:- object('00-util').

:- uses(user,[exists_directory/1,exists_file/1,getenv/2]).

:- uses(marelle,[bash/1,join/2]).

:- public[expand_path/2,isdir/1].

:- include('00-util.pl').

:- end_object.

